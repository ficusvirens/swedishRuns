library(lubridate)
library(data.table)
library(sf)
library(tidyverse)
library(mapview)
library(rgeos)
multiLayer=TRUE

source("functions.r")

load('rdata/se.carbon.soil.meteo.preles.biomass.gv.PRIME.RData')
load('rdata/SWE.par.tair.vpd.precip.RData')

vPREBAS <- "v0.2.x"   #### choose PREBAS verson to run the model  "master"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)


####################climate##########################
####################start##########################
# these are the meteo stations that are relevant to forests
meteo.id<-sort(unique(cu$meteo.id))
cu$Year<-as.numeric(substr(cu$id,1,4))
# as.numeric(as.factor(sort(unique(cu$meteo.id))))
# this is the number of the used meteo stations
nclim<-length(meteo.id)

# create matrices for weather data for 10 years
PAR<-matrix(NA,nrow = nclim, ncol=3650)
TAir<-matrix(NA,nrow = nclim,ncol=3650)
Precip<-matrix(NA,nrow=nclim,ncol=3650)
VPD<-matrix(NA,nrow = nclim,ncol=3650)
CO2<-matrix(380,nrow=nclim,ncol=3650)

meteodata<-SWE.par.tair.vpd.precip[[1]]

for (i in 2:151){
  meteodata<-rbind(meteodata,SWE.par.tair.vpd.precip[[i]])
}
summary(meteodata)
meteodata$year<-as.numeric(substr(meteodata$date,1,4))

for (i in 1:nclim) {
  # select meteodata for meteo station i
  d1<-meteodata[which(meteodata$id==meteo.id[i]),]
  d1$year[which.min(abs(as.numeric(d1$year)-1993))]
  # this picks meteo data for the year that's closest to year 1993
  d2<-d1[which(d1$year==d1$year[which.min(abs(as.numeric(d1$year)-1993))]),]
  # and here we repeat the weather data for that year ten times for each variable
  PAR[i,]<-rep(d2$par_mjm2day[1:365],10)*1e6/2.35/1e5 
  ###'the energy content of solar radiation in the PAR waveband is 2.35 x 10^5 J/mol'
  TAir[i,]<-rep(d2$DTT.mean[1:365],10)
  Precip[i,]<-rep(d2$DRR.sum[1:365],10)
  VPD[i,]<-rep(d2$vpd_kpa[1:365],10)
}


### find and fix the stations with annual precipitation of 300 or less ###

# annual precipitations
Precip_annual <- rowSums(Precip, na.rm=T)/10

# in which stations the annual precipitation is under 300
bad_stations <- which(Precip_annual < 300)

good_stations <- which(Precip_annual >= 300)

# to get the coordinates of the sites
st_data <- meteodata[c("id", "lat", "long")]
st_coord <- unique(st_data)

# put the stations on map
stations <- st_as_sf(st_coord, coords=c("long", "lat"))  %>%
  st_set_crs(4326)

# remove the stations that are not in meteo.id
stations <- stations[stations$id %in% meteo.id,]
stations <- arrange(stations, by=id)
# check the stations out
mapview(stations)

bst <- stations[bad_stations,]
gst <- stations[good_stations,] 

# setup for distance calculation
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs <- CRS(sprintf(utmStr, 32))
gstUTM <- st_transform(gst, crs)
bstUTM <- st_transform(bst, crs)

## Set up containers for results
n <- nrow(bstUTM)
nearestGST <- numeric(n)
distToNearestGST <- numeric(n)

## For each point, find name of nearest station
for (i in seq_along(nearestGST)) {
  gDists <- st_distance(bstUTM[i,], gstUTM, byid=TRUE)
  nearestGST[i] <- gstUTM$id[which.min(gDists)]
  distToNearestGST[i] <- min(gDists)
}

bst$replacing_station <- nearestGST

# meteo.id.2 is a vector where the stations with unrealistic precipitation
# data are replaced with the nearest neighbor
meteo.id.2 <- meteo.id

for (i in 1:nclim) {
  j <- which(bst$id==meteo.id[i]) 
  if (length(j)!=0) meteo.id.2[i] <- bst$replacing_station[j]  
}

# and now we get the precipitation data again with the new stations
for (i in 1:nclim) {
  # select meteodata for meteo station i
  d1<-meteodata[which(meteodata$id==meteo.id.2[i]),]
  # ????
  d1$year[which.min(abs(as.numeric(d1$year)-1993))]
  # this picks meteo data for the year that's closest to year 1993
  d2<-d1[which(d1$year==d1$year[which.min(abs(as.numeric(d1$year)-1993))]),]
  # and here we repeat the weather data for year 1989 ten times for each variable
  PAR[i,]<-rep(d2$par_mjm2day[1:365],10)*1e6/2.35/1e5 
  ###'the energy content of solar radiation in the PAR waveband is 2.35 x 10^5 J/mol'
  TAir[i,]<-rep(d2$DTT.mean[1:365],10)
  Precip[i,]<-rep(d2$DRR.sum[1:365],10)
  VPD[i,]<-rep(d2$vpd_kpa[1:365],10)
}


# save(PAR,file = 'PAR.rdata')
# save(TAir,file = 'TAir.rdata')
# save(VPD,file = 'VPD.rdata')
# save(Precip,file = 'Precip.rdata')
# save(CO2,file = 'CO2.rdata')

# plot(PAR[1,1:365])
#################### Climate end##########################

####################SiteInfo start##########################
FI<-read.csv('input/up1380xredigerad.csv')
FI$ID<-as.numeric(paste(FI$taxar,FI$traktnr,FI$palslagnr,FI$delytanr,sep = ''))
Initial<-cu
nSites<-nrow(Initial)
cu$SIpine<-NA
for (i in 1:nSites ) {
  cu$SIpine[i]<-FI$SiPine[which(FI$ID==cu$id[i])[1]]
}

summary(cu$SIpine)


siteInfo<- data.frame(siteID=c(1:nrow(cu)),
                              climID=as.numeric(as.factor(Initial$meteo.id)),
                              siteType=rep(NA,nSites),
                              SWinit=rep(200,nSites),
                              CWinit=rep(0,nSites),
                              SOGinit=rep(0,nSites),
                              Sinit =rep(0,nSites),
                               nLayers =rep(1,nSites),
                               nSpecies=rep(1,nSites),
                               Dsoil=rep(413,nSites),
                               FC=rep(0.450,nSites),
                               WP=rep(0.118,nSites)
                                    )
siteInfo$siteType[which(cu$SIpine>=26)]<-1
siteInfo$siteType[which(cu$SIpine<26&cu$SIpine>=20)]<-2
siteInfo$siteType[which(cu$SIpine<20&cu$SIpine>=14)]<-3
siteInfo$siteType[which(cu$SIpine<14&cu$SIpine>=8)]<-4

summary(siteInfo)
if(multiLayer){
  siteInfo$nLayers <- 3
  siteInfo$nSpecies <- 3
}
# save(siteInfo,file = 'siteInfo.rdata')
####################SiteInfo end##########################


####################MultInitVar start##########################
maxNlayers<-1
multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
multiInitVar[,1,1]<- Initial$sp.do
multiInitVar[,2,1]<-   as.integer(Initial$age)
multiInitVar[,3,1]<-   Initial$h_mbaw#-Initial$hgrowth5y_m
multiInitVar[,4,1]<-   Initial$d_cm_baw#-Initial$dgrowth5y_cm
multiInitVar[,5,1]<-  Initial$ba_m2ha#-Initial$bagrowth5y_m2ha
multiInitVar[,6,1]<-   NA
multiInitVar[,7,1]<-   0.01
summary(multiInitVar[,,1])
# save(multiInitVar,file = 'multiInitVar.rdata')
if(multiLayer){
  Initial <- data.table(Initial)
  maxNlayers<-3
  multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
  multiInitVar[,1,]<- matrix(1:3,nSites,3,byrow = T)
  multiInitVar[,2,] <- as.integer(Initial$age)
  multiInitVar[,3,] <-   Initial$h_mbaw#-Initial$hgrowth5y_m
  multiInitVar[,4,] <-   Initial$d_cm_baw#-Initial$dgrowth5y_cm
  Initial[is.na(needles.pine), needles.pine:=0]
  Initial[is.na(needles.spruce), needles.spruce:=0]
  Initial[is.na(leaves.rep.decid), leaves.rep.decid:=0]
  multiInitVar[,5,1]<-  Initial$ba_m2ha * Initial$needles.pine/
    (Initial$needles.pine +Initial$needles.spruce + Initial$leaves.rep.decid)
  multiInitVar[,5,2]<-  Initial$ba_m2ha * Initial$needles.spruce/
    (Initial$needles.pine +Initial$needles.spruce + Initial$leaves.rep.decid)
  multiInitVar[,5,3]<-  Initial$ba_m2ha * Initial$leaves.rep.decid/
    (Initial$needles.pine +Initial$needles.spruce + Initial$leaves.rep.decid)
  multiInitVar[,6,]<-   NA
  multiInitVar[,7,]<-   NA
  # summary(multiInitVar[,,1])
}
# exclude the sites where BA, h and d are 0

siteX <- which(apply(multiInitVar[,2,],1,sum,na.rm=T)>0 & 
                 apply(multiInitVar[,3,],1,sum,na.rm=T)>0 & 
                 apply(multiInitVar[,4,],1,sum,na.rm=T) > 0 &
                 apply(multiInitVar[,5,],1,sum,na.rm=T)>0)
InitialX <- Initial[siteX,]
siteInfoX<-siteInfo[siteX,]
multiInitVarX<-multiInitVar[siteX,,]

####use old model hc
source("oldHcMod.r")
inHc_p <- createInputsHc(multiInitVarX,1,1)
inHc_sp <- createInputsHc(multiInitVarX,2,2)
inHc_d <- createInputsHc(multiInitVarX,3,3)
multiInitVarX[,6,1] <- apply(inHc_p,1,HcModOld)
multiInitVarX[,6,2] <- apply(inHc_sp,1,HcModOld)
multiInitVarX[,6,3] <- apply(inHc_d,1,HcModOld)


# multiInitVar2 <- array(0,dim=c(nrow(siteInfo),7,maxNlayers))
# multiInitVar2[,,1]<-multiInitVar0
nYears<- rep(5,nrow(siteInfoX))
library(Rprebasso)

# replace NA values in weather data with mean values
for (i in 1:3650) {
  PAR[which(is.na(PAR[,i])),i]<-  mean(PAR[which(!is.na(PAR[,i])),i])
  TAir[which(is.na(TAir[,i])),i]<-  mean(TAir[which(!is.na(TAir[,i])),i])
  Precip[which(is.na(Precip[,i])),i]<-  mean(Precip[which(!is.na(Precip[,i])),i])
  VPD[which(is.na(VPD[,i])),i]<-  mean(VPD[which(!is.na(VPD[,i])),i])
}

initPrebas <- InitMultiSite(nYearsMS = nYears,
                            siteInfo=siteInfoX,
                            # pCROBAS = pCrobas, #soil information haven't been considered
                            # litterSize = litterSize,
                            # pAWEN = parsAWEN,
                            defaultThin=0.,
                            ClCut = 0.,
                            multiInitVar = multiInitVarX,
                            # multiInitVar = multiInitVar2,
                            PAR = PAR,
                            TAir= TAir,
                            VPD= VPD,
                            Precip= Precip,
                            CO2= CO2,
                            yassoRun = 0.
                            # lukeRuns = 0
                            # initCLcutRatio = initCLcutRatio
                            # multiThin = multiThin,
                            # multiNthin = multiNthin
)

load("rdata/regions.rdata")
sitesR1 <- regions[[1]]
sitesR2 <- regions[[2]]
sitesR3 <- regions[[3]]
sitesR4 <- regions[[4]]
initPrebasR1 <- subSetInitPrebas(sitesR1,defaultThin = 1,ClCut = 1)
initPrebasR2 <- subSetInitPrebas(sitesR2,defaultThin = 1,ClCut = 1)
initPrebasR3 <- subSetInitPrebas(sitesR3,defaultThin = 1,ClCut = 1)
initPrebasR4 <- subSetInitPrebas(sitesR4,defaultThin = 1,ClCut = 1)


H<-   Initial$h_mbaw[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
D<-   Initial$d_cm_baw[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
BA<-  Initial$ba_m2ha[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
obs <- list(H=H,D=D,BA=BA)


save(obs,initPrebas,
     initPrebasR1,sitesR1,
     initPrebasR2,sitesR2,
     initPrebasR3,sitesR3,
     initPrebasR4,sitesR4,
     file="rdata/initPrebas.rdata")

