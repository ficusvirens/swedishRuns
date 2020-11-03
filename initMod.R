library(lubridate)
library(data.table)
library(sf)
library(tidyverse)
library(mapview)
library(rgeos)
multiLayer=TRUE

#load('rdata/se.carbon.soil.meteo.preles.biomass.gv.PRIME.RData')
#load('rdata/SWE.par.tair.vpd.precip.RData')

#to remove Rprebasso:
#remove.packages("Rprebasso")
#detach("package:Rprebasso")
vPREBAS <- "v0.2.x"   #### choose PREBAS verson to run the model  "master"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

#devtools::install_github("ForModLabUHel/Rprebasso", ref="v0.2x_gvW")

source("functions.r")

load('rdata/se.carbon.soil.meteo.preles.biomass.gv.PRIME.RData')
load('rdata/SWE.par.tair.vpd.precip.RData')
# this loads the weather inputs 
load("rdata/weather.rdata")

####################SiteInfo start##########################
cu$Year<-as.numeric(substr(cu$id,1,4))
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
siteInfo$siteType[which(cu$SIpine>=26)]<-2
siteInfo$siteType[which(cu$SIpine<26&cu$SIpine>=20)]<-3
siteInfo$siteType[which(cu$SIpine<20&cu$SIpine>=14)]<-4
siteInfo$siteType[which(cu$SIpine<14&cu$SIpine>=8)]<-5

# test?
Initial$siteType[which(cu$SIpine<20&cu$SIpine>=14)]<-4
Initial$siteType[which(cu$SIpine<14&cu$SIpine>=8)]<-5
Initial$siteType[which(cu$SIpine>=26)]<-2
Initial$siteType[which(cu$SIpine<26&cu$SIpine>=20)]<-3

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

initPrebas <- InitMultiSite(nYearsMS = nYears,
                            siteInfo=siteInfoX,
                            # pCROBAS = pCrobas, #soil information haven't been considered
                            # litterSize = litterSize,
                            # pAWEN = parsAWEN,
                            #defaultThin=0.,
                            #ClCut = 0.,
                            multiInitVar = multiInitVarX,
                            # multiInitVar = multiInitVar2,
                            PAR = PAR,
                            TAir= TAir,
                            VPD= VPD,
                            Precip= Precip,
                            CO2= CO2
                            #yassoRun = 0.
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

source("countSites.r")
initPrebas_svea <- list()
for(i in 1:3) {
  initPrebas_svea[[i]] <- subSetInitPrebas(svea[[i]],defaultThin = 1,ClCut = 1)
}

initPrebas_got <- list()
for(i in 1:3) {
  initPrebas_got[[i]] <- subSetInitPrebas(got[[i]],defaultThin = 1,ClCut = 1)
}

initPrebas_sn <- list()
for(i in 1:3) {
  initPrebas_sn[[i]] <- subSetInitPrebas(sn[[i]],defaultThin = 1,ClCut = 1)
}

initPrebas_nn <- list()
for(i in 1:3) {
  initPrebas_nn[[i]] <- subSetInitPrebas(nn[[i]],defaultThin = 1,ClCut = 1)
}


H<-   Initial$h_mbaw[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
D<-   Initial$d_cm_baw[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
BA<-  Initial$ba_m2ha[which(multiInitVar[,2,1]>0&multiInitVar[,3,1]>0&multiInitVar[,4,1]>0&multiInitVar[,5,1]>0)]
obs <- list(H=H,D=D,BA=BA)


save(obs,initPrebas,
     initPrebasR1,sitesR1,
     initPrebasR2,sitesR2,
     initPrebasR3,sitesR3,
     initPrebasR4,sitesR4,
     initPrebas_svea, initPrebas_got, initPrebas_nn, initPrebas_sn,
     
     file="rdata/initPrebas.rdata")

