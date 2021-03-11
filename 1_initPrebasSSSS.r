
#to remove Rprebasso:
#remove.packages("Rprebasso")
#detach("package:Rprebasso")
source("settings.r")

litterSize <- litterSizeDef
litterSize[1,1:3] <- sizeCwoodyLit

load('rdata/se.carbon.soil.meteo.preles.biomass.gv.PRIME.RData')
#load('rdata/SWE.par.tair.vpd.precip.RData')
# this loads the weather inputs

load(weatherFile)

####################SiteInfo start##########################
cu$Year<-as.numeric(substr(cu$id,1,4))
FI<-read.csv('input/up1380xredigerad.csv')
FI$ID<-as.numeric(paste(FI$taxar,FI$traktnr,FI$palslagnr,FI$delytanr,sep = ''))


Initial<-cu
nSites<-nrow(cu)

Initial$SIpine<-NA
for (i in 1:nSites ) {
  Initial$SIpine[i]<-FI$SiPine[which(FI$ID==Initial$id[i])[1]]
}

summary(Initial$SIpine)


siteInfo<- data.frame(siteID=Initial$id,
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
siteInfo$siteType[which(Initial$SIpine>=26)]<-2
siteInfo$siteType[which(Initial$SIpine<26&Initial$SIpine>=20)]<-3
siteInfo$siteType[which(Initial$SIpine<20&Initial$SIpine>=14)]<-4
siteInfo$siteType[which(Initial$SIpine<14&Initial$SIpine>=8)]<-5

# test?
Initial$siteType[which(Initial$SIpine<20&Initial$SIpine>=14)]<-4
Initial$siteType[which(Initial$SIpine<14&Initial$SIpine>=8)]<-5
Initial$siteType[which(Initial$SIpine>=26)]<-2
Initial$siteType[which(Initial$SIpine<26&Initial$SIpine>=20)]<-3

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

####initialize from plantation
if(fromPlant){
  multiInitVar[,3,] <- 1.5 #H
  multiInitVar[,4,] <- .5  #D
  multiInitVar[,5,] <- multiInitVar[,5,]/apply(multiInitVar[,5,],1,sum) * 0.0431969  #B
  multiInitVar[,2,] <- 1   #age
}


# exclude the sites where BA, h and d are 0
siteX <- which(apply(multiInitVar[,2,],1,sum,na.rm=T)>0 & 
                 apply(multiInitVar[,3,],1,sum,na.rm=T)>0 & 
                 apply(multiInitVar[,4,],1,sum,na.rm=T) > 0 &
                 apply(multiInitVar[,5,],1,sum,na.rm=T)>0)



gotX <- intersect(Initial[siteX]$id, got_id)
sveaX <- intersect(Initial[siteX]$id, svea_id)
snX <- intersect(Initial[siteX]$id, sn_id)
nnX <- intersect(Initial[siteX]$id, nn_id)
swedenX <- intersect(Initial[siteX]$id, mineral_id)

got <- which(Initial$id %in% gotX)
svea <- which(Initial$id %in% sveaX)
sn <- which(Initial$id %in% snX)
nn <- which(Initial$id %in% nnX)
siteX <- which(Initial$id %in% swedenX)



###select just 100 sites for test runs 
if(testRun){
  # select 25 sites from each region
  siteX = c(got[1:25], svea[1:25], sn[1:25], nn[1:25])
} 

InitialX <- Initial[siteX,]
run_sites <- siteX

nSites <- nrow(Initial)
siteInfoX<-siteInfo[siteX,]
multiInitVarX<-multiInitVar[siteX,,]

####use old model hc
source("oldHcMod.r")
inHc_p <- createInputsHc(multiInitVar,1,1)
inHc_sp <- createInputsHc(multiInitVar,2,2)
inHc_d <- createInputsHc(multiInitVar,3,3)
multiInitVar[,6,1] <- apply(inHc_p,1,HcModOld)
multiInitVar[,6,2] <- apply(inHc_sp,1,HcModOld)
multiInitVar[,6,3] <- apply(inHc_d,1,HcModOld)

# run for simRuns years
nYears<- rep(simRuns,nrow(siteInfo))

# ###proc weather for test sites
# if(testRun){
#   climIDx <- sort(unique(siteInfoX[,2]))
#   siteInfoX[,2] <- match(siteInfoX[,2],climIDx)
#   PAR = PAR[climIDx,]
#   TAir= TAir[climIDx,]
#   VPD= VPD[climIDx,]
#   Precip= Precip[climIDx,]
#   CO2= CO2[climIDx,]
#   inHc_p <- createInputsHc(multiInitVarX,1,1)
#   inHc_sp <- createInputsHc(multiInitVarX,2,2)
#   inHc_d <- createInputsHc(multiInitVarX,3,3)
#   multiInitVarX[,6,1] <- apply(inHc_p,1,HcModOld)
#   multiInitVarX[,6,2] <- apply(inHc_sp,1,HcModOld)
#   multiInitVarX[,6,3] <- apply(inHc_d,1,HcModOld)
#   siteInfo <- siteInfoX
#   multiInitVar <- multiInitVarX
# } 

initPrebas <- subSetInitPrebas(siteX,defaultThin = def_thin,ClCut = cl_cut)



# initPrebas <- InitMultiSite(nYearsMS = nYears,
#                             siteInfo=siteInfo,
#                             # pCROBAS = pCrobas, #soil information haven't been considered
#                             litterSize = litterSize,
#                             # pAWEN = parsAWEN,
#                             defaultThin=def_thin,
#                             ClCut = cl_cut,
#                             multiInitVar = multiInitVar,
#                             # multiInitVar = multiInitVar2,
#                             PAR = PAR,
#                             TAir= TAir,
#                             VPD= VPD,
#                             Precip= Precip,
#                             CO2= CO2
#                             #yassoRun = 0.
#                             # lukeRuns = 0
#                             # initCLcutRatio = initCLcutRatio
#                             # multiThin = multiThin,
#                             # multiNthin = multiNthin
# )

#####assign the age at the beginning of rotation
if(fromPlant){
  initPrebas$multiInitVar[,2,1] <- initPrebas$multiInitVar[,2,2] <- 
    initPrebas$multiInitVar[,2,3] <- round(6 + 2* initPrebas$siteInfo[,3] - 
                                             0.005*rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]] + 2.25)
}

save(Initial, run_sites, file="rdata/runs/Initial.rdata")

#save(initPrebas,file = "rdata/runs/initPrebas.rdata")


