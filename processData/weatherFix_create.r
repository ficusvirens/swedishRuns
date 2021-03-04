
load("rdata/weather30.rdata")

# create matrices for weather data for 100 years
PAR_100<-matrix(NA,nrow = nclim, ncol=36500)
TAir_100<-matrix(NA,nrow = nclim,ncol=36500)
Precip_100<-matrix(NA,nrow=nclim,ncol=36500)
VPD_100<-matrix(NA,nrow = nclim,ncol=36500)
CO2_100<-matrix(380,nrow=nclim,ncol=36500)

n <- 0:29

randlist <- sample(n, 70, replace=T)

# first 30 years we can take as it is
PAR_150[,1:10950] <- PAR
TAir_150[,1:10950] <- TAir
Precip_150[,1:10950] <- Precip
VPD_150[,1:10950] <- VPD

index <- 10951

for (i in 1:70) {
  PAR_100[,index:(index+364)] <- PAR[,(randlist[i]*365+1):(randlist[i]*365+365)]
  TAir_100[,index:(index+364)] <- TAir[,(randlist[i]*365+1):(randlist[i]*365+365)]
  Precip_100[,index:(index+364)] <- Precip[,(randlist[i]*365+1):(randlist[i]*365+365)]
  VPD_100[,index:(index+364)] <- VPD[,(randlist[i]*365+1):(randlist[i]*365+365)]
  
  index <- index+365
}

PAR <- PAR_100
TAir <- TAir_100
Precip <- Precip_100
VPD <- VPD_100
CO2 <- CO2_100

save(PAR, TAir, Precip, VPD, CO2, file="rdata/weather100.rdata")

#----------- for 150 years ----------------


load("rdata/weather30.rdata")

load('rdata/SWE.par.tair.vpd.precip.RData')

# these are the meteo stations that are relevant to forests
meteo.id<-sort(unique(cu$meteo.id))
cu$Year<-as.numeric(substr(cu$id,1,4))
# this is the number of the used meteo stations
nclim<-length(meteo.id)

# create matrices for weather data for 150 years
PAR_150<-matrix(NA,nrow = nclim, ncol=54750)
TAir_150<-matrix(NA,nrow = nclim,ncol=54750)
Precip_150<-matrix(NA,nrow=nclim,ncol=54750)
VPD_150<-matrix(NA,nrow = nclim,ncol=54750)
CO2_150<-matrix(380,nrow=nclim,ncol=54750)

n <- 0:29

# 120 = 150-30
randlist <- sample(n, 120, replace=T)

# first 30 years we can take as it is
PAR_150[,1:10950] <- PAR
TAir_150[,1:10950] <- TAir
Precip_150[,1:10950] <- Precip
VPD_150[,1:10950] <- VPD

index <- 10951

for (i in 1:120) {
  PAR_150[,index:(index+364)] <- PAR[,(randlist[i]*365+1):(randlist[i]*365+365)]
  TAir_150[,index:(index+364)] <- TAir[,(randlist[i]*365+1):(randlist[i]*365+365)]
  Precip_150[,index:(index+364)] <- Precip[,(randlist[i]*365+1):(randlist[i]*365+365)]
  VPD_150[,index:(index+364)] <- VPD[,(randlist[i]*365+1):(randlist[i]*365+365)]
  
  index <- index+365
}

PAR <- PAR_150
TAir <- TAir_150
Precip <- Precip_150
VPD <- VPD_150
CO2 <- CO2_150

save(PAR, TAir, Precip, VPD, CO2, file="rdata/weather150.rdata")
