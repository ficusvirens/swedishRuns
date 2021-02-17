library(Rprebasso)

# initializes prebas for a subset area
subSetInitPrebas <- function(siteX,defaultThin=0.,ClCut=0.){
  PARx <- PAR[unique(siteInfo[siteX,2]),]
  TAirx <- TAir[unique(siteInfo[siteX,2]),]
  Precipx <- Precip[unique(siteInfo[siteX,2]),]
  VPDx <- VPD[unique(siteInfo[siteX,2]),]
  CO2x <- CO2[unique(siteInfo[siteX,2]),]
  siteInfox <- siteInfo[siteX,]
  siteInfox[,2] <- match(siteInfox[,2],unique(siteInfo[siteX,2]))
  
  initPrebasX <- InitMultiSite(nYearsMS = nYears[siteX],
                               siteInfo=siteInfox,
                               # pCROBAS = pCrobas, #soil information haven't been considered
                               # litterSize = litterSize,
                               # pAWEN = parsAWEN,
                               defaultThin=defaultThin,
                               ClCut = ClCut,
                               multiInitVar = multiInitVarX[siteX,,],
                               # multiInitVar = multiInitVar2,
                               PAR = PARx,
                               TAir= TAirx,
                               VPD= VPDx,
                               Precip= Precipx,
                               CO2= CO2x,
                               yassoRun = 0.
                               # lukeRuns = 0
                               # initCLcutRatio = initCLcutRatio
                               # multiThin = multiThin,
                               # multiNthin = multiNthin
  )
  return(initPrebasX)
}


# counts the soil carbon in steady state for region
countSoilC <- function(prebas_output, species, only1st = 1, gvrun = 1) {
  
  meansLit <- colMeans(prebas_output$multiOut[,1,26:29,,1]) ###Calculate the mean for the first year
  nSp <- length(species)
  litForStst <- matrix(0,nSp,3)    ####object for litterfall at steady state
  weatherYasso = apply(prebas_output$weatherYasso,3,mean)
  Tmean <- weatherYasso[1]
  Precip <- weatherYasso[2]
  Tamp <- weatherYasso[3]
  litterSize = prebas_output$litterSize
  
  soilC_lit <- array(NA,dim=c(nSp,5,3),dimnames = list(paste0("spec",1:nSp),
                                                       c("A", "W", "E", "N", "H"),
                                                       c("soilC_nwLit", "soilC_fwLit", "soilC_cwLit"))) # create object for yasso output
  
  for(ij in species){
    litForStst[ij,1] <- sum(meansLit[1:2,ij])   ####non woody littter (foliage + fine root)
    litForStst[ij,2] <- meansLit[3,ij]  ##fine woody litter (branches)
    litForStst[ij,3] <- meansLit[4,ij]  ###coarse woody litter (stems and stumps)
    nwlit <- litForStst[ij,1] ###non woody litter
    fwlit <- litForStst[ij,2] ##fine woody litter
    cwlit <- litForStst[ij,3] ###coarse woody litter
    ###litterSize 0, 2, 30 for nwlit, fwlit, cwlit, respectively
    ###litType 1,2,3 for nwlit, fwlit, cwlit, respectively
    soilC_nwLit <- StStYasso(litter = nwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                             Precip = Precip,litterSize = litterSize[3,species[ij]],litType = 1,pYasso = pYAS)
    soilC_fwLit <- StStYasso(litter = fwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                             Precip = Precip,litterSize = litterSize[2,species[ij]],litType = 2,pYasso = pYAS)
    soilC_cwLit <- StStYasso(litter = cwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                             Precip = Precip,litterSize = litterSize[1,species[ij]],litType = 3,pYasso = pYAS)
    
    soilC_lit[ij,,] <- cbind(soilC_nwLit, soilC_fwLit, soilC_cwLit)
  }
  
  if (gvrun == 1) {
    
    ###calculate steady state C for gv
    fAPAR <- prebas_output$fAPAR
    #   climIDs <- prebas_output$siteInfo[,2]
    ets <- prebas_output$multiOut[,,5,1,1]
    siteType <- prebas_output$multiOut[,,3,1,1]
    p0 <- prebas_output$multiOut[,,6,1,1]
    fAPAR[which(is.na(prebas_output$fAPAR),arr.ind = T)] <- 0.
    AWENgv <- array(NA,dim=c(dim(prebas_output$fAPAR),4))
    for(ij in 1:dim(fAPAR)[2]){
      AWENgv[,ij,] <- t(sapply(1:nrow(fAPAR), function(i) .Fortran("fAPARgv",fAPAR[i,ij],
                                                                   ets[i,ij],siteType[ij],
                                                                   0,0,p0[ij,1],rep(0,4))[[7]]))
    }
    if (only1st == 1) {
      AWENgv2 <- colMeans(AWENgv[,1,],na.rm = T)
      #    weatherYasso <- prebas_output$weatherYasso[,1,]
    } else {
      AWENgv2 <- apply(AWENgv,3,mean,na.rm=T)
      #    weatherYasso <- apply(prebas_output$weatherYasso,c(1,3), mean, na.rm=T)
    }
    AWENgv2[which(is.na(AWENgv2),arr.ind = T)] <- 0.
    
    ###calculate steady state soil C per GV
    # ststGV <- matrix(NA,nSites,5)
    #    ststGV <- t(sapply(1:dim(fAPAR)[1], function(ij) .Fortran("mod5c",
    #                                                       pYAS,1.,weatherYasso[climIDs[ij],],rep(0,5),
    #                                                       c(AWENgv2[ij,],0),litterSize=0,leac=0.,rep(0,5),stSt=1.)[[8]]))
    ststGV <- .Fortran("mod5c", pYAS,1.,weatherYasso,rep(0,5), c(AWENgv2,0),litterSize=0,leac=0.,rep(0,5),stSt=1.)[[8]]
    
  } else {
    ststGV = NA
  }
  
  
  return(list(treeLitter = soilC_lit, gvLitter = ststGV))
}



# counts the soil carbon in steady state site spesific
countSoilCstsp <- function(prebas_output, species, gvrun = 1,rotLength=NA, simLength=NA) {
  ### prebas_output = output object of PREBAS simulations
  ### species = vector of species, use same IDs of pCROBAS and yasso
  ### gvrun = 1 (default) to run the ground vegetation calculations
  ### simLength = vector of the length of the number of sites indicating the number
  #####of years to consider in the calculations. default is NA means that all years
  #####of the simulations are considered
  #### rotLength = vector of the site specific rotation length calculated by rotationLength function
  nSites <- prebas_output$nSites
  if(all(is.na(rotLength)) | all(is.na(simLength))) {
    rotFactor = rep(1, nSites)
  }
  else {
    rotFactor <- simLength/rotLength
  }
  
  nSp <- length(species)
  if(all(is.na(simLength))){
    meansLit <- apply(prebas_output$multiOut[,,26:29,,1], c(1,3,4), mean) ###Calculate the mean for the first year
  }else{
    meansLit <- array(NA,dim=c(nSites,4,nSp))
    for(i in 1:nSites){
      meansLit[i,,] <- apply(prebas_output$multiOut[i,1:simLength[i],26:29,,1], c(2,3), mean)*rotFactor[i] ###Calculate the mean for the first year
    }
  }
  ###GV calculations inititialization
  if (gvrun == 1) {
    ###calculate steady state C for gv
    fAPAR <- prebas_output$fAPAR
    #   climIDs <- prebas_output$siteInfo[,2]
    ets <- prebas_output$multiOut[,,5,1,1]
    siteType <- prebas_output$multiOut[,,3,1,1]
    p0 <- prebas_output$multiOut[,,6,1,1]
    fAPAR[which(is.na(prebas_output$fAPAR),arr.ind = T)] <- 0.
    AWENgv <- array(NA,dim=c(dim(prebas_output$fAPAR),4))
    for(ij in 1:dim(fAPAR)[2]){
      AWENgv[,ij,] <- t(sapply(1:nrow(fAPAR), function(i) .Fortran("fAPARgv",fAPAR[i,ij],
                                                                   ets[i,ij],siteType[ij],
                                                                   0,0,p0[ij,1],rep(0,4))[[7]]))
    }
    AWENgv2 <- apply(AWENgv,c(1,3),mean,na.rm=T)*rotFactor
    AWENgv2[which(is.na(AWENgv2),arr.ind = T)] <- 0.
  }
  soilC_lit <- array(NA,dim=c(prebas_output$nSites,nSp,5,3),dimnames = list(site=NULL,
                                                                            species=paste0("spec",1:nSp),
                                                                            AWENH= c("A", "W", "E", "N", "H"),
                                                                            litType=c("soilC_nwLit", "soilC_fwLit", "soilC_cwLit"))) # create object for yasso output trees
  ststGV <- array(NA,dim=c(nSites,5),dimnames = list(site=NULL,AWENH=c("A", "W", "E", "N", "H"))) # create object for yasso output GV
  for(siteX in 1:nSites){
    ###########################
    # litForStst <- matrix(0,nSp,3)    ####object for litterfall at steady state
    climID <- prebas_output$siteInfo[siteX,2]
    Tmean <- mean(prebas_output$weatherYasso[climID,,1])
    Precip <- mean(prebas_output$weatherYasso[climID,,2])
    Tamp <- mean(prebas_output$weatherYasso[climID,,3])
    litterSize <- prebas_output$litterSize
    for(ij in species){
      nwlit <- sum(meansLit[siteX,1:2,ij])   ####non woody litter (foliage + fine root)
      fwlit <- meansLit[siteX,3,ij]  ##fine woody litter (branches)
      cwlit <- meansLit[siteX,4,ij]  ###coarse woody litter (stems and stumps)
      ###litterSize 0, 2, 30 for nwlit, fwlit, cwlit, respectively
      ###litType 1,2,3 for nwlit, fwlit, cwlit, respectively
      soilC_nwLit <- StStYasso(litter = nwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                               Precip = Precip,litterSize = litterSize[3,species[ij]],litType = 1,pYasso = pYAS)
      soilC_fwLit <- StStYasso(litter = fwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                               Precip = Precip,litterSize = litterSize[2,species[ij]],litType = 2,pYasso = pYAS)
      soilC_cwLit <- StStYasso(litter = cwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                               Precip = Precip,litterSize = litterSize[1,species[ij]],litType = 3,pYasso = pYAS)
      soilC_lit[siteX,ij,,] <- cbind(soilC_nwLit, soilC_fwLit, soilC_cwLit)
      ###GV calculations
      if (gvrun == 1) {
        ststGV[siteX,] <- .Fortran("mod5c", pYAS,1.,c(Tmean,Precip,Tamp),rep(0,5), c(AWENgv2[siteX,],0),litterSize=0,leac=0.,rep(0,5),stSt=1.)[[8]]
      } else {
        ststGV = NA
      }
    }
  }
  return(list(treeLitter = soilC_lit, gvLitter = ststGV))
}


# find out the length of simulation period
simulationLength <- function(output) {
  sumBA <- apply(output$multiOut[,,13,,1], 1:2, sum)
  
  simlength <- vector()
  
  for(i in 1:nrow(sumBA)) {
    simlength[i] <- match(0, sumBA[i,])
  }
  
  return(simlength) 
}


# rotation length
rotationLength <- function(output, simLength) {
  # length = year of clearcut + age when the simulation starts
  rotlength <- simLength+output$multiInitVar[,2,1]
  return(rotlength)
}


# makes plots of the output
makePlots <- function(output,siteX=NULL){
  nSites<-dim(output)[1]
  nYears <- dim(output)[2]
  dim.H<-cbind(c(1:nSites),nYears,11,rep(1,nSites),rep(1,nSites))
  dim.D<-cbind(c(1:nSites),nYears,12,rep(1,nSites),rep(1,nSites))
  dim.B<-cbind(c(1:nSites),nYears,13,rep(1,nSites),rep(1,nSites))
  
  if(all(is.na(siteX))){
    H <- obs$H
    D <- obs$D
    BA <- obs$BA
  }else{
    H <- obs$H[siteX]
    D <- obs$D[siteX]
    BA <- obs$BA[siteX]
  }
  pH <- ggplot()+geom_point(aes(x=output[dim.H],y=H))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand Height(m)')+ylab('Oberserved stand Height')
  # ggsave(filename = paste('stand H .png'),height=15,width = 15,units = 'cm')
  
  pD <- ggplot()+geom_point(aes(output[dim.D],D))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand DBH(cm)')+ylab('Oberserved stand DBH')
  # ggsave(filename = paste('stand DBH .png'),height=15,width = 15,units = 'cm')
  
  pB <- ggplot()+geom_point(aes(output[dim.B],BA))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand basal area(m2/ha)')+ylab('Oberserved stand basal area')
  # ggsave(filename = paste('stand B.png'),height=15,width = 15,units = 'cm')
  # png("fit.png")
  allPlot <- ggarrange(pH,pD,pB)
  return(list(pH=pH,pD = pD,pB=pB,allPlot=allPlot))
  
}
# dev.off()
regionName <- function(region) {
  if(identical(region, mineral)) return("Sweden")
  else if(identical(region, got_m)) return("Götaland")
  else if(identical(region, svea_m)) return("Svealand")
  else if(identical(region, sn_m)) return("Södra Norrland")
  else if(identical(region, nn_m)) return("Norra Norrland")
}

# dev.off()