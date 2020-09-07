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


# counts the soil carbon in steady state
countSoilC <- function(prebas_output, species) {
  
  meansLit <- colMeans(prebas_output$multiOut[,1,26:29,,1]) ###Calculate the mean for the first year
  nSp <- length(species)
  litForStst <- matrix(0,nSp,3)    ####object for litterfall at steady state
  weatherYasso = apply(prebas_output$weatherYasso,3,mean)
  Tmean <- weatherYasso[1]
  Precip <- weatherYasso[2]
  Tamp <- weatherYasso[3]
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
                             Precip = Precip,litterSize = litterSizeDef[3,species[ij]],litType = 1,pYasso = pYAS)
    soilC_fwLit <- StStYasso(litter = fwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                             Precip = Precip,litterSize = litterSizeDef[2,species[ij]],litType = 2,pYasso = pYAS)
    soilC_cwLit <- StStYasso(litter = cwlit,parsAWEN = parsAWEN,spec = species[ij],Tmean = Tmean,Tamp = Tamp,
                             Precip = Precip,litterSize = litterSizeDef[1,species[ij]],litType = 3,pYasso = pYAS)
    
    soilC_lit[ij,,] <- cbind(soilC_nwLit, soilC_fwLit, soilC_cwLit)
  }
  return(soilC_lit)
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