library(Rprebasso)

setwd("C:/LocalData/hssairol/R/swedishData/PREBAS DATA SW/PREBAS DATA SW")
load("output_sweden.rdata")


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
    
    soilC_lit[ij,,] <- data.table(cbind(soilC_nwLit, soilC_fwLit, soilC_cwLit))
  }
  return(soilC_lit)
}

species <- 1:3

soilC_svea <- countSoilC(output_svea, species)
soilC_got <- countSoilC(output_got, species)
soilC_nn <- countSoilC(output_nn, species)
soilC_sn <- countSoilC(output_sn, species)

