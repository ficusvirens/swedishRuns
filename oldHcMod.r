###parameters old hc model
#speciesNam<- c("pine","spruce","decid")
speciesNam<- c("pisy", "piab", "beal", "fasy", "pipi", "eugl", "rops")
pHcM <- matrix(NA,6,7,dimnames = list(NULL,speciesNam))
###parameters Hcmodel pisy
pHcM[,1] <- c(-1.67918379,	1.16550784,	-0.23744806, 
              0.19196957,	0.07822739,	-0.26698427)
###parameters Hcmodel piab
pHcM[,2] <- c(-2.9597995,0.8591922,-0.354981,0.3362082,0.2469336,0.1316909)
###parameters Hcmodel beal
pHcM[,3] <- c(-1.9287089,1.0760549,-0.107913,0.1922377,0.1363654,-0.3804504)
###parameters Hcmodel fasy
pHcM[1:5,4] <- c(1.26813,-0.21981,-0.1405,0.50624,-0.3196)
###parameters Hcmodel pipi
pHcM[,5] <- c(-1.67918379,	1.16550784,	-0.23744806, 
              0.19196957,	0.07822739,	-0.26698427)
###parameters Hcmodel eugl
pHcM[1:5,6] <- c(-1.067271, -0.017684, 0.07708,	-0.619978,0.045046)
###parameters Hcmodel rops
pHcM[1:3,7] <- c(0.04237,-0.13308,0.31382)

###old hc model
HcModOld<- function(inputs){ 
  pValues=inputs[1:6]
  H=inputs[7]
  D=inputs[8]
  age=inputs[9]
  BA_sp=inputs[10]
  BA_tot=inputs[11]
  lnHc_sim <- pValues[1]+pValues[2]*log(H)+pValues[3]*D/H+
    pValues[4]*log(age)+ pValues[5]*log(BA_sp)+
    pValues[6]*(BA_sp/BA_tot)
  Hc_sim <- exp(lnHc_sim)
  return(pmax(Hc_sim,0.,na.rm = T)) 
} 


createInputsHc <- function(multiInitVar,layerX,spX){
  nSites <- dim(multiInitVar)[1]
  inputHc <- matrix(NA,nSites,11)
  inputHcT <- aperm(inputHc)
  inputHcT[1:6,] <- pHcM[,2]
  inputHc <- aperm(inputHcT)
  inputHc[,7] <- multiInitVar[,3,layerX]
  inputHc[,8] <- multiInitVar[,4,layerX]
  inputHc[,9] <- multiInitVar[,2,layerX]
  inputHc[,10] <- multiInitVar[,5,layerX]
  inputHc[,11] <- rowSums(multiInitVar[,5,],na.rm = T)
  return(inputHc)
}

