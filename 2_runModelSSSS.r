#load("rdata/initPrebas.rdata")

# run model
#if(CSCrun) {
#  output <- regionPrebas(initPrebas)
#} else output <- multiPrebas(initPrebas)
output <- multiPrebas(initPrebas)


nSites <- output$nSites
simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

# initialize model for 1.5x longer rotation lengths
rotLength_tot <- rotLength*1.5
rotLength_tot[rotLength_tot>simRuns] <- simRuns

initPrebas1.5 <- initPrebas

# increase rotation length
initPrebas1.5$inAclct[,1:5] <- rotLength_tot
# no clearcut because of diameter
initPrebas1.5$inAclct[,7] <- rep(999, nSites)
initPrebas1.5$inDclct[,] <- rep(999, nSites)

# initialize model for max rotation lengths 
# = rotation length is maximum between stand age and normal clearcut age
rotLengthMax <- pmax(rotLength, InitialX$age)
rotLengthMax[rotLengthMax>simRuns] <- simRuns

initPrebasMax <- initPrebas1.5
# increase rotation length
initPrebasMax$inAclct[,1:5] <- rotLengthMax

# make output lighter
if(!CSCrun) {
  rm(initPrebas);gc()
  
  output <- list(multiOut=output$multiOut,GVout=output$GVout,
                 nSites=output$nSites,fAPAR=output$fAPAR,
                 siteInfo=output$siteInfo,weatherYasso=output$weatherYasso, 
                 litterSize=output$litterSize,multiInitVar=output$multiInitVar)
  gc()
}

# run model for 1.5x
#if(CSCrun) {
#  output_1.5 <- regionPrebas(initPrebas1.5)
#} else output_1.5 <- multiPrebas(initPrebas1.5)

output_1.5 <- multiPrebas(initPrebas1.5)

# make output lighter
if(!CSCrun) {
  rm(initPrebas1.5);gc()
  
  output_1.5 <- list(multiOut=output_1.5$multiOut,GVout=output_1.5$GVout,
                 nSites=output_1.5$nSites,fAPAR=output_1.5$fAPAR,
                 siteInfo=output_1.5$siteInfo,weatherYasso=output_1.5$weatherYasso,
                 litterSize=output_1.5$litterSize,multiInitVar=output_1.5$multiInitVar)
  gc()
}

# run model for max
#if(CSCrun) {
#  output_max <- regionPrebas(initPrebasMax)
#} else output_max <- multiPrebas(initPrebasMax)

output_max <- multiPrebas(initPrebasMax)


# make output lighter
if(!CSCrun) {
  rm(initPrebasMax);gc()
  
  output_max <- list(multiOut=output_max$multiOut,GVout=output_max$GVout,
                     nSites=output_max$nSites,fAPAR=output_max$fAPAR,
                     siteInfo=output_max$siteInfo,weatherYasso=output_max$weatherYasso,
                     litterSize=output_max$litterSize,multiInitVar=output_max$multiInitVar)
  gc()
}

#save(output, file=outFile)
#save(output_1.5, file=outFile1.5)
#save(output_max, file=outFileMax)

