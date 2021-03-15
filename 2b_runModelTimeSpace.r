library(Rprebasso)
#load(InitPrebasTSFile)

harvestData <- read.csv(hdataFile)

harvLimSvea <- initPrebas_svea$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
harvLimGot <- initPrebas_got$nSites*harvestData[2,1]/harvestData[2,2]*1000*harvLvl
harvLimNN <- initPrebas_nn$nSites*harvestData[3,1]/harvestData[3,2]*1000*harvLvl
harvLimSN <- initPrebas_sn$nSites*harvestData[4,1]/harvestData[4,2]*1000*harvLvl

harvLimM <- initPrebas_m$nSites*harvestData[5,1]/harvestData[5,2]*1000*harvLvl

output_svea <- regionPrebas(initPrebas_svea, c(harvLimSvea, 0))
output_got <- regionPrebas(initPrebas_got, c(harvLimGot, 0))
output_nn <- regionPrebas(initPrebas_nn, c(harvLimNN, 0))
output_sn <- regionPrebas(initPrebas_sn, c(harvLimSN, 0))

output_m <- regionPrebas(initPrebas_m, c(harvLimM,0))

#save(output_got, output_svea, output_sn,
#     output_nn, output_m, file=outFileTS)

