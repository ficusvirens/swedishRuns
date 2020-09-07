library(ggplot2)
library(ggpubr)
library(Rprebasso)

load("rdata/initPrebas.rdata")

#output <- multiPrebas(initPrebas)$multiOut
#outputR1 <- multiPrebas(initPrebasR1)$multiOut
#outputR2 <- multiPrebas(initPrebasR2)$multiOut
#outputR3 <- multiPrebas(initPrebasR3)$multiOut
#outputR4 <- multiPrebas(initPrebasR4)$multiOut
harvestData <- read.csv("input/harvestData.csv")
# to test the impact of harvest level into the output, 1 is normal
harvLvl <- 1
harvLimSvea <- initPrebasR1$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
harvLimGot <- initPrebasR2$nSites*harvestData[2,1]/harvestData[2,2]*1000*harvLvl
harvLimNN <- initPrebasR3$nSites*harvestData[3,1]/harvestData[3,2]*1000*harvLvl
harvLimSN <- initPrebasR4$nSites*harvestData[4,1]/harvestData[4,2]*1000*harvLvl

# run the model
output <- regionPrebas(initPrebas)
output_svea <- regionPrebas(initPrebasR1, c(harvLimSvea, 0))
output_got <- regionPrebas(initPrebasR2, c(harvLimGot, 0))
output_nn <- regionPrebas(initPrebasR3, c(harvLimNN, 0))
output_sn <- regionPrebas(initPrebasR4, c(harvLimSN, 0))

#save(output_svea, output_got, output_nn, output_sn, file="rdata/output_sweden.rdata")
#load("rdata/output_sweden.rdata")

# count the soil carbon in steady state
species <- 1:3

soilC_svea <- countSoilC(output_svea, species)
soilC_got <- countSoilC(output_got, species)
soilC_nn <- countSoilC(output_nn, species)
soilC_sn <- countSoilC(output_sn, species)

# allPlots
#plots <- makePlots(output)
plots_svea <- makePlots(output_svea$multiOut,sitesR1)
plots_got <- makePlots(output_got,sitesR2)
plots_nn <- makePlots(output_nn,sitesR3)
plots_sn <- makePlots(output_sn,sitesR4)

makePlots(output_svea, sitesR1)

