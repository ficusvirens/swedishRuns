library(lubridate)
library(data.table)
library(sf)
library(tidyverse)
library(mapview)
library(rgeos)
library(ggplot2)
library(ggpubr)


# run these before installing Rprebasso!
library(abind)
library(plyr)

multiLayer = TRUE
fromPlant = TRUE
CSCrun = FALSE
testRun = F  ###set to True if you want to make a test run
simRuns = 100 ####years of simulations
# load the regions of Sweden
load("rdata/mineral_regions.rdata")
plot_area <- mineral # set the region to plot here: alternatives are
                        # mineral = whole Sweden
                        # got_m = Götaland
                        # svea_m = Svealand
                        # sn_m = Södra Norrland
                        # nn_m = Norra Norrland
                    # all areas are only mineral sites

weatherFile <- "rdata/weather100.rdata" ###weather input file

outFile <- "rdata/100runs/output.rdata" # output file normal run
outFile1.5 <- "rdata/100runs/output1_5.rdata" # output file 1.5 x run 
outFileMax <- "rdata/100runs/outputMax.rdata" # output file max run
outFileSoilC <- "rdata/100runs/soilC.rdata" # output file with soilC results

timeSpaceRuns <- "rdata/5run.rdata" # file with time/space output results
litterdata <- "rdata/litterdata.rdata" # file with litter data

litterSizeDef[1,1:3] <- c(10,10,5)


if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
}

species <- 1:3

vPREBAS <- "master"   #### choose PREBAS verson to run the model  "master" or "v0.2.x"
#vPREBAS <- "v0.2.x"   #### choose PREBAS verson to run the model  "master"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(Rprebasso)


source("functions.r")