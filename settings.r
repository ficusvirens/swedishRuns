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
fromPlant = F
CSCrun = F
testRun = T  ###set to True if you want to make a test run
simRuns = 150 ####years of simulations
simRunsTS = 5 ### years of simulations for time/space run
def_thin = 1 # default thin, 1 or 0 
cl_cut = 1 # clearcut, 1 or 0

# for time/space run
harvLvl <- 1 # to test the impact of harvest level into the output, 1 is normal

if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
}

vPREBAS <- "master"   #### choose PREBAS verson to run the model  "master" or "v0.2.x"
#vPREBAS <- "v0.2.x"   #### choose PREBAS verson to run the model  "master"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)
library(Rprebasso)


weatherFile <- "rdata/weather150.rdata" ###weather input file

# load the regions of Sweden
load("rdata/region_ids.rdata")

outFile <- "rdata/runs/output.rdata" # output file normal run
outFile1.5 <- "rdata/runs/output1_5.rdata" # output file 1.5 x run 
outFileMax <- "rdata/runs/outputMax.rdata" # output file max run
outFileTS <- "rdata/runs/TSruns.rdata" # output file time/space runs
outFileSoilC <- "rdata/runs/soilC.rdata" # output file with soilC results


litterdata <- "rdata/litterdata.rdata" # file with litter data

sizeCwoodyLit <- c(10,10,5) #### size coarse woody litter for pine,spruce and birch 
#sizeCwoodyLit <- c(30,30,10) #### size coarse woody litter for pine,spruce and birch 




species <- 1:3
source("functions.r")




# -------- settings for plots -----------
plot_area <- "Sweden" # set the region to plot here: alternatives are
# "Sweden" = whole Sweden
# "GOT" = Götaland
# "SVEA" = Svealand
# "SN" = Södra Norrland
# "NN" = Norra Norrland
# all areas are only mineral sites
# some plots include all the regions; this setting does not alter them

plotrun <- "normal" # set the simulation run to plot here: alternatives are
# "normal" = normal run with tapio rules (no separate harvest limit)
# "1.5" = run with 1.5x rotation time but no more than simulation time
# "max" = run with rotation time that is maximum between tapio rules
#               clearcut timing and stand age, though no more than simulation time
# some plots include all the runs; this setting does not alter them
