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

multiLayer=TRUE
fromPlant = TRUE
CSCrun = FALSE
testRun = F  ###set to True if you want to make a test run
simRuns = 100 ####years of simulations
weatherFile <- "rdata/weather100.rdata" ###weather input file
outFile <- "rdata/soilCtapio.rdata" ###output file with results of steady state calculations
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