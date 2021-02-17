# THE FIGURES WON'T BE CORRECT IF MADE TO TEST RUN
# DIVISION TO DIFFERENT REGIONS IS BASED ON SITE ORDER

# --------- load data ------------
#load(outFileSoilC)
#load(outFile)
#load(outFile1.5)
#load(outFileMax)
#load("rdata/InitialX.rdata")
#load(litterdata)

# -------- colors ---------------
myCols3 <- c("orangered3", "royalblue4", "seagreen4")


# -------- labels --------------
regs_all = c("Götaland", "Svealand", "Södra Norrland", 
             "Norra Norrland", "All Sweden")

 
# ---- soil C with different rotation lengths ---
snms = c("measurements", "normal rotation length", 
         "1.5x rotation length", "max rotation length")

data_soilC <- InitialX$c.tot.tha/10


boxplot(data_soilC[plot_area], soilCstst[plot_area], 
        soilCstst1.5[plot_area], soilCststMax[plot_area], 
        main = c("Soil C in ", paste0(regionName(plot_area))),
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,20))
#abline(h=c_m, col = "red") # fiksaa nämä vielä !!!
#abline(h=c_m0, col = "blue")
#legend("topright", title = "Prebas space/time", 
#       fill = c("red", "blue"), 
#       legend = c("with gv","without gv"), 
#       horiz = F)

# ---------------- harvest levels -----------------
nSites <- plot_run$nSites
simLength_prun <- simulationLength(plot_run)
rotLength_prun <- rotationLength(plot_run, simLength_prun)

harvMean <- vector()
for(i in 1:nSites) {
  harvMean[i] <- sum(plot_run$multiOut[i,1:simLength_prun[i],37,,1])/rotLength_prun[i]
}

boxplot(harvMean[got_m], harvMean[svea_m], 
        harvMean[sn_m],harvMean[nn_m],
        harvMean[mineral],
        main = c("Harvest levels in Sweden (Prebas),",paste0(simName(plot_run))), 
        ylab = "m3/ha", 
        names = regs_all)


h_per_ha <- read.csv("input/harvest_per_ha.csv", row.names = 1)

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
y93s <- h_per_ha$X1993
# 2003
y03s <- h_per_ha$X2003
# 2013
y13s <- h_per_ha$X2013

# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = myCols3[1])
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = myCols3[2])
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = myCols3[3])

legend("topright", title = "Data", fill = myCols3, 
       legend = c("1993","2003", "2013"), horiz = F)

#-------------- harvest levels: thinnings & clearcuts separately

thinMean <- vector()
for(i in 1:nSites) {
  thinMean[i] <- sum(plot_run$multiOut[i,1:simLength_prun[i]-1,37,,1])/rotLength_prun[i]
}

thin_per_ha <- read.csv("input/thin_per_ha.csv", row.names = 1)

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
y93s <- thin_per_ha$X1993
# 2003
y03s <- thin_per_ha$X2003
# 2013
y13s <- thin_per_ha$X2013

boxplot(thinMean[got_m], thinMean[svea_m], 
        thinMean[sn_m], thinMean[nn_m],
        thinMean[mineral],
        main = c("Thinning levels in Sweden (Prebas),",paste0(simName(plot_run))),
        ylab = "m3/ha", 
        names = regs_all)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = myCols3[1])
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = myCols3[2])
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = myCols3[3])

legend("topright", title = "Data", fill = myCols3, 
       legend = c("1993","2003", "2013"), horiz = F)



ccMean <- vector()
for(i in 1:nSites) {
  ccMean[i] <- sum(plot_run$multiOut[i,simLength_prun[i],37,,1])/rotLength_prun[i]
}

ff_per_ha <- read.csv("input/ff_per_ha.csv", row.names = 1)

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
y93s <- ff_per_ha$X1993
# 2003
y03s <- ff_per_ha$X2003
# 2013
y13s <- ff_per_ha$X2013

boxplot(ccMean[got_m], ccMean[svea_m], 
        ccMean[sn_m], ccMean[nn_m],
        ccMean[mineral],
        main = c("Final Felling levels in Sweden (Prebas),",paste0(simName(plot_run))),
        ylab = "m3/ha", 
        names = rnms)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = myCols3[1])
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = myCols3[2])
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = myCols3[3])

legend("topright", title = "Data", fill = myCols3, 
       legend = c("1993","2003", "2013"), horiz = F)


