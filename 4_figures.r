
# --------- load data ------------
#load(outFileSoilC)
#load(outFile)
#load(outFile1.5)
#load(outFileMax)
load(litterdata)

# ----- general data -----
plot_run <- select_plotrun(plotrun)
nSites <- plot_run$nSites
simLength_prun <- simulationLength(plot_run)
rotLength_prun <- rotationLength(plot_run, simLength_prun)


reg_output <- regionPTS(plot_area)

load("rdata/runs/InitialX.rdata")
data_soilC <- InitialX$c.tot.tha/10

#---- regions -------
load("rdata/region_ids.rdata")
plot_area_sites <- which(plot_run$siteInfo[,1] %in% regionID(plot_area))
got <- which(plot_run$siteInfo[,1] %in% got_id)
svea <- which(plot_run$siteInfo[,1] %in% svea_id)
sn <- which(plot_run$siteInfo[,1] %in% sn_id)
nn <- which(plot_run$siteInfo[,1] %in% nn_id)
sweden <- which(plot_run$siteInfo[,1] %in% mineral_id)


# -------- colors ---------------
myCols3 <- c("orangered3", "royalblue4", "seagreen4")
myCols5 = c("grey", "maroon4", "seagreen4", "darkorange3", "deepskyblue4")

# -------- labels --------------
regs_label = c("Götaland", "Svealand", "Södra Norrland", 
             "Norra Norrland", "All Sweden")
rotLength_label = c("measurements", "normal rotation length", 
         "1.5x rotation length", "max rotation length")
tupek_prebas_rot_label = c("Tupek","Prebas time/space", 
                           "Prebas whole rotation",
                           "Prebas rotation x1.5", 
                           "Prebas max rotation")
litter_label_5empty = c("","", "tree foliage", "","","", "", "fine root", "", "",
        "","", "fine woody", "", "","","", "coarse woody", "","", 
        "","","ground vegetation", "","")
species_label = c("pine", "spruce", "deciduous")

# --------- LITTERDATA --------------
gv.biomlitX <- gv.biomlit[siteX,]
gv.biomlitX[is.na(gv.biomlitX)]=0
litter.origX <- litter.orig[siteX,]
litter.origX[is.na(litter.origX)]=0

# ---------- soil C in different regions ----------

boxplot(data_soilC[got], data_soilC[svea], 
        data_soilC[sn], data_soilC[nn], data_soilC[sweden],
        main = "Soil C in Sweden",
        names = regs_label, 
        ylab = "kg2/m2", 
        ylim = c(0,20))

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
Cline <- matrix(data=NA, nrow=5, ncol=2)

Cline[1,] <- soilC_prebasST("GOT")
Cline[2,] <- soilC_prebasST("SVEA")
Cline[3,] <- soilC_prebasST("SN")
Cline[4,] <- soilC_prebasST("NN")
Cline[5,] <- soilC_prebasST("Sweden")

# add segments
segments(x0 = x0s, x1 = x1s, y0 = Cline[,1], col = "blue")
segments(x0 = x0s, x1 = x1s, y0 = Cline[,2], col = "red")

legend("topright", title = "Prebas space/time", 
         fill = c("red", "blue"), 
         legend = c("with gv","without gv"), 
         horiz = F)


# ---- soil C with different rotation lengths ---
pst_line <- soilC_prebasST(plot_area)

boxplot(data_soilC[plot_area_sites], soilCstst[plot_area_sites], 
        soilCstst1.5[plot_area_sites], soilCststMax[plot_area_sites], 
        main = c("Soil C in ", paste0(regionName(plot_area))),
        names = rotLength_label, 
        ylab = "kg/m2", 
        ylim = c(0,20))
abline(h=pst_line[1], col = "red") 
abline(h=pst_line[2], col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

# ---------------- HARVEST LEVELS -----------------

harvMean <- vector()
for(i in 1:nSites) {
  harvMean[i] <- sum(plot_run$multiOut[i,1:simLength_prun[i],37,,1])/rotLength_prun[i]
}

boxplot(harvMean[got], harvMean[svea], 
        harvMean[sn],harvMean[nn],
        harvMean[sweden],
        main = c("Harvest levels in Sweden (Prebas)",paste0(simName(plot_run))), 
        ylab = "m3/ha", 
        names = regs_label)


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

boxplot(thinMean[got], thinMean[svea], 
        thinMean[sn], thinMean[nn],
        thinMean[sweden],
        main = c("Thinning levels in Sweden (Prebas)",paste0(simName(plot_run))),
        ylab = "m3/ha", 
        names = regs_label)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = myCols3[1])
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = myCols3[2])
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = myCols3[3])

legend("topright", title = "Data", fill = myCols3, 
       legend = c("1993","2003", "2013"), horiz = F)

# --- final felling levels ---

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

boxplot(ccMean[got], ccMean[svea], 
        ccMean[sn], ccMean[nn],
        ccMean[sweden],
        main = c("Final felling levels in Sweden (Prebas)",paste0(simName(plot_run))),
        ylab = "m3/ha", 
        names = regs_label)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = myCols3[1])
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = myCols3[2])
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = myCols3[3])

legend("topright", title = "Data", fill = myCols3, 
       legend = c("1993","2003", "2013"), horiz = F)


# ---------- LITTERFALL ALL SEPARATED -------------

# litter from whole rotation prebas runs
litter <- meanLitter(output)
litter1.5 <- meanLitter(output_1.5)
litterMax <- meanLitter(output_max)

# litter from prebas time/space runs
fol_pts <- rowSums(reg_output$multiOut[,1,26,,1])
fr_pts <- rowSums(reg_output$multiOut[,1,27,,1])
fw_pts <- rowSums(reg_output$multiOut[,1,28,,1])
cw_pts <- rowSums(reg_output$multiOut[,1,29,,1])  
gv_pts <- reg_output$GVout[,1,2]
  
# litter from data
fol_d <- litter.origX[plot_area_sites,]$lit.foliage.tot/2 # foliage
fr_d <- litter.origX[plot_area_sites,]$lit.foliage.tot/2 # fine root
fw_d <- (litter.origX[plot_area_sites,]$lit.root.tot+
           litter.origX[plot_area_sites,]$lit.branch.tot)/2 # fine woody
cw_d <- (litter.origX[plot_area_sites,]$lit.stump.tot+
           litter.origX[plot_area_sites,]$lit.stem.tot)/2 # coarse woody
gv_d <- (gv.biomlitX[plot_area_sites,]$gvb.abv.dwarfshrub+
           gv.biomlitX[plot_area_sites,]$gvb.abv.herb+
           gv.biomlitX[plot_area_sites,]$gvb.abv.grass+
           gv.biomlitX[plot_area_sites,]$gvb.abv.moss+
           gv.biomlitX[plot_area_sites,]$gvb.abv.lichen)/2 # ground vegetation


boxplot(fol_d, fol_pts, rowSums(litter$fol[plot_area_sites,]), 
        rowSums(litter1.5$fol[plot_area_sites,]), rowSums(litterMax$fol[plot_area_sites,]),
        fr_d, fr_pts, rowSums(litter$fr[plot_area_sites,]),
        rowSums(litter1.5$fr[plot_area_sites,]), rowSums(litterMax$fr[plot_area_sites,]),
        fw_d, fw_pts, rowSums(litter$fw[plot_area_sites,]),
        rowSums(litter1.5$fw[plot_area_sites,]), rowSums(litterMax$fw[plot_area_sites,]),
        cw_d, cw_pts, rowSums(litter$cw[plot_area_sites,]),
        rowSums(litter1.5$cw[plot_area_sites,]), rowSums(litterMax$cw[plot_area_sites,]),
        gv_d, gv_pts, litter$gv[plot_area_sites], 
        litter1.5$gv[plot_area_sites], litterMax$gv[plot_area_sites], 
        main = c("Litter ",paste0(regionName(plot_area))), 
        names = litter_label_5empty, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myCols5)
legend("topright", fill = myCols5, 
       legend = tupek_prebas_rot_label, horiz = F)

