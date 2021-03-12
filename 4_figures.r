
# --------- load data ------------
load(outFileSoilC)
load(outFile)
load(outFile1.5)
load(outFileMax)
load(outFileTS)
load("rdata/runs/initPrebas.rdata")
load(litterdata)

# ----- general data -----
plot_run <- select_plotrun(plotrun)
nSites <- plot_run$nSites
simLength_prun <- simulationLength(plot_run)
rotLength_prun <- rotationLength(plot_run, simLength_prun)


reg_output <- regionPTS(plot_area)

load("rdata/runs/Initial.rdata")
data_soilC <- Initial$c.tot.tha/10

#---- regions -------
load("rdata/region_ids.rdata")

plot_area_sites <- which(plot_run$siteInfo[,1] %in% regionID(plot_area))


prun_ids <- plot_run$siteInfo[,1]
initial_ids <- Initial$id
litter_ids <- litter.orig$id
gv_ids <- gv.biomlit$id

# this means that the sites might not be exactly the same 
# if some sites are missing from the runs, for example
reg <- regionGroups(prun_ids, initial_ids, litter_ids, gv_ids)

regNo <- regionOrder(plot_area)

# -------- colors ---------------
myCols2 <- c("grey", "maroon4")
myCols3 <- c("orangered3", "royalblue4", "seagreen4")
myCols5 = c("grey", "maroon4", "seagreen4", "darkorange3", "deepskyblue4")

# -------- labels --------------
regs_label_0 = c("Götaland", "Svealand", "Södra Norrland", 
                 "Norra Norrland")
regs_label = c("Götaland", "Svealand", "Södra Norrland", 
               "Norra Norrland", "All Sweden")
regs_label_2 = c("Götaland","", "Svealand","", "Södra Norrland","", 
                 "Norra Norrland","", "All Sweden","")
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

gv.biomlit[is.na(gv.biomlit)]=0
litter.orig[is.na(litter.orig)]=0



# ---------- soil C in different regions ----------

boxplot(data_soilC[reg$got$initial], soilCstst[reg$got$plot_run], 
        data_soilC[reg$svea$initial], soilCstst[reg$svea$plot_run],
        data_soilC[reg$sn$initial], soilCstst[reg$sn$plot_run],
        data_soilC[reg$nn$initial], soilCstst[reg$nn$plot_run],
        data_soilC[reg$sweden$initial], soilCstst[reg$sweden$plot_run],
        main = "Soil C in Sweden",
        names = regs_label_2, 
        col = myCols2,
        ylab = "kg2/m2", 
        ylim = c(0,20))

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- (1:n)*2 - 1.4
x1s <- (1:n)*2 - 0.6
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

legend("topright", title = "Prebas", 
       fill = c("red", "blue", "maroon4"), 
       legend = c("time/space with gv","time/space without gv", "site specific"), 
       horiz = F)


# ---- soil C with different rotation lengths ---
pst_line <- soilC_prebasST(plot_area)

boxplot(data_soilC[reg[[regNo]][[2]]], soilCstst[reg[[regNo]][[1]]], 
        soilCstst1.5[reg[[regNo]][[1]]], soilCststMax[reg[[regNo]][[1]]], 
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

boxplot(harvMean[reg$got$plot_run], harvMean[reg$svea$plot_run], 
        harvMean[reg$sn$plot_run],harvMean[reg$nn$plot_run],
        harvMean[reg$sweden$plot_run],
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

boxplot(thinMean[reg$got$plot_run], thinMean[reg$svea$plot_run], 
        thinMean[reg$sn$plot_run], thinMean[reg$nn$plot_run],
        thinMean[reg$sweden$plot_run],
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

boxplot(ccMean[reg$got$plot_run], ccMean[reg$svea$plot_run], 
        ccMean[reg$sn$plot_run], ccMean[reg$nn$plot_run],
        ccMean[reg$sweden$plot_run],
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
fol_d <- litter.orig[reg[[regNo]][[3]],]$lit.foliage.tot/2 # foliage
fr_d <- litter.orig[reg[[regNo]][[3]],]$lit.foliage.tot/2 # fine root
fw_d <- (litter.orig[reg[[regNo]][[3]],]$lit.root.tot+
           litter.orig[reg[[regNo]][[3]],]$lit.branch.tot)/2 # fine woody
cw_d <- (litter.orig[reg[[regNo]][[3]],]$lit.stump.tot+
           litter.orig[reg[[regNo]][[3]],]$lit.stem.tot)/2 # coarse woody
gv_d <- (gv.biomlit[reg[[regNo]][[4]],]$gvb.abv.dwarfshrub+
           gv.biomlit[reg[[regNo]][[4]],]$gvb.abv.herb+
           gv.biomlit[reg[[regNo]][[4]],]$gvb.abv.grass+
           gv.biomlit[reg[[regNo]][[4]],]$gvb.abv.moss+
           gv.biomlit[reg[[regNo]][[4]],]$gvb.abv.lichen)/2 # ground vegetation


boxplot(fol_d, fol_pts, rowSums(litter$fol[reg[[regNo]][[1]],]), 
        rowSums(litter1.5$fol[reg[[regNo]][[1]],]), rowSums(litterMax$fol[reg[[regNo]][[1]],]),
        fr_d, fr_pts, rowSums(litter$fr[reg[[regNo]][[1]],]),
        rowSums(litter1.5$fr[reg[[regNo]][[1]],]), rowSums(litterMax$fr[reg[[regNo]][[1]],]),
        fw_d, fw_pts, rowSums(litter$fw[reg[[regNo]][[1]],]),
        rowSums(litter1.5$fw[reg[[regNo]][[1]],]), rowSums(litterMax$fw[reg[[regNo]][[1]],]),
        cw_d, cw_pts, rowSums(litter$cw[reg[[regNo]][[1]],]),
        rowSums(litter1.5$cw[reg[[regNo]][[1]],]), rowSums(litterMax$cw[reg[[regNo]][[1]],]),
        gv_d, gv_pts, litter$gv[reg[[regNo]][[1]]], 
        litter1.5$gv[reg[[regNo]][[1]]], litterMax$gv[reg[[regNo]][[1]]], 
        main = c("Litter ",paste0(regionName(plot_area))), 
        names = litter_label_5empty, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myCols5)

legend("topright", fill = myCols5, 
       legend = tupek_prebas_rot_label, horiz = F)


# --------- litter / soil C

# this is litter from Prebas
litter_prebas <- meanLitter(output)

# sum it up
litter_prebas_all <- rowSums(litter_prebas$fol)+rowSums(litter_prebas$fr)+
  rowSums(litter_prebas$cw)+rowSums(litter_prebas$fw)+
  litter_prebas$gv

# litter from tupek: merge and sum
litter_tupek <- fol_d+fr_d+fw_d+cw_d

lit_tupek <- cbind(litter_tupek, litter.orig[reg[[regNo]][[3]],]$id)
colnames(lit_tupek) <- c("litter_tupek", "id")
lit_tupek_gv <- cbind(gv_d, gv.biomlit[reg[[regNo]][[3]],]$id)
colnames(lit_tupek_gv) <- c("litter_tupek_gv", "id")

lit_tupek_all <- merge(lit_tupek, lit_tupek_gv, by="id")

lit_tupek_all$tupek_littersum <- lit_tupek_all$litter_tupek+lit_tupek_all$litter_tupek_gv

# Prebas soil C
plot(litter_prebas_all, soilCstst,
     xlab = "Prebas litterfall kg C/ha",
     ylab = "Prebas soil C kg/m2", 
     main = "Soil C / litter")

# put together site ID, prebas litter and prebas soil C
soil_data_prebas <- cbind(output$siteInfo[,1], litter_prebas_all, soilCstst) 
colnames(soil_data_prebas) <- c("id", "litter_prebas", "soilCstst")

# add soil C from data
sdata <- merge(soil_data_prebas, Initial[,c(1,20)], by="id")

# change the unit
sdata$soilC_data<-sdata$c.tot.tha/10

sdata2 <- merge(sdata, lit_tupek_all, by="id")

# drop the useless stuff
soil_data <- sdata2[,c(1:3,5,8)]

colnames(soil_data) <- c("id", "litter_prebas", "soilC_prebas",
                         "soilC_data", "litter_tupek")

# data soil C
plot(soil_data$litter_prebas, soil_data$soilC_data,
     xlab = "Prebas litterfall kg C/ha",
     #     ylab = "Measured soil C kg/m2", 
     ylab = "Soil C kg/m2",
     main = "Litter / soil C", 
     ylim=c(0,30))

points(soil_data$litter_prebas, soil_data$soilC_prebas, col="red")
legend("topright", fill = c("black", "red"), 
       legend = c("Measured", "Prebas"))


# linear models
model <- lm(soilC_data~litter_prebas, data=soil_data)
summary(model)

model2 <- lm(soilC_prebas~litter_prebas, data=soil_data)
summary(model2)

# plot litter
plot(soil_data$litter_tupek, soil_data$litter_prebas,
     main="Litter", 
     ylab="Prebas", 
     xlab="Tupek")

#----- soil C / ETS

plot(rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]], soilCstst,
     ylab="Prebas soil C kg/m2",
     xlab="ETS",
     main="Soil C / ETS")




# !!! FROM THIS ON CHECKING // DOCUMENTATION IS STILL UNDER PROGRESS !!!

#---- soil types in different regions --- 

got_in <- which(Initial$id %in% got_id)
svea_in <- which(Initial$id %in% svea_id)
sn_in <- which(Initial$id %in% sn_id)
nn_in <- which(Initial$id %in% nn_id)


boxplot(Initial[got_in,]$clay, Initial[svea_in,]$clay, 
        Initial[sn_in,]$clay, Initial[nn_in,]$clay)

boxplot(Initial[got_in,]$silt, Initial[svea_in,]$silt, 
        Initial[sn_in,]$silt, Initial[nn_in,]$silt)

boxplot(Initial[got_in,]$sand, Initial[svea_in,]$sand, 
        Initial[sn_in,]$sand, Initial[nn_in,]$sand, 
        main = "Sand",
        names = regs_label_0)

boxplot(Initial[got_in,]$texture, Initial[svea_in,]$texture, 
        Initial[sn_in,]$texture, Initial[nn_in,]$texture, 
        main = "Soil texture",
        names = regs_label_0)

boxplot(Initial[got_in,]$sortmater, Initial[svea_in,]$sortmater, 
        Initial[sn_in,]$sortmater, Initial[nn_in,]$sortmater)


#---- MAPS ----
library(mapview)

hist(Initial[nn_in]$tempsum)
hist(Initial[sn_in]$tempsum)
hist(Initial[svea_in]$tempsum)
hist(Initial[got_in]$tempsum)

# this is where the coordinates of the plots are
load("rdata/coordPlots.rdata")

# and this is where the regions are in shape files 
load("rdata/sweden_landsdel.rdata")

plots_sf <- st_as_sf(coordPlots, coords=c("long", "lat")) %>%
  st_set_crs(4326)

plots_sf$number <- c(1:nrow(plots_sf))

mapview(sweden_landsdel) + mapview(plots_sf)


set <- plots_sf[which(plots_sf$id %in% Initial$id),]

set2 <- merge(set, Initial[,c(1,72,78:79)], by="id")

mapview(set2, zcol="tempsum")


set_p <- plots_sf[which(plots_sf$id %in% initPrebas$siteInfo[,1]),]

ETS_p <- cbind(initPrebas$siteInfo[,1], rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]])

colnames(ETS_p) <- c("id", "ETS")

set_p2 <- merge(set, ETS_p, by="id")

ETS_set <- merge(set2, ETS_p, by="id")

mapview(set_p2, zcol="ETS")


plot(ETS_set$tempsum, ETS_set$ETS, 
     ylab = "ETS Prebas", 
     xlab = "ETS data")


# make this again!

plot(rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]], Initial[use]$c.tot.tha/10,
     ylab="Measured soil C kg/m2",
     xlab="ETS",
     main="Soil C / ETS")
