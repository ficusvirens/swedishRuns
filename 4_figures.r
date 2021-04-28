library(ggplot2)
library(viridis)


# --------- load data ------------
load(outFileSoilC)
load(outFileSoilCTS)
load(outFile)
load(outFile1.5)
load(outFileMax)
load(outFileTS)
load(InitPrebasFile)

load(litterdata)

# ----- general data -----
plot_run <- select_plotrun(plotrun)
nSites <- plot_run$nSites
simLength_prun <- simulationLength(plot_run)
rotLength_prun <- rotationLength(plot_run)


reg_output <- regionPTS(plot_area)

load(InitialFile)
data_soilC <- Initial$c.tot.tha/10

#---- regions -------
# these are the sites (order number) that are used in the 150 years simulation
plot_area_sites <- which(plot_run$siteInfo[,1] %in% regionID(plot_area))

# site ID's of 150 years run
prun_ids <- plot_run$siteInfo[,1]

# this means that the sites might not be exactly the same 
# if some sites are missing from the runs, for example
reg <- regionGroups(prun_ids, Initial$id, litter.orig$id, gv.biomlit$id)

# reg[[regNo]] is a list of lists of the site order numbers for each different 
# data set (1: 150 years simulation, 2: Initial, 3: litter.orig, 4: gv.biomlit)
regNo <- regionOrder(plot_area)

# -------- colors ---------------
myCols2 <- c("grey", "maroon4")
myCols3 <- c("orangered3", "royalblue4", "seagreen4")
myCols5 = c("grey", "maroon4", "seagreen4", "darkorange3", "deepskyblue4")
myCols2.2 <- c("seagreen3", "steelblue3")

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
tupek_prebas_label = c("Tupek","Prebas time/space", 
                       "Prebas whole rotation")
prebas_twolabel = c("Prebas site specific", "Prebas time/space")
litter_label = c("Tree foliage", "Fine root", "Fine woody", 
                 "Coarse woody", "Ground vegetation")
litter_label_5empty = c("","", "tree foliage", "","","", "", "fine root", "", "",
                        "","", "fine woody", "", "","","", "coarse woody", "","", 
                        "","","ground vegetation", "","")
species_label = c("pine", "spruce", "deciduous")

# --------- LITTERDATA --------------

# tupek litter data for ground vegetation and other litter
# set NA's to 0
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

clines <- c("GOT", "SVEA", "SN", "NN", "Sweden")

for(i in 1:5) {
  Cline[i,] <- soilC_prebasST(clines[i])
}

# add segments
segments(x0 = x0s, x1 = x1s, y0 = Cline[,1], col = "red")
segments(x0 = x0s, x1 = x1s, y0 = Cline[,2], col = "blue")

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

#----------- trying to do it again better with ggplot2

# put all litter data in a list of lists
megaLitter <- list("fol" = list("d" = fol_d, "ts" = fol_pts, 
                                "normal" = rowSums(litter$fol[reg[[regNo]][[1]],]), 
                                "1.5" = rowSums(litter1.5$fol[reg[[regNo]][[1]],]), 
                                "max" = rowSums(litterMax$fol[reg[[regNo]][[1]],])),
                   "fr" = list("d" = fr_d, "ts" = fr_pts, 
                               "normal" = rowSums(litter$fr[reg[[regNo]][[1]],]),
                               "1.5" = rowSums(litter1.5$fr[reg[[regNo]][[1]],]), 
                               "max" = rowSums(litterMax$fr[reg[[regNo]][[1]],])),
                   "fw" = list("d" = fw_d, "ts" = fw_pts, 
                               "normal" = rowSums(litter$fw[reg[[regNo]][[1]],]),
                               "1.5" = rowSums(litter1.5$fw[reg[[regNo]][[1]],]), 
                               "max" = rowSums(litterMax$fw[reg[[regNo]][[1]],])),
                   "cw" = list("d" = cw_d, "ts" = cw_pts, 
                               "normal" = rowSums(litter$cw[reg[[regNo]][[1]],]),
                               "1.5" = rowSums(litter1.5$cw[reg[[regNo]][[1]],]), 
                               "max" = rowSums(litterMax$cw[reg[[regNo]][[1]],])),
                   "gv" = list("d" = gv_d, "ts"= gv_pts, 
                               "normal" = litter$gv[reg[[regNo]][[1]]], 
                               "1.5" = litter1.5$gv[reg[[regNo]][[1]]], 
                               "max" = litterMax$gv[reg[[regNo]][[1]]]))

# melt it to make a boxplot
megaLitterMelt <- melt(megaLitter)
colnames(megaLitterMelt) <- c("value", "source", "variable")

# factorize to make the order in the plot right
megaLitterMelt$source <- factor(megaLitterMelt$source, levels = c("d", "ts", "normal", "1.5", "max"))
megaLitterMelt$variable <- factor(megaLitterMelt$variable, levels = c("fol", "fr", "fw", "cw", "gv"))


ggplot(megaLitterMelt, aes(x=variable, y=value, fill=source)) + 
  ylim(0, 2000) +  
  geom_boxplot() + 
  labs(title = paste0("Litter ",regionName(plot_area)), fill = "",
       x="", y = "kg C / ha") +
  theme(plot.title = element_text(hjust = 0.5, size =22), # title in the center
        axis.text = element_text(size=14), # axis text size
        axis.title = element_text(size=16), # axis title size
        legend.text = element_text(size=12)) + # legend text size
  scale_fill_discrete(labels=tupek_prebas_rot_label) + # legend labels
  scale_x_discrete(labels=litter_label) # x axis labels



# ----- megaLitter plot without 1.5 & max

# put all litter data in a list of lists
megaLitter2 <- list("fol" = list("d" = fol_d, "ts" = fol_pts, 
                                 "normal" = rowSums(litter$fol[reg[[regNo]][[1]],])), 
                    "fr" = list("d" = fr_d, "ts" = fr_pts, 
                                "normal" = rowSums(litter$fr[reg[[regNo]][[1]],])),
                    "fw" = list("d" = fw_d, "ts" = fw_pts, 
                                "normal" = rowSums(litter$fw[reg[[regNo]][[1]],])),
                    "cw" = list("d" = cw_d, "ts" = cw_pts, 
                                "normal" = rowSums(litter$cw[reg[[regNo]][[1]],])),
                    "gv" = list("d" = gv_d, "ts"= gv_pts, 
                                "normal" = litter$gv[reg[[regNo]][[1]]]))

# melt it to make a boxplot
megaLitterMelt2 <- melt(megaLitter2)
colnames(megaLitterMelt2) <- c("value", "source", "variable")

# factorize to make the order in the plot right
megaLitterMelt2$source <- factor(megaLitterMelt2$source, levels = c("d", "ts", "normal"))
megaLitterMelt2$variable <- factor(megaLitterMelt2$variable, levels = c("fol", "fr", "fw", "cw", "gv"))



ggplot(megaLitterMelt2, aes(x=variable, y=value, fill=source)) + 
  ylim(0, 2000) +  
  geom_boxplot() + 
  labs(title = paste0("Litter ",regionName(plot_area)), fill = "",
       x="", y = "kg C / ha") +
  theme(plot.title = element_text(hjust = 0.5, size =22), # title in the center
        axis.text = element_text(size=14), # axis text size
        axis.title = element_text(size=16), # axis title size
        legend.text = element_text(size=12)) + # legend text size
  scale_fill_discrete(labels=tupek_prebas_label) + # legend labels
  scale_x_discrete(labels=litter_label) # x axis labels





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
     xlab="ETS (Prebas)",
     main="Soil C / ETS")


ETS_p <- cbind(initPrebas$siteInfo[,1], rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]])
temp <- merge(ETS_p, Initial[,c(1,20)], by="id")

plot(temp$ETS, temp$c.tot.tha/10,
     ylab="Measured soil C kg/m2",
     xlab="ETS (Prebas)",
     main="Soil C / ETS")

plot(Initial$tempsum, Initial$c.tot.tha/10,
     ylab="Measured soil C kg/m2",
     xlab="ETS (data)",
     main="Soil C / ETS")

#---- soil types in different regions, DATA --- 


boxplot(Initial[reg$got$initial,]$clay, Initial[reg$svea$initial,]$clay, 
        Initial[reg$sn$initial,]$clay, Initial[reg$nn$initial,]$clay, 
        main = "Clay", 
        names = regs_label_0)

boxplot(Initial[reg$got$initial,]$silt, Initial[reg$svea$initial,]$silt, 
        Initial[reg$sn$initial,]$silt, Initial[reg$nn$initial,]$silt, 
        main = "Silt", 
        names = regs_label_0)

boxplot(Initial[reg$got$initial,]$sand, Initial[reg$svea$initial,]$sand, 
        Initial[reg$sn$initial,]$sand, Initial[reg$nn$initial,]$sand, 
        main = "Sand",
        names = regs_label_0)

boxplot(Initial[reg$got$initial,]$texture, Initial[reg$svea$initial,]$texture, 
        Initial[reg$sn$initial,]$texture, Initial[reg$nn$initial,]$texture, 
        main = "Soil texture",
        names = regs_label_0)

boxplot(Initial[reg$got$initial,]$sortmater, Initial[reg$svea$initial,]$sortmater, 
        Initial[reg$sn$initial,]$sortmater, Initial[reg$nn$initial,]$sortmater,
        main = "Sortmater", 
        names = regs_label_0)

#---- MAPS ----
library(mapview)

# this is where the coordinates of the plots are
load(coordinateFile)

# and this is where the regions are in shape files 
load(swedenLandsdelFile)

# change the coordinate system
plots_sf <- st_as_sf(coordPlots, coords=c("long", "lat")) %>%
  st_set_crs(4326)

# put tempsum and the coordinates together
set <- plots_sf[which(plots_sf$id %in% Initial$id),]
set2 <- merge(set, Initial[,c(1,72,78:79)], by="id")

# tempsum in the map
mapview(set2, zcol="tempsum")

# put ETS (by Prebas) and coordinates together
set_p <- plots_sf[which(plots_sf$id %in% initPrebas$siteInfo[,1]),]
ETS_p <- cbind(initPrebas$siteInfo[,1], rowMeans(initPrebas$ETSy)[initPrebas$siteInfo[,2]])

colnames(ETS_p) <- c("id", "ETS")
set_p2 <- merge(set, ETS_p, by="id")
ETS_set <- merge(set2, ETS_p, by="id")

# ETS (by Prebas) in the map
mapview(set_p2, zcol="ETS")

#----------- ETS -------------

# ETS Prebas and tempsum
plot(ETS_set$tempsum, ETS_set$ETS, 
     ylab = "ETS Prebas", 
     xlab = "ETS data")

# ETS from time/space analysis
ETS_got <- rowMeans(output_got$multiOut[,,5,1,1])
ETS_svea <- rowMeans(output_svea$multiOut[,,5,1,1])
ETS_sn <- rowMeans(output_sn$multiOut[,,5,1,1])
ETS_nn <- rowMeans(output_nn$multiOut[,,5,1,1])

# ETS from data, same sites
ETS_got_d <- Initial[which(Initial$id %in% output_got$siteInfo[,1]),]$tempsum
ETS_svea_d <- Initial[which(Initial$id %in% output_svea$siteInfo[,1]),]$tempsum
ETS_sn_d <- Initial[which(Initial$id %in% output_sn$siteInfo[,1]),]$tempsum
ETS_nn_d <- Initial[which(Initial$id %in% output_nn$siteInfo[,1]),]$tempsum


megaETS <- list("got" = cbind("prebas" = ETS_got, "data" = ETS_got_d), 
                "svea" = cbind("prebas" = ETS_svea, "data" = ETS_svea_d),
                "sn" = cbind("prebas" = ETS_sn, "data" = ETS_sn_d),
                "nn" = cbind("prebas" = ETS_nn, "data" = ETS_nn_d))

megaETS_melt <- melt(megaETS)

colnames(megaETS_melt) <- c("var1", "source", "value", "region")

# factorizing to get the regions in right order
megaETS_melt$region <- factor(megaETS_melt$region, 
                              levels = c("got", "svea", "sn", "nn"))


ggplot(megaETS_melt, aes(x=region, y=value, fill=source)) + 
  geom_boxplot() +
  labs(title = "ETS", fill = "", # titles
       x="", y = "Degree days") +
  theme(plot.title = element_text(hjust = 0.5, size =22), # title in the center
        axis.text = element_text(size=14), # axis text size
        axis.title = element_text(size=16), # axis title size
        legend.text = element_text(size=12)) + # legend text size
  scale_fill_discrete(labels=c("Prebas", "Measurements")) + # legend labels
  scale_x_discrete(labels=regs_label_0) # x axis labels

# ------- GROSS GROWTH ---------------


maiData <- read.csv("input/mai2.csv", row.names = 1)

simLength <- simulationLength(plot_run)
rotLength <- rotationLength(plot_run)


avGG_ssss <- vector()
###avGG = average gross growth
for(i in 1:nSites){
  avGG_ssss[i] <- sum(plot_run$multiOut[i,1:simLength[i],43,,1])/rotLength[i]
}

avGG_ts <- rowSums(reg_output$multiOut[,1:5,43,,1])/5

boxplot(avGG_ssss[reg[[regNo]]$plot_run], avGG_ts, 
        main = paste0("Gross growth ",regionName(plot_area)), 
        names = c("Prebas site specific", "Prebas time/space"), 
        ylab = "m3/ha") 
abline(h=maiData[4,regionOrder(plot_area)], col="red")
legend("topright", fill = c("red"), 
       legend = c("Measured"))


# ---- try to put all the regions in the same figure -----


GG_got <- rowSums(output_got$multiOut[,1:5,43,,1])/5
GG_svea <- rowSums(output_svea$multiOut[,1:5,43,,1])/5
GG_sn <- rowSums(output_sn$multiOut[,1:5,43,,1])/5
GG_nn <- rowSums(output_nn$multiOut[,1:5,43,,1])/5
GG_sweden <- rowSums(output_m$multiOut[,1:5,43,,1])/5

megaGG <- list("got" = list("ssss" = avGG_ssss[reg$got$plot_run], "ts" = GG_got), 
               "svea" = list("ssss" = avGG_ssss[reg$svea$plot_run], "ts" = GG_svea), 
               "sn" = list("ssss" = avGG_ssss[reg$sn$plot_run], "ts" = GG_sn), 
               "nn" = list("ssss" = avGG_ssss[reg$nn$plot_run], "ts" = GG_nn), 
               "sweden" = list("ssss" = avGG_ssss[reg$sweden$plot_run], "ts" = GG_sweden))


megaGGmelt <- melt(megaGG)
colnames(megaGGmelt) <- c("value", "source", "region")

megaGGmelt$region<- factor(megaGGmelt$region, levels = c("got", "svea", "sn", "nn", "sweden"))



ggplot(megaGGmelt, aes(x=region, y=value, fill=source)) + 
  geom_boxplot() +
  labs(title = "Gross growth in Sweden", fill = "",
       x="", y = "m3/ha") +
  theme(plot.title = element_text(hjust = 0.5, size =22), # title in the center
        axis.text = element_text(size=14), # axis text size
        axis.title = element_text(size=16), # axis title size
        legend.text = element_text(size=12)) + # legend text size
  scale_fill_manual(labels=prebas_twolabel, values = myCols2.2) + # legend labels & box colors
  
  scale_x_discrete(labels=regs_label)+ # x axis labels
  # measurement lines
  geom_segment(aes(x = 0.6, y = maiData[4,1], xend = 1.4, 
                   yend = maiData[4,1], linetype="Measurements"), color = "hotpink3")+
  geom_segment(aes(x = 1.6, y = maiData[4,2], xend = 2.4, 
                   yend = maiData[4,2], linetype="Measurements"), color = "hotpink3")+
  geom_segment(aes(x = 2.6, y = maiData[4,3], xend = 3.4, 
                   yend = maiData[4,3], linetype="Measurements"), color = "hotpink3")+
  geom_segment(aes(x = 3.6, y = maiData[4,4], xend = 4.4, 
                   yend = maiData[4,4], linetype="Measurements"), color = "hotpink3")+
  geom_segment(aes(x = 4.6, y = maiData[4,5], xend = 5.4, 
                   yend = maiData[4,5], linetype="Measurements"), color = "hotpink3")+
  theme(legend.title=element_blank()) # legend title off
