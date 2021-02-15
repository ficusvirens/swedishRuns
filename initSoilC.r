rotLengthOld <- rotLength

rotLength <- rotationLength(output)
species <- 1:3

rotLength[is.na(rotLength)] <- 100

soilC <- countSoilCstsp(output, species, rotLength, gvrun = 1)

# initialize soil C in steady state from previous run
soilC$treeLitter <- aperm(soilC$treeLitter,c(1,3,4,2))
soilSTST <- soilC$treeLitter
soilSTST <- soilC$treeLitter[,,1,1] + soilC$gvLitter
initPrebas_mineral$soilC[,1,,,] <- soilSTST #Steady state calculations

# ####### sensitivity analysis
nSites <- length(mineral)
# increase rotation length
initPrebas_mineral$inAclct[,1:5] <- rotLength*1.2
# no clearcut because of diameter
initPrebas_mineral$inAclct[,7] <- rep(999, nSites)
initPrebas_mineral$inDclct[,] <- rep(999, nSites)


#inc_rotLength <- prebas_gv

#normal_rotLength <- prebas_gv

#save(inc_rotLength, normal_rotLength, file="rdata/sensana.rdata")

litter_fol <- apply(output$multiOut[,,26,,1], 1:2, sum)
litter_fr <- apply(output$multiOut[,,27,,1], 1:2, sum)
litter_branch <- apply(output$multiOut[,,28,,1], 1:2, sum)
litter_wood <- apply(output$multiOut[,,29,,1], 1:2, sum)

plot(litter_fol[1292,], type="l")
lines(litter_fr[1292,], col="red")
lines(litter_branch[1292,], col="blue")
lines(litter_wood[1292,], col="green")

plot(litter_fol[1289,], type="l")
lines(litter_fr[1289,], col="red")
lines(litter_branch[1289,], col="blue")
lines(litter_wood[1289,], col="green")

plot(output$multiOut[3,,44,1,1], type="l")#, xlab="age", ylab="BA")

plot(output$multiOut[3,,11,1,1], type="l")#, xlab="age", ylab="BA")

output$p0y

p0x <- rowMeans(output$P0y)
ETSx <- rowMeans(output$ETSy)
lm(p0x~ETSx)


plot(p0x, ETSx)
plot(ETSx, p0x, xlim=c(0, 2000), ylim=c(0,1800))
abline(lm(p0x~ETSx))
abline(v=1700)
abline(h=1500)
abline(v=1000)
abline(h=1000)



litter.orig[1292,]


#------------ MAI --------------------

maiData <- read.csv("input/mai.csv", row.names = 1)

# 43 = gross growth
# this is gross growth for site 1, spruce, mean through all the simulation
mean(output$multiOut[1,,43,2,1])

# here we have mean MAI for Svealand separated to different species
mai_svea <- colMeans(colMeans(output$multiOut[svea_m,,43,,1]))
# got
mai_got <- colMeans(colMeans(output$multiOut[got_m,,43,,1]))
# sn
mai_sn <- colMeans(colMeans(output$multiOut[sn_m,,43,,1]))
# nn
mai_nn <- colMeans(colMeans(output$multiOut[nn_m,,43,,1]))


maiPrebas <- cbind(mai_nn, mai_sn, mai_svea, mai_got)
maiPrebas <- as.data.frame(maiPrebas)
maiPrebas$all <- rowMeans(maiPrebas)
all <- colSums(maiPrebas)
maiPrebas <- rbind(maiPrebas, all)
colnames(maiPrebas) <- c("nn", "sn", "svea", "got", "all")
rownames(maiPrebas) <- c("pine", "spruce", "deciduous", "all")

# MAI in Sweden total
barplot(as.matrix(maiPrebas[4,]), col="red", ylab = "m3/ha", main="MAI in Sweden 2011-2015 total", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiData[4,]), col="grey", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
myColors = c("grey", "red")
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# transparent colors
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
marLight <- rgb(139, 28, 98, max = 255, alpha = 80, names = "lt.maroon")
greyLight<- rgb(128, 128, 128, max = 255, alpha = 180, names = "lt.grey")
myColors = c("grey", "red")

# MAI in Sweden pine
barplot(as.matrix(maiData[1,]), col="grey", ylab = "m3/ha", ylim = c(0, 2.5), main="MAI in Sweden 2011-2015 pine", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiPrebas[1,]), col=c2, border = "red", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# MAI in Sweden spruce
barplot(as.matrix(maiPrebas[2,]), col="red", ylab = "m3/ha", main="MAI in Sweden 2011-2015 spruce", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiData[2,]), col="grey", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# MAI in Sweden deciduous
barplot(as.matrix(maiData[3,]), col="grey", ylab = "m3/ha", main="MAI in Sweden 2011-2015 deciduous", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiPrebas[3,]), col="red", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)


#-------------separate pure spruce stands--------------

pureSpruce <- which(output$multiOut[,1,30,3,1]==0 & output$multiOut[,1,30,1,1]==0)
purePine <- which(output$multiOut[,1,30,2,1]==0 & output$multiOut[,1,30,3,1]==0)
pureDeciduous <- which(output$multiOut[,1,30,1,1]==0 & output$multiOut[,1,30,2,1]==0)

# pure spruce stands in Götaland
pSpruce_got <- pureSpruce[which(pureSpruce %in% got_m)]

mai_spruce <- colMeans(colMeans(output$multiOut[pureSpruce,,43,,1]))
mai_pine <- colMeans(colMeans(output$multiOut[purePine,,43,,1]))
mai_deciduous <- colMeans(colMeans(output$multiOut[pureDeciduous,,43,,1]))

mai_ps_got <- colMeans(colMeans(output$multiOut[pSpruce_got,,43,,1]))


mai_pure <- cbind(mai_pine[1], mai_spruce[2], mai_deciduous[3])
colnames(mai_pure) <- c("pine", "spruce", "deciduous")

barplot(mai_pure)
barplot(as.matrix(maiData[]))






#--------- calculate mean for the rotation length
#------ add stand volume in the beginning

simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

# stand i, gross growth from year 1 to simulation end
output$multiOut[i,1:simLength[i],43,,1]
# and this is stand volume in the beginning
output$multiOut[i,1,30,,1]

mai_rot <- matrix(data = NA, nrow = nSites, ncol = 3)

for(i in 1:nSites) {
  mai_rot[i, 1:3] <- (colSums(output$multiOut[i,1:simLength[i],43,,1])+output$multiOut[i,1,30,,1])/rotLength[i]
}

all <- colMeans(mai_rot[mineral,])
svea <- colMeans(mai_rot[svea_m,])
sn <- colMeans(mai_rot[sn_m,])
got <- colMeans(mai_rot[got_m,])
nn <- colMeans(mai_rot[nn_m,])


maiRot <- data.frame(nn, sn, svea, got, all)
sumAll <- colSums(maiRot)
maiRot <- rbind(maiRot, as.vector(sumAll))

myColors = c("grey", "maroon3")

# MAI in Sweden all
barplot(as.matrix(maiRot[4,]), col="maroon3", ylab = "m3/ha", main="MAI in Sweden 2011-2015 total", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiData[4,]), col=greyLight, border="black", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# MAI in Sweden pine
barplot(as.matrix(maiData[1,]), col="grey", ylab = "m3/ha", ylim = c(0, 2.5), main="MAI in Sweden 2011-2015 pine", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiRot[1,]), col=marLight, border = "maroon4", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("bottomright", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# MAI in Sweden spruce
barplot(as.matrix(maiRot[2,]), col="maroon3", ylab = "m3/ha", main="MAI in Sweden 2011-2015 spruce", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiData[2,]), col=greyLight, add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# MAI in Sweden deciduous
barplot(as.matrix(maiData[3,]), col="grey", ylab = "m3/ha", main="MAI in Sweden 2011-2015 deciduous", names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
barplot(as.matrix(maiRot[3,]), col=marLight, border = "maroon4", add=T, names=c("Norra Norrland", "Södra Norrland", "Svealand", "Götaland", "Whole Sweden"))
legend("topleft", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)



#--------- new try with the pure stands --------

purePine <- which(output$multiOut[,1,30,2,1]==0 & output$multiOut[,1,30,3,1]==0)
pureSpruce <- which(output$multiOut[,1,30,3,1]==0 & output$multiOut[,1,30,1,1]==0)
pureDeciduous <- which(output$multiOut[,1,30,1,1]==0 & output$multiOut[,1,30,2,1]==0)

pPine <- colMeans(mai_rot[purePine,])[1]
pSpruce <- colMeans(mai_rot[pureSpruce,])[2]
pDec <- colMeans(mai_rot[pureDeciduous,])[3]

pSpec <- c(pPine, pSpruce, pDec)
barplot(pSpec, col="sienna3", ylab = "m3/ha", 
        main = "Average gross growth in one species stands in Sweden",
        names = c("pine", "spruce", "deciduous"))

pPine_bar <- mai_rot[purePine,1]
pSpruce_bar <- mai_rot[pureSpruce, 2]
pDec_bar <- mai_rot[pureDeciduous, 3]

boxplot(pPine_bar, pSpruce_bar, pDec_bar, 
        col="sienna3", ylab = "m3/ha",
        main = "Average gross growth in pure one species stands in Sweden",
        names = c("pine", "spruce", "deciduous"))

#------------ plot agains ETS------------------

# here's mean ETS for each climID
ETSs <- rowMeans(output$ETSy)

# here's climID for each site
siteInfoX$climID

# here's ETS for each site
ETSs[siteInfoX$climID]

# here's MAI total
plot(rowSums(mai_rot), ETSs[siteInfoX$climID], 
     main = "MAI in Sweden 2011-2015 total", 
     xlab = "MAI", ylab = "ETS")

# MAI pure pine
plot(mai_rot[purePine,1], ETSs[siteInfoX[purePine,]$climID], 
     main = "MAI in Sweden 2011-2015 pine", 
     xlab = "MAI", ylab = "ETS")

# MAI pure spruce
plot(mai_rot[pureSpruce,2], ETSs[siteInfoX[pureSpruce,]$climID], 
     main = "MAI in Sweden 2011-2015 spruce", 
     xlab = "MAI", ylab = "ETS")

# MAI pure deciduous
plot(mai_rot[pureDeciduous,3], ETSs[siteInfoX[pureDeciduous,]$climID], 
     main = "MAI in Sweden 2011-2015 deciduous", 
     xlab = "MAI", ylab = "ETS")


#------------- plot soilC age <80 and age >80----------

prebas_gv <- (rowSums(soilC_sites$treeLitter)+rowSums(soilC_sites$gvLitter))/10000

under80 <- which(InitialX$age<80)
over80 <- which(InitialX$age>=80)

u80 <- intersect(under80, mineral)
o80 <- intersect(over80, mineral)

myColors = c("grey", "maroon3")

boxplot(InitialX[u80]$c.tot.tha/10, prebas_gv[u80], InitialX[o80]$c.tot.tha/10, prebas_gv[o80],  
        col=myColors, ylab = "kg C / ha", ylim = c(0,20),
        main = "Soil C in Sweden",
        names = c("<80", "", ">80", ""))
legend("topright", fill = myColors, legend = c("Statistical data","Prebas"), horiz = F)

# ----------- different rotation lengths ------------------
simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

simLength1.5 <- simulationLength(output_1.5)
rotLength1.5 <- rotationLength(output_1.5, simLength1.5)

simLengthMax <- simulationLength(output_max)
rotLengthMax <- rotationLength(output_max, simLengthMax)

species <- 1:3

soilC_1 <- countSoilCstsp(output, species, gvrun=1, rotLength, simLength)
soilC_1.5 <- countSoilCstsp(output_1.5, species, gvrun=1, rotLength1.5, simLength1.5)
soilC_max <- countSoilCstsp(output_max, species, gvrun=1, rotLengthMax, simLengthMax)


prebas_gv1 <- (rowSums(soilC_1$treeLitter)+rowSums(soilC_1$gvLitter))/10000
prebas_gv1.5 <- (rowSums(soilC_1.5$treeLitter)+rowSums(soilC_1.5$gvLitter))/10000
prebas_gvmax <- (rowSums(soilC_max$treeLitter)+rowSums(soilC_max$gvLitter))/10000

data_soilC <- InitialX$c.tot.tha/10

prebas_got <- countSoilC(output_got, species)
prebas_svea <- countSoilC(output_svea, species)
prebas_sn <- countSoilC(output_sn, species)
prebas_nn <- countSoilC(output_nn, species)
prebas_m <- countSoilC(output_m, species)

c_got <- (sum(prebas_got$treeLitter)+sum(prebas_got$gvLitter))/10000
c_svea <- (sum(prebas_svea$treeLitter)+sum(prebas_svea$gvLitter))/10000
c_sn <- (sum(prebas_sn$treeLitter)+sum(prebas_sn$gvLitter))/10000
c_nn <- (sum(prebas_nn$treeLitter)+sum(prebas_nn$gvLitter))/10000
c_m <- (sum(prebas_m$treeLitter)+sum(prebas_m$gvLitter))/10000

c_got0 <- sum(prebas_got$treeLitter)/10000
c_svea0 <- sum(prebas_svea$treeLitter)/10000
c_sn0 <- sum(prebas_sn$treeLitter)/10000
c_nn0 <- sum(prebas_nn$treeLitter)/10000
c_m0 <- sum(prebas_m$treeLitter)/10000

snms = c("measurements", "normal rotation length", "1.5x rotation length", "max rotation length")

# soil C with different rotation lengths
boxplot(data_soilC[mineral], prebas_gv1[mineral], 
        prebas_gv1.5[mineral], prebas_gvmax[mineral], 
        main = "Soil C in Sweden",
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,20))
abline(h=c_m, col = "red")
abline(h=c_m0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

#hist(data_soilC[mineral], breaks = seq(0,80, by=1))
#hist(prebas_gv3[mineral], add=T, breaks = seq(0,50, by=1), col=c1)

#median(data_soilC[mineral], na.rm=T)
#median(prebas_gv3[mineral])
#mean(data_soilC[mineral], na.rm=T)
#mean(prebas_gv1[mineral])
svea_m <- intersect(regions$svea, mineral)
got_m <- intersect(regions$got, mineral)
sn_m <- intersect(regions$sn, mineral)
nn_m <- intersect(regions$nn, mineral)


# soil C with different rotation lengths in Svea
boxplot(data_soilC[svea_m], prebas_gv1[svea_m], 
        prebas_gv1.5[svea_m], prebas_gvmax[svea_m], 
        main = "Soil C in Svealand",
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,20))
abline(h=c_svea, col = "red")
abline(h=c_svea0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

# soil C with different rotation lengths in Got
boxplot(data_soilC[got_m], prebas_gv1[got_m], 
        prebas_gv1.5[got_m], prebas_gvmax[got_m], 
        main = "Soil C in Götaland",
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,20))
abline(h=c_got, col = "red")
abline(h=c_got0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

# soil C with different rotation lengths in SN
boxplot(data_soilC[sn_m], prebas_gv1[sn_m], 
        prebas_gv1.5[sn_m], prebas_gvmax[sn_m], 
        main = "Soil C in Södra Norrland",
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,15))
abline(h=c_sn, col = "red")
abline(h=c_sn0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

# soil C with different rotation lengths in NN
boxplot(data_soilC[nn_m], prebas_gv1[nn_m], 
        prebas_gv1.5[nn_m], prebas_gvmax[nn_m], 
        main = "Soil C in Norra Norrland",
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,15))
abline(h=c_nn, col = "red")
abline(h=c_nn0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

# check if there's mortality in method 1 # YES!
plot(output_svea$multiOut[5,,17,1,1], type="l")
lines(output_svea$multiOut[5,,17,2,1], col="red", type="l")
lines(output_svea$multiOut[5,,17,3,1], col="blue", type="l")


# ---------------- harvest levels -----------------

simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

harvMean <- vector()
for(i in 1:nSites) {
  harvMean[i] <- sum(output$multiOut[i,1:simLength[i],37,,1])/rotLength1.5[i]
}

rnms = c("Götaland", "Svealand", "Södra Norrland", "Norra Norrland", "All Sweden")

boxplot(harvMean[got_m], harvMean[svea_m], 
        harvMean[sn_m],harvMean[nn_m],
        harvMean[mineral],
        main = "Harvest levels in Sweden (Prebas, 1.5 rotation length)", 
        ylab = "m3/ha", 
        names = rnms)


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
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = "orangered3")
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = "royalblue4")
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = "seagreen4")

myColors <- c("orangered3", "royalblue4", "seagreen4")
legend("topright", title = "Data", fill = myColors, legend = c("1993","2003", "2013"), horiz = F)

#-------------- harvest levels: thinnings & clearcuts separately

simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

thinMean <- vector()
for(i in 1:nSites) {
  thinMean[i] <- sum(output$multiOut[i,1:simLength[i]-1,37,,1])/rotLengthMax[i]
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

rnms = c("Götaland", "Svealand", "Södra Norrland", "Norra Norrland", "All Sweden")

boxplot(thinMean[got_m], thinMean[svea_m], 
        thinMean[sn_m], thinMean[nn_m],
        thinMean[mineral],
        main = "Thinning levels in Sweden (Prebas, max rotation length)", 
        ylab = "m3/ha", 
        names = rnms)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = "orangered3")
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = "royalblue3")
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = "seagreen3")

myColors <- c("orangered3", "royalblue3", "seagreen3")
legend("bottomleft", title = "Data", fill = myColors, legend = c("1993","2003", "2013"), horiz = F)



ccMean <- vector()
for(i in 1:nSites) {
  ccMean[i] <- sum(output$multiOut[i,simLength[i],37,,1])/rotLengthMax[i]
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
        main = "Final felling levels in Sweden (Prebas, max rotation length)", 
        ylab = "m3/ha", 
        names = rnms)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = y93s, col = "orangered3")
segments(x0 = x0s, x1 = x1s, y0 = y03s, col = "royalblue3")
segments(x0 = x0s, x1 = x1s, y0 = y13s, col = "seagreen3")

myColors <- c("orangered3", "royalblue3", "seagreen3")
legend("topright", title = "Data", fill = myColors, legend = c("1993","2003", "2013"), horiz = F)



# ------------ soil C in Sweden boxplot -------------

prebas_got <- countSoilC(output_got, species)
prebas_svea <- countSoilC(output_svea, species)
prebas_sn <- countSoilC(output_sn, species)
prebas_nn <- countSoilC(output_nn, species)
prebas_m <- countSoilC(output_m, species)

c_got <- (sum(prebas_got$treeLitter)+sum(prebas_got$gvLitter))/10000
c_svea <- (sum(prebas_svea$treeLitter)+sum(prebas_svea$gvLitter))/10000
c_sn <- (sum(prebas_sn$treeLitter)+sum(prebas_sn$gvLitter))/10000
c_nn <- (sum(prebas_nn$treeLitter)+sum(prebas_nn$gvLitter))/10000
c_m <- (sum(prebas_m$treeLitter)+sum(prebas_m$gvLitter))/10000

c_got0 <- sum(prebas_got$treeLitter)/10000
c_svea0 <- sum(prebas_svea$treeLitter)/10000
c_sn0 <- sum(prebas_sn$treeLitter)/10000
c_nn0 <- sum(prebas_nn$treeLitter)/10000
c_m0 <- sum(prebas_m$treeLitter)/10000



rnms = c("Götaland", "Svealand", "Södra Norrland", "Norra Norrland", "All Sweden")


boxplot(InitialX[got_m]$c.tot.tha/10, InitialX[svea_m]$c.tot.tha/10,
        InitialX[sn_m]$c.tot.tha/10, InitialX[nn_m]$c.tot.tha/10, 
        InitialX[mineral]$c.tot.tha/10, 
        ylim = c(0, 40), 
        names = rnms, 
        main = "Soil C in Sweden")

# create data for segments
# n = number of boxes
n <- 5
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
ygvs <- c(c_got, c_svea, c_sn, c_nn, c_m)
ygv0s <- c(c_got0, c_svea0, c_sn0, c_nn0, c_m0)
# add segments
segments(x0 = x0s, x1 = x1s, y0 = ygvs, col = "orangered2")
segments(x0 = x0s, x1 = x1s, y0 = ygv0s, col = "royalblue3")

myColors <- c("orangered2", "royalblue3")
legend("topright", title = "Prebas", 
       fill = myColors, 
       legend = c("with gv","without gv"), 
       horiz = F)

#------------- scatterplot soil C ----------

plot(prebas_gv1[mineral], data_soilC[mineral], 
     xlim=c(2,11), pch = 16, cex = 0.5,
     ylab = "measurements kg/m2", xlab = "prebas kg/m2",
     main = "Soil C in Sweden")
points(prebas_gv2[mineral], data_soilC[mineral], col="red", pch = 16, cex = 0.5)
points(prebas_gv3[mineral], data_soilC[mineral], col="blue", pch = 16, cex = 0.5)
legend("topleft", fill=c("black", "red", "blue"), 
       legend = c("normal rotlength", "1.5 x rotlength", "max rotlength"),
       horiz=F)
