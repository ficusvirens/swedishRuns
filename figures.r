library(viridis)
library(RColorBrewer)
library(mapview)

load("rdata/sweden_landsdel.rdata")

# to make a pretty map
Landsdel <- sweden_landsdel
Landsdel$Landsdel <- c("Gotaland", "Norra Norrland", "Sodra Norrland", "Svealand")

# map with just the regions
mapview(Landsdel)

# ages of the stands
plots_sf$age <- as.numeric(initPrebas$multiInitVar[,2,])

myvars <- c("geometry", "age")
Age_of_plots <- plots_sf[myvars]

Plots <- plots_sf


#mapviewOptions(vector.palette = plasma,
 #              na.color = "magenta",
  #             layers.control.pos = "topright")

mapview(Landsdel) + mapview(Plots)


# soil carbon in steady state mineral soils

svea_m <- intersect(regions$svea, mineral)
got_m <- intersect(regions$got, mineral)
sn_m <- intersect(regions$sn, mineral)
nn_m <- intersect(regions$nn, mineral)

# soil carbon in steady state peatlands

svea_p <- intersect(regions$svea, peat)
got_p <- intersect(regions$got, peat)
sn_p <- intersect(regions$sn, peat)
nn_p <- intersect(regions$nn, peat)

# all
hist(InitialX$c.tot.tha/10, main="Soil carbon in Sweden", xlab="kg/m2")#, xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree+mean_soilC_gv)/10000, col="red")
abline(v=mean_soilC_tree/10000, col="blue")


# mineral soil
hist(InitialX[mineral]$c.tot.tha/10, main="Soil carbon in Sweden mineral", xlab="kg/m2")#, xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree+mean_soilC_gv)/10000, col="red")
abline(v=mean_soilC_tree/10000, col="blue")

# peatland

hist(InitialX[peat]$c.tot.tha/10, main="Soil carbon in Sweden peat", xlab="kg/m2")#, xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_p_gv/10000, col="red")
abline(v=mean_soilC_p_gv0/10000, col="blue")



hist(InitialX[svea_m]$c.tot.tha/10, main="Soil carbon in Svealand mineral", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree_svea+mean_soilC_gv_svea)/10000, col="red")
abline(v=mean_soilC_tree_svea/10000, col="blue")

hist(InitialX[got_m]$c.tot.tha/10, main="Soil carbon in G?taland mineral", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree_got+mean_soilC_gv_got)/10000, col="red")
abline(v=mean_soilC_tree_got/10000, col="blue")

hist(InitialX[sn_m]$c.tot.tha/10, main="Soil carbon in S?dra Norrland mineral", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree_sn+mean_soilC_gv_sn)/10000, col="red")
abline(v=mean_soilC_tree_sn/10000, col="blue")

hist(InitialX[nn_m]$c.tot.tha/10, main="Soil carbon in Norra Norrland mineral", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=(mean_soilC_tree_nn+mean_soilC_gv_nn)/10000, col="red")
abline(v=mean_soilC_tree_nn/10000, col="blue")


# peatlands
hist(InitialX[svea_p]$c.tot.tha/10, main="Soil carbon in Svealand peatland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_svea_gv/10000, col="red")
abline(v=mean_soilC_svea_gv0/10000, col="blue")

hist(InitialX[got_p]$c.tot.tha/10, main="Soil carbon in G?taland peatland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_got_gv/10000, col="red")
abline(v=mean_soilC_got_gv0/10000, col="blue")

hist(InitialX[sn_p]$c.tot.tha/10, main="Soil carbon in S?dra Norrland peatland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_sn_gv/10000, col="red")
abline(v=mean_soilC_sn_gv0/10000, col="blue")

hist(InitialX[nn_p]$c.tot.tha/10, main="Soil carbon in Norra Norrland peatland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_nn_gv/10000, col="red")
abline(v=mean_soilC_nn_gv0/10000, col="blue")


################

myvars <- "c.tot.tha"
c_svea <- data.frame(InitialX[svea_m])[myvars]
c_got <- data.frame(InitialX[got_m])[myvars]
c_sn <- data.frame(InitialX[sn_m])[myvars]
c_nn <- data.frame(InitialX[nn_m])[myvars]


c_svea_melt <- melt(c_svea)
c_svea_melt$region <- "svea"
c_got_melt <- melt(c_got)
c_got_melt$region <- "got"
c_sn_melt <- melt(c_sn)
c_sn_melt$region <- "sn"
c_nn_melt <- melt(c_nn)
c_nn_melt$region <- "nn"

c_melt_all <- rbind(c_svea_melt, c_got_melt, c_sn_melt, c_nn_melt)
# to kg/m2
c_melt_all$value <- c_melt_all$value/10


boxplot(value~variable+region, data=c_melt_all,  
        xlab="region", ylab="soil C (kg/m2)", main="Soil C in Sweden", names = c("G?taland", "Norra Norrland", "S?dra Norrland", "Svealand"))


svea_c_r <- (mean_soilC_tree_svea+mean_soilC_gv_svea)/10000
got_c_r <- (mean_soilC_tree_got+mean_soilC_gv_got)/10000
sn_c_r <- (mean_soilC_tree_sn+mean_soilC_gv_sn)/10000
nn_c_r <- (mean_soilC_tree_nn+mean_soilC_gv_nn)/10000

svea_c_r0 <- (mean_soilC_tree_svea)/10000
got_c_r0 <- (mean_soilC_tree_got)/10000
sn_c_r0 <- (mean_soilC_tree_sn)/10000
nn_c_r0 <- (mean_soilC_tree_nn)/10000


# create data for segments
# n = number of boxes
n <- 4
# width of each boxplot is 0.8
x0s <- 1:n - 0.4
x1s <- 1:n + 0.4
# these are the y-coordinates for the horizontal lines
# that you need to set to the desired values.
y0s <- c(got_c_r, nn_c_r, sn_c_r, svea_c_r)

# add segments
segments(x0 = x0s, x1 = x1s, y0 = y0s, col = "red")

# without gv
y0s_b <- c(got_c_r0, nn_c_r0, sn_c_r0, svea_c_r0)

# add segments
segments(x0 = x0s, x1 = x1s, y0 = y0s_b, col = "blue")



hist(InitialX[regions$svea]$c.tot.tha/10, main="Soil carbon in Svealand", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_svea_gv/10000, col="red")
abline(v=mean_soilC_svea_gv0/10000, col="blue")
#abline(v=7.86, col="red")
#abline(v=9.42, col="blue")

hist(InitialX[regions$got]$c.tot.tha/10, main="Soil carbon in G?taland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_got_gv/10000, col="red")
abline(v=mean_soilC_got_gv0/10000, col="blue")
#abline(v=9.30, col="red")
#abline(v=11.44, col="blue")

hist(InitialX[regions$sn]$c.tot.tha/10, main="Soil carbon in S?dra Norrland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_sn_gv/10000, col="red")
abline(v=mean_soilC_sn_gv0/10000, col="blue")
#abline(v=6.86, col="red")
#abline(v=7.84, col="blue")

hist(InitialX[regions$nn]$c.tot.tha/10, main="Soil carbon in Norra Norrland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=mean_soilC_nn_gv/10000, col="red")
abline(v=mean_soilC_nn_gv0/10000, col="blue")
#abline(v=5.77, col="red")
#abline(v=6.40, col="blue")



median(InitialX[regions$svea]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$svea]$c.tot.tha/10, na.rm = T)   

median(InitialX[regions$got]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$got]$c.tot.tha/10, na.rm = T)  

median(InitialX[regions$sn]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$sn]$c.tot.tha/10, na.rm = T)  

median(InitialX[regions$nn]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$nn]$c.tot.tha/10, na.rm = T)  

# soil C vs site type
plot(Initial[regions$svea]$siteType, Initial[regions$svea]$c.tot.tha, main="soil C Svealand", xlab="site type", ylab="soil C, t/ha")
plot(Initial[regions$got]$siteType, Initial[regions$got]$c.tot.tha, main="soil C Götaland", xlab="site type", ylab="soil C, t/ha")
plot(Initial[regions$sn]$siteType, Initial[regions$sn]$c.tot.tha, main="soil C Södra Norrland", xlab="site type", ylab="soil C, t/ha")
plot(Initial[regions$nn]$siteType, Initial[regions$nn]$c.tot.tha, main="soil C Norra Norrland", xlab="site type", ylab="soil C, t/ha")

myvars <- c("siteType", "c.tot.tha")
c_svea <- data.frame(Initial[regions$svea])[myvars]
c_got <- data.frame(Initial[regions$got])[myvars]
c_sn <- data.frame(Initial[regions$sn])[myvars]
c_nn <- data.frame(Initial[regions$nn])[myvars]

c_svea_melt <- melt(c_svea, id.vars="siteType")
c_got_melt <- melt(c_got, id.vars="siteType")
c_sn_melt <- melt(c_sn, id.vars="siteType")
c_nn_melt <- melt(c_nn, id.vars="siteType")


boxplot(value~variable+siteType, data=c_svea_melt,  
        xlab="site type", ylab="soil C (t/ha)", main="soil C, Svealand", names = c("1", "2", "3", "4"))
boxplot(value~variable+siteType, data=c_got_melt,  
        xlab="site type", ylab="soil C (t/ha)", main="soil C, Götaland", names = c("1", "2", "3", "4"))
boxplot(value~variable+siteType, data=c_sn_melt,  
        xlab="site type", ylab="soil C (t/ha)", main="soil C, Södra Norrland", names = c("1", "2", "3", "4"))
boxplot(value~variable+siteType, data=c_nn_melt,  
        xlab="site type", ylab="soil C (t/ha)", main="soil C, Norra Norrland", names = c("1", "2", "3", "4"))


myvars <- c("siteType", "age")
age_svea <- data.frame(InitialX[regions$svea])[myvars]
age_got <- data.frame(InitialX[regions$got])[myvars]
age_sn <- data.frame(InitialX[regions$sn])[myvars]
age_nn <- data.frame(InitialX[regions$nn])[myvars]

age_svea_melt <- melt(age_svea, id.vars="siteType")
age_got_melt <- melt(age_got, id.vars="siteType")
age_sn_melt <- melt(age_sn, id.vars="siteType")
age_nn_melt <- melt(age_nn, id.vars="siteType")


boxplot(value~variable+siteType, data=age_svea_melt,  
        xlab="site type", ylab="age", main="age, Svealand", names = c("2", "3", "4", "5"))
boxplot(value~variable+siteType, data=age_got_melt,  
        xlab="site type", ylab="age", main="age, Götaland", names = c("2", "3", "4", "5"))
boxplot(value~variable+siteType, data=age_sn_melt,  
        xlab="site type", ylab="age", main="age, Södra Norrland", names = c("2", "3", "4", "5"))
boxplot(value~variable+siteType, data=age_nn_melt,  
        xlab="site type", ylab="age", main="age, Norra Norrland", names = c("2", "3", "4", "5"))




# plot height vs. age by site type

st1 <- which(initPrebas$siteInfo[,3]==1)
st2 <- which(initPrebas$siteInfo[,3]==2)
st3 <- which(initPrebas$siteInfo[,3]==3)
st4 <- which(initPrebas$siteInfo[,3]==4)
plot(initPrebas$multiInitVar[st1,2,],initPrebas$multiInitVar[st1,3,])
points(initPrebas$multiInitVar[st2,2,],initPrebas$multiInitVar[st2,3,], col=2)
points(initPrebas$multiInitVar[st3,2,],initPrebas$multiInitVar[st3,3,], col=3)
points(initPrebas$multiInitVar[st4,2,],initPrebas$multiInitVar[st4,3,], col=4)

#site, year, variable, layer
rowSums(output_svea_gv[[1]]$multiOut[,,26:29,,1], dims=3)

sveaLitter <- list()
gotLitter <- list()
snLitter <- list()
nnLitter <- list()

for(i in 1:3){
  output_svea_gv[[i]] <- regionPrebas(initPrebas_svea[[i]], c(hLimSvea[[i]], 0))
  sveaLitter[[i]] <- rowMeans(rowSums(output_svea_gv[[i]]$multiOut[,,26:29,,1], dims=2))
}

for(i in 1:3){
  output_got_gv[[i]] <- regionPrebas(initPrebas_got[[i]], c(hLimGot[[i]], 0))
  gotLitter[[i]] <- rowMeans(rowSums(output_got_gv[[i]]$multiOut[,,26:29,,1], dims=2))
}

for(i in 1:3){
  output_sn_gv[[i]] <- regionPrebas(initPrebas_sn[[i]], c(hLimSN[[i]], 0))
  snLitter[[i]] <- rowMeans(rowSums(output_sn_gv[[i]]$multiOut[,,26:29,,1], dims=2))
}

for(i in 1:3){
  output_nn_gv[[i]] <- regionPrebas(initPrebas_nn[[i]], c(hLimNN[[i]], 0))
  nnLitter[[i]] <- rowMeans(rowSums(output_nn_gv[[i]]$multiOut[,,26:29,,1], dims=2))
}

sveaLitter_melt <- melt(sveaLitter)
gotLitter_melt <- melt(gotLitter)
snLitter_melt <- melt(snLitter)
nnLitter_melt <- melt(nnLitter)

boxplot(value~L1, data=sveaLitter_melt,  
        xlab="site type", ylab="litter", main="litter inputs, Svealand", names = c("2", "3", "4+5"))
boxplot(value~L1, data=gotLitter_melt,  
        xlab="site type", ylab="litter", main="litter inputs, Götaland", names = c("2", "3", "4+5"))
boxplot(value~L1, data=snLitter_melt,  
        xlab="site type", ylab="litter", main="litter inputs, Södra Norrland", names = c("2", "3", "4+5"))
boxplot(value~L1, data=nnLitter_melt,  
        xlab="site type", ylab="litter", main="litter inputs, Norra Norrland", names = c("2", "3", "4+5"))


# litter inputs, above ground & below ground
# THIS IS MESSED UP

# SVEALAND

sveaLitter <- list()
# above ground
for(i in 1:3) {
  sveaLitter[[i]] <- output_st_svea[[i]]$GVabgW[,1,1]+output_st_svea[[i]]$GVabgW[,1,2]+output_st_svea[[i]]$GVabgW[,1,3]
}

sveaLitter_abg <- melt(sveaLitter)
colnames(sveaLitter_abg) <- c("abg", "sT")

sveaLitter <- list()
# below ground
for(i in 1:3) {
  sveaLitter[[i]] <- output_st_svea[[i]]$GVbgW[,1,1]+output_st_svea[[i]]$GVbgW[,1,2]
}

sveaLitter_bg <- melt(sveaLitter)
colnames(sveaLitter_bg) <- c("bg", "sT")
sveaLitter_all <- merge(sveaLitter_abg, sveaLitter_bg)
sveaLitter_melt <- melt(sveaLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=sveaLitter_melt, col=myColors,  main="Litter input, Svealand", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)


# GÖTALAND

gotLitter <- list()
# above ground
for(i in 1:3) {
  gotLitter[[i]] <- output_st_got[[i]]$GVabgW[,1,1]+output_st_got[[i]]$GVabgW[,1,2]+output_st_got[[i]]$GVabgW[,1,3]
}

gotLitter_abg <- melt(gotLitter)
colnames(gotLitter_abg) <- c("abg", "sT")

gotLitter <- list()
# below ground
for(i in 1:3) {
  gotLitter[[i]] <- output_st_got[[i]]$GVbgW[,1,1]+output_st_got[[i]]$GVbgW[,1,2]
}

gotLitter_bg <- melt(gotLitter)
colnames(gotLitter_bg) <- c("bg", "sT")
gotLitter_all <- merge(gotLitter_abg, gotLitter_bg)
gotLitter_melt <- melt(gotLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=gotLitter_melt, col=myColors,  main="Litter input, Götaland", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)


# SÖDRA NORRLAND


snLitter <- list()
# above ground
for(i in 1:3) {
  snLitter[[i]] <- output_st_sn[[i]]$GVabgW[,1,1]+output_st_sn[[i]]$GVabgW[,1,2]+output_st_sn[[i]]$GVabgW[,1,3]
}

snLitter_abg <- melt(snLitter)
colnames(snLitter_abg) <- c("abg", "sT")

snLitter <- list()
# below ground
for(i in 1:3) {
  snLitter[[i]] <- output_st_sn[[i]]$GVbgW[,1,1]+output_st_sn[[i]]$GVbgW[,1,2]
}

snLitter_bg <- melt(snLitter)
colnames(snLitter_bg) <- c("bg", "sT")
snLitter_all <- merge(snLitter_abg, snLitter_bg)
snLitter_melt <- melt(snLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=snLitter_melt, col=myColors,  main="Litter input, Södra Norrland", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)

# NORRA NORRLAND


nnLitter <- list()
# above ground
for(i in 1:3) {
  nnLitter[[i]] <- output_st_nn[[i]]$GVabgW[,1,1]+output_st_nn[[i]]$GVabgW[,1,2]+output_st_nn[[i]]$GVabgW[,1,3]
}

nnLitter_abg <- melt(nnLitter)
colnames(nnLitter_abg) <- c("abg", "sT")

nnLitter <- list()
# below ground
for(i in 1:3) {
  nnLitter[[i]] <- output_st_nn[[i]]$GVbgW[,1,1]+output_st_nn[[i]]$GVbgW[,1,2]
}

nnLitter_bg <- melt(nnLitter)
colnames(nnLitter_bg) <- c("bg", "sT")
nnLitter_all <- merge(nnLitter_abg, nnLitter_bg)
nnLitter_melt <- melt(nnLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=nnLitter_melt, col=myColors,  main="Litter input, Norra Norrland", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)

save(sveaLitter_all, gotLitter_all, snLitter_all, nnLitter_all, file="rdata/gvLitter_all.rdata")

# same from data
# SVEALAND
svea_data <- list()
svea_abg <- list()
svea_bg <- list()

for(i in 1:3) {
  svea_data[[i]] <- InitialX[svea[[i]]]
  svea_abg[[i]] <- svea_data[[i]]$gvb.mod.abv.dwarfshrub/2+svea_data[[i]]$gvb.mod.abv.herb/2+svea_data[[i]]$gvb.mod.abv.grass/2+svea_data[[i]]$gvb.mod.abv.moss/2+svea_data[[i]]$gvb.mod.abv.lichen/2
  svea_bg[[i]] <- svea_data[[i]]$gvb.mod.belw.dwarfshrub/2+svea_data[[i]]$gvb.mod.belw.herb/2+svea_data[[i]]$gvb.mod.belw.grass/2
}

m_svea_abg <- melt(svea_abg)
m_svea_bg <- melt(svea_bg)

colnames(m_svea_abg) <- c("abg", "sT")
colnames(m_svea_bg) <- c("bg", "sT")
sveaLitter_all <- merge(m_svea_abg, m_svea_bg)
sveaLitter_melt <- melt(sveaLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=sveaLitter_melt, col=myColors,  main="Litter input, Svealand from data", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)

# GÖTALAND

got_data <- list()
got_abg <- list()
got_bg <- list()

for(i in 1:3) {
  got_data[[i]] <- InitialX[got[[i]]]
  got_abg[[i]] <- got_data[[i]]$gvb.mod.abv.dwarfshrub/2+got_data[[i]]$gvb.mod.abv.herb/2+got_data[[i]]$gvb.mod.abv.grass/2+got_data[[i]]$gvb.mod.abv.moss/2+got_data[[i]]$gvb.mod.abv.lichen/2
  got_bg[[i]] <- got_data[[i]]$gvb.mod.belw.dwarfshrub/2+got_data[[i]]$gvb.mod.belw.herb/2+got_data[[i]]$gvb.mod.belw.grass/2
}

m_got_abg <- melt(got_abg)
m_got_bg <- melt(got_bg)

colnames(m_got_abg) <- c("abg", "sT")
colnames(m_got_bg) <- c("bg", "sT")
gotLitter_all <- merge(m_got_abg, m_got_bg)
gotLitter_melt <- melt(gotLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=gotLitter_melt, col=myColors,  main="Litter input, Götaland from data", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)

# SÖDRA NORRLAND

sn_data <- list()
sn_abg <- list()
sn_bg <- list()

for(i in 1:3) {
  sn_data[[i]] <- InitialX[sn[[i]]]
  sn_abg[[i]] <- sn_data[[i]]$gvb.mod.abv.dwarfshrub/2+sn_data[[i]]$gvb.mod.abv.herb/2+sn_data[[i]]$gvb.mod.abv.grass/2+sn_data[[i]]$gvb.mod.abv.moss/2+sn_data[[i]]$gvb.mod.abv.lichen/2
  sn_bg[[i]] <- sn_data[[i]]$gvb.mod.belw.dwarfshrub/2+sn_data[[i]]$gvb.mod.belw.herb/2+sn_data[[i]]$gvb.mod.belw.grass/2
}

m_sn_abg <- melt(sn_abg)
m_sn_bg <- melt(sn_bg)

colnames(m_sn_abg) <- c("abg", "sT")
colnames(m_sn_bg) <- c("bg", "sT")
snLitter_all <- merge(m_sn_abg, m_sn_bg)
snLitter_melt <- melt(snLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=snLitter_melt, col=myColors,  main="Litter input, Södra Norrland from data", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)

# NORRA NORRLAND

nn_data <- list()
nn_abg <- list()
nn_bg <- list()

for(i in 1:3) {
  nn_data[[i]] <- InitialX[nn[[i]]]
  nn_abg[[i]] <- nn_data[[i]]$gvb.mod.abv.dwarfshrub/2+nn_data[[i]]$gvb.mod.abv.herb/2+nn_data[[i]]$gvb.mod.abv.grass/2+nn_data[[i]]$gvb.mod.abv.moss/2+nn_data[[i]]$gvb.mod.abv.lichen/2
  nn_bg[[i]] <- nn_data[[i]]$gvb.mod.belw.dwarfshrub/2+nn_data[[i]]$gvb.mod.belw.herb/2+nn_data[[i]]$gvb.mod.belw.grass/2
}

m_nn_abg <- melt(nn_abg)
m_nn_bg <- melt(nn_bg)

colnames(m_nn_abg) <- c("abg", "sT")
colnames(m_nn_bg) <- c("bg", "sT")
nnLitter_all <- merge(m_nn_abg, m_nn_bg)
nnLitter_melt <- melt(nnLitter_all, id.vars="sT")


myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+sT, data=nnLitter_melt, col=myColors,  main="Litter input, Norra Norrland from data", xlab="site type", ylab="litter input", names = c("2", "", "3", "", "4+5", ""))
legend("topleft", fill = myColors, legend = c("above ground","below ground"), horiz = F)


# biomass comparison

# PINE
# stem
plot(initPrebas$multiOut[,1,31,1,1], InitialX$stem.pine/2, main = "Pine stem biomass")
abline(0,1)

# branch
plot(initPrebas$multiOut[,1,24,1,1], InitialX$branch.pine/2, main = "Pine branch biomass")
abline(0,1)

# needles
plot(initPrebas$multiOut[,1,33,1,1], InitialX$needles.pine/2, main = "Pine needles biomass")
abline(0,1)

# stump & root
plot((initPrebas$multiOut[,1,32,1,1]+initPrebas$multiOut[,1,25,1,1]), (InitialX$stump.pine+InitialX$root.pine)/2, main = "Pine root biomass")
abline(0,1)

# SPRUCE
# stem
plot(initPrebas$multiOut[,1,31,2,1], InitialX$stem.spruce/2, main = "Spruce stem biomass")
abline(0,1)

# branch
plot(initPrebas$multiOut[,1,24,2,1], InitialX$branch.spruce/2, main = "Spruce branch biomass")
abline(0,1)

# needles
plot(initPrebas$multiOut[,1,33,2,1], InitialX$needles.spruce/2, main = "Spruce needles biomass")
abline(0,1)

# stump & root
plot((initPrebas$multiOut[,1,32,2,1]+initPrebas$multiOut[,1,25,2,1]), (InitialX$stump.spruce+InitialX$root.spruce)/2, main = "Spruce root biomass")
abline(0,1)

# DECIDUOUS
# stem
plot(initPrebas$multiOut[,1,31,3,1], InitialX$stem.decid/2, main = "Deciduous stem biomass")
abline(0,1)

# branch
plot(initPrebas$multiOut[,1,24,3,1], InitialX$branch.decid/2, main = "Deciduous branch biomass")
abline(0,1)

# leaves
plot(initPrebas$multiOut[,1,33,3,1], InitialX$leaves.rep.decid/2, main = "Deciduous leaves biomass")
abline(0,1)

# stump & root
plot((initPrebas$multiOut[,1,32,3,1]+initPrebas$multiOut[,1,25,3,1]), (InitialX$stump.decid+InitialX$root.decid)/2, main = "Deciduous root biomass")
abline(0,1)




# above and below ground gv biomass

# above ground shrubs
d.a.s <- InitialX$gvb.mod.abv.dwarfshrub/2
m.a.s <- output$GVabgW[,1,1]

plot(d.a.s, m.a.s, main="biomass above ground shrubs", xlab="data", ylab="model")
abline(0,1)

# below ground shrubs
d.b.s <- InitialX$gvb.mod.belw.dwarfshrub/2
m.b.s <- output$GVbgW[,1,1]

plot(d.b.s, m.b.s, main="biomass below ground shrubs", xlab="data", ylab="model")
abline(0,1)

# above ground herbs & grasses
d.a.hg <- InitialX$gvb.mod.abv.herb/2+InitialX$gvb.mod.abv.grass/2
m.a.hg <- output$GVabgW[,1,2]

plot(d.a.hg, m.a.hg, main="biomass above ground herbs & grasses", xlab="data", ylab="model")
abline(0,1)

# below ground herbs & grasses
d.b.hg <- InitialX$gvb.mod.belw.herb/2+InitialX$gvb.mod.belw.grass/2
m.b.hg <- output$GVbgW[,1,2]

plot(d.b.hg, m.b.hg, main="biomass below ground herbs & grasses", xlab="data", ylab="model")
abline(0,1)


# above ground moss & lichen
d.a.ml <- InitialX$gvb.mod.abv.moss/2+InitialX$gvb.mod.abv.lichen/2
m.a.ml <- output$GVabgW[,1,3]  

plot(d.a.ml, m.a.ml, main="biomass above ground moss & lichen", xlab="data", ylab="model")
abline(0,1)

# total abobe ground
d.a.t <- d.a.s+d.a.hg+d.a.ml
m.a.t <- m.a.s+m.a.hg+m.a.ml

plot(d.a.t, m.a.t, main="biomass above ground total",  xlab="data", ylab="model")
abline(0,1)

# total below ground
d.b.t <- d.b.s+d.b.hg
m.b.t <- m.b.s+m.b.hg

plot(d.b.t, m.b.t, main="biomass below ground total",  xlab="data", ylab="model")
abline(0,1)

# grand total
d.t <- d.a.s+d.a.hg+d.a.ml+d.b.s+d.b.hg
m.t <- m.a.s+m.a.hg+m.a.ml+m.b.s+m.b.hg

plot(d.t, m.t, main="biomass total",  xlab="data", ylab="model")
abline(0,1)


# litter inputs, above & below ground, new try

load("rdata/litterdata.rdata")


names(gv.biomlit)
names(litter.orig)

gv.biomlitX <- gv.biomlit[siteX,]
gv.biomlitX[is.na(gv.biomlitX)]=0
litter.origX <- litter.orig[siteX,]
litter.origX[is.na(litter.origX)]=0

test <- litter.orig[siteX,]
test2 <- test[regions$got,]

# all country
abv_lo_m <- rowSums(output$multiOut[,1,26,,])+rowSums(output$multiOut[,1,28,,])+rowSums(output$multiOut[,1,29,,])
abv_gv_m <- output$GVabgW[,1,1]*0.37+output$GVabgW[,1,2]*0.54+output$GVabgW[,1,3]*0.2
ag_model <- abv_lo_m+abv_gv_m

bg_model <- rowSums(output$multiOut[,1,27,,])+output$GVbgW[,1,1]*0.08+output$GVbgW[,1,2]*0.59

abv_lo_d <- litter.origX$lit.stem.tot+litter.origX$lit.branch.tot+litter.origX$lit.foliage.tot+litter.origX$lit.stump.tot
abv_gv_d <- gv.biomlitX$gvlit.abv.dwarfshrub+gv.biomlitX$gvlit.abv.herb+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.abv.moss+gv.biomlitX$gvlit.abv.lichen
# /2 to convert to kg C/ha
ag_data <- (abv_lo_d+abv_gv_d)/2

blw_lo_d <- litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
blw_gv_d <- gv.biomlitX$gvlit.belw.dwarfshrub+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.belw.herb
bg_data <- (blw_lo_d+blw_gv_d)/2

plot(ag_model, ag_data)
abline(0,1)

test_ag <- which(ag_model<10000)

test_bg <- which(bg_model<6000)
# 23 radical results removed
plot(ag_model[test_ag], ag_data[test_ag], main="Above ground litter kg C/ha")
abline(0,1)

length(test_ag)
length(ag_model)

plot(bg_model, bg_data, main="Below ground litter kg C/ha")
abline(0,1)

hist(ag_model[test_ag], main = "Litter above ground all country", xlab = "kgC / ha")
abline(v=mean(ag_data), col="red")

hist(bg_model, main = "Litter below ground all country", xlab = "kgC / ha")
abline(v=mean(bg_data), col="red")

# split to tree foliage, woody components and groung vege

abv_tf_m <- rowSums(output$multiOut[,1,26,,])
abv_wc_m <- rowSums(output$multiOut[,1,28,,])+rowSums(output$multiOut[,1,29,,])
abv_gv_m <- output$GVabgW[,1,1]*0.37+output$GVabgW[,1,2]*0.54+output$GVabgW[,1,3]*0.2

abv_tf_d <- litter.origX$lit.foliage.tot/2
abv_wc_d <- (litter.origX$lit.stem.tot+litter.origX$lit.branch.tot+litter.origX$lit.stump.tot)/2
abv_gv_d <- (gv.biomlitX$gvlit.abv.dwarfshrub+gv.biomlitX$gvlit.abv.herb+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.abv.moss+gv.biomlitX$gvlit.abv.lichen)/2

# boxplot 
model_litter <- cbind(abv_tf_m, abv_wc_m, abv_gv_m)
colnames(model_litter) <- c("tf", "wc", "gv")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "partition", "litter_m")
tupek_litter <- cbind(abv_tf_d, abv_wc_d, abv_gv_d)
colnames(tupek_litter) <- c("tf", "wc", "gv")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","partition", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="partition")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+partition, data=litter_melt, col=myColors,  main="Above ground litter all country", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("tree foliage", "", "woody components", "", "ground vegetation", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# split by species

abv_pi_m <- rowSums(output$multiOut[,1,26,1,])+rowSums(output$multiOut[,1,28,1,])+rowSums(output$multiOut[,1,29,1,])
abv_su_m <- rowSums(output$multiOut[,1,26,2,])+rowSums(output$multiOut[,1,28,2,])+rowSums(output$multiOut[,1,29,2,])
abv_de_m <- rowSums(output$multiOut[,1,26,3,])+rowSums(output$multiOut[,1,28,3,])+rowSums(output$multiOut[,1,29,3,])

abv_pi_d <- (litter.origX$lit.stem.pi+litter.origX$lit.branch.pi+litter.origX$lit.foliage.pi+litter.origX$lit.stump.pi)/2
abv_su_d <- (litter.origX$lit.stem.su+litter.origX$lit.branch.su+litter.origX$lit.foliage.su+litter.origX$lit.stump.su)/2
abv_de_d <- (litter.origX$lit.stem.de+litter.origX$lit.branch.de+litter.origX$lit.foliage.de+litter.origX$lit.stump.de)/2

# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Above ground litter all country", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)


# only foliage
abv_pi_m <- rowSums(output$multiOut[,1,26,1,])#+rowSums(output$multiOut[,1,28,1,])+rowSums(output$multiOut[,1,29,1,])
abv_su_m <- rowSums(output$multiOut[,1,26,2,])#+rowSums(output$multiOut[,1,28,2,])+rowSums(output$multiOut[,1,29,2,])
abv_de_m <- rowSums(output$multiOut[,1,26,3,])#+rowSums(output$multiOut[,1,28,3,])+rowSums(output$multiOut[,1,29,3,])

abv_pi_d <- litter.origX$lit.foliage.pi/2
abv_su_d <- litter.origX$lit.foliage.su/2
abv_de_d <- litter.origX$lit.foliage.de/2


# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Foliage litter all country", xlab= "", ylab="kg C / ha", ylim=c(0,1500),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)



# SVEALAND
length(output_svea$multiOut[,1,26,1,1])

svea_abv_lo_m <- rowSums(output_svea$multiOut[,1,26,,])+rowSums(output_svea$multiOut[,1,28,,])+rowSums(output_svea$multiOut[,1,29,,])
svea_abv_gv_m <- output_svea$GVabgW[,1,1]*0.37+output_svea$GVabgW[,1,2]*0.54+output_svea$GVabgW[,1,3]*0.2
svea_ag_model <- svea_abv_lo_m+svea_abv_gv_m

svea_bg_model <- rowSums(output_svea$multiOut[,1,27,,])+output_svea$GVbgW[,1,1]*0.08+output_svea$GVbgW[,1,2]*0.59

svea.litter.origX <- litter.origX[regions$svea,]
svea.gv.biomlitX <- gv.biomlitX[regions$svea,]

svea_abv_lo_d <- svea.litter.origX$lit.stem.tot+svea.litter.origX$lit.branch.tot+svea.litter.origX$lit.foliage.tot+svea.litter.origX$lit.stump.tot
svea_abv_gv_d <- svea.gv.biomlitX$gvlit.abv.dwarfshrub+svea.gv.biomlitX$gvlit.abv.herb+svea.gv.biomlitX$gvlit.abv.grass+svea.gv.biomlitX$gvlit.abv.moss+svea.gv.biomlitX$gvlit.abv.lichen
# /2 to convert to kg C/ha
svea_ag_data <- (svea_abv_lo_d+svea_abv_gv_d)/2

svea_blw_lo_d <- svea.litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
svea_blw_gv_d <- svea.gv.biomlitX$gvlit.belw.dwarfshrub+svea.gv.biomlitX$gvlit.abv.grass+svea.gv.biomlitX$gvlit.belw.herb
svea_bg_data <- (svea_blw_lo_d+svea_blw_gv_d)/2

plot(svea_ag_model, svea_ag_data)
abline(0,1)

test_svea_ag <- which(svea_ag_model<8000)

# 11 radical results removed
plot(svea_ag_model[test_svea_ag], svea_ag_data[test_svea_ag], main="Svealand above ground litter kg C/ha")
abline(0,1)

length(test_svea_ag)
length(svea_ag_model)

plot(svea_bg_model, svea_bg_data, main="Svealand below ground litter kg C/ha")
abline(0,1)


hist(test_svea_ag, main = "Litter above ground Svealand", xlab = "kgC / ha", xlim=c(0,1600))
abline(v=mean(svea_ag_data), col="red")

hist(svea_bg_model, main = "Litter below ground Svealand", xlab = "kgC / ha")
abline(v=mean(svea_bg_data), col="red")
# split to tree foliage, woody components and groung vege

abv_tf_m <- rowSums(output_svea$multiOut[,1,26,,])
abv_wc_m <- rowSums(output_svea$multiOut[,1,28,,])+rowSums(output_svea$multiOut[,1,29,,])
abv_gv_m <- output_svea$GVabgW[,1,1]*0.37+output_svea$GVabgW[,1,2]*0.54+output_svea$GVabgW[,1,3]*0.2

abv_tf_d <- svea.litter.origX$lit.foliage.tot/2
abv_wc_d <- (svea.litter.origX$lit.stem.tot+svea.litter.origX$lit.branch.tot+svea.litter.origX$lit.stump.tot)/2
abv_gv_d <- (svea.gv.biomlitX$gvlit.abv.dwarfshrub+svea.gv.biomlitX$gvlit.abv.herb+svea.gv.biomlitX$gvlit.abv.grass+svea.gv.biomlitX$gvlit.abv.moss+svea.gv.biomlitX$gvlit.abv.lichen)/2

# boxplot 
model_litter <- cbind(abv_tf_m, abv_wc_m, abv_gv_m)
colnames(model_litter) <- c("tf", "wc", "gv")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "partition", "litter_m")
tupek_litter <- cbind(abv_tf_d, abv_wc_d, abv_gv_d)
colnames(tupek_litter) <- c("tf", "wc", "gv")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","partition", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="partition")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+partition, data=litter_melt, col=myColors,  main="Above ground litter Svealand", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("tree foliage", "", "woody components", "", "ground vegetation", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# split by species

abv_pi_m <- rowSums(output_svea$multiOut[,1,26,1,])+rowSums(output_svea$multiOut[,1,28,1,])+rowSums(output_svea$multiOut[,1,29,1,])
abv_su_m <- rowSums(output_svea$multiOut[,1,26,2,])+rowSums(output_svea$multiOut[,1,28,2,])+rowSums(output_svea$multiOut[,1,29,2,])
abv_de_m <- rowSums(output_svea$multiOut[,1,26,3,])+rowSums(output_svea$multiOut[,1,28,3,])+rowSums(output_svea$multiOut[,1,29,3,])

abv_pi_d <- (svea.litter.origX$lit.stem.pi+svea.litter.origX$lit.branch.pi+svea.litter.origX$lit.foliage.pi+svea.litter.origX$lit.stump.pi)/2
abv_su_d <- (svea.litter.origX$lit.stem.su+svea.litter.origX$lit.branch.su+svea.litter.origX$lit.foliage.su+svea.litter.origX$lit.stump.su)/2
abv_de_d <- (svea.litter.origX$lit.stem.de+svea.litter.origX$lit.branch.de+svea.litter.origX$lit.foliage.de+svea.litter.origX$lit.stump.de)/2

# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Above ground litter Svealand", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)


# GÖTALAND

length(output_got$multiOut[,1,26,1,1])

got_abv_lo_m <- rowSums(output_got$multiOut[,1,26,,])+rowSums(output_got$multiOut[,1,28,,])+rowSums(output_got$multiOut[,1,29,,])
got_abv_gv_m <- output_got$GVabgW[,1,1]*0.37+output_got$GVabgW[,1,2]*0.54+output_got$GVabgW[,1,3]*0.2
got_ag_model <- got_abv_lo_m+got_abv_gv_m

got_bg_model <- rowSums(output_got$multiOut[,1,27,,])+output_got$GVbgW[,1,1]*0.08+output_got$GVbgW[,1,2]*0.59

got.litter.origX <- litter.origX[regions$got,]
got.gv.biomlitX <- gv.biomlitX[regions$got,]

got_abv_lo_d <- got.litter.origX$lit.stem.tot+got.litter.origX$lit.branch.tot+got.litter.origX$lit.foliage.tot+got.litter.origX$lit.stump.tot
got_abv_gv_d <- got.gv.biomlitX$gvlit.abv.dwarfshrub+got.gv.biomlitX$gvlit.abv.herb+got.gv.biomlitX$gvlit.abv.grass+got.gv.biomlitX$gvlit.abv.moss+got.gv.biomlitX$gvlit.abv.lichen
# /2 to convert to kg C/ha
got_ag_data <- (got_abv_lo_d+got_abv_gv_d)/2

got_blw_lo_d <- got.litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
got_blw_gv_d <- got.gv.biomlitX$gvlit.belw.dwarfshrub+got.gv.biomlitX$gvlit.abv.grass+got.gv.biomlitX$gvlit.belw.herb
got_bg_data <- (got_blw_lo_d+got_blw_gv_d)/2

plot(got_ag_model, got_ag_data)
abline(0,1)

test_got_ag <- which(got_ag_model<8000)

# 12 radical results removed
plot(got_ag_model[test_got_ag], got_ag_data[test_got_ag], main="Götaland above ground litter kg C/ha")
abline(0,1)

length(test_got_ag)
length(got_ag_model)

plot(got_bg_model, got_bg_data, main="Götaland below ground litter kg C/ha")
abline(0,1)


hist(test_got_ag, main = "Litter above ground Götaland", xlab = "kgC / ha", xlim=c(0,1700))
abline(v=mean(got_ag_data), col="red")

hist(got_bg_model, main = "Litter below ground Götaland", xlab = "kgC / ha")
abline(v=mean(got_bg_data), col="red")

# split to tree foliage, woody components and groung vege

abv_tf_m <- rowSums(output_got$multiOut[,1,26,,])
abv_wc_m <- rowSums(output_got$multiOut[,1,28,,])+rowSums(output_got$multiOut[,1,29,,])
abv_gv_m <- output_got$GVabgW[,1,1]*0.37+output_got$GVabgW[,1,2]*0.54+output_got$GVabgW[,1,3]*0.2

abv_tf_d <- got.litter.origX$lit.foliage.tot/2
abv_wc_d <- (got.litter.origX$lit.stem.tot+got.litter.origX$lit.branch.tot+got.litter.origX$lit.stump.tot)/2
abv_gv_d <- (got.gv.biomlitX$gvlit.abv.dwarfshrub+got.gv.biomlitX$gvlit.abv.herb+got.gv.biomlitX$gvlit.abv.grass+got.gv.biomlitX$gvlit.abv.moss+got.gv.biomlitX$gvlit.abv.lichen)/2

# boxplot 
model_litter <- cbind(abv_tf_m, abv_wc_m, abv_gv_m)
colnames(model_litter) <- c("tf", "wc", "gv")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "partition", "litter_m")
tupek_litter <- cbind(abv_tf_d, abv_wc_d, abv_gv_d)
colnames(tupek_litter) <- c("tf", "wc", "gv")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","partition", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="partition")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+partition, data=litter_melt, col=myColors,  main="Above ground litter Götaland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("tree foliage", "", "woody components", "", "ground vegetation", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# split by species

abv_pi_m <- rowSums(output_got$multiOut[,1,26,1,])+rowSums(output_got$multiOut[,1,28,1,])+rowSums(output_got$multiOut[,1,29,1,])
abv_su_m <- rowSums(output_got$multiOut[,1,26,2,])+rowSums(output_got$multiOut[,1,28,2,])+rowSums(output_got$multiOut[,1,29,2,])
abv_de_m <- rowSums(output_got$multiOut[,1,26,3,])+rowSums(output_got$multiOut[,1,28,3,])+rowSums(output_got$multiOut[,1,29,3,])

abv_pi_d <- (got.litter.origX$lit.stem.pi+got.litter.origX$lit.branch.pi+got.litter.origX$lit.foliage.pi+got.litter.origX$lit.stump.pi)/2
abv_su_d <- (got.litter.origX$lit.stem.su+got.litter.origX$lit.branch.su+got.litter.origX$lit.foliage.su+got.litter.origX$lit.stump.su)/2
abv_de_d <- (got.litter.origX$lit.stem.de+got.litter.origX$lit.branch.de+got.litter.origX$lit.foliage.de+got.litter.origX$lit.stump.de)/2

# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Above ground litter Götaland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)


# SÖDRA NORRLAND

length(output_sn$multiOut[,1,26,1,1])

sn_abv_lo_m <- rowSums(output_sn$multiOut[,1,26,,])+rowSums(output_sn$multiOut[,1,28,,])+rowSums(output_sn$multiOut[,1,29,,])
sn_abv_gv_m <- output_sn$GVabgW[,1,1]*0.37+output_sn$GVabgW[,1,2]*0.54+output_sn$GVabgW[,1,3]*0.2
sn_ag_model <- sn_abv_lo_m+sn_abv_gv_m

sn_bg_model <- rowSums(output_sn$multiOut[,1,27,,])+output_sn$GVbgW[,1,1]*0.08+output_sn$GVbgW[,1,2]*0.59

sn.litter.origX <- litter.origX[regions$sn,]
sn.gv.biomlitX <- gv.biomlitX[regions$sn,]

sn_abv_lo_d <- sn.litter.origX$lit.stem.tot+sn.litter.origX$lit.branch.tot+sn.litter.origX$lit.foliage.tot+sn.litter.origX$lit.stump.tot
sn_abv_gv_d <- sn.gv.biomlitX$gvlit.abv.dwarfshrub+sn.gv.biomlitX$gvlit.abv.herb+sn.gv.biomlitX$gvlit.abv.grass+sn.gv.biomlitX$gvlit.abv.moss+sn.gv.biomlitX$gvlit.abv.lichen
# /2 to convert to kg C/ha
sn_ag_data <- (sn_abv_lo_d+sn_abv_gv_d)/2

sn_blw_lo_d <- sn.litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
sn_blw_gv_d <- sn.gv.biomlitX$gvlit.belw.dwarfshrub+sn.gv.biomlitX$gvlit.abv.grass+sn.gv.biomlitX$gvlit.belw.herb
sn_bg_data <- (sn_blw_lo_d+sn_blw_gv_d)/2

plot(sn_ag_model, sn_ag_data)
abline(0,1)

test_sn_ag <- which(sn_ag_model<8000)

# 6 radical results removed
plot(sn_ag_model[test_sn_ag], sn_ag_data[test_sn_ag], main="Södra Norrland above ground litter kg C/ha")
abline(0,1)

length(test_sn_ag)
length(sn_ag_model)

plot(sn_bg_model, sn_bg_data, main="Södra Norrland below ground litter kg C/ha")
abline(0,1)


hist(test_sn_ag, main = "Litter above ground Södra Norrland", xlab = "kgC / ha", xlim=c(0,1600))
abline(v=mean(sn_ag_data), col="red")

hist(sn_bg_model, main = "Litter below ground Södra Norrland", xlab = "kgC / ha")
abline(v=mean(sn_bg_data), col="red")

# split to tree foliage, woody components and groung vege

abv_tf_m <- rowSums(output_sn$multiOut[,1,26,,])
abv_wc_m <- rowSums(output_sn$multiOut[,1,28,,])+rowSums(output_sn$multiOut[,1,29,,])
abv_gv_m <- output_sn$GVabgW[,1,1]*0.37+output_sn$GVabgW[,1,2]*0.54+output_sn$GVabgW[,1,3]*0.2

abv_tf_d <- sn.litter.origX$lit.foliage.tot/2
abv_wc_d <- (sn.litter.origX$lit.stem.tot+sn.litter.origX$lit.branch.tot+sn.litter.origX$lit.stump.tot)/2
abv_gv_d <- (sn.gv.biomlitX$gvlit.abv.dwarfshrub+sn.gv.biomlitX$gvlit.abv.herb+sn.gv.biomlitX$gvlit.abv.grass+sn.gv.biomlitX$gvlit.abv.moss+sn.gv.biomlitX$gvlit.abv.lichen)/2

# boxplot 
model_litter <- cbind(abv_tf_m, abv_wc_m, abv_gv_m)
colnames(model_litter) <- c("tf", "wc", "gv")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "partition", "litter_m")
tupek_litter <- cbind(abv_tf_d, abv_wc_d, abv_gv_d)
colnames(tupek_litter) <- c("tf", "wc", "gv")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","partition", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="partition")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+partition, data=litter_melt, col=myColors,  main="Above ground litter Södra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("tree foliage", "", "woody components", "", "ground vegetation", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# split by species

abv_pi_m <- rowSums(output_sn$multiOut[,1,26,1,])+rowSums(output_sn$multiOut[,1,28,1,])+rowSums(output_sn$multiOut[,1,29,1,])
abv_su_m <- rowSums(output_sn$multiOut[,1,26,2,])+rowSums(output_sn$multiOut[,1,28,2,])+rowSums(output_sn$multiOut[,1,29,2,])
abv_de_m <- rowSums(output_sn$multiOut[,1,26,3,])+rowSums(output_sn$multiOut[,1,28,3,])+rowSums(output_sn$multiOut[,1,29,3,])

abv_pi_d <- (sn.litter.origX$lit.stem.pi+sn.litter.origX$lit.branch.pi+sn.litter.origX$lit.foliage.pi+sn.litter.origX$lit.stump.pi)/2
abv_su_d <- (sn.litter.origX$lit.stem.su+sn.litter.origX$lit.branch.su+sn.litter.origX$lit.foliage.su+sn.litter.origX$lit.stump.su)/2
abv_de_d <- (sn.litter.origX$lit.stem.de+sn.litter.origX$lit.branch.de+sn.litter.origX$lit.foliage.de+sn.litter.origX$lit.stump.de)/2

# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Above ground litter Södra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)



# NORRA NORRLAND

length(output_nn$multiOut[,1,26,1,1])

nn_abv_lo_m <- rowSums(output_nn$multiOut[,1,26,,])+rowSums(output_nn$multiOut[,1,28,,])+rowSums(output_nn$multiOut[,1,29,,])
nn_abv_gv_m <- output_nn$GVabgW[,1,1]*0.37+output_nn$GVabgW[,1,2]*0.54+output_nn$GVabgW[,1,3]*0.2
nn_ag_model <- nn_abv_lo_m+nn_abv_gv_m

nn_bg_model <- rowSums(output_nn$multiOut[,1,27,,])+output_nn$GVbgW[,1,1]*0.08+output_nn$GVbgW[,1,2]*0.59

nn.litter.origX <- litter.origX[regions$nn,]
nn.gv.biomlitX <- gv.biomlitX[regions$nn,]

nn_abv_lo_d <- nn.litter.origX$lit.stem.tot+nn.litter.origX$lit.branch.tot+nn.litter.origX$lit.foliage.tot+nn.litter.origX$lit.stump.tot
nn_abv_gv_d <- nn.gv.biomlitX$gvlit.abv.dwarfshrub+nn.gv.biomlitX$gvlit.abv.herb+nn.gv.biomlitX$gvlit.abv.grass+nn.gv.biomlitX$gvlit.abv.moss+nn.gv.biomlitX$gvlit.abv.lichen
# /2 to convert to kg C/ha
nn_ag_data <- (nn_abv_lo_d+nn_abv_gv_d)/2

nn_blw_lo_d <- nn.litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
nn_blw_gv_d <- nn.gv.biomlitX$gvlit.belw.dwarfshrub+nn.gv.biomlitX$gvlit.abv.grass+nn.gv.biomlitX$gvlit.belw.herb
nn_bg_data <- (nn_blw_lo_d+nn_blw_gv_d)/2

plot(nn_ag_model, nn_ag_data)
abline(0,1)

test_nn_ag <- which(nn_ag_model<10000)

# 3 radical results removed
plot(nn_ag_model[test_nn_ag], nn_ag_data[test_nn_ag], main="Norra Norrland above ground litter kg C/ha")
abline(0,1)

length(test_nn_ag)
length(nn_ag_model)

plot(nn_bg_model, nn_bg_data, main="Norra Norrland below ground litter kg C/ha")
abline(0,1)

hist(test_nn_ag, main = "Litter above ground Norra Norrland", xlab = "kgC / ha", xlim=c(0,1300))
abline(v=mean(nn_ag_data), col="red")

hist(nn_bg_model, main = "Litter below ground Norra Norrland", xlab = "kgC / ha")
abline(v=mean(nn_bg_data), col="red")

# split to tree foliage, woody components and groung vege

abv_tf_m <- rowSums(output_nn$multiOut[,1,26,,])
abv_wc_m <- rowSums(output_nn$multiOut[,1,28,,])+rowSums(output_nn$multiOut[,1,29,,])
abv_gv_m <- output_nn$GVabgW[,1,1]*0.37+output_nn$GVabgW[,1,2]*0.54+output_nn$GVabgW[,1,3]*0.2

abv_tf_d <- nn.litter.origX$lit.foliage.tot/2
abv_wc_d <- (nn.litter.origX$lit.stem.tot+nn.litter.origX$lit.branch.tot+nn.litter.origX$lit.stump.tot)/2
abv_gv_d <- (nn.gv.biomlitX$gvlit.abv.dwarfshrub+nn.gv.biomlitX$gvlit.abv.herb+nn.gv.biomlitX$gvlit.abv.grass+nn.gv.biomlitX$gvlit.abv.moss+nn.gv.biomlitX$gvlit.abv.lichen)/2

# boxplot 
model_litter <- cbind(abv_tf_m, abv_wc_m, abv_gv_m)
colnames(model_litter) <- c("tf", "wc", "gv")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "partition", "litter_m")
tupek_litter <- cbind(abv_tf_d, abv_wc_d, abv_gv_d)
colnames(tupek_litter) <- c("tf", "wc", "gv")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","partition", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="partition")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+partition, data=litter_melt, col=myColors,  main="Above ground litter Norra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("tree foliage", "", "woody components", "", "ground vegetation", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# split by species

abv_pi_m <- rowSums(output_nn$multiOut[,1,26,1,])+rowSums(output_nn$multiOut[,1,28,1,])+rowSums(output_nn$multiOut[,1,29,1,])
abv_su_m <- rowSums(output_nn$multiOut[,1,26,2,])+rowSums(output_nn$multiOut[,1,28,2,])+rowSums(output_nn$multiOut[,1,29,2,])
abv_de_m <- rowSums(output_nn$multiOut[,1,26,3,])+rowSums(output_nn$multiOut[,1,28,3,])+rowSums(output_nn$multiOut[,1,29,3,])

abv_pi_d <- (nn.litter.origX$lit.stem.pi+nn.litter.origX$lit.branch.pi+nn.litter.origX$lit.foliage.pi+nn.litter.origX$lit.stump.pi)/2
abv_su_d <- (nn.litter.origX$lit.stem.su+nn.litter.origX$lit.branch.su+nn.litter.origX$lit.foliage.su+nn.litter.origX$lit.stump.su)/2
abv_de_d <- (nn.litter.origX$lit.stem.de+nn.litter.origX$lit.branch.de+nn.litter.origX$lit.foliage.de+nn.litter.origX$lit.stump.de)/2

# boxplot 
model_litter <- cbind(abv_pi_m, abv_su_m, abv_de_m)
colnames(model_litter) <- c("pine", "spruce", "deciduous")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "species", "litter_m")
tupek_litter <- cbind(abv_pi_d, abv_su_d, abv_de_d)
colnames(tupek_litter) <- c("pine", "spruce", "deciduous")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","species", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="species")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+species, data=litter_melt, col=myColors,  main="Above ground litter Norra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,2000),names = c("pine", "", "spruce", "", "deciduous", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)



hist(test2_ag, main = "Litter above ground all regions summed up", xlab = "kg C/ha")

test2_bg <- c(svea_bg_model, got_bg_model, sn_bg_model, nn_bg_model)


clSites <- unique(which(output$multiOut[,1,11,,2]>0,arr.ind = T)[,1])
radicalLitter <- which(ag_model>10000)

# all country
model_litter <- cbind(ag_model, bg_model)
colnames(model_litter) <- c("ag", "bg")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "gl", "litter_m")
tupek_litter <- cbind(ag_data, bg_data)
colnames(tupek_litter) <- c("ag", "bg")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","gl", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="gl")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+gl, data=litter_melt, col=myColors,  main="Litter all country", xlab= "", ylab="kg C / ha", ylim=c(0,8000),names = c("above ground", "", "below ground", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# svealand
model_litter <- cbind(svea_ag_model, svea_bg_model)
colnames(model_litter) <- c("ag", "bg")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "gl", "litter_m")
tupek_litter <- cbind(svea_ag_data, svea_bg_data)
colnames(tupek_litter) <- c("ag", "bg")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","gl", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="gl")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+gl, data=litter_melt, col=myColors,  main="Litter Svealand", xlab= "", ylab="kg C / ha",ylim=c(0,7000),names = c("above ground", "", "below ground", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# götaland
model_litter <- cbind(got_ag_model, got_bg_model)
colnames(model_litter) <- c("ag", "bg")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "gl", "litter_m")
tupek_litter <- cbind(got_ag_data, got_bg_data)
colnames(tupek_litter) <- c("ag", "bg")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","gl", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="gl")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+gl, data=litter_melt, col=myColors,  main="Litter Götaland", xlab= "", ylab="kg C / ha", ylim=c(0,8000),names = c("above ground", "", "below ground", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# södra norrland
model_litter <- cbind(sn_ag_model, sn_bg_model)
colnames(model_litter) <- c("ag", "bg")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "gl", "litter_m")
tupek_litter <- cbind(sn_ag_data, sn_bg_data)
colnames(tupek_litter) <- c("ag", "bg")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","gl", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="gl")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+gl, data=litter_melt, col=myColors,  main="Litter Södra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,6000),names = c("above ground", "", "below ground", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)

# norra norrland
model_litter <- cbind(nn_ag_model, nn_bg_model)
colnames(model_litter) <- c("ag", "bg")
m_litter <- melt(model_litter)
colnames(m_litter) <- c("index", "gl", "litter_m")
tupek_litter <- cbind(nn_ag_data, nn_bg_data)
colnames(tupek_litter) <- c("ag", "bg")
t_litter <- melt(tupek_litter)
colnames(t_litter) <- c("index","gl", "litter_t")
litter_all <- merge(m_litter, t_litter)
litter_all <- litter_all[,2:4]
litter_melt <- melt(litter_all, id.vars="gl")
myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+gl, data=litter_melt, col=myColors,  main="Litter Norra Norrland", xlab= "", ylab="kg C / ha", ylim=c(0,6000),names = c("above ground", "", "below ground", ""))
legend("topright", fill = myColors, legend = c("Prebas","Tupek"), horiz = F)


# soil C in steady state KESKEN

InitialX[regions$svea]$c.tot.tha/10



TAir_koe = matrix(TAir,2,length(TAir),byrow = T)
maxYears <- max(nYears)
t(apply(TAir_koe[,1:(maxYears*365)],1,aTmean,maxYears))



# output$totHarv/nSites*23.1
# harvest level figures
forestArea <- read.csv("input/forestArea.csv", sep=";")

forestArea_81_10 <- c(rep(forestArea$all[1], 4), forestArea$all[1:26])
forestArea_81_10 <- forestArea_81_10/1000

model_harvest <- output$totHarv/nSites*forestArea_81_10

harvestAll <- read.csv("input/harvestAll.csv")

harvest_81_10 <- c(rep(harvestAll$all[1], 2), harvestAll$all[1:28])

years <- c(1981:2010)

data <- cbind(years, model_harvest, harvest_81_10)

write.table(data, file="harvestOutput.txt", col.names = T, quote=F, sep=",")


#################
output <- output_noharv

plot(output$multiOut[1,,13,2,1], type="l")
plot(output$multiOut[1,,13,2,1], type="l")

# plot the basal area of the first site
plot(output$multiOut[1,,13,3,1], type = "l", main ="BA", xlab="year", ylab="m2/ha", ylim=c(0,40))
lines(output$multiOut[3,,13,2,1], col="red")

for(i in 2:100) {
  lines(output_svea$multiOut[i,,13,2,1])
}

for(i in 101:200) {
  lines(output$multiOut[i,,13,2,1])
}

for(i in 201:300) {
  lines(output$multiOut[i,,13,2,1])
}

sumBA <- apply(output$multiOut[,,13,,1], 1:2, sum)
hist(rowMeans(sumBA))

sumBA2 <- apply(output$multiOut[,1:75,13,,1], 1:2, sum)
hist(rowMeans(sumBA2), add=T, col = "red")


suspicious_sites <- which(rowMeans(sumBA)<1)

# HUOM! Tarkista ovatko ep?ilytt?v?t koealat soita!

ageX <- as.vector(output$multiOut[,,7,1,1])
basalX <- as.vector(apply(output$multiOut[,,13,,1],1:2,sum,na.rm=T))
plot(ageX, basalX, pch=".")

plot(output$multiOut[suspicious_sites[1],,13,3,1], type="l", ylim=c(0, 4))

for(i in 2:length(suspicious_sites)){
  lines(output$multiOut[suspicious_sites[i],,13,3,1])
}

sumBA3 <- rowSums(output$multiOut[suspicious_sites,1,13,,1])
sumBAtest <- rowSums(output$multiOut[,1,13,,1])
hist(sumBA3)


# BA site means
plot(colMeans(output$multiOut[,,13,2,1]), type="l")

# soil carbon in steady state mineral soils

svea_m <- intersect(regions$svea, mineral)
got_m <- intersect(regions$got, mineral)
sn_m <- intersect(regions$sn, mineral)
nn_m <- intersect(regions$nn, mineral)



# triple histograms

# transparent colors
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")



# SWEDEN

# rotation length for each site
simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)
species <- 1:3

rotLength[rotLength>100] <- 100

# soil C in steady state for each site
soilC_sites2 <- countSoilCstsp(output, species, gvrun = 1,rotLength = rotLength, simLength = simLength)

soilC_sites3 <- countSoilCstsp(output, species, gvrun = 1,rotLength = simLength, simLength = simLength)

#hist(rowSums(soilC_sites$treeLitter))
#hist(rowSums(soilC_sites$gvLitter))

#test2 <- rowSums(soilC_sites$treeLitter)+rowSums(soilC_sites$gvLitter)
#hist(test2, main = "treeLitter + gvLitter")
prebas_gv02 <- rowSums(soilC_sites$treeLitter)/10000
prebas_gv2 <- (rowSums(soilC_sites$treeLitter)+rowSums(soilC_sites$gvLitter))/10000

prebas_gv02 <- rowSums(soilC_sites2$treeLitter)/10000
prebas_gv2 <- (rowSums(soilC_sites2$treeLitter)+rowSums(soilC_sites2$gvLitter))/10000


gbr <- c("grey", "blue", "red")

hist(InitialX[mineral]$c.tot.tha/10, breaks = seq(0,50, by=1), main="Soil carbon in Sweden", xlab="kg/m2", xlim=c(0,40),ylim=c(0,800))
hist(prebas_gv0[mineral], add=T, breaks = seq(0,12, by=1), col="yellow")
hist(prebas_gv[mineral], add=T, breaks = seq(0,14, by=1), col="green")


hist(prebas_gv02[mineral], add=T, breaks = seq(0,12, by=1), border="blue", col=c1)
hist(prebas_gv2[mineral], add=T, breaks = seq(0,14, by=1), border="red", col=c2)


legend("topright", fill = gbr, legend = c("Measurements","Prebas without gv", "Prebas with gv"), horiz = F)


# SVEA

hist(InitialX[svea_m]$c.tot.tha/10, breaks=seq(0,36,by=1), main="Soil carbon in Svealand", xlab="kg/m2", xlim=c(0,30),ylim=c(0,250))
hist(prebas_gv0[svea_m], breaks=seq(0,14,by=1), add=T, border="blue", col=c1)
hist(prebas_gv[svea_m], breaks=seq(0,14,by=1), add=T, border="red", col=c2)
legend("topright", fill = gbr, legend = c("Measurements","Prebas without gv", "Prebas with gv"), horiz = F)


# GOT
hist(InitialX[got_m]$c.tot.tha/10, breaks=seq(0,42,by=1), main="Soil carbon in G?taland", xlab="kg/m2", xlim=c(0,30), ylim=c(0,400))
hist(prebas_gv0[got_m], add=T, breaks = seq(0,12, by=1), border="blue", col=c1)
hist(prebas_gv[got_m], add=T, breaks = seq(0,14, by=1), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Measurements","Prebas without gv", "Prebas with gv"), horiz = F)

# SN
hist(InitialX[sn_m]$c.tot.tha/10, breaks = seq(0,80, by=1), main="Soil carbon in S?dra Norrland", xlab="kg/m2", xlim=c(0,30), ylim=c(0,170))
hist(prebas_gv0[sn_m], add=T, breaks = seq(0,12, by=1), border="blue", col=c1)
hist(prebas_gv[sn_m], add=T, breaks = seq(0,12, by=1), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Measurements","Prebas without gv", "Prebas with gv"), horiz = F)


# NN
hist(InitialX[nn_m]$c.tot.tha/10, breaks = seq(0,34, by=1), main="Soil carbon in Norra Norrland", xlab="kg/m2", xlim=c(0,30),ylim=c(0,160))
hist(prebas_gv0[nn_m], add=T, breaks = seq(0,12, by=1), border="blue", col=c1)
hist(prebas_gv[nn_m], add=T, breaks = seq(0,14, by=1), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Measurements","Prebas without gv", "Prebas with gv"), horiz = F)

###---------- sensitivity analysis: increased rotation length 1.2 x

load("rdata/sensana.rdata")

gbr <- c("grey", "blue", "red")

hist(InitialX[mineral]$c.tot.tha/10, breaks = seq(0,60, by=2), main="Soil carbon in Sweden", xlab="kg/m2", xlim=c(0,30), ylim=c(0,1800))
hist(normal_rotLength, add=T, breaks = seq(0,14, by=2), border="blue", col=c1)
hist(inc_rotLength, add=T, breaks = seq(0,14, by=2), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Tupek","Normal rotation length", "Increased rotation length"), horiz = F)

hist(InitialX[svea_m]$c.tot.tha/10, breaks = seq(0,60, by=2), main="Soil carbon in Svealand", xlab="kg/m2", xlim=c(0,35), ylim=c(0,400))
hist(normal_rotLength[svea_m], add=T, breaks = seq(0,12, by=2), border="blue", col=c1)
hist(inc_rotLength[svea_m], add=T, breaks = seq(0,12, by=2), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Tupek","Normal rotation length", "Increased rotation length"), horiz = F)

hist(InitialX[got_m]$c.tot.tha/10, breaks = seq(0,60, by=2), main="Soil carbon in Götaland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,500))
hist(normal_rotLength[got_m], add=T, breaks = seq(0,14, by=2), border="blue", col=c1)
hist(inc_rotLength[got_m], add=T, breaks = seq(0,14, by=2), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Tupek","Normal rotation length", "Increased rotation length"), horiz = F)

hist(InitialX[sn_m]$c.tot.tha/10, breaks = seq(0,60, by=2), main="Soil carbon in Södra Norrland", xlab="kg/m2", xlim=c(0,25), ylim=c(0,300))
hist(normal_rotLength[sn_m], add=T, breaks = seq(0,14, by=2), border="blue", col=c1)
hist(inc_rotLength[sn_m], add=T, breaks = seq(0,12, by=2), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Tupek","Normal rotation length", "Increased rotation length"), horiz = F)

hist(InitialX[nn_m]$c.tot.tha/10, breaks = seq(0,60, by=2), main="Soil carbon in Norra Norrland", xlab="kg/m2", xlim=c(0,30), ylim=c(0,300))
hist(normal_rotLength[nn_m], add=T, breaks = seq(0,12, by=2), border="blue", col=c1)
hist(inc_rotLength[nn_m], add=T, breaks = seq(0,14, by=2), border="red", col=c2)
legend("topright", fill = gbr, legend = c("Tupek","Normal rotation length", "Increased rotation length"), horiz = F)



# boxplot of soil C in steady state 
# all regions, method 2
myvars <- "c.tot.tha"

c_all <- data.frame(InitialX[mineral])[myvars]

c_svea <- data.frame(InitialX[svea_m])[myvars]
c_got <- data.frame(InitialX[got_m])[myvars]
c_sn <- data.frame(InitialX[sn_m])[myvars]
c_nn <- data.frame(InitialX[nn_m])[myvars]

c_all_melt <- melt(c_all)
c_all_melt$region <- "all"
c_svea_melt <- melt(c_svea)
c_svea_melt$region <- "svea"
c_got_melt <- melt(c_got)
c_got_melt$region <- "got"
c_sn_melt <- melt(c_sn)
c_sn_melt$region <- "sn"
c_nn_melt <- melt(c_nn)
c_nn_melt$region <- "nn"

c_melt_all_tupek <- rbind(c_all_melt, c_svea_melt, c_got_melt, c_sn_melt, c_nn_melt)
# to kg/m2
c_melt_all_tupek$value <- c_melt_all_tupek$value/10


colnames(c_melt_all_tupek) <- c("variable", "tupek", "region")
c_melt_all_tupek <- c_melt_all_tupek[,2:3]

c_all <- prebas_gv[mineral]
c_svea <- prebas_gv[svea_m]
c_got <- prebas_gv[got_m]
c_sn <- prebas_gv[sn_m]
c_nn <- prebas_gv[nn_m]

c_all_melt <- melt(c_all)
c_all_melt$region <- "all"
c_svea_melt <- melt(c_svea)
c_svea_melt$region <- "svea"
c_got_melt <- melt(c_got)
c_got_melt$region <- "got"
c_sn_melt <- melt(c_sn)
c_sn_melt$region <- "sn"
c_nn_melt <- melt(c_nn)
c_nn_melt$region <- "nn"

c_melt_all_prebas <- rbind(c_all_melt,c_svea_melt, c_got_melt, c_sn_melt, c_nn_melt)
#c_melt_all_prebas$variable <- "soilC"

colnames(c_melt_all_prebas) <- c("prebas", "region")
c_melt_all <- merge(c_melt_all_tupek, c_melt_all_prebas)

c_all <- melt(c_melt_all)

myColors <- c(rgb(0.1,0.1,0.7,0.5), "grey90" )
boxplot(value~variable+region, data=c_all,  
        xlab="region", ylab="soil C (kg/m2)", main="Soil C in Sweden", names = c("All Sweden", "", "G?taland", "", "Norra Norrland", "", "S?dra Norrland", "", "Svealand", ""), col=myColors)
legend("topright", fill = myColors, legend = c("Measurements","Prebas"), horiz = F)



# testing stuff for 100 years run--------------
rotlength <- rotationLength(output)
rotlength <- rotLength

# average harvest volume per year per site
hVsite <- vector()
for(i in 1:nSites){
  hVsite[i] <- sum(output$multiOut[i,1:rotlength[i],37,,1])/rotlength[i]
}

sum(hVsite)

# total harvest per year in all sweden
nSites <- length(mineral)
hVyear <- vector()
for(i in 1:100){
  hVyear[i] <- sum(output$multiOut[1:nSites,i,37,,1])
}
plot(hVyear, type="l", xlab="year", ylab="total harvest")

# total volume of the sites per year
hVV <- vector()
for(i in 1:100){
  hVV[i] <- sum(output$multiOut[1:nSites,i,30,,1])
}
plot(hVV, type="l", xlab="year", ylab="total volume")

# volume of the sites one year before clearcut
Vsite <- vector()
for(i in 1:nSites){
  Vsite[i] <- sum(output$multiOut[i,rotlength[i]-1,30,,1])
}

hist(Vsite, main ="Volume", xlab="")

# harvested volume at clearcut
hVsite2 <- vector()
for(i in 1:nSites){
  hVsite2[i] <- sum(output$multiOut[i,rotlength[i],37,,1])
}
hist(hVsite2, main = "Vharvested", xlab="")

# BA one year before clearcut
BAsite <- vector()
for(i in 1:nSites){
  BAsite[i] <- sum(output$multiOut[i,rotlength[i]-1,13,,1])
}
hist(BAsite)

# mean BA 
BAsiteMean <- vector()
for(i in 1:nSites){
  BAsiteMean[i] <- sum(output$multiOut[i,1:rotlength[i],13,,1])/rotlength[i]
}
hist(BAsiteMean)

# mean H
HsiteMean <- vector()
for(i in 1:nSites){
  HsiteMean[i] <- sum(output$multiOut[i,1:rotlength[i],11,,1])/rotlength[i]
}
hist(HsiteMean)

# max H at one year before clearcut
Hsite <- vector()
for(i in 1:nSites){
  Hsite[i] <- max(output$multiOut[i,rotlength[i]-1,11,,1], na.rm=T)
}
hist(Hsite, main="Max height", xlab="")

# max tree height
max(output$multiOut[,,11,,1])


BAsiteMean <- vector()
for(i in 1:nSites){
  BAsiteMean[i] <- sum(output$multiOut[i,,13,,1])/100
}
hist(BAsiteMean)

Hsite <- vector()
for(i in 1:nSites){
  Hsite[i] <- mean(output$multiOut[i,100,11,,1])
}
hist(Hsite)

BAsite <- vector()
for(i in 1:nSites){
  BAsite[i] <- sum(output$multiOut[i,100,13,,1])
}
hist(BAsite)


###########---------------