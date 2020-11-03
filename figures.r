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
