# litter inputs, above & below ground, new try

load("rdata/litterdata.rdata")
load("rdata/100run.rdata")
load("rdata/5run.rdata")

#save(output, initPrebas, file="rdata/100run.rdata")
#save(output_m, output_svea, output_got, output_sn, output_nn, 
#  initPrebas_mineral, initPrebasR1, initPrebasR2, initPrebasR3,
#  initPrebasR4, file = "rdata/5run.rdata")

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



###---------------------- new start from here--------------

# litter inputs, above & below ground, new try

load("rdata/litterdata.rdata")
#save(output, file="rdata/100run.rdata")
#save(output_svea, output_got, output_nn, output_sn, output_m, file="rdata/5run.rdata")

names(gv.biomlit)
names(litter.orig)

gv.biomlitX <- gv.biomlit[siteX,]
gv.biomlitX[is.na(gv.biomlitX)]=0
litter.origX <- litter.orig[siteX,]
litter.origX[is.na(litter.origX)]=0


abv_tf_d <- litter.origX$lit.foliage.tot/2
abv_wc_d <- (litter.origX$lit.stem.tot+litter.origX$lit.branch.tot+litter.origX$lit.stump.tot)/2
abv_gv_d <- (gv.biomlitX$gvlit.abv.dwarfshrub+gv.biomlitX$gvlit.abv.herb+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.abv.moss+gv.biomlitX$gvlit.abv.lichen)/2


abv_br_d <- litter.origX$lit.branch.tot/2
abv_st_d <- (litter.origX$lit.stem.tot+litter.origX$lit.stump.tot)/2

# litter above ground data
abv_lo_d <- litter.origX$lit.stem.tot+litter.origX$lit.branch.tot+litter.origX$lit.foliage.tot+litter.origX$lit.stump.tot
abv_gv_d <- gv.biomlitX$gvlit.abv.dwarfshrub+gv.biomlitX$gvlit.abv.herb+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.abv.moss+gv.biomlitX$gvlit.abv.lichen

ag_data <- (abv_lo_d+abv_gv_d)/2

# fine woody & coarse woody
fw_d <- (litter.origX$lit.branch.tot+litter.origX$lit.root.tot)/2
cw_d <- (litter.origX$lit.stem.tot+litter.origX$lit.stump.tot)/2

# fineroot
fr_d <- litter.origX$lit.fineroot.tot/2

# total tree litter
lit_tot_d <- (litter.origX$lit.branch.tot+litter.origX$lit.fineroot.tot+ 
  litter.origX$lit.foliage.tot+litter.origX$lit.root.tot+ 
  litter.origX$lit.stem.tot+litter.origX$lit.stump.tot)/2

# total pine litter
lit_pi_d <- (litter.origX$lit.branch.pi+litter.origX$lit.fineroot.pi+ 
                litter.origX$lit.foliage.pi+litter.origX$lit.root.pi+ 
                litter.origX$lit.stem.pi+litter.origX$lit.stump.pi)/2

# total spruce litter
lit_su_d <- (litter.origX$lit.branch.su+litter.origX$lit.fineroot.su+ 
               litter.origX$lit.foliage.su+litter.origX$lit.root.su+ 
               litter.origX$lit.stem.su+litter.origX$lit.stump.su)/2

# total deciduous litter
lit_de_d <- (litter.origX$lit.branch.de+litter.origX$lit.fineroot.de+ 
               litter.origX$lit.foliage.de+litter.origX$lit.root.de+ 
               litter.origX$lit.stem.de+litter.origX$lit.stump.de)/2


boxplot(litter.origX$lit.branch.tot, litter.origX$lit.fineroot.tot, litter.origX$lit.foliage.tot, litter.origX$lit.root.tot, litter.origX$lit.stem.tot, litter.origX$lit.stump.tot, ylim=c(0, 6000))


# litter below ground data
blw_lo_d <- litter.origX$lit.fineroot.tot#+litter.origX$lit.root.tot
blw_gv_d <- gv.biomlitX$gvlit.belw.dwarfshrub+gv.biomlitX$gvlit.abv.grass+gv.biomlitX$gvlit.belw.herb
bg_data <- (blw_lo_d+blw_gv_d)/2


# -------- prebas space-time analysis ---------

# WHOLE SWEDEN
# litter foliage
abv_tf_m <- output_m$multiOut[,1,26,,1]
# woody components
abv_wc_m <- output_m$multiOut[,1,28,,1]+output_m$multiOut[,1,29,,1]
# ground vegetation
abv_gv_m <- output_m$GVout[,1,2]

#branch
abv_br_m <- output_m$multiOut[,1,28,,1]
#stem
abv_st_m <- output_m$multiOut[,1,29,,1]

#all tree
abv_tot_m <- output_m$multiOut[,1,26,,1]+output_m$multiOut[,1,27,,1]+
  output_m$multiOut[,1,28,,1]+output_m$multiOut[,1,29,,1]

# SVEALAND
# litter foliage
abv_tf_m_svea <- output_svea$multiOut[,1,26,,1]
# woody components
abv_wc_m_svea <- output_svea$multiOut[,1,28,,1]+output_svea$multiOut[,1,29,,1]
# ground vegetation
abv_gv_m_svea <- output_svea$GVout[,1,2]
#all tree
abv_tot_svea <- output_svea$multiOut[,1,26,,1]+output_svea$multiOut[,1,27,,1]+
  output_svea$multiOut[,1,28,,1]+output_svea$multiOut[,1,29,,1]

# GÖTALAND
# litter foliage
abv_tf_m_got <- output_got$multiOut[,1,26,,1]
# woody components
abv_wc_m_got <- output_got$multiOut[,1,28,,1]+output_got$multiOut[,1,29,,1]
# ground vegetation
abv_gv_m_got <- output_got$GVout[,1,2]
#all tree
abv_tot_got <- output_got$multiOut[,1,26,,1]+output_got$multiOut[,1,27,,1]+
  output_got$multiOut[,1,28,,1]+output_got$multiOut[,1,29,,1]

# NORRA NORRLAND
# litter foliage
abv_tf_m_nn <- output_nn$multiOut[,1,26,,1]
# woody components
abv_wc_m_nn <- output_nn$multiOut[,1,28,,1]+output_nn$multiOut[,1,29,,1]
# ground vegetation
abv_gv_m_nn <- output_nn$GVout[,1,2]
#all tree
abv_tot_nn <- output_nn$multiOut[,1,26,,1]+output_nn$multiOut[,1,27,,1]+
  output_nn$multiOut[,1,28,,1]+output_nn$multiOut[,1,29,,1]

# SÖDRA NORRLAND
# litter foliage
abv_tf_m_sn <- output_sn$multiOut[,1,26,,1]
# woody components
abv_wc_m_sn <- output_sn$multiOut[,1,28,,1]+output_sn$multiOut[,1,29,,1]
# ground vegetation
abv_gv_m_sn <- output_sn$GVout[,1,2]
#all tree
abv_tot_sn <- output_sn$multiOut[,1,26,,1]+output_sn$multiOut[,1,27,,1]+
  output_sn$multiOut[,1,28,,1]+output_sn$multiOut[,1,29,,1]



# ---- prebas 100 years: calculate mean litter through rotation time
nSites <- output$nSites
# ground vegetation litter
lit_gv_m100 <- vector()

# litter foliage
lit_fol_m100 <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fr_m100 <- matrix(data=NA, nrow=nSites, ncol=3)

# woody litter = coarse woody + fine
#lit_woody_m100 <- matrix(data=NA, nrow=nSites, ncol=3)

lit_cw_m100 <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fw_m100 <- matrix(data=NA, nrow=nSites, ncol=3)



simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

# if rotation length is >100, set it to 100
rotLength_100 <- rotLength
rotLength_100[rotLength_100>100] <- 100


for (i in 1:nSites) {
  lit_gv_m100[i] <- sum(output$GVout[i,1:rotLength_100[i],2])/rotLength_100[i]
  lit_fol_m100[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],26,,1])/rotLength_100[i]
  lit_fr_m100[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],27,,1])/rotLength_100[i]
#  lit_woody_m100[i,1:3] <- colSums(colSums(output$multiOut[i,1:rotLength_100[i],28:29,,1])/rotLength_100[i])
  lit_fw_m100[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],28,,1])/rotLength_100[i]
  lit_cw_m100[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],29,,1])/rotLength_100[i]
  }

colnames(lit_fol_m100) <- c("pine", "spruce", "deciduous")
colnames(lit_woody_m100) <- c("pine", "spruce", "deciduous")

colnames(lit_fw_m100) <- c("pine", "spruce", "deciduous")
colnames(lit_cw_m100) <- c("pine", "spruce", "deciduous")
colnames(lit_fr_m100) <- c("pine", "spruce", "deciduous")

lit_tot_m100 <- lit_fol_m100+lit_fw_m100+lit_cw_m100+lit_fr_m100

# ---- prebas 100 years 1.5rot: calculate mean litter through rotation time
nSites <- output$nSites
# ground vegetation litter
lit_gv_m100_1.5 <- vector()

# litter foliage
lit_fol_m100_1.5 <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fr_m100_1.5 <- matrix(data=NA, nrow=nSites, ncol=3)

# woody litter = coarse woody + fine
#lit_woody_m100 <- matrix(data=NA, nrow=nSites, ncol=3)

lit_cw_m100_1.5 <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fw_m100_1.5 <- matrix(data=NA, nrow=nSites, ncol=3)



simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)
rotLength1.5 <- rotLength*1.5


# if rotation length is >100, set it to 100
rotLength_100 <- rotLength1.5
rotLength_100[rotLength_100>100] <- 100


for (i in 1:nSites) {
  lit_gv_m100_1.5[i] <- sum(output$GVout[i,1:rotLength_100[i],2])/rotLength_100[i]
  lit_fol_m100_1.5[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],26,,1])/rotLength_100[i]
  lit_fr_m100_1.5[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],27,,1])/rotLength_100[i]
  #  lit_woody_m100[i,1:3] <- colSums(colSums(output$multiOut[i,1:rotLength_100[i],28:29,,1])/rotLength_100[i])
  lit_fw_m100_1.5[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],28,,1])/rotLength_100[i]
  lit_cw_m100_1.5[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],29,,1])/rotLength_100[i]
}

colnames(lit_fol_m100_1.5) <- c("pine", "spruce", "deciduous")
#colnames(lit_woody_m100_1.5) <- c("pine", "spruce", "deciduous")

colnames(lit_fw_m100_1.5) <- c("pine", "spruce", "deciduous")
colnames(lit_cw_m100_1.5) <- c("pine", "spruce", "deciduous")
colnames(lit_fr_m100_1.5) <- c("pine", "spruce", "deciduous")

# ---- prebas 100 years maxRot: calculate mean litter through rotation time
nSites <- output$nSites
# ground vegetation litter
lit_gv_m100_max <- vector()

# litter foliage
lit_fol_m100_max <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fr_m100_max <- matrix(data=NA, nrow=nSites, ncol=3)

# woody litter = coarse woody + fine
#lit_woody_m100 <- matrix(data=NA, nrow=nSites, ncol=3)

lit_cw_m100_max <- matrix(data=NA, nrow=nSites, ncol=3)
lit_fw_m100_max <- matrix(data=NA, nrow=nSites, ncol=3)



simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)
rotLengthMax <- pmax(rotLength, InitialX$age)
# if rotation length is >100, set it to 100
rotLength_100 <- rotLengthMax
rotLength_100[rotLength_100>100] <- 100


for (i in 1:nSites) {
  lit_gv_m100_max[i] <- sum(output$GVout[i,1:rotLength_100[i],2])/rotLength_100[i]
  lit_fol_m100_max[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],26,,1])/rotLength_100[i]
  lit_fr_m100_max[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],27,,1])/rotLength_100[i]
  #  lit_woody_m100[i,1:3] <- colSums(colSums(output$multiOut[i,1:rotLength_100[i],28:29,,1])/rotLength_100[i])
  lit_fw_m100_max[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],28,,1])/rotLength_100[i]
  lit_cw_m100_max[i,1:3] <- colSums(output$multiOut[i,1:rotLength_100[i],29,,1])/rotLength_100[i]
}

colnames(lit_fol_m100_max) <- c("pine", "spruce", "deciduous")
#colnames(lit_woody_m100_max) <- c("pine", "spruce", "deciduous")

colnames(lit_fw_m100_max) <- c("pine", "spruce", "deciduous")
colnames(lit_cw_m100_max) <- c("pine", "spruce", "deciduous")
colnames(lit_fr_m100_max) <- c("pine", "spruce", "deciduous")


nms = c("", "tree foliage", "", "", "woody components", "", "", "ground vegetation", "")

myColors = c("grey", "maroon3", "seagreen3")

# WHOLE SWEDEN
boxplot(abv_tf_d[mineral], rowSums(abv_tf_m), rowSums(lit_fol_m100[mineral,]), 
        abv_wc_d[mineral], rowSums(abv_wc_m), rowSums(lit_woody_m100[mineral,]), 
        abv_gv_d[mineral], abv_gv_m, lit_gv_m100[mineral], 
        main = "Above ground litter Sweden", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

# SVEALAND
boxplot(abv_tf_d[svea_m], rowSums(abv_tf_m_svea), rowSums(lit_fol_m100[svea_m,]), 
        abv_wc_d[svea_m], rowSums(abv_wc_m_svea), rowSums(lit_woody_m100[svea_m,]), 
        abv_gv_d[svea_m], abv_gv_m_svea, lit_gv_m100[svea_m], 
        main = "Above ground litter Svealand", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)


# GÖTALAND
boxplot(abv_tf_d[got_m], rowSums(abv_tf_m_got), rowSums(lit_fol_m100[got_m,]), 
        abv_wc_d[got_m], rowSums(abv_wc_m_got), rowSums(lit_woody_m100[got_m,]), 
        abv_gv_d[got_m], abv_gv_m_got, lit_gv_m100[got_m], 
        main = "Above ground litter Götaland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)


# NORRA NORRLAND
boxplot(abv_tf_d[nn_m], rowSums(abv_tf_m_nn), rowSums(lit_fol_m100[nn_m,]), 
        abv_wc_d[nn_m], rowSums(abv_wc_m_nn), rowSums(lit_woody_m100[nn_m,]), 
        abv_gv_d[nn_m], abv_gv_m_nn, lit_gv_m100[nn_m], 
        main = "Above ground litter Norra Norrland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)


# SÖDRA NORRLAND
boxplot(abv_tf_d[sn_m], rowSums(abv_tf_m_sn), rowSums(lit_fol_m100[sn_m,]), 
        abv_wc_d[sn_m], rowSums(abv_wc_m_sn), rowSums(lit_woody_m100[sn_m,]), 
        abv_gv_d[sn_m], abv_gv_m_sn, lit_gv_m100[sn_m], 
        main = "Above ground litter Södra Norrland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

rnms2 = c("", "Götaland", "", "", "Svealand", "", "", "Södra Norrland", "", "", "Norra Norrland", "", "", "All Sweden", "")
myColors = c("grey", "maroon3", "seagreen3")

# tree litter total
boxplot(lit_tot_d[got_m], rowSums(abv_tot_got), rowSums(lit_tot_m100[got_m,]),
        lit_tot_d[svea_m], rowSums(abv_tot_svea), rowSums(lit_tot_m100[svea_m,]),
        lit_tot_d[sn_m], rowSums(abv_tot_sn), rowSums(lit_tot_m100[sn_m,]),
        lit_tot_d[nn_m], rowSums(abv_tot_nn), rowSums(lit_tot_m100[nn_m,]),
        lit_tot_d[mineral], rowSums(abv_tot_m), rowSums(lit_tot_m100[mineral,]), 
        ylim=c(0,6000), col = myColors, names=rnms2, 
        ylab = "kg C / ha", main = "Total tree litter in Sweden")
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

# ------------------ all litter types separately -------------
nms = c("","", "tree foliage", "","","", "", "fine root", "", "",
        "","", "coarse woody", "", "","","", "fine woody", "","", 
        "","","ground vegetation", "","")
myColors = c("grey", "maroon4", "seagreen4", "darkorange3", "deepskyblue4")

# WHOLE SWEDEN
boxplot(abv_tf_d[mineral], rowSums(abv_tf_m), rowSums(lit_fol_m100[mineral,]), 
          rowSums(lit_fol_m100_1.5[mineral,]), rowSums(lit_fol_m100_max[mineral,]),
        fr_d[mineral], rowSums(output_m$multiOut[,1,27,,1]), rowSums(lit_fr_m100[mineral,]),
          rowSums(lit_fr_m100_1.5[mineral,]), rowSums(lit_fr_m100_max[mineral,]),
        cw_d[mineral], rowSums(output_m$multiOut[,1,29,,1]), rowSums(lit_cw_m100[mineral,]),
          rowSums(lit_cw_m100_1.5[mineral,]), rowSums(lit_cw_m100_max[mineral,]),
        fw_d[mineral], rowSums(output_m$multiOut[,1,28,,1]), rowSums(lit_fw_m100[mineral,]),
          rowSums(lit_fw_m100_1.5[mineral,]), rowSums(lit_fw_m100_max[mineral,]),
        abv_gv_d[mineral], abv_gv_m, lit_gv_m100[mineral], 
          lit_gv_m100_1.5[mineral], lit_gv_m100_max[mineral], 
        main = "Litter Sweden", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, 
       legend = c("Tupek","Prebas time/space", 
                  "Prebas whole rotation",
                  "Prebas rotation x1.5", 
                  "Prebas max rotation"), horiz = F)

# SVEALAND
boxplot(abv_tf_d[svea_m], rowSums(output_svea$multiOut[,1,26,,1]), rowSums(lit_fol_m100[svea_m,]), 
        fr_d[svea_m], rowSums(output_svea$multiOut[,1,27,,1]), rowSums(lit_fr_m100[svea_m,]),
        cw_d[svea_m], rowSums(output_svea$multiOut[,1,29,,1]), rowSums(lit_cw_m100[svea_m,]),
        fw_d[svea_m], rowSums(output_svea$multiOut[,1,28,,1]), rowSums(lit_fw_m100[svea_m,]),
        abv_gv_d[svea_m], abv_gv_m_svea, lit_gv_m100[svea_m], 
        main = "Litter Svealand", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

# GÖTALAND
boxplot(abv_tf_d[got_m], rowSums(output_got$multiOut[,1,26,,1]), rowSums(lit_fol_m100[got_m,]), 
        fr_d[got_m], rowSums(output_got$multiOut[,1,27,,1]), rowSums(lit_fr_m100[got_m,]),
        cw_d[got_m], rowSums(output_got$multiOut[,1,29,,1]), rowSums(lit_cw_m100[got_m,]),
        fw_d[got_m], rowSums(output_got$multiOut[,1,28,,1]), rowSums(lit_fw_m100[got_m,]),
        abv_gv_d[got_m], abv_gv_m_got, lit_gv_m100[got_m], 
        main = "Litter Götaland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

# SÖDRA NORRLAND
boxplot(abv_tf_d[sn_m], rowSums(output_sn$multiOut[,1,26,,1]), rowSums(lit_fol_m100[sn_m,]), 
        fr_d[sn_m], rowSums(output_sn$multiOut[,1,27,,1]), rowSums(lit_fr_m100[sn_m,]),
        cw_d[sn_m], rowSums(output_sn$multiOut[,1,29,,1]), rowSums(lit_cw_m100[sn_m,]),
        fw_d[sn_m], rowSums(output_sn$multiOut[,1,28,,1]), rowSums(lit_fw_m100[sn_m,]),
        abv_gv_d[sn_m], abv_gv_m_sn, lit_gv_m100[sn_m], 
        main = "Litter Södra Norrland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

# NORRA NORRLAND
boxplot(abv_tf_d[nn_m], rowSums(output_nn$multiOut[,1,26,,1]), rowSums(lit_fol_m100[nn_m,]), 
        fr_d[nn_m], rowSums(output_nn$multiOut[,1,27,,1]), rowSums(lit_fr_m100[nn_m,]),
        cw_d[nn_m], rowSums(output_nn$multiOut[,1,29,,1]), rowSums(lit_cw_m100[nn_m,]),
        fw_d[nn_m], rowSums(output_nn$multiOut[,1,28,,1]), rowSums(lit_fw_m100[nn_m,]),
        abv_gv_d[nn_m], abv_gv_m_nn, lit_gv_m100[nn_m], 
        main = "Litter Norra Norrland", 
        names = nms, ylim=c(0,2000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)





#------------split to species ONLY WOODY LITTER------------

# data
abv_pi_d <- (litter.origX$lit.stem.pi+litter.origX$lit.branch.pi+litter.origX$lit.stump.pi)/2
abv_su_d <- (litter.origX$lit.stem.su+litter.origX$lit.branch.su+litter.origX$lit.stump.su)/2
abv_de_d <- (litter.origX$lit.stem.de+litter.origX$lit.branch.de+litter.origX$lit.stump.de)/2

specnam = c("", "pine","", "", "spruce", "", "", "deciduous", "")

# remove 0:s and make them NA
pid <- abv_pi_d[mineral]
pid[pid==0] <- NA
sud <- abv_su_d[mineral]
sud[sud==0] <- NA
ded <- abv_de_d[mineral]
ded[ded==0] <- NA
pim <- abv_wc_m[,1]
pim[pim==0] <- NA
sum <- abv_wc_m[,2]
sum[sum==0] <- NA
dem <- abv_wc_m[,3]
dem[dem==0] <- NA
pim100 <- lit_woody_m100[mineral,1]
pim100[pim100==0] <- NA
sum100 <- lit_woody_m100[mineral,2]
sum100[sum100==0] <- NA
dem100 <- lit_woody_m100[mineral,3]
dem100[dem100==0] <- NA

# woody litter all sweden divided by species
boxplot(abv_pi_d[mineral], abv_wc_m[,1], lit_woody_m100[mineral,1], 
        abv_su_d[mineral], abv_wc_m[,2], lit_woody_m100[mineral,2], 
        abv_de_d[mineral], abv_wc_m[,3], lit_woody_m100[mineral,3], 
        main = "Woody litter Sweden", 
        names = specnam, ylim=c(0,1000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

#vioplot(abv_pi_d[mineral])
#library(vioplot)
#install.packages("vioplot")

# woody litter all sweden divided by species, 0-values removed
boxplot(pid, pim, pim100, 
        sud, sum, sum100, 
        ded, dem, dem100, 
        main = "Woody litter Sweden", 
        names = specnam, ylim=c(0,1000), 
        ylab = "kg C / ha", col = myColors)
legend("topright", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)

#--------- mystery of woody litter ---------------

plot(output$multiOut[2,,29,2,1], type = "l")

plot(output$multiOut[2,,28,2,1], type = "l")

mean(output$multiOut[2,,28,2,1])
mean(output$multiOut[2,,29,2,1])


myColors = c("grey", "maroon3", "seagreen3")
wnms <- c("", "stem", "", "", "branch", "")


# WHOLE SWEDEN
boxplot(abv_st_d[mineral], rowSums(abv_st_m), rowSums(lit_stem_m100[mineral,]), 
        abv_br_d[mineral], rowSums(abv_br_m), rowSums(lit_branch_m100[mineral,]), 
        main = "Above ground woody litter Sweden", 
        names = wnms, ylim=c(0,1000), 
        ylab = "kg C / ha", col = myColors)
legend("topleft", fill = myColors, legend = c("Tupek","Prebas time/space", "Prebas whole rotation"), horiz = F)


#-------------figuring out the branch litter ----------

# list of the years of harvest
harvestYears <- list()

for(i in 1:nSites) {
  harvestYears[[i]] <- which(rowSums(output$multiOut[i,,37,,1]) != 0)
}

tSum <- vector()
hSum <- vector()
oSum <- vector()

for(i in 1:nSites) {
  tSum[i] <- sum(output$multiOut[i,,28,,1])
  n <- length(harvestYears[[i]])
  hSum[i] <- sum(output$multiOut[i,harvestYears[[i]],28,,1])/100
  oSum[i] <- (tSum[i]-sum(output$multiOut[i,harvestYears[[i]],28,,1]))/(100)
}

bnames = c("harvest years", "other years")

boxplot(hSum, oSum, main = "Branch litter in Sweden / 100 years", 
        names = bnames, ylab = "kg C / ha")

harvestYears[[1]]

plot(output$multiOut[3,,c(17,29),1,1], type="l")


tSum <- vector()
hSum <- vector()
oSum <- vector()

for(i in 1:nSites) {
  tSum[i] <- sum(output$multiOut[i,,26,,1])
  n <- length(harvestYears[[i]])
  hSum[i] <- sum(output$multiOut[i,harvestYears[[i]],26,,1])/n
  oSum[i] <- (tSum[i]-sum(output$multiOut[i,harvestYears[[i]],26,,1]))/(100-n)
}

boxplot(hSum, oSum, main = "Mean foliage root litter in Sweden", 
        names = bnames, ylab = "kg C / ha")


