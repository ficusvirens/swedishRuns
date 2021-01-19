library(ggplot2)
library(ggpubr)
library(Rprebasso)

load("rdata/initPrebas.rdata")

#output <- multiPrebas(initPrebas)$multiOut
#outputR1 <- multiPrebas(initPrebasR1)$multiOut
#outputR2 <- multiPrebas(initPrebasR2)$multiOut
#outputR3 <- multiPrebas(initPrebasR3)$multiOut
#outputR4 <- multiPrebas(initPrebasR4)$multiOut
harvestData <- read.csv("input/harvestData.csv")
# to test the impact of harvest level into the output, 1 is normal
harvLvl <- 1
harvLimAll <- initPrebas$nSites*harvestData[5,1]/harvestData[5,2]*1000*harvLvl
harvLimSvea <- initPrebasR1$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
harvLimGot <- initPrebasR2$nSites*harvestData[2,1]/harvestData[2,2]*1000*harvLvl
harvLimNN <- initPrebasR3$nSites*harvestData[3,1]/harvestData[3,2]*1000*harvLvl
harvLimSN <- initPrebasR4$nSites*harvestData[4,1]/harvestData[4,2]*1000*harvLvl

harvLimMin <- initPrebas_mineral$nSites*harvestData[5,1]/harvestData[5,2]*1000*harvLvl
harvLimPeat <- initPrebas_peat$nSites*harvestData[5,1]/harvestData[5,2]*1000*harvLvl


# run the model
output <- regionPrebas(initPrebas, c(harvLimAll,0))
output_svea <- regionPrebas(initPrebasR1, c(harvLimSvea, 0))
output_got <- regionPrebas(initPrebasR2, c(harvLimGot, 0))
output_nn <- regionPrebas(initPrebasR3, c(harvLimNN, 0))
output_sn <- regionPrebas(initPrebasR4, c(harvLimSN, 0))

output_m <- regionPrebas(initPrebas_mineral, c(harvLimMin,0))
output_p <- regionPrebas(initPrebas_peat, c(harvLimPeat,0))

ll <- regionPrebas(initPrebas, c(harvLimAll,0))

#save(output_svea1, output_svea2, output_svea3, output_svea4, output_svea5, output_svea6, output_svea7, output_svea8, output_svea9, output_svea10, file="rdata/sveaOutput.rdata")
#save(output_svea, output_got, output_nn, output_sn, file="rdata/output_sweden.rdata")
#load("rdata/output_sweden.rdata")

# count the soil carbon in steady state
species <- 1:3

#test <- countSoilC(output, species)
soilC_svea <- countSoilC(output_svea, species)
soilC_got <- countSoilC(output_got, species)
soilC_nn <- countSoilC(output_nn, species)
soilC_sn <- countSoilC(output_sn, species)


op_svea <- list()
op_svea[[1]] <- output_svea1
op_svea[[2]] <- output_svea2
op_svea[[3]] <- output_svea3
op_svea[[4]] <- output_svea4
op_svea[[5]] <- output_svea5
op_svea[[6]] <- output_svea6
op_svea[[7]] <- output_svea7
op_svea[[8]] <- output_svea8
op_svea[[9]] <- output_svea9
op_svea[[10]] <- output_svea10

op_got <- list()
op_got[[1]] <- output_got1
op_got[[2]] <- output_got2
op_got[[3]] <- output_got3
op_got[[4]] <- output_got4
op_got[[5]] <- output_got5
op_got[[6]] <- output_got6
op_got[[7]] <- output_got7
op_got[[8]] <- output_got8
op_got[[9]] <- output_got9
op_got[[10]] <- output_got10

op_nn <- list()
op_nn[[1]] <- output_nn1
op_nn[[2]] <- output_nn2
op_nn[[3]] <- output_nn3
op_nn[[4]] <- output_nn4
op_nn[[5]] <- output_nn5
op_nn[[6]] <- output_nn6
op_nn[[7]] <- output_nn7
op_nn[[8]] <- output_nn8
op_nn[[9]] <- output_nn9
op_nn[[10]] <- output_nn10

op_sn <- list()
op_sn[[1]] <- output_sn1
op_sn[[2]] <- output_sn2
op_sn[[3]] <- output_sn3
op_sn[[4]] <- output_sn4
op_sn[[5]] <- output_sn5
op_sn[[6]] <- output_sn6
op_sn[[7]] <- output_sn7
op_sn[[8]] <- output_sn8
op_sn[[9]] <- output_sn9
op_sn[[10]] <- output_sn10

soilC_svea <- list()
soilC_got <- list()
soilC_nn <- list()
soilC_sn <- list()
for(i in 1:10) {
  soilC_svea[[i]] <- sum(countSoilC(op_svea[[i]], species))
  soilC_got[[i]] <- sum(countSoilC(op_got[[i]], species))
  soilC_nn[[i]] <- sum(countSoilC(op_nn[[i]], species))
  soilC_sn[[i]] <- sum(countSoilC(op_sn[[i]], species))
}

C10_table <- cbind(unlist(soilC_got), unlist(soilC_svea), unlist(soilC_sn), unlist(soilC_nn))
colnames(C10_table) <- c("got", "svea", "sn", "nn")

write.table(C10_table, file="C10_table.txt", quote=F, sep=",")
# allPlots
#plots <- makePlots(output)
plots_svea <- makePlots(output_svea$multiOut,sitesR1)
plots_got <- makePlots(output_got,sitesR2)
plots_nn <- makePlots(output_nn,sitesR3)
plots_sn <- makePlots(output_sn,sitesR4)

makePlots(output_svea, sitesR1)

# COUNT SOIL C MEANS WITH AND WITHOUT GROUND VEGETATION

# all mineral
species <- 1:3
initPrebas <- initPrebas_mineral
initPrebas$GVrun <- 1

output_gv <- list()
soilC_gv <- list()
Csum_tree <- list()
Csum_gv <- list()

for(i in 1:10) {
  output_gv[[i]] <- regionPrebas(initPrebas, c(harvLimMin, 0))
  soilC_gv[[i]] <- countSoilC(output_gv[[i]], species)
  Csum_tree[[i]] <- sum(soilC_gv[[i]]$treeLitter)
  Csum_gv[[i]] <- sum(soilC_gv[[i]]$gvLitter)
}
mean_soilC_tree <- mean(unlist(Csum_tree))

mean_soilC_gv <- mean(unlist(Csum_gv))

# svea
initPrebas <- initPrebasR1
initPrebas$GVrun <- 1

output_gv <- list()
soilC_gv <- list()
Csum_tree <- list()
Csum_gv <- list()

for(i in 1:10) {
  output_gv[[i]] <- regionPrebas(initPrebas, c(harvLimSvea, 0))
  soilC_gv[[i]] <- countSoilC(output_gv[[i]], species)
  Csum_tree[[i]] <- sum(soilC_gv[[i]]$treeLitter)
  Csum_gv[[i]] <- sum(soilC_gv[[i]]$gvLitter)
}
mean_soilC_tree_svea <- mean(unlist(Csum_tree))

mean_soilC_gv_svea <- mean(unlist(Csum_gv))

# got
initPrebas <- initPrebasR2
initPrebas$GVrun <- 1

output_gv <- list()
soilC_gv <- list()
Csum_tree <- list()
Csum_gv <- list()

for(i in 1:10) {
  output_gv[[i]] <- regionPrebas(initPrebas, c(harvLimGot, 0))
  soilC_gv[[i]] <- countSoilC(output_gv[[i]], species)
  Csum_tree[[i]] <- sum(soilC_gv[[i]]$treeLitter)
  Csum_gv[[i]] <- sum(soilC_gv[[i]]$gvLitter)
}
mean_soilC_tree_got <- mean(unlist(Csum_tree))

mean_soilC_gv_got <- mean(unlist(Csum_gv))

# nn
initPrebas <- initPrebasR3
initPrebas$GVrun <- 1

output_gv <- list()
soilC_gv <- list()
Csum_tree <- list()
Csum_gv <- list()

for(i in 1:10) {
  output_gv[[i]] <- regionPrebas(initPrebas, c(harvLimNN, 0))
  soilC_gv[[i]] <- countSoilC(output_gv[[i]], species)
  Csum_tree[[i]] <- sum(soilC_gv[[i]]$treeLitter)
  Csum_gv[[i]] <- sum(soilC_gv[[i]]$gvLitter)
}
mean_soilC_tree_nn <- mean(unlist(Csum_tree))

mean_soilC_gv_nn <- mean(unlist(Csum_gv))

# sn
initPrebas <- initPrebasR4
initPrebas$GVrun <- 1

output_gv <- list()
soilC_gv <- list()
Csum_tree <- list()
Csum_gv <- list()

for(i in 1:10) {
  output_gv[[i]] <- regionPrebas(initPrebas, c(harvLimSN, 0))
  soilC_gv[[i]] <- countSoilC(output_gv[[i]], species)
  Csum_tree[[i]] <- sum(soilC_gv[[i]]$treeLitter)
  Csum_gv[[i]] <- sum(soilC_gv[[i]]$gvLitter)
}
mean_soilC_tree_sn <- mean(unlist(Csum_tree))

mean_soilC_gv_sn <- mean(unlist(Csum_gv))



# regionwise divided into different site types

harvestData <- read.csv("input/harvestData.csv")
# to test the impact of harvest level into the output, 1 is normal
harvLvl <- 1

hLimSvea <- list()
for(i in 1:3) {
  hLimSvea[[i]] <- initPrebas_svea[[i]]$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
}

hLimGot <- list()
for(i in 1:3) {
  hLimGot[[i]] <- initPrebas_got[[i]]$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
}
hLimNN <- list()
for(i in 1:3) {
  hLimNN[[i]] <- initPrebas_nn[[i]]$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
}
hLimSN <- list()
for(i in 1:3) {
  hLimSN[[i]] <- initPrebas_sn[[i]]$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
}

# run the model

output_st_svea <- list()
for(i in 1:3) {
  output_st_svea[[i]] <- regionPrebas(initPrebas_svea[[i]], c(hLimSvea[[i]],0))
}

output_st_got <- list()
for(i in 1:3) {
  output_st_got[[i]] <- regionPrebas(initPrebas_got[[i]], c(hLimGot[[i]],0))
}

output_st_sn <- list()
for(i in 1:3) {
  output_st_sn[[i]] <- regionPrebas(initPrebas_sn[[i]], c(hLimSN[[i]],0))
}

output_st_nn <- list()
for(i in 1:3) {
  output_st_nn[[i]] <- regionPrebas(initPrebas_nn[[i]], c(hLimNN[[i]],0))
}

# count the soil carbon in steady state
species <- 1:3

soilC_st_svea <- list()
for(i in 1:3) {
  soilC_st_svea[[i]] <- countSoilC(output_st_svea[[i]], species)
}

soilC_st_got <- list()
for(i in 1:3) {
  soilC_st_got[[i]] <- countSoilC(output_st_got[[i]], species)
}

soilC_st_sn <- list()
for(i in 1:3) {
  soilC_st_sn[[i]] <- countSoilC(output_st_sn[[i]], species)
}

soilC_st_nn <- list()
for(i in 1:3) {
  soilC_st_nn[[i]] <- countSoilC(output_st_nn[[i]], species)
}




# COUNT SOIL C MEANS WITH AND WITHOUT GROUND VEGETATION
# DIFFERENT SITE TYPES

# SVEALAND
# switch on ground vegetation
for(i in 1:3) {
  initPrebas_svea[[i]]$GVrun <- 1
}

output_svea_gv <- list()
soilC_svea_gv <- list()
Csum_svea_gv <- list()
mean_soilC_svea_gv <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_svea_gv[[i]] <- regionPrebas(initPrebas_svea[[j]], c(hLimSvea[[j]], 0))
    soilC_svea_gv[[i]] <- countSoilC(output_svea_gv[[i]], species)
    Csum_svea_gv[[i]] <- sum(soilC_svea_gv[[i]])
  }
  mean_soilC_svea_gv[[j]] <- mean(unlist(Csum_svea_gv))
}

# switch off ground vegetation
for(i in 1:3) {
  initPrebas_svea[[i]]$GVrun <- 0
}

output_svea_gv0 <- list()
soilC_svea_gv0 <- list()
Csum_svea_gv0 <- list()
mean_soilC_svea_gv0 <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_svea_gv0[[i]] <- regionPrebas(initPrebas_svea[[j]], c(hLimSvea[[j]], 0))
    soilC_svea_gv0[[i]] <- countSoilC(output_svea_gv0[[i]], species)
    Csum_svea_gv0[[i]] <- sum(soilC_svea_gv0[[i]])
  }
  mean_soilC_svea_gv0[[j]] <- mean(unlist(Csum_svea_gv0))
}

# GÖTALAND
# switch on ground vegetation
for(i in 1:3) {
  initPrebas_got[[i]]$GVrun <- 1
}

output_got_gv <- list()
soilC_got_gv <- list()
Csum_got_gv <- list()
mean_soilC_got_gv <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_got_gv[[i]] <- regionPrebas(initPrebas_got[[j]], c(hLimGot[[j]], 0))
    soilC_got_gv[[i]] <- countSoilC(output_got_gv[[i]], species)
    Csum_got_gv[[i]] <- sum(soilC_got_gv[[i]])
  }
  mean_soilC_got_gv[[j]] <- mean(unlist(Csum_got_gv))
}

# switch off ground vegetation
for(i in 1:3) {
  initPrebas_got[[i]]$GVrun <- 0
}

output_got_gv0 <- list()
soilC_got_gv0 <- list()
Csum_got_gv0 <- list()
mean_soilC_got_gv0 <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_got_gv0[[i]] <- regionPrebas(initPrebas_got[[j]], c(hLimGot[[j]], 0))
    soilC_got_gv0[[i]] <- countSoilC(output_got_gv0[[i]], species)
    Csum_got_gv0[[i]] <- sum(soilC_got_gv0[[i]])
  }
  mean_soilC_got_gv0[[j]] <- mean(unlist(Csum_got_gv0))
}

# NORRA NORRLAND
# switch on ground vegetation
for(i in 1:3) {
  initPrebas_nn[[i]]$GVrun <- 1
}

output_nn_gv <- list()
soilC_nn_gv <- list()
Csum_nn_gv <- list()
mean_soilC_nn_gv <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_nn_gv[[i]] <- regionPrebas(initPrebas_nn[[j]], c(hLimNN[[j]], 0))
    soilC_nn_gv[[i]] <- countSoilC(output_nn_gv[[i]], species)
    Csum_nn_gv[[i]] <- sum(soilC_nn_gv[[i]])
  }
  mean_soilC_nn_gv[[j]] <- mean(unlist(Csum_nn_gv))
}

# switch off ground vegetation
for(i in 1:3) {
  initPrebas_nn[[i]]$GVrun <- 0
}

output_nn_gv0 <- list()
soilC_nn_gv0 <- list()
Csum_nn_gv0 <- list()
mean_soilC_nn_gv0 <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_nn_gv0[[i]] <- regionPrebas(initPrebas_nn[[j]], c(hLimNN[[j]], 0))
    soilC_nn_gv0[[i]] <- countSoilC(output_nn_gv0[[i]], species)
    Csum_nn_gv0[[i]] <- sum(soilC_nn_gv0[[i]])
  }
  mean_soilC_nn_gv0[[j]] <- mean(unlist(Csum_nn_gv0))
}

# SÖDRA NORRLAND
# switch on ground vegetation
for(i in 1:3) {
  initPrebas_sn[[i]]$GVrun <- 1
}

output_sn_gv <- list()
soilC_sn_gv <- list()
Csum_sn_gv <- list()
mean_soilC_sn_gv <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_sn_gv[[i]] <- regionPrebas(initPrebas_sn[[j]], c(hLimSN[[j]], 0))
    soilC_sn_gv[[i]] <- countSoilC(output_sn_gv[[i]], species)
    Csum_sn_gv[[i]] <- sum(soilC_sn_gv[[i]])
  }
  mean_soilC_sn_gv[[j]] <- mean(unlist(Csum_sn_gv))
}

# switch off ground vegetation
for(i in 1:3) {
  initPrebas_sn[[i]]$GVrun <- 0
}

output_sn_gv0 <- list()
soilC_sn_gv0 <- list()
Csum_sn_gv0 <- list()
mean_soilC_sn_gv0 <- list()

for(j in 1:3) {
  for(i in 1:10) {
    output_sn_gv0[[i]] <- regionPrebas(initPrebas_sn[[j]], c(hLimSN[[j]], 0))
    soilC_sn_gv0[[i]] <- countSoilC(output_sn_gv0[[i]], species)
    Csum_sn_gv0[[i]] <- sum(soilC_sn_gv0[[i]])
  }
  mean_soilC_sn_gv0[[j]] <- mean(unlist(Csum_sn_gv0))
}

C_table <- cbind(unlist(mean_soilC_got_gv), unlist(mean_soilC_got_gv0), unlist(mean_soilC_svea_gv), unlist(mean_soilC_svea_gv0), unlist(mean_soilC_sn_gv), unlist(mean_soilC_sn_gv0), unlist(mean_soilC_nn_gv), unlist(mean_soilC_nn_gv0))
rownames(C_table) <- c("1", "2", "3+4")
colnames(C_table) <- c("got_gv", "got_gv0", "svea_gv", "svea_gv0", "sn_gv", "sn_gv0", "nn_gv", "nn_gv0")

write.table(C_table, file="C_table.txt", quote=F, sep=",")


#Wf_old <- apply(output$multiOut[,1,33,,1],1,sum,na.rm=T)
#Wf_new <- apply(output$multiOut[,1,33,,1],1,sum,na.rm=T)
#Wf_svea_new <- apply(output_svea$multiOut[,1,33,,1],1,sum,na.rm=T)
#Wf_svea_old <- apply(output_svea$multiOut[,1,33,,1],1,sum,na.rm=T)

plot(Wf_svea_old,Wf_svea_new)

plot(Wf_old,Wf_new)
