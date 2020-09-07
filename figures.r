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
abline(v=7.86, col="red")
abline(v=9.42, col="blue")

hist(InitialX[regions$got]$c.tot.tha/10, main="Soil carbon in Götaland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=9.30, col="red")
abline(v=11.44, col="blue")

hist(InitialX[regions$sn]$c.tot.tha/10, main="Soil carbon in Södra Norrland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=6.86, col="red")
abline(v=7.84, col="blue")

hist(InitialX[regions$nn]$c.tot.tha/10, main="Soil carbon in Norra Norrland", xlab="kg/m2", xlim=c(0,40), ylim=c(0,300))
abline(v=5.77, col="red")
abline(v=6.40, col="blue")



median(InitialX[regions$svea]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$svea]$c.tot.tha/10, na.rm = T)   

median(InitialX[regions$got]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$got]$c.tot.tha/10, na.rm = T)  

median(InitialX[regions$sn]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$sn]$c.tot.tha/10, na.rm = T)  

median(InitialX[regions$nn]$c.tot.tha/10, na.rm = T)
mean(InitialX[regions$nn]$c.tot.tha/10, na.rm = T)  
