load(outFileSoilC)
#load(litterdata)

snms = c("measurements", "normal rotation length", 
         "1.5x rotation length", "max rotation length")

data_soilC <- InitialX$c.tot.tha/10

# soil C with different rotation lengths
# plot_area 
boxplot(data_soilC[plot_area], soilCstst[plot_area], 
        soilCstst1.5[plot_area], soilCststMax[plot_area], 
        main = c("Soil C in ", paste0(regionName(plot_area))),
        names = snms, 
        ylab = "kg/m2", 
        ylim = c(0,20))
abline(h=c_m, col = "red") # fiksaa nämä vielä !!!
abline(h=c_m0, col = "blue")
legend("topright", title = "Prebas space/time", 
       fill = c("red", "blue"), 
       legend = c("with gv","without gv"), 
       horiz = F)

