# these are the site specific simulation runs
#load(outfile)
#load(outFile1.5)
#load(outFileMax)
#load(outFileTS)


simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

# count soil C  
soilCbyLayer <- countSoilCstsp(output, species, 
                               gvrun=1, rotLength, simLength)
soilCstst <- (rowSums(soilCbyLayer$treeLitter)
              +rowSums(soilCbyLayer$gvLitter))/10000

simLength1.5 <- simulationLength(output_1.5)
rotLength1.5 <- rotationLength(output_1.5, simLength1.5)


# count soil C  
soilCbyLayer1.5 <- countSoilCstsp(output_1.5, species, 
                               gvrun=1, rotLength1.5, simLength1.5)
soilCstst1.5 <- (rowSums(soilCbyLayer1.5$treeLitter)
              +rowSums(soilCbyLayer1.5$gvLitter))/10000

simLengthMax <- simulationLength(output_max)
rotLengthMax <- rotationLength(output_max, simLengthMax)


# count soil C  
soilCbyLayerMax <- countSoilCstsp(output_max, species, 
                               gvrun=1, rotLengthMax, simLengthMax)
soilCststMax <- (rowSums(soilCbyLayerMax$treeLitter)
              +rowSums(soilCbyLayerMax$gvLitter))/10000


# count soil C for time/space runs

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


#save(soilCstst, soilCstst1.5, soilCststMax,
#     simLength, rotLength, simLength1.5, rotLength1.5,
#     simLengthMax, rotLengthMax,
#     c_got, c_svea, c_sn, c_nn, c_m, 
#     c_got0, c_svea0, c_sn0, c_nn0, c_m0, file=outFileSoilC)
