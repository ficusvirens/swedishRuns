nSites <- initPrebas$nSites
simLength <- simulationLength(output)
rotLength <- rotationLength(output, simLength)

rotLength_100 <- rotLength*1.5
rotLength_100[rotLength_100>100] <- 100

initPrebas1.5 <- initPrebas

# increase rotation length
initPrebas1.5$inAclct[,1:5] <- rotLength_100
# no clearcut because of diameter
initPrebas1.5$inAclct[,7] <- rep(999, nSites)
initPrebas1.5$inDclct[,] <- rep(999, nSites)


rotLengthMax1 <- pmax(rotLength, InitialX$age)
rotLengthMax1[rotLengthMax1>100] <- 100

initPrebasMax <- initPrebas1.5
# increase rotation length
initPrebasMax$inAclct[,1:5] <- rotLengthMax

#save(initPrebas1.5, output_1.5, file="rdata/rotLength1_5_2.rdata")
#save(initPrebasMax, output_max, file="rdata/rotLengthMax2.rdata")
#save(initPrebas, output, file="rdata/100run2.rdata")


load("rdata/rotLengthMax.rdata")
load("rdata/rotLength1_5.rdata")
load("rdata/100run.rdata")
load("rdata/5run.rdata")
