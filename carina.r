
carina <- read.csv("input/skdata_carina.csv")

carina$id <- paste(carina$AR, carina$TRAKT, carina$PALSLAG ,sep="")

myvars <- c("id", "hist")
hist <- carina[myvars]

cu <- merge(cu, hist)


mineral <- which(cu$hist==0)
mineral <- intersect(mineral, siteX)
mineral <- intersect(mineral, all_plots$number)

peat <- which(cu$hist==1)
peat <- intersect(peat, siteX)
peat <- intersect(peat, all_plots$number)

# meillä näyttäisi olevan melkein 400 koealaa joiden sijainti ei ole tiedossa
length(intersect(siteX, all_plots$number))
length(siteX)

save(mineral, peat, file="rdata/peatlands.rdata")
