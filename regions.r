library(sf)
#library(tidyverse)
library(mapview)
library(rgeos)
library(dplyr)

sweden_lan <- st_read("shapefiles/LanSweref99TM/Lan_Sweref99TM_region.shp")
sweden_kommun <- st_read("shapefiles/KommunSweref99TM/Kommun_Sweref99TM_region.shp")


# assign the right landsdel to each lan; 
# SVEA = Svealand, GÖT = Götaland, SN = Södra Norrland, NN = Norra Norrland
sweden_lan$Landsdel <- "NN"
sweden_lan[1,]$Landsdel <- "SVEA"
sweden_lan[2,]$Landsdel <- "SVEA"
sweden_lan[3,]$Landsdel <- "SVEA"
sweden_lan[4,]$Landsdel <- "GÖT"
sweden_lan[5,]$Landsdel <- "GÖT"
sweden_lan[6,]$Landsdel <- "GÖT"
sweden_lan[7,]$Landsdel <- "GÖT"
sweden_lan[8,]$Landsdel <- "GÖT"
sweden_lan[9,]$Landsdel <- "GÖT"
sweden_lan[10,]$Landsdel <- "GÖT"
sweden_lan[11,]$Landsdel <- "GÖT"
sweden_lan[12,]$Landsdel <- "GÖT"
sweden_lan[13,]$Landsdel <- "SVEA"
sweden_lan[14,]$Landsdel <- "SVEA"
sweden_lan[15,]$Landsdel <- "SVEA"
sweden_lan[16,]$Landsdel <- "SVEA"
sweden_lan[17,]$Landsdel <- "SN"
sweden_lan[18,]$Landsdel <- "SN"
sweden_lan[19,]$Landsdel <- "SN"

# just landsdel
sweden_landsdel = sweden_lan %>% 
  group_by(Landsdel) %>%
  summarise() 

sweden_landsdel <- st_transform(sweden_landsdel, 4326)

# tämä yhdistää kaikki polygonit
#st_union(sweden_lan)

#tämä yhdistää ensimmäiset kolme
#st_union(sweden_lan[1:3,])

# load the coordinates of the Swedish plots
load("rdata/coordPlots.rdata")

# take only the plots where there is trees
#coordPlots <- coordPlots[siteInfoX$siteID]

plots_sf <- st_as_sf(coordPlots, coords=c("long", "lat")) %>%
  st_set_crs(4326)

plots_sf$number <- c(1:nrow(plots_sf))

#save(sweden_landsdel, plots_sf, file="rdata/sweden_landsdel.rdata")

load("rdata/sweden_landsdel.rdata")
mapview(sweden_landsdel) + mapview(plots_sf)

plots_ld <- st_intersection(plots_sf, sweden_landsdel)

# just SVEA landsdel
plots_ld[which(plots_ld$Landsdel=="SVEA"),]

# these fall outside of the landsdels
outside_plots <- plots_sf[!lengths(st_intersects(plots_sf, sweden_landsdel)), ]

# tästä tulee matriisi jossa on etäisyydet joka alueeseen
st_distance(outside_plots, sweden_landsdel)


utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs <- CRS(sprintf(utmStr, 32))
pUTM <- st_transform(sweden_landsdel, crs)
ptsUTM <- st_transform(outside_plots, crs)

## Set up containers for results
n <- nrow(outside_plots)
nearestLandsdel <- character(n)
distToNearestLandsdel <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearestLandsdel)) {
  gDists <- st_distance(ptsUTM[i,], pUTM, byid=TRUE)
  nearestLandsdel[i] <- pUTM$Landsdel[which.min(gDists)]
  distToNearestLandsdel[i] <- min(gDists)
}

outside_plots$Landsdel <- nearestLandsdel

all_plots <- rbind(plots_ld, outside_plots)

# seuraavaksi kerää koealojen numerot kullekin alueelle
svea <- all_plots[which(all_plots$Landsdel=="SVEA"),]
got <- all_plots[which(all_plots$Landsdel=="GÖT"),]
nn <- all_plots[which(all_plots$Landsdel=="NN"),]
sn <- all_plots[which(all_plots$Landsdel=="SN"),]

regions <- list()
regions$svea <- svea$number
regions$got <- got$number
regions$nn <- nn$number
regions$sn <- sn$number

#save(regions, file = "rdata/regions.rdata")

which(svea$id %in% InitialX$id)

# mineral soils / peatlands
carina <- read.csv("input/skdata_carina.csv")

carina$id <- paste(carina$AR, carina$TRAKT, carina$PALSLAG ,sep="")

myvars <- c("id", "hist")
hist <- carina[myvars]

cu <- merge(cu, hist)


mineral_sites <- which(cu$hist==0)


mineral <- intersect(mineral, siteX)
mineral <- intersect(mineral, all_plots$number)

peat <- which(cu$hist==1)
peat <- intersect(peat, siteX)
peat <- intersect(peat, all_plots$number)

# meillä näyttäisi olevan melkein 400 koealaa joiden sijainti ei ole tiedossa
length(intersect(siteX, all_plots$number))
length(siteX)


mineral_id <- cu[mineral_sites,]$id
got_id <- intersect(got$id, mineral_id)
svea_id <- intersect(svea$id, mineral_id)
sn_id <- intersect(sn$id, mineral_id)
nn_id <- intersect(nn$id, mineral_id)

#save(mineral_id, got_id, svea_id, sn_id, nn_id, file="rdata/region_ids.rdata")
#save(mineral, peat, file="rdata/peatlands.rdata")
