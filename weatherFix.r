library(sf)
library(dplyr)
library(rgeos)

# missing 24.9.20: 
# - replace data for NA sequences of 10 or longer
# - fix mean calculations near the change of year
# - fix mean calculations when there's NA also around the missing value that we are replacing

# load the weather data
load('rdata/se.carbon.soil.meteo.preles.biomass.gv.PRIME.RData')
load('rdata/SWE.par.tair.vpd.precip.RData')

# these are the meteo stations that are relevant to forests
meteo.id<-sort(unique(cu$meteo.id))
cu$Year<-as.numeric(substr(cu$id,1,4))
# this is the number of the used meteo stations
nclim<-length(meteo.id)

# create matrices for weather data for 10 years
PAR<-matrix(NA,nrow = nclim, ncol=3650)
TAir<-matrix(NA,nrow = nclim,ncol=3650)
Precip<-matrix(NA,nrow=nclim,ncol=3650)
VPD<-matrix(NA,nrow = nclim,ncol=3650)
CO2<-matrix(380,nrow=nclim,ncol=3650)

meteodata<-SWE.par.tair.vpd.precip[[1]]

for (i in 2:151){
  meteodata<-rbind(meteodata,SWE.par.tair.vpd.precip[[i]])
}

meteodata$year<-as.numeric(substr(meteodata$date,1,4))


# pos = vector of positions
# longest_seq returns the length and starting position of the longest sequence
# NOT USED AT THE MOMENT
longest_seq <- function(pos) {
  current = 1
  max = 1
  mstart = 1
  cstart = 1
  for(i in 2:length(pos)) {
    if(pos[i] == pos[i-1]+1) current = current+1
    else {
      if(current > max) {
        max = current
        mstart = cstart
      }
      current = 1
      cstart = pos[i]
    }
  }
  if(current > max) {
    max = current
    mstart = cstart
  }
  return(c(mstart,max))
}

# seq_lengths returns the lengths and starting positions of the sequences
# in the descending order of the lengths
seq_lengths <- function(pos) {
  current_seq = c(1,1)
  seqs <- current_seq
  for(i in 2:length(pos)) {
    if(pos[i] == pos[i-1]+1) current_seq[2] = current_seq[2]+1
    else {
      seqs <- rbind(seqs, current_seq)
      current_seq[2] = 1
      current_seq[1] = pos[i]
    }
  }
  seqs <- rbind(seqs, current_seq)
  seqs <- seqs[2:nrow(seqs),]
  colnames(seqs) <- c("index", "length")
  seqs2 <- data.frame(seqs)
  seqs2 <- seqs2[order(-seqs2$length),]

  return(seqs2)
}

## here! find NA's and replace them with data from nearest station

# datetable is a data.frame with dates from start_date to end_date 
dates <- vector()
start_date <- strptime("1993-01-01", "%Y-%m-%d")
end_date <- strptime("2002-12-31", "%Y-%m-%d")
dates <- seq.POSIXt(as.POSIXct(start_date), as.POSIXct(end_date), by = 86400) #86400 sec is a day
year <- as.numeric(format(dates, "%Y"))
date <- format(dates, "%Y-%m-%d")
datetable <- data.frame(date, year)
 
myvars <- c("DTT.mean", "DRR.sum", "par_mjm2day", "vpd_kpa", "id", "year", "date", "lat", "long")
m.data <- meteodata[myvars]
m.data <- m.data[which(m.data$year>1992 & m.data$year<2003),]
m.data$date <- as.character(m.data$date)


d2 <- vector()
for (i in 1:nclim) {
  for (j in 1993:2002) {
    d1 <- m.data[which(m.data$year==j & m.data$id==meteo.id[i]),]
    # if the whole year is missing, then leave it be
    # if the year contains 365 days, then leave it be
    if(nrow(d1)>0 & nrow(d1)<365) {
      t1 <- datetable[which(datetable$year==j),]
      d1 <- merge(t1, d1, all=TRUE)
    }
    d2 <- rbind(d2, d1)
  }
}

# check NA's, replace with mean values
for (i in 1:nclim) {
  for (j in 1993:2002) {
    for (k in 1:4) {
      d1 <- d2[which(d2$year==j & d2$id==meteo.id[i]),]
      
      # if there is any data and any NA's
      if(nrow(d1)>0 & anyNA(d1[,k])) {
        # check precipitation data; more than 50 days of 0 in a row is replaced with NA's
        if(k==2) {
          zero_pos <- which(d1[,k]<0.01)
          if(!identical(zero_pos, integer(0))) {
            z_seqs <- seq_lengths(zero_pos)
            h=1
            while(z_seqs[h,2]>50) {
              d1[z_seqs[h,1]:(z_seqs[h,1]+z_seqs[h,2]-1),k] <- NA
              h=h+1
              if(h>nrow(z_seqs)) break
            }
          }
        }
        # na_pos = indeces of the NA's in the year
        na_pos <- which(is.na(d1[,k]))
        # if there's more than 50 days of data missing, leave it be
        # kuinka monta na:ta jonossa? jos yli 10, lähimmältä asemalta, muuten keskiarvot
        if(length(na_pos)<50) {
          for (n in 1:length(na_pos)) {
            # replace the missing data with mean value of 10 surrounding days
            formean <- (na_pos[n]-5):(na_pos[n]+5)
            # delete all values that are not between 1:365
            formean <- formean[formean %in% 1:365]
            d1[na_pos[n],k] <- mean(d1[formean,k], na.rm = T)
          }
        }
      d2[which(d2$year==j & d2$id==meteo.id[i]),] <- d1  
      }
    }
  }
}

# w_data is a list of data frames that has information of which stations
# have the weather data from which years
# 1 for data and 0 for no data

w_varnam <- c("TAir", "Precip", "PAR", "VPD")
w_data <- list()
  
for(k in 1:4) {
  wdata <- meteo.id
  containsData = 0

  for(j in 1993:2002) {
    dataVector <- vector()
    for(i in 1:nclim) {
      d1 <- d2[which(d2$year==j & d2$id==meteo.id[i]),]

      # if data contains any NA or there is no data, containsData = 0
      if(anyNA(d1[,k]) | nrow(d1)<365) {
        containsData = 0
      }
      else containsData = 1
      dataVector <- c(dataVector, containsData)
    }
    wdata <- cbind(wdata, dataVector)
  }
  colnames(wdata) <- c("id", 1993:2002)
  w_data[[k]] <- wdata
}

names(w_data) <- w_varnam

# to get the coordinates of the sites
st_data <- meteodata[c("id", "lat", "long")]
st_coord <- unique(st_data)
# put the stations on map
stations <- st_as_sf(st_coord, coords=c("long", "lat"))  %>%
  st_set_crs(4326)
# remove the stations that are not in meteo.id
stations <- stations[stations$id %in% meteo.id,]
stations <- arrange(stations, by=id)
# setup for distance calculation
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs <- CRS(sprintf(utmStr, 32))
stUTM <- st_transform(stations, crs)

# make a data frame with a list of nearest stations for each station in descending order
for (i in 1:nrow(stUTM)) {
  t1 <- st_distance(stUTM, stUTM[i,], byid=TRUE)
  t2 <- cbind(t1, c(1:nrow(stUTM)))
  
  # this is for different weather variables
  for(k in 1:4) {
    wd_temp <- vector()
    t3 <- cbind(t2, w_data[[k]])
    t4 <- t3[order(t1),]

    # this is for the years
    for(j in 1:10) {
      # take the closest station with data - may be the original
      id.to.use = t4[match(1, t4[,(j+3)]),3]
      a1 <- d2[which(d2$id==id.to.use),]
      a2 <- a1[which(a1$year==(1992+j)),]
      a3 <- a2[,k]
      wd_temp <- c(wd_temp, a3[1:365])
    }
  # store the weather data to the right variable
  if(k==1) TAir[i,] <- wd_temp
  if(k==2) Precip[i,] <- wd_temp
  if(k==3) PAR[i,] <- wd_temp
  if(k==4) VPD[i,] <- wd_temp
  }
}

save(TAir, Precip, PAR, VPD, CO2, file="rdata/weather.rdata")

# move the weather data from the first year to the end
for(i in 1:nclim) {
  temp <- TAir[i, 1:365]
  temp2 <- TAir[i, 366:3650]
  TAir[i,] <- c(temp2, temp)
  
  temp <- Precip[i, 1:365]
  temp2 <- Precip[i, 366:3650]
  Precip[i,] <- c(temp2, temp)
  
  temp <- PAR[i, 1:365]
  temp2 <- PAR[i, 366:3650]
  PAR[i,] <- c(temp2, temp)
  
  temp <- VPD[i, 1:365]
  temp2 <- VPD[i, 366:3650]
  VPD[i,] <- c(temp2, temp)
}
