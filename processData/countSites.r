
# count the number of sites of different site types
#table(siteInfo[regions$svea,]$siteType)
#table(siteInfo[regions$got,]$siteType)
#table(siteInfo[regions$nn,]$siteType)
#table(siteInfo[regions$sn,]$siteType)

svea_sites <- siteInfo[regions$svea,]
got_sites <- siteInfo[regions$got,]
nn_sites <- siteInfo[regions$nn,]
sn_sites <- siteInfo[regions$sn,]

svea <- list() 
svea[[1]] <- svea_sites[which(svea_sites$siteType==2),]$siteID
svea[[2]] <- svea_sites[which(svea_sites$siteType==3),]$siteID
svea[[3]] <- svea_sites[which(svea_sites$siteType>3),]$siteID
#svea[[3]] <- svea_sites[which(svea_sites$siteType==3),]$siteID
#svea[[4]] <- svea_sites[which(svea_sites$siteType==4),]$siteID

got <- list()
got[[1]] <- got_sites[which(got_sites$siteType==2),]$siteID
got[[2]] <- got_sites[which(got_sites$siteType==3),]$siteID
got[[3]] <- got_sites[which(got_sites$siteType>3),]$siteID
#got[[3]] <- got_sites[which(got_sites$siteType==3),]$siteID
#got[[4]] <- got_sites[which(got_sites$siteType==4),]$siteID

nn <- list()
nn[[1]] <- nn_sites[which(nn_sites$siteType==2),]$siteID
nn[[2]] <- nn_sites[which(nn_sites$siteType==3),]$siteID
nn[[3]] <- nn_sites[which(nn_sites$siteType>3),]$siteID
#nn[[3]] <- nn_sites[which(nn_sites$siteType==3),]$siteID
#nn[[4]] <- nn_sites[which(nn_sites$siteType==4),]$siteID


sn <- list()
sn[[1]] <- sn_sites[which(sn_sites$siteType==2),]$siteID
sn[[2]] <- sn_sites[which(sn_sites$siteType==3),]$siteID
sn[[3]] <- sn_sites[which(sn_sites$siteType>3),]$siteID
#sn[[3]] <- sn_sites[which(sn_sites$siteType==3),]$siteID
#sn[[4]] <- sn_sites[which(sn_sites$siteType==4),]$siteID
