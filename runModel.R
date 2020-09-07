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
harvLimSvea <- initPrebasR1$nSites*harvestData[1,1]/harvestData[1,2]*1000*harvLvl
harvLimGot <- initPrebasR2$nSites*harvestData[2,1]/harvestData[2,2]*1000*harvLvl
harvLimNN <- initPrebasR3$nSites*harvestData[3,1]/harvestData[3,2]*1000*harvLvl
harvLimSN <- initPrebasR4$nSites*harvestData[4,1]/harvestData[4,2]*1000*harvLvl


output <- regionPrebas(initPrebas)
output_svea <- regionPrebas(initPrebasR1, c(harvLimSvea, 0))
output_got <- regionPrebas(initPrebasR2, c(harvLimGot, 0))
output_nn <- regionPrebas(initPrebasR3, c(harvLimNN, 0))
output_sn <- regionPrebas(initPrebasR4, c(harvLimSN, 0))

save(output_svea, output_got, output_nn, output_sn, file="rdata/output_sweden.rdata")


makePlots <- function(output,siteX=NULL){
  nSites<-dim(output)[1]
  nYears <- dim(output)[2]
  dim.H<-cbind(c(1:nSites),nYears,11,rep(1,nSites),rep(1,nSites))
  dim.D<-cbind(c(1:nSites),nYears,12,rep(1,nSites),rep(1,nSites))
  dim.B<-cbind(c(1:nSites),nYears,13,rep(1,nSites),rep(1,nSites))
  
  if(all(is.na(siteX))){
    H <- obs$H
    D <- obs$D
    BA <- obs$BA
  }else{
    H <- obs$H[siteX]
    D <- obs$D[siteX]
    BA <- obs$BA[siteX]
  }
  pH <- ggplot()+geom_point(aes(x=output[dim.H],y=H))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand Height(m)')+ylab('Oberserved stand Height')
  # ggsave(filename = paste('stand H .png'),height=15,width = 15,units = 'cm')
  
  pD <- ggplot()+geom_point(aes(output[dim.D],D))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand DBH(cm)')+ylab('Oberserved stand DBH')
  # ggsave(filename = paste('stand DBH .png'),height=15,width = 15,units = 'cm')
  
  pB <- ggplot()+geom_point(aes(output[dim.B],BA))+geom_abline(slope=1,intercept=0,color='red')+
    xlab('Predicted stand basal area(m2/ha)')+ylab('Oberserved stand basal area')
  # ggsave(filename = paste('stand B.png'),height=15,width = 15,units = 'cm')
  # png("fit.png")
  allPlot <- ggarrange(pH,pD,pB)
  return(list(pH=pH,pD = pD,pB=pB,allPlot=allPlot))
  
}
# dev.off()

# allPlots
#plots <- makePlots(output)
plots_svea <- makePlots(output_svea$multiOut,sitesR1)
plots_got <- makePlots(output_got,sitesR2)
plots_nn <- makePlots(output_nn,sitesR3)
plots_sn <- makePlots(output_sn,sitesR4)

makePlots(output_svea, sitesR1)

