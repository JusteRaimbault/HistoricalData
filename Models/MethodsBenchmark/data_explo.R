
setwd(paste0(Sys.getenv("CS_HOME"),'/NetworksTerritories/HistoricalData/'))

library(dplyr,warn.conflicts = F)
library(ggplot2)
library(readr)
library(jsonlite)
library(corpus)
library(stringdist)
library(corrplot)
library(ggmap)

#d <- read_csv('Data/Soduco/ftp3.ign.fr/Data/data-1641486178716.csv')
d <- read_csv('Data/directories_geocoded/processed/alldata_didotbottin.csv')

# not needed with new data
#addresses = sapply(d$address, function(s){if(s=="NULL") NA else parse_json(s)})
#lon = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lon)) NA else r[[1]]$lon}); names(lon)=NULL
#lat = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lat)) NA else r[[1]]$lat}); names(lat)=NULL
#d$lon = lon; d$lat = lat


length(which(is.na(d$lon)))/length(d$lon)
# 67% of NA - older sample
# 11.9% only in newer data !

length(which(is.na(d$activity)))/length(d$activity)


#map <- get_map(location=c(left = 2.25, bottom=48.825, right=2.41, top=48.9), zoom = 14, source = "osm", color = "bw")
# new bbox? -> same! (remaining 2% coding errors?)
#quantile(d$lon,c(0.01,0.99),na.rm=T) ; 2.275089 2.402903
#quantile(d$lat,c(0.01,0.99),na.rm=T) ; 48.82634 48.89333
map <- get_map(location=c(left = 2.25, bottom=48.825, right=2.41, top=48.9), zoom = 14, source = "osm", color = "bw")


for(y in unique(d$year)){
  inds = (d$year==y)
  
  df = data.frame(lon = lon[inds],lat = lat[inds])
  
  # works with bbox location only for OSM (otherwise get gmaps - needs api key)
  
  ggsave(
    ggmap(map)+geom_point(data=df, aes(x=lon,y=lat), colour="red", alpha = .1)+
      #geom_density2d_filled(data=df,aes(x=lon,y=lat), alpha =0.3) + 
      stat_density2d(data=df,aes(x=lon,y=lat, fill= ..level..), alpha =0.3, geom="polygon")+
      ggtitle(y)
  
 
    #ggplot(,aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/density-',y,'.png'), width=20,height=18, units='cm'
  )
}



# maps by activity
for(y in unique(d$year)){
  for(act in unique(d$mainsynthact)){
  inds = (d$year==y&d$mainsynthact==act)
  df = data.frame(lon = d$lon[inds],lat = d$lat[inds])
  ggsave(
    #ggplot(data.frame(lon = d$lon[inds],lat = d$lat[inds]),aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    ggmap(map)+geom_point(data=df, aes(x=lon,y=lat), colour="red", alpha = .1)+
      stat_density2d(data=df,aes(x=lon,y=lat, fill= ..level..), alpha =0.3, geom="polygon")+
      ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/density-',y,'-',act,'.png'), width=20,height=18, units='cm'
  )
}
}



## todo: plot with number of entries per year (loc and synthact)




##### coevolution



d$lonsq = cut(d$lon,10)
d$latsq = cut(d$lat,10)
dcounts = d %>% group_by(lonsq,latsq,year,mainsynthact) %>% summarise(count=n())
# add missing squares
allsq = data.frame(lonsq = rep(unique(d$lonsq),length(unique(d$latsq))),
 latsq =c(sapply(unique(d$latsq),function(l){rep(l,length(unique(d$lonsq)))}))
)
allsq$sqid = paste0(allsq$lonsq,allsq$latsq)

for(y in unique(d$year)){
  for(act in unique(d$mainsynthact)){
  currentd = dcounts[dcounts$year==y&dcounts$mainsynthact==act,]
  missing = allsq[!allsq$sqid%in%paste0(currentd$lonsq,currentd$latsq),]
  missing$sqid=NULL;missing$year = rep(y,nrow(missing));
  missing$mainsynthact = rep(act,nrow(missing))
  missing$count=rep(0,nrow(missing))
  dcounts = rbind(dcounts,missing)
}
}

deltas = data.frame()
for(act in unique(d$mainsynthact)){
 delta1 = dcounts[dcounts$year==1841&dcounts$mainsynthact==act,] %>% left_join(dcounts[dcounts$year==1842&dcounts$mainsynthact==act,],by=c('lonsq'='lonsq','latsq'='latsq')) %>% summarise(delta = count.y - count.x)
 delta2 = dcounts[dcounts$year==1842&dcounts$mainsynthact==act,] %>% left_join(dcounts[dcounts$year==1844&dcounts$mainsynthact==act,],by=c('lonsq'='lonsq','latsq'='latsq')) %>% summarise(delta = count.y - count.x)
 deltas = rbind(deltas,data.frame(delta1[,c("lonsq","latsq")],delta1=delta1$delta,delta2=delta2$delta,mainsynthact=rep(act,nrow(delta1))))
}

acts = unique(d$mainsynthact)

# basic correlations - for all couples of activities - all lags are simple in that case: 4
cormat11 = matrix(data = 0,nrow = length(acts),ncol=length(acts))
rownames(cormat11) = acts; colnames(cormat11) = acts
cormat11low = cormat11; cormat11high = cormat11
cormat22 = cormat11; cormat22low = cormat11; cormat22high = cormat11
cormat21 = cormat11; cormat21low = cormat11; cormat21high = cormat11
cormat12 = cormat11; cormat12low = cormat11; cormat12high = cormat11

for(act1 in acts){
  for(act2 in acts){
    d1 = deltas[deltas$mainsynthact==act1,]
    d2 = deltas[deltas$mainsynthact==act2,]
    rho11 = cor.test(d1$delta1,d2$delta1);cormat11[act1,act2] = rho11$estimate; cormat11low[act1,act2] = rho11$conf.int[1]; cormat11high[act1,act2] = rho11$conf.int[2]
    rho22 = cor.test(d1$delta2,d2$delta2);cormat22[act1,act2] = rho22$estimate; cormat22low[act1,act2] = rho22$conf.int[1]; cormat22high[act1,act2] = rho22$conf.int[2]
    rho12 = cor.test(d1$delta1,d2$delta2);cormat12[act1,act2] = rho12$estimate; cormat12low[act1,act2] = rho12$conf.int[1]; cormat12high[act1,act2] = rho12$conf.int[2]
    rho21 = cor.test(d1$delta2,d2$delta1);cormat21[act1,act2] = rho21$estimate; cormat21low[act1,act2] = rho21$conf.int[1]; cormat21high[act1,act2] = rho21$conf.int[2]
  }
}

png(file='Results/MethodsBenchmark/laggedcorrs-11.png')
corrplot(cormat11,lowCI.mat=cormat11low, uppCI.mat = cormat11high,title = "Delta1 - Delta1")
dev.off()

png(file='Results/MethodsBenchmark/laggedcorrs-22.png')
corrplot(cormat22,lowCI.mat=cormat22low, uppCI.mat = cormat22high,title = "Delta2 - Delta2")
dev.off()

png(file='Results/MethodsBenchmark/laggedcorrs-12.png')
corrplot(cormat12,lowCI.mat=cormat12low, uppCI.mat = cormat12high,title = "Delta1 - Delta2")
dev.off()

png(file='Results/MethodsBenchmark/laggedcorrs-21.png')
corrplot(cormat21,lowCI.mat=cormat21low, uppCI.mat = cormat21high,title = "Delta2 - Delta1")
dev.off()
