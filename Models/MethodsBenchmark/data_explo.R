
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
# only 15%

# loc and activity
1 - length(which(is.na(d$activity)|is.na(d$lon)|is.na(d$lat)))/length(d$activity)
# 0.8032564

#map <- get_map(location=c(left = 2.25, bottom=48.825, right=2.41, top=48.9), zoom = 14, source = "osm", color = "bw")
# new bbox? -> same! (remaining 2% coding errors?)
#quantile(d$lon,c(0.01,0.99),na.rm=T) ; 2.275089 2.402903
#quantile(d$lat,c(0.01,0.99),na.rm=T) ; 48.82634 48.89333
map <- get_map(location=c(left = 2.25, bottom=48.825, right=2.41, top=48.9), zoom = 14, source = "osm", color = "bw")


for(y in unique(d$year)){
  show(y)
  inds = (d$year==y)
  
  df = data.frame(lon = d$lon[inds],lat = d$lat[inds])
  
  # works with bbox location only for OSM (otherwise get gmaps - needs api key)
  
  ggsave(
    ggmap(map)+geom_point(data=df, aes(x=lon,y=lat), colour="red", alpha = .1)+
      #geom_density2d_filled(data=df,aes(x=lon,y=lat), alpha =0.3) + 
      stat_density2d(data=df,aes(x=lon,y=lat, fill= ..level..), alpha =0.3, geom="polygon")+
      ggtitle(y)
  
 
    #ggplot(,aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/maps/density-',y,'.png'), width=20,height=18, units='cm'
  )
}



####
# load synth activities


# maps by activity
for(y in unique(d$year)){
  for(act in unique(d$mainsynthact)){
    show(paste0(y,"-",act))
  inds = (d$year==y&d$mainsynthact==act)
  df = data.frame(lon = d$lon[inds],lat = d$lat[inds])
  ggsave(
    #ggplot(data.frame(lon = d$lon[inds],lat = d$lat[inds]),aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    ggmap(map)+geom_point(data=df, aes(x=lon,y=lat), colour="red", alpha = .1)+
      stat_density2d(data=df,aes(x=lon,y=lat, fill= ..level..), alpha =0.3, geom="polygon")+
      ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/maps/density-',y,'-',act,'.png'), width=20,height=18, units='cm'
  )
}
}



## todo: plot with number of entries per year (loc and synthact)

dy = d %>% group_by(year,mainsynthact) %>% summarise(act_count=n())
ggsave(
  ggplot(dy,aes(x=year,y=act_count,fill=mainsynthact))+geom_col(),
  file='Results/MethodsBenchmark/counts-year.png'
)

##### coevolution


## data preparation

# spatial grouping
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

# temporal diffs
years = sort(unique(d$year),decreasing = F)
deltas = data.frame()
for(act in unique(d$mainsynthact)){
  for (i in 2:length(years)){
   currentdelta = dcounts[dcounts$year==years[i-1]&dcounts$mainsynthact==act,] %>% left_join(dcounts[dcounts$year==years[i]&dcounts$mainsynthact==act,],by=c('lonsq'='lonsq','latsq'='latsq')) %>% summarise(delta = count.y - count.x)
   deltas = rbind(deltas,data.frame(currentdelta[,c("lonsq","latsq")],delta=currentdelta$delta,mainsynthact=rep(act,nrow(currentdelta)),year=rep(years[i],nrow(currentdelta))))
  }
}

acts = unique(d$mainsynthact)


# estimate lagged corrs for a time window and two activities

cormat = matrix(data = 0,nrow = length(acts),ncol=length(acts))
rownames(cormat) = acts; colnames(cormat) = acts
cormatlow = cormat; cormathigh = cormat; taumat = cormat

taus = 2:10
K = length(which(deltas$mainsynthact=="craftsmanship"&deltas$year==1858))

for(act1 in acts){
  for(act2 in acts){
    show(paste0(act1,"-",act2))
    d1 = deltas$delta[deltas$mainsynthact==act1]
    d2 = deltas$delta[deltas$mainsynthact==act2]
    corrs = c();corrsmin=c();corrsmax=c()
    for(tau in taus){
       rho = cor.test(d1[1:(length(d1)-(tau*K))],d2[(tau*K+1):length(d2)])
       corrs=append(corrs,rho$estimate);corrsmin=append(corrsmin,rho$conf.int[1]);corrsmax=append(corrsmax,rho$conf.int[2])
    }
    indmax = which(abs(corrs)==max(abs(corrs)))
    cormat[act1,act2] = corrs[indmax];cormatlow[act1,act2] = corrsmin[indmax]; cormathigh[act1,act2] = corrsmax[indmax]; taumat[act1,act2] = taus[indmax]
  }   
}

png(file='Results/MethodsBenchmark/laggedcorrs.png',width = 1000, height = 1000)
corrplot(cormat,lowCI.mat=cormatlow, uppCI.mat = cormathigh)
dev.off()

library(reshape2)
ggsave(
  ggplot(melt(taumat),aes(x=Var1,y=Var2,fill=value))+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name=expression(tau)),
  file='Results/MethodsBenchmark/taus.png'
)

