
setwd(paste0(Sys.getenv("CS_HOME"),'/NetworksTerritories/HistoricalData/'))

library(dplyr,warn.conflicts = F)
library(ggplot2)
library(readr)
library(jsonlite)
library(corpus)
library(stringdist)
library(corrplot)

d <- read_csv('Data/Soduco/ftp3.ign.fr/Data/data-1641486178716.csv')

addresses = sapply(d$address, function(s){if(s=="NULL") NA else parse_json(s)})
lon = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lon)) NA else r[[1]]$lon}); names(lon)=NULL
lat = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lat)) NA else r[[1]]$lat}); names(lat)=NULL

d$lon = lon; d$lat = lat

length(which(is.na(lon)))/length(lon)
# 67% of NA

for(y in unique(d$year)){
  inds = (d$year==y)
  ggsave(
    ggplot(data.frame(lon = lon[inds],lat = lat[inds]),aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/density-',y,'.png'), width=20,height=18, units='cm'
  )
}



# classify activities

# table(d$activity)

todel = c("[","]",".","\\n",",","\"",";","-",":","(",")")
# issue: newlines should sometimes be removes, sometimes as space: ?
for(car in todel){d$activity=gsub(car," ",d$activity,fixed = T)}
activity_stems= sapply(d$activity,function(s){text_tokens(s, stemmer = 'fr')})

# remove stop words
stopw = stopwords_fr
activities = sapply(activity_stems,function(l){l[!l%in%stopw]})

# index stems
stemids = unlist(sapply(1:length(activities),function(i){rep(i,length(activities[[i]]))}))
allactivities = unlist(activities)
names(stemids) <- allactivities

allactcount = table(allactivities)
allactcount = sort(allactcount,decreasing = T)
# -> from this, curate a list of main activities
#  aggregate proximities using levenstein distance? + aggregate into higher hierarchy by hand

countedids = sapply(names(allactcount),function(s){stemids[which(names(stemids)==s)]})

# levenstein between most frequent stems
stemdists = stringdistmatrix(names(allactcount)[1:1000],names(allactcount)[1:1000],method="osa")
summary(c(stemdists))
closeinds = which(stemdists==1,arr.ind = T)
paste0(names(allactcount)[closeinds[,1]]," - ",names(allactcount)[closeinds[,2]])

# -> not very concluding: works for many, but also aggregates some which shouldn't
# -> keep only N most used stems?

# arbitrary test: with count larger than 100

write_csv(data.frame( count = allactcount[allactcount>=100], classif = rep("",length(which(allactcount>=100)))),file = 'Models/MethodsBenchmark/classif100.csv',col_names = F)

# manual table stem -> classif
classifcategs = c("NA","food","craftsmanship","art","health","law","service","teaching")

# missing ouvriers? Gribaudi, M. (2014). Paris ville ouvrière. Une histoire occultée (1789-1848). La Découverte.
# -> pas dans le bottin par construction!
# avocats Joana, J. (1998). ENTRE LA BARRE ET LA TRIBUNE LES SECRÉTAIRES DE LA CONFÉRENCE DU STAGE DU BARREAU DE PARIS FACE À L'ACTIVITÉ PARLEMENTAIRE AU 19 e SIÈCLE. Revue française de science politique, 480-506.
#

classif = read_csv(file='Models/MethodsBenchmark/classif100_manual.csv',col_names = F)
classif[which(is.na(classif[,3])),3]="NA"

synthact = unlist(classif[,3])
names(synthact) = unlist(classif[,1])
allsynthact =synthact[names(stemids)]
allsynthact[allsynthact=="NA"]=NA
synthactcounts = table(allsynthact)

mainsynthact = as_tibble(data.frame(synth = allsynthact, id = stemids)) %>% group_by(id) %>%
  summarise(mainsynthact = if(length(which(is.na(synth)))==length(synth)) NA else synth[!is.na(synth)][c(synthactcounts[synth[!is.na(synth)]]) == max(synthactcounts[synth[!is.na(synth)]])])

mainsynthact = mainsynthact[!duplicated(mainsynthact$id),]
mainsynthactc = mainsynthact$mainsynthact; names(mainsynthactc)<-mainsynthact$id

d$mainsynthact = mainsynthactc[1:nrow(d)]

d = d[which(!is.na(d$lat)&!is.na(d$lon)&!is.na(d$mainsynthact)&!is.na(d$year)),]

# maps by activity
for(y in unique(d$year)){
  for(act in unique(d$mainsynthact)){
  inds = (d$year==y&d$mainsynthact==act)
  ggsave(
    ggplot(data.frame(lon = d$lon[inds],lat = d$lat[inds]),aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/density-',y,'-',act,'.png'), width=20,height=18, units='cm'
  )
}
}


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
