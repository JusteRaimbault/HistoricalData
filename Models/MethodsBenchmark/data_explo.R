
setwd(paste0(Sys.getenv("CS_HOME"),'/NetworksTerritories/HistoricalData/'))

library(dplyr,warn.conflicts = F)
library(ggplot2)
library(readr)
library(jsonlite)
library(corpus)

d <- read_csv('Data/Soduco/ftp3.ign.fr/Data/data-1641486178716.csv')

addresses = sapply(d$address, function(s){if(s=="NULL") NA else parse_json(s)})
lon = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lon)) NA else r[[1]]$lon}); names(lon)=NULL
lat = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lat)) NA else r[[1]]$lat}); names(lat)=NULL

length(which(is.na(lon)))/length(lon)
# 67% of NA

for(y in unique(d$year)){
  inds = (d$year==y)
  ggsave(
    ggplot(data.frame(lon = lon[inds],lat = lat[inds]),aes(x=lon,y=lat))+geom_density2d_filled()+ggtitle(y)
    , file=paste0('Results/MethodsBenchmark/density-',y,'.png'), width=20,height=18, units='cm'
  )
}



# try some activities
table(d$activity)

todel = c("[","]",".","\\n",",","\"")
# issue: newlines should sometimes be removes, sometimes as space: ?
for(car in todel){d$activity=gsub(car,"",d$activity,fixed = T)}
activity_stems= sapply(d$activity,function(s){text_tokens(s, stemmer = 'fr')})

# remove stop words
stopw = stopwords_fr
activities = sapply(activity_stems,function(l){l[!l%in%stopw]})

allactivities = unlist(activities)

allactcount = table(allactivities)
sort(allactcount,decreasing = T)
# -> from this, curate a list of main activities
#  aggregate proximities using levenstein distance? + aggregate into higher hierarchy by hand

