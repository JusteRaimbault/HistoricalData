setwd(paste0(Sys.getenv("CS_HOME"),'/NetworksTerritories/HistoricalData/'))

library(dplyr,warn.conflicts = F)
library(readr)
library(corpus)
library(parallel)

d <- read_csv('Data/directories_geocoded/processed/alldata_didotbottin.csv')


# classify activities

# table(d$activity)

# done in preprocessing with newer data
#todel = c("[","]",".","\\n",",","\"",";","-",":","(",")")
# issue: newlines should sometimes be removes, sometimes as space: ?
#for(car in todel){d$activity=gsub(car," ",d$activity,fixed = T)}

#show(system.time(sapply(d$activity[1:100000],function(s){text_tokens(s, stemmer = 'fr')})))
# 86.554  -> length(d$activity)/100000*86/60 = 60: 1h~ - ok launch 

#activity_stems=sapply(d$activity,function(s){text_tokens(s, stemmer = 'fr')})
activity_stems=mclapply(d$activity,function(s){text_tokens(s, stemmer = 'fr')},mc.cores = 10)


# remove stop words
stopw = stopwords_fr
#activities = sapply(activity_stems,function(l){l[!l%in%stopw]})
activities = mclapply(activity_stems,function(l){l[!l%in%stopw]},mc.cores = 10)

# index stems
#stemids = unlist(sapply(1:length(activities),function(i){rep(i,length(activities[[i]]))}))
stemids = unlist(sapply(1:length(activities),function(i){rep(i,length(activities[[i]][[1]]))}))# relou //: double list level
allactivities = unlist(activities)
names(stemids) <- allactivities

allactcount = table(allactivities)
allactcount = sort(allactcount,decreasing = T)
# -> from this, curate a list of main activities
#  aggregate proximities using levenstein distance? + aggregate into higher hierarchy by hand

countedids = sapply(names(allactcount),function(s){stemids[which(names(stemids)==s)]})


save(activity_stems,activities,stemids,allactivities,allactcount,countedids,file='Data/directories_geocoded/processed/activities.RData')

