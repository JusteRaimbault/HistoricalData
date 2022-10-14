
# preprocessing of full dataset (Didot-Bottin only for now)

setwd(paste0(Sys.getenv("CS_HOME"),'/NetworksTerritories/HistoricalData/'))

library(jsonlite)
library(sf)

datadir = paste0(Sys.getenv("CS_HOME_EXT"),'/NetworksTerritories/HistoricalData/Data/directories_geocoded/')

files = list.files(datadir, pattern="DidotBottin")

# columns to export: lon, lat, activity, year (no need of adress)
# rq: for coarse, should compute spatial sd from bbox of street geom? -> later if test on all (for now only precise)
# !some entries have two activities: keep first only for now (pb parsing act then?)

# do this here
todel = c("[","]",".","\\n",",","\"",";","-",":","(",")","\n")

start=as.numeric(Sys.time())
alldata=data.frame()
for(i in 1:length(files)){
  #show(i)
  if(i%%10==0){
    show(paste0(i," / ",length(files), " in ",(as.numeric(Sys.time())-start)/60," min"))
    write.table(alldata,file=paste0(datadir,'processed/',i,'.csv'),row.names = F,col.names = F,sep=',')
    alldata=data.frame()
    gc()
  }
  f = files[i]
  currentyear = substr(strsplit(f,'_')[[1]][2],1,4)
  currentraw = read_json(paste0(datadir,f))
  currentraw=currentraw[sapply(currentraw,function(e){e$type=="ENTRY"})]
  currentdata = sapply(currentraw,function(e){
    georef = NA; precision=NA
    if(!is.null(e$geocoded)){
      if(is.null(e$geocoded[[1]]$precise.geom)){
        georef = e$geocoded[[1]]$coarse.geom; precision = 'coarse'
      }else {
        georef = e$geocoded[[1]]$precise.geom; precision = 'precise'
      }
    }
    if(length(georef)==0){coords=c(NA,NA)}else{
      if(is.na(georef)){coords=c(NA,NA)} else {coords = c(st_coordinates(st_centroid(st_transform(st_as_sf(data.frame(geom=georef),wkt='geom',crs=st_crs(2154)),st_crs(4326)))))}
    }
    activity=strsplit(strsplit(e$ner_xml,'<ACT>')[[1]][2],'</ACT>')[[1]][1] # pb if two activities? no, first
    for(car in todel){activity=gsub(car," ",activity,fixed = T)}
    return(c(
      activity = activity,
      lon = coords[1],
      lat=coords[2],
      precision=precision
     )
    )
  })
  currentdata = t(currentdata)
  currentdata=as.data.frame(cbind(currentdata,rep(currentyear,nrow(currentdata))))
  names(currentdata)<-c("activity","lon","lat","precision","year")
  alldata = rbind(alldata,currentdata)
}

write.table(alldata,file=paste0(datadir,'processed/',i,'.csv'),row.names = F,col.names = F,sep=",")

