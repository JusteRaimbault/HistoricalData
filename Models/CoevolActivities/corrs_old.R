

# lagged corrs estimations, with data:
# d <- read_csv('Data/Soduco/ftp3.ign.fr/Data/data-1641486178716.csv')

# not needed with new data
#addresses = sapply(d$address, function(s){if(s=="NULL") NA else parse_json(s)})
#lon = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lon)) NA else r[[1]]$lon}); names(lon)=NULL
#lat = sapply(addresses,function(r){if(is.na(r)) NA else if(is.null(r[[1]]$lat)) NA else r[[1]]$lat}); names(lat)=NULL
#d$lon = lon; d$lat = lat
# 67% of NA - older sample



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
