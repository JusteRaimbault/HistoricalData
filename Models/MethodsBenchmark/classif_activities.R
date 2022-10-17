


# (after stem_activities)
load('Data/directories_geocoded/processed/activities_count.RData')

# levenstein between most frequent stems
#stemdists = stringdistmatrix(names(allactcount)[1:1000],names(allactcount)[1:1000],method="osa")
#summary(c(stemdists))
#closeinds = which(stemdists==1,arr.ind = T)
#paste0(names(allactcount)[closeinds[,1]]," - ",names(allactcount)[closeinds[,2]])

# -> not very concluding: works for many, but also aggregates some which shouldn't
# -> keep only N most used stems?

# arbitrary test: with count larger than 100

# new data: too much -> 1000

write_csv(data.frame( count = allactcount[allactcount>=1000], classif = rep("",length(which(allactcount>=1000)))),file = 'Models/MethodsBenchmark/classif1000.csv',col_names = F)

# manual table stem -> classif
classifcategs = c("NA","food","craftsmanship","art","health","law","service","teaching")

# missing ouvriers? Gribaudi, M. (2014). Paris ville ouvrière. Une histoire occultée (1789-1848). La Découverte.
# -> pas dans le bottin par construction!
# avocats Joana, J. (1998). ENTRE LA BARRE ET LA TRIBUNE LES SECRÉTAIRES DE LA CONFÉRENCE DU STAGE DU BARREAU DE PARIS FACE À L'ACTIVITÉ PARLEMENTAIRE AU 19 e SIÈCLE. Revue française de science politique, 480-506.
#

classif = read_csv(file='Models/MethodsBenchmark/classif1000_manual.csv',col_names = F)
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
