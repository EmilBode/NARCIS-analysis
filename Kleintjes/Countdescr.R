source(paste0(getSrcDirectory(function(x) {x}), '/../SetLocal.r'))
libinstandload('plyr')
if(!exists('Total')) ReadForAnalysisfromTotal(F)

Tell <- data.frame(descriptionlengths=Total$descriptionlengths, dataset=Total$setSpec$dataset, bron=Total$Bron)
Tell$hasdescr <- !is.na(Tell$descriptionlengths) & Tell$descriptionlengths>1
perBron <- plyr::count(Tell, vars = c('dataset', 'bron', 'hasdescr'))
perBron$perc <- perBron$freq/apply(perBron,1,function(x) {sum(perBron$freq[perBron$bron==x['bron']])})
Tell$descrna <- is.na(Tell$descriptionlengths)
plyr::count(Tell, vars = c('dataset', 'hasdescr','descrna'))
