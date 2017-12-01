source(paste0(getSrcDirectory(function(x) {x}), '/../SetLocal.r'))
source(paste0(getSrcDirectory(function(x) {x}), '/../Help/VSNU-pdf.R'))
KUOZ <- ReadVSNUpdf()

VSNU <- readRDS(paste0(Paths$input,'/VSNU.rds'))
New <- KUOZ$doc1[KUOZ$doc1$Bron=='KUOZ',]
New$VSNUcat <- substring(New$Type,1,3)
New$Categorie <- factor(New$VSNUcat)
levels(New$Categorie) <- c('Thesis','Populair','Totalen','Vak','Wetensch')
New <- plyr::count(New, vars=c('Categorie','jaar','Uni'), wt_var = 'count')
VSNU$Categorie <- as.character(VSNU$Categorie)
VSNU$Universiteit <- as.character(VSNU$Universiteit)
New$Categorie <- as.character(New$Categorie)
New$jaar <- as.numeric(as.character(New$jaar))
New$Uni <- as.character(New$Uni)
New <- merge(New, VSNU, by.x = c('Categorie','jaar','Uni'), by.y=c('Categorie','Jaar','Universiteit'), all.x=T)


