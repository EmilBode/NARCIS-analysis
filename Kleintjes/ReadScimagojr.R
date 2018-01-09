src <- getSrcDirectory(function(x) {x})
if(src!='') setwd(paste0(src,'/..'))
rm(src)
source(paste0(getwd(), '/SetLocal.r'))
libinstandload('readxl','data.table')

sjr <- read_excel(paste0(Paths$input, '/scimagojr 2016.xlsx'))
sjr <- sjr[c(2,3,4,15,16)]
sjr$ISSN <- sapply(sjr$Issn, strsplit, split=',', USE.NAMES = F)
sjr$Issn <- NULL
sjr$ISSN <- sapply(sjr$ISSN, function(x) {
  x <- gsub('[^0-9X]+','',x)
  if(!all(nchar(x) %in% c(0,8))) stop('Unknown ISSN-format')
  return(x)
})
sjr$Categories <- sapply(sjr$Categories, strsplit, split=';', USE.NAMES = F)
sjr$Categories <- sapply(sjr$Categories, sub, pattern='^ +', replacement='')
sjr$Categories <- sapply(sjr$Categories, sub, pattern=' *\\(Q[1-4]\\)$', replacement='')
sjr$n <- sapply(sjr$ISSN, length)*sapply(sjr$Categories, length)
sjr <- data.frame(Title=unlist(apply(sjr, 1, function(x) {rep(x$Title, x$n)})),
                  Type=as.factor(unlist(apply(sjr, 1, function(x) {rep(x$Type, x$n)}))),
                  Country=as.factor(unlist(apply(sjr, 1, function(x) {rep(x$Country, x$n)}))),
                  Categorie=as.factor(unlist(apply(sjr, 1, function(x) {rep(x$Categories, length.out=x$n)}))),
                  ISSN=unlist(apply(sjr, 1, function(x) {rep(x$ISSN, each=length(x$Categories))})),
                  stringsAsFactors = F)
sjr$isDutch <- !is.na(sjr$Country) & sjr$Country=='Netherlands'
sjr <- unique(sjr)
remove <- which(sjr$ISSN=='16740068' & sjr$Title=='Canadian Journal of Remote Sensing')
remove <- c(remove, which(sjr$ISSN=='15788423' & sjr$Title=='Daimon'))
remove <- c(remove, which(sjr$ISSN=='18753507' & sjr$Title=='IUTAM Bookseries'))
remove <- c(remove, which(sjr$ISSN=='09250042' & sjr$Title=='IUTAM Bookseries'))
sjr <- sjr[-remove,]

write.csv2(sjr, paste0(Paths$output, '/Scimagojr.csv'), row.names = F)
if(exists('Results') && exists('Journals')) {
  ISSN <- rbind.fill(apply(Results$Journal,1,function(x) {
    suppressWarnings(
      data.frame(ISSN=as.character(x[['ISSN']]),
                 title=as.character(unlist(rep(x['title'],length.out=length(x['ISSN'])))), 
                 subTitle=as.character(unlist(rep(x['subTitle'],length.out=length(x['ISSN'])))),
                 role=as.factor(unlist(rep(x['role'],length.out=length(x['ISSN']))))
      ))}))
  ISSN <- unique(ISSN)
  ISSN <- ISSN[order(ISSN$ISSN),]
  #ISSN <- merge(ISSN, Journals[c(1:5,16)])
  write.csv2(ISSN, paste0(Paths$output,'/ISSNs in NARCIS.csv'), row.names = F)
} # Run didlmods Analyse subset to generate list of ISSNs

duptitles <- data.frame(ISSN=as.character(unique(sjr$ISSN)), stringsAsFactors = F)
duptitles$titles <- NA
part <- nrow(duptitles)
duptitles$titles[1:part] <- sapply(duptitles$ISSN[1:part], function(x) {unique(as.character(sjr$Title[sjr$ISSN==x]))}, USE.NAMES = F)
dupls <- duptitles[sapply(duptitles$titles, length)>1&duptitles$ISSN!='',]
dupls <- sjr[sjr$ISSN %in% dupls$ISSN,]
dupls <- dupls[order(dupls$ISSN),]
stop('Sched')
merge(ISSN, Journals, by='ISSN')


# Script for making a dataframe from the scimagojr, with attached classification
