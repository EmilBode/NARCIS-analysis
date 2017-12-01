print(paste('Starting script', Sys.time()))
source(paste0(getSrcDirectory(function(x) {x}), '/SetLocal.r'))
libinstandload('stringi','stringdist','ggplot2')
if(!exists('Total')) ReadForAnalysisfromTotal(F)

cols <- c('titlelow','title','titlepart1','titlepart2','ID','Publisherfull','description','access','date.meta.date','Bron','LangFull','BronCat','ppl.creator.X1')
options(stringsAsFactors = F)
method <- 'lcs'
q <- 6
charcorr <- -10
if(!exists('werkset')) {
  werkset <- Total[Total$Type=='Doctoral Thesis',]
  werkset <- werkset[sapply(werkset, function(x) {is.null(ncol(x)) || ncol(x)!=0})]
  werkset <- data.frame(lapply(werkset, function(x) {
    if(class(x)[1]=='data.frame') {
      return(x[1:min(ncol(x),2)])
    } else {
      return(x)
    }
  }))
  werkset <- droplevels(werkset)
  werkset$titlelow <- tolower(stri_trans_general(werkset$title, "Latin-ASCII"))
  werkset$titlepart1 <- regexpr('[\\(:\\.\\[\\=]',werkset$titlelow)
  werkset$titlepart2 <- ifelse(werkset$titlepart1==-1, NA, substr(werkset$titlelow, werkset$titlepart1+1, nchar(werkset$titlelow)))
  werkset$titlepart1 <- ifelse(werkset$titlepart1==-1, NA, substr(werkset$titlelow, 1, werkset$titlepart1-1))
  werkset$titlelow <- gsub('[^a-z 0-9]','',werkset$titlelow)
}
df <- werkset[!werkset$BronCat %in% c('Universiteit (4TU)','Universiteit (alg)'),cols]
df$ID <- as.character(df$ID)

part <- sample(1:nrow(df), size=100)
part <- 1:nrow(df)
cmpr <- werkset[werkset$BronCat %in% c('Universiteit (4TU)','Universiteit (alg)'),]
cmpr$ID <- as.character(cmpr$ID)
print(paste('Starting amatch', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlelow, df$titlelow[part], method=method, q=q)+1
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlelow)+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlelow[part])+charcorr)))
distres <- data.frame(which='full',ID=cmpr$ID[apply(distmatr,2,which.min)],dist=apply(distmatr,2,min))
print(paste('Basic matrix calculated', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlepart1[!is.na(cmpr$titlepart1)], df$titlelow[part], method=method, q=q)+2
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlepart1[!is.na(cmpr$titlepart1)])+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlelow[part])+charcorr)))
distresnew <- data.frame(which='p1f',ID=cmpr$ID[!is.na(cmpr$titlepart1)][apply(distmatr,2,which.min)],dist=apply(distmatr,2,min))
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (2)', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlepart2[!is.na(cmpr$titlepart2)], df$titlelow[part], method=method, q=q)+4
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlepart2[!is.na(cmpr$titlepart2)])+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlelow[part])+charcorr)))
distresnew <- data.frame(which='p2f',ID=cmpr$ID[!is.na(cmpr$titlepart2)][apply(distmatr,2,which.min)],dist=apply(distmatr,2,min))
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (3)', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlelow, df$titlepart1[part][!is.na(df$titlepart1[part])], method=method, q=q)+2
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlelow)+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlepart1[part][!is.na(df$titlepart1[part])])+charcorr)))
distresnew <- data.frame(which='fp1', ID=character(length(part)), dist=Inf)
haspart <- which(!is.na(df$titlepart1[part]))
distresnew$ID[haspart] <- cmpr$ID[apply(distmatr,2,which.min)]
distresnew$dist[haspart] <- apply(distmatr,2,min)
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (4)', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlelow, df$titlepart2[part][!is.na(df$titlepart2[part])], method=method, q=q)+4
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlelow)+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlepart2[part][!is.na(df$titlepart2[part])])+charcorr)))
distresnew <- data.frame(which='fp2', ID=character(length(part)), dist=Inf)
haspart <- which(!is.na(df$titlepart2[part]))
distresnew$ID[haspart] <- cmpr$ID[apply(distmatr,2,which.min)]
distresnew$dist[haspart] <- apply(distmatr,2,min)
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (5)', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlepart1[!is.na(cmpr$titlepart1)], df$titlepart1[part][!is.na(df$titlepart1[part])], method=method, q=q)+6
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlepart1[!is.na(cmpr$titlepart1)])+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlepart1[part][!is.na(df$titlepart1[part])])+charcorr)))
distresnew <- data.frame(which='p1p1', ID=character(length(part)), dist=Inf)
haspart <- which(!is.na(df$titlepart1[part]))
distresnew$ID[haspart] <- cmpr$ID[!is.na(cmpr$titlepart2)][apply(distmatr,2,which.min)]
distresnew$dist[haspart] <- apply(distmatr,2,min)
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (6)', Sys.time()))

distmatr <- stringdistmatrix(cmpr$titlepart2[!is.na(cmpr$titlepart2)], df$titlepart2[part][!is.na(df$titlepart2[part])], method=method, q=q)+10
distmatr <- distmatr/sqrt(pmax(.0001,nchar(cmpr$titlepart2[!is.na(cmpr$titlepart2)])+charcorr))
distmatr <- t(t(distmatr)/sqrt(pmax(.0001,nchar(df$titlepart2[part][!is.na(df$titlepart2[part])])+charcorr)))
distresnew <- data.frame(which='p2p2', ID=character(length(part)), dist=Inf)
haspart <- which(!is.na(df$titlepart2[part]))
distresnew$ID[haspart] <- cmpr$ID[!is.na(cmpr$titlepart2)][apply(distmatr,2,which.min)]
distresnew$dist[haspart] <- apply(distmatr,2,min)
distres <- data.frame(which=ifelse(distresnew$dist<distres$dist, as.character(distresnew$which), as.character(distres$which)),
                      ID=ifelse(distresnew$dist<distres$dist, distresnew$ID, distres$ID),
                      dist=ifelse(distresnew$dist<distres$dist, distresnew$dist, distres$dist))
print(paste('Other matrix calculated (7)', Sys.time()))

df$matchmethod <- "NotChecked"
df$matchmethod[part] <- as.character(distres$which)
df$matchID <- NA
df$matchID[part] <- distres$ID
df$matchdist <- Inf
df$matchdist[part] <- distres$dist

names(df)[1:length(cols)] <- paste0(names(df)[1:length(cols)],'_orig')

df <- merge(df, cmpr[cols], by.x='matchID', by.y='ID',all.x=T, all.y=F)
names(df)[names(df) %in% cols] <- paste0(names(df)[names(df) %in% cols],'_match')
df <- data.frame(lapply(df,function(x) {
  if(class(x)[[1]]=='factor') {
    return(as.character(x))
  } else {
    return(x)
  }
}), stringsAsFactors = F)
df$description_match <- substring(as.character(df$description_match),1,30000)
df <- droplevels(df)
part <- which(!is.na(df$matchID))

df[part,] <- df[part,][order(df$matchdist[part]),]
# Find cutoff value
# To-Do: establish sensible default, for pmin, from, to, dens$x>...
cutoffparams <- c(0,2,10,0,2) # Min and max for dens-function, cutoff for densfunction, range to look for minimum
dens <- density(pmin(df$matchdist[part],cutoffparams[3]), from=cutoffparams[1], to=cutoffparams[2])
dens$y <- dens$y[dens$x>=cutoffparams[4] & dens$x<=cutoffparams[5]]
dens$x <- dens$x[dens$x>=cutoffparams[4] & dens$x<=cutoffparams[5]]
cutoff <- dens$x[c(diff(dens$y),-1)>0 & c(1, diff(dens$y))<=0][1] # Find first local minimum
par(mfrow=c(1,2))
plot(dens)
abline(v=c(cutoff))
plot(diff(dens$y))
df$matchdistscaled[part] <- df$matchdist[part]/(cutoff*2) # Meaning: 50% for cutoff, 0% for certain match, 100%+ certain non-match

write.csv2(df[part,order(names(df), decreasing = T)], 
           paste0(Paths$output,'/Theses from non-universities.csv'),
           row.names=F)
plot <- ggplot(data=df[part,]) + 
        geom_bar(aes(x=Bron_orig, y=1, fill=pmin(1,matchdist)), stat='identity') +
        scale_fill_gradientn(colours=c('green','yellow','red'),values=c(0,.5,1)) +
        theme(axis.text.x = element_text(angle=90))
print(plot)
ggsave(paste0(Paths$plots,'Non-uni theses.png'), plot)
