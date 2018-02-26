if(length(ls())==0) {
  src <- getSrcDirectory(function(x) {x})
  if(src!='') setwd(src)
  Paths <- list(NowRunningScripts=getSrcFilename(function(x) {x}, full.names = T))
  source(paste0(getwd(), '/SetLocal.r'))
  rm(src)
  libinstandload('stringr','plyr','dplyr','profvis','tidyr')
}
if(T) {
  count <- plyr::count
  
  AutIDs <- read.csv2('~/Documents/DBs/persoon_externid.txt', stringsAsFactors = F)
  AutIDs$prefix <- as.factor(toupper(str_extract(AutIDs$extern_id, '^[A-Za-z]*')))
  
  AutIDs$extern_id[AutIDs$prefix=='ISNIO'] <- gsub('ISNIO', 'ISNI', AutIDs$extern_id[AutIDs$prefix=='ISNIO'], ignore.case = T)
  AutIDs$extern_id[AutIDs$prefix=='NWONWO'] <- gsub('NWONWO', 'NWO', AutIDs$extern_id[AutIDs$prefix=='NWONWO'], ignore.case = T)
  AutIDs$prefix <- as.factor(toupper(str_extract(AutIDs$extern_id, '^[A-Za-z]*')))
  
  doubles <- split(AutIDs$pers_id, AutIDs$extern_id)
  doubles <- doubles[sapply(doubles, function(x) {length(x)>1})]
  
  print(table(AutIDs$prefix))
  ISNIs <- AutIDs[AutIDs$prefix=='ISNI',1:2]
  names(ISNIs) <- c('NARCIS','ID')
  ISNIs$ID <- gsub('(ISNI)| |-', '', ISNIs$ID, ignore.case = T)
  table(nchar(ISNIs$ID))
  print(ISNIs[nchar(ISNIs$ID)!=16,])
  ISNIs <- ISNIs[nchar(ISNIs$ID)==16,]
  ISNIs$checksum <- sapply(ISNIs$ID, function(x) {(12-sum(sapply(1:15, function(n) {as.numeric(substring(x,n,n))*2^(16-n)}))) %% 11})
  ISNIs$lastdigit <- substring(ISNIs$ID,16,16)
  #print(count(ISNIs[3:4]))
  ISNIs$checksum <- ISNIs$checksum==substring(ISNIs$ID,16,16) | ISNIs$checksum==10 & substring(ISNIs$ID,16,16)=='X'
  ISNIs$lastdigit <- NULL
  cat('Wrong checksums in ISNIs:',sum(!ISNIs$checksum),'\n')
  ISNIs$ID <- tolower(ISNIs$ID)
  ISNIs$viaf <- NA
  
  DAIs <- AutIDs[AutIDs$prefix=='DAI',1:2]
  names(DAIs) <- c('NARCIS','ID')
  DAIs$ID <- gsub('(DAI)| |-', '', DAIs$ID, ignore.case = T)
  print(DAIs[!grepl('^[0-9Xx]+$', DAIs$ID),])
  DAIs <- DAIs[grepl('^[0-9Xx]+$', DAIs$ID),]
  table(nchar(DAIs$ID))
  print(DAIs[nchar(DAIs$ID)!=9,])
  DAIs <- DAIs[sapply(DAIs$ID, nchar)==9,]
  DAIs$viaf <- NA
  DAIs$ID <- tolower(DAIs$ID) # Should be redundant, just to make sure
  
  ORCIDs <- AutIDs[AutIDs$prefix=='ORCID',1:2]
  names(ORCIDs) <- c('NARCIS','ID')
  ORCIDs$ID <- gsub('(ORCID)| |-', '', ORCIDs$ID, ignore.case = T)
  table(nchar(ORCIDs$ID))
  print(ORCIDs[nchar(ORCIDs$ID)!=16,])
  ORCIDs <- ORCIDs[nchar(ORCIDs$ID)==16,]
  ORCIDs$checksum <- sapply(ORCIDs$ID, function(x) {(12-sum(sapply(1:15, function(n) {as.numeric(substring(x,n,n))*2^(16-n)}))) %% 11})
  ORCIDs$lastdigit <- substring(ORCIDs$ID,16,16)
  #print(count(ORCIDs[3:4]))
  ORCIDs$checksum <- ORCIDs$checksum==substring(ORCIDs$ID,16,16) | ORCIDs$checksum==10 & substring(ORCIDs$ID,16,16)=='X'
  ORCIDs$lastdigit <- NULL
  cat('\nWrong checksums in ORCIDs:',sum(!ORCIDs$checksum))
  ORCIDs$viaf <- NA
  ORCIDs$ID <- tolower(paste0('http://orcid.org/', 
                              substring(ORCIDs$ID,1,4), '-',
                              substring(ORCIDs$ID,5,8), '-',
                              substring(ORCIDs$ID,9,12), '-',
                              substring(ORCIDs$ID,13,16)))
}
stopat <- 2e12

finished <- F
n <- 0
fileprogress <- 0
types <- data.frame()
exDAIs <- data.frame(viaf=character(0), type=character(0), ID=character(0), stringsAsFactors = F)
con <- file('~/Downloads/viaf-20180204-links.txt', open='r')
while(!finished) {
  raw <- readLines(con, n = 1e5)
  VIAF <- sapply(raw, str_split, pattern='\\||@|\t', USE.NAMES = F)
  VIAF <- data.frame(viaf=sapply(VIAF, `[`, i=1, USE.NAMES = F),
                     type=sapply(VIAF, `[`, i=2, USE.NAMES = F),
                     ID=tolower(sapply(VIAF, `[`, i=3, USE.NAMES = F)),
                     stringsAsFactors = F)
  types <- count(rbind(count(VIAF$type), types),vars='x', wt_var = 'freq')
  n <- n+nrow(VIAF)
  fileprogress <- fileprogress + sum(nchar(raw, type='bytes'))
  if(nrow(VIAF)<1e5) finished <- T
  if(any(types %in% AutIDs$prefix & !types %in% c('ISNI', 'ORCID'))) stop('Unexpectedly there are more identifier-matches')
  VIAF <- VIAF[VIAF$type %in% c('ISNI','ORCID','NTA'),]
  
  matches <- match(ISNIs$ID, VIAF$ID[VIAF$type=='ISNI'])
  if(any(!is.na(ISNIs$viaf) & !is.na(matches))) stop('Double match')
  ISNIs$viaf[!is.na(matches)] <- VIAF$viaf[VIAF$type=='ISNI'][matches[!is.na(matches)]]
  
  matches <- match(DAIs$ID, VIAF$ID[VIAF$type=='NTA'])
  if(any(!is.na(DAIs$viaf) & !is.na(matches))) stop('Double match')
  DAIs$viaf[!is.na(matches)] <- VIAF$viaf[VIAF$type=='NTA'][matches[!is.na(matches)]]
  exDAIs <- rbind(exDAIs, VIAF[VIAF$type=='NTA',][-matches[!is.na(matches)],])
  
  matches <- match(ORCIDs$ID, VIAF$ID[VIAF$type=='ORCID'])
  if(any(!is.na(ORCIDs$viaf) & !is.na(matches))) stop('Double match')
  ORCIDs$viaf[!is.na(matches)] <- VIAF$viaf[VIAF$type=='ORCID'][matches[!is.na(matches)]]
  cat('\rParsed ',n/1e6,' million lines (approximately ',round(fileprogress/2^20,1),' MB of 4660 MB, ',
      round(100*fileprogress/(4.55*2^30), 1),'%)                  ', sep='')
  if(n==stopat) finished <- T
  if(n %% 5e6==0) {
    cat('\nPausing, now would be a good moment to break (', format(Sys.time(), usetz=T),')', sep='')
    Sys.sleep(3)
    cat('\nResuming')
  }
}

DAIs$type <- as.factor('DAI')
ISNIs$type <- as.factor('ISNI')
if(!all(ISNIs$checksum)) {
  stop('Checksum errors in ISNIs, solve first (line ',getSrcLocation(function(x) {x}),')')
} else {
  ISNIs$checksum <- NULL
}
ORCIDs$type <- as.factor('ORCID')
if(!all(ORCIDs$checksum)) {
  stop('Checksum errors in ORCIDs, solve first (line ',getSrcLocation(function(x) {x}),')')
} else {
  ORCIDs$checksum <- NULL
}
unfound <- rbind(DAIs[is.na(DAIs$viaf),], ISNIs[is.na(ISNIs$viaf),], ORCIDs[is.na(ORCIDs$viaf),])
unfound$viaf <- NULL
found <- rbind(DAIs[!is.na(DAIs$viaf),], ISNIs[!is.na(ISNIs$viaf),], ORCIDs[!is.na(ORCIDs$viaf),])
#if(any(!which(duplicated(found$viaf)) %in% duplicated(found$NARCIS))) stop('Check first')
doublesfrom <- doubles
doubles <- split(found$NARCIS, found$viaf)
doubles <- doubles[sapply(doubles, function(x) {length(unique(x))>1})]
doubles <- lapply(doubles, unique)
doubles <- doubles[sapply(doubles, function(x) {!all(unlist(x) %in% unlist(doublesfrom))})]
doubles3 <- rbind.fill(ISNIs[duplicated(ISNIs$ID) & !ISNIs$NARCIS %in% unlist(doublesfrom) & !ISNIs$NARCIS %in% unlist(doubles),], # Dit zijn degenen die nog overblijven, waarbij de identifiers gelijk werden bij het normaliseren (0000 0000 1234 5678 en 0000000012345678)                            
                       DAIs[duplicated(DAIs$ID) & !DAIs$NARCIS %in% unlist(doublesfrom) & !DAIs$NARCIS %in% unlist(doubles),],
                       ORCIDs[duplicated(ORCIDs$ID) & !ORCIDs$NARCIS %in% unlist(doublesfrom) & !ORCIDs$NARCIS %in% unlist(doubles),])

origviafsfound <- found
found <- found[!duplicated(found$viaf)&!duplicated(found$NARCIS),]

finished <- F
n <- 0
fileprogress <- 0
allIDs <- data.frame(NARCIS=character(), viaf=character(), type=factor(levels=types$x), ID=character(), stringsAsFactors = F)
close(con)
con <- file('~/Downloads/viaf-20180204-links.txt', open='r')
stopat <- 1e12
found$type <- as.factor(found$type)
#profvis({
while(!finished) {
  raw <- readLines(con, n = 1e5)
  VIAFpre <- sapply(raw, str_split, pattern='\\||@|\t', USE.NAMES = F)
  VIAF <- data.frame(viaf=sapply(VIAFpre, `[`, i=1, USE.NAMES = F),
                     type=sapply(VIAFpre, `[`, i=2, USE.NAMES = F),
                     ID=sapply(VIAFpre, `[`, i=3, USE.NAMES = F),
                     stringsAsFactors = F)
  n <- n+nrow(VIAF)
  fileprogress <- fileprogress + sum(nchar(raw, type='bytes'))
  if(nrow(VIAF)<1e5) finished <- T
  matches <- VIAF[VIAF$viaf %in% found$viaf,]
  matches$NARCIS <- found$NARCIS[match(matches$viaf, found$viaf)]
  allIDs <- rbind(allIDs, matches, stringsAsFactors=F)
  
  cat('\rParsed ',n/1e6,' million lines (approximately ',round(fileprogress/2^20,1),' MB of 4660 MB, ',
      round(100*fileprogress/(4.55*2^30), 1),'%)                  ', sep='')
  if(n==stopat) finished <- T
  if(n %% 5e6==0) {
    cat('\nPausing, now would be a good moment to break (', format(Sys.time(), usetz=T),')', sep='')
    Sys.sleep(3)
    cat('\nResuming')
  }
}
#}) # Closing profvis

allIDs$type <- as.factor(allIDs$type)
dupls <- allIDs[duplicated(allIDs[c('viaf','type','NARCIS')])|duplicated(allIDs[c('viaf','type','NARCIS')], fromLast = T),]
dupls <- dupls[order(dupls$NARCIS, dupls$viaf, dupls$type, dupls$ID),]
wikis <- dupls[dupls$type=='Wikipedia',]
dupls <- dupls[dupls$type!='Wikipedia',]
realdupls <- split(dupls,list(dupls$viaf, dupls$type), drop=T)
realdupls <- realdupls[sapply(realdupls, nrow)>2 | sapply(realdupls, function(x) {
  x$ID <- gsub(' ', '', x$ID)
  !(grepl(x$ID[1], x$ID[2], fixed=T) | 
      grepl(x$ID[2], x$ID[1], fixed=T))
})]
realdupls <- bind_rows(realdupls)
realdupls <- realdupls[order(realdupls$NARCIS, realdupls$viaf, realdupls$type),c('viaf','NARCIS','type','ID')]

allIDs$ID[!is.na(match(paste0(allIDs$viaf, allIDs$type, allIDs$NARCIS), paste0(realdupls$viaf, realdupls$type, realdupls$NARCIS)))] <- 'Multiple'
  
allIDs <- allIDs[!duplicated(allIDs[-3]),]
output <- spread(allIDs, type, ID)
output <- output[order(output$NARCIS, output$viaf),]
write.csv2(output, paste0(Paths$output,'/VIAF/resultaatlijst.csv'), na='', row.names=F)
write.csv2(realdupls, paste0(Paths$output, '/VIAF/Multi-ID-lijst.csv'), row.names = F)
write.csv2(wikis[order(wikis$NARCIS),], paste0(Paths$output,'/VIAF/Wikilijst.csv'), row.names = F)
write.csv2(unfound[order(unfound$NARCIS),], paste0(Paths$output, '/VIAF/Ongevonden.csv'), row.names = F)
write.csv2(bind_rows(doublesfrom, doubles), Paths$output, '/VIAF/dubbelen.csv', row.names = F)






















