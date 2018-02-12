ReadVSNUpdf <- function(pdfs=paste0(Paths$input,c('/Narcis versus KUOZ 1.pdf','/Narcis versus KUOZ 2 uitgebreid.pdf'))) {
  libinstandload('pdftools','data.table','zoo','tidyr')
  
  VSNU <- c(sapply(pdfs, pdf_text))
  VSNU <- sapply(VSNU, strsplit, split='\\n')
  VSNU[[1]] <- unname(VSNU[[1]][-c(1,39)])
  VSNU[[2]] <- unname(VSNU[[2]][-c(1,21)])
  VSNU[[3]] <- unname(VSNU[[3]][-c(1,32)])
  VSNU[[4]] <- unname(VSNU[[4]][-c(31)])
  
  headers <- lapply(VSNU, function(x) {
    t <- strsplit(x[1],'\\s')[[1]]
    return(t[t!=''])
  })
  headers[[1]] <- c('Type',headers[[1]])
  headers[[2]] <- c('Type',headers[[2]])
  
  VSNU <- unname(lapply(VSNU, function(x) {unname(x[-1])}))
  names(VSNU) <- c(paste0('P',1:4))
  delimlines <- c(P1=VSNU[[1]][34], P2=VSNU[[2]][13], P3=VSNU[[3]][1], P4=VSNU[[4]][18])
  #delimlines <- gsub('[[:space:]]','_',delimlines)
  delimchars <- strsplit(delimlines, split='\\s',perl=T)
  delims <- lapply(delimchars, function(x) {list(start=unname(cumsum(sapply(x, nchar)+1)[c(x[-1],'')!='']+1),
                                                 stop=unname(cumsum(sapply(x, nchar)+1)[x!='']-1))})
  delims <- lapply(delims, function(x) {
    list(start=c(x$start[1:2], x$stop[3]-5, x$stop[3:(length(x$stop)-1)]+2),
         stop=c(x$start[2]-1, x$stop[3]-6, x$stop[3:length(x$stop)]+1))
  })
  delims$P2$start <- delims$P2$start[-16]
  delims$P2$stop <- delims$P2$stop[-15]
  
  dfs <- lapply(1:4, function(n) {
    df <- as.data.frame(do.call(rbind, lapply(VSNU[[n]], function(l) {
      gsub('([0-9]) ([0-9])','\\1\\2', gsub('^ *| *$|','',substring(l, delims[[n]]$start, delims[[n]]$stop)))
    })), stringsAsFactors = F)
    df[3:ncol(df)] <- sapply(df[3:ncol(df)], function(x) {ifelse(x=='',0,as.numeric(x))})
    names(df) <- headers[[n]]
    return(df)
  })
  doc1 <- rbind(dfs[[1]], dfs[[2]])
  doc1$Type[doc1$Type==''] <- NA
  doc1$Type <- na.locf(doc1$Type)
  NARCISTypes <- c('Annotation','Article','Book','Contribution to periodical','Doctoral Thesis','Report')
  doc1$Bron <- ifelse(doc1$Type %in% NARCISTypes, 'NARCIS','KUOZ')
  doc1$HoofdType <- na.locf(ifelse(doc1$Bron=='NARCIS', doc1$Type,NA))
  doc1 <- gather(doc1, 'Uni','count', -Type, -jaar, -Bron, -HoofdType)
  doc1[-6] <- lapply(doc1[-6], as.factor)
  doc1 <- doc1[c(3,4,1,2,5,6)]
  
  doc2 <- rbind(dfs[[3]],dfs[[4]])
  doc2 <- doc2[doc2$Bron!='Bron' & doc2$Onderwerp!='Onderwerp',]
  names(doc2)[2] <- 'Type'
  doc2$HoofdType <- na.locf(ifelse(doc2$Bron=='NARCIS', doc2$Type, NA))
  doc2$HoofdType[substr(doc2$HoofdType,1,10)=='Conference'] <- 'Conference'
  doc2 <- gather(doc2, 'jaar','count', -Bron, -Type, -HoofdType)
  
  doc2[1:3] <- lapply(doc2[1:3], as.factor)
  return(list(doc1=doc1, doc2=doc2))
}