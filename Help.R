# Collection of functions, useful for NARCIS-analysis
libinstandload <- function(..., order=F, quiet=T) {
  packages <- unlist(as.vector(unlist(list(...))))
  if('extrafont' %in% packages && !'package:extrafont' %in% search()) loadfonts <- T else loadfonts <- F
  if(!all(sapply(packages, function(x) {class(x)=='character' && length(x)==1}))) {
    stop('Error in libinstandload: ... must be a list or vector of character, without nesting')
  }
  if(!order) {
    if(quiet) {
      suppressMessages(suppressWarnings(install <- !sapply(packages, require, character.only=T, quietly=T)))
    } else {
      install <- !sapply(packages, require, character.only=T)
    }
    if(any(install)) {
      install.packages(unlist(packages[install]))
      sapply(packages[install], library, character.only=T)
      if('extrafont' %in% install) {
        font_import()
        if(!is.null(Paths$Fonts)) font_import(paths=Paths$Fonts)
      }
    }
  } else {
    for(p in packages) {
      if(quiet) {
        if(!suppressMessages(suppressWarnings(require(p, character.only = T,quietly = T)))) {
          install.packages(p)
          library(p, character.only=T)
          if(p=='extrafont') {
            font_import()
            if(!is.null(Paths$Fonts)) font_import(paths=Paths$Fonts)
          }
        }
      } else {
        if(!require(p, character.only = T)) {
          install.packages(p)
          library(p, character.only=T)
          if(p=='extrafont') {
            font_import()
            if(!is.null(Paths$Fonts)) font_import(paths=Paths$Fonts)
          }
        }
      }
    }
  }
  if(loadfonts) {
    if(quiet) {
      suppressMessages({
        loadfonts()
        loadfonts(device='postscript')
      })
    } else {
      loadfonts()
      loadfonts(device='postscript')
    }
  }
}
ReadForAnalysisfromTotal <- function(Summarize=FALSE, FilePath='Auto', ForceReload=F, silent=F, DropCols=NULL, KeepCols=NULL, KeepSets=NULL, KeepMulti=NULL) {
  libinstandload('plyr','lubridate')
  tz <- Sys.getenv('TZ', unset=NA)
  Sys.setenv(TZ='Europe/Amsterdam')
  
  if(FilePath=='Auto') {
    RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                           pattern='.*Total.*\\.rds)', full.name=T, ignore.case=T), stringsAsFactors = F)
    RDSfiles$time <- file.mtime(RDSfiles$name)
    RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
    FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
    if(is.na(FilePath) || is.null(FilePath)) {
      FilePath <- paste0(Paths$Dumps,'/TotalSet.rds')
    }
  }
  if(!exists('Total')||ForceReload||is.null(Paths$WerksetTotal)||Paths$WerksetTotal!=FilePath) {
    if(!silent) print('Reading rds-file, which might take a while')
    Paths$WerksetTotal <- NA
    Total <- readRDS(FilePath)
    if(!silent && !(is.null(DropCols) && is.null(KeepCols) && is.null(KeepSets) && is.null(KeepMulti))) print('Loading finished, now subsetting...')
  }
  if(is.null(KeepCols)) {
    if(!is.null(DropCols)) Total <- Total[-DropCols]
  } else {
    Total <- Total[KeepCols]
  }
  if (!all(unique(unlist(sapply(Total$setSpec, levels))) %in% names(Total$setSpec)[sapply(Total$setSpec,class)=='logical'])||
      any(is.na(Total$setSpec[sapply(Total$setSpec,class)!='factor']))) {
    m <- sum(sapply(Total$setSpec,class)!='logical')
    for(n in unique(unlist(sapply(Total$setSpec, levels)))) {
      Total$setSpec[n] <- apply(Total$setSpec[,1:m],1,function(x) {any(n==x,na.rm=T)})
      print(paste(n,'tested'))
    }
  }
  if(!is.null(KeepSets)) {Total <- Total[apply(Total$setSpec[KeepSets],1,any),]}
  if(!is.null(KeepMulti)) {
    for (n in 1:(length(KeepPlotList)-1)) {
      Total <- Total[Total[,names(KeepMulti)[[n]]] %in% KeepMulti[[n]],]
    }
  }
  Total$year <- year(Total$date.meta.date)
  Total$Type[Total$Bron=='UvA'&Total$date.header<as.POSIXct('2017-08-20')&Total$Type=='Other'] <- 'Doctoral Thesis'
  levels(Total$Type) <- c(levels(Total$Type),'Unknown')
  Total$Type[is.na(Total$Type)] <- 'Unknown'
  if(!is.na(tz)) {
    Sys.setenv(TZ=tz)
  }
  Paths$WerksetTotal <- FilePath
  if (Summarize==FALSE) {
    assign('Total', Total, pos=.GlobalEnv)
    if(!silent) {
      print('Reading Total completed')
    }
    return(0)
  } else {
    levels(Total$access) <- c(levels(Total$access),'Other')
    Total$access[!Total$access %in% c('Closed','Open')] <- 'Other'
    Total$access <- droplevels(Total$access)
    Vals <- plyr::count(data.frame(Access=Total$access,
                                   Jaar=Total$year,
                                   Bron=Total$Bron,
                                   BronSoort=Total$BronCat,
                                   Type=Total$Type,
                                   lang=Total$language,
                                   set=Total$setSpec))
    Paths$WerksetTotal <- FilePath
    if(Summarize!='Only') {
      assign('Total', Total, pos=.GlobalEnv)
    }
    if(!silent) {
      print('Reading Total completed')
    }
    return(Vals)
  }
}
readNARCIScla <- function(FilePath=paste0(Paths$Params,'/classification_en.pdf')) {
  libinstandload('pdftools')
  cl <- pdf_text(FilePath)
  cl <- substr(cl, 1, nchar(cl)-4) # Removing pagenumbers
  cl <- paste(cl, collapse = ' ')
  cla <- data.frame(start=unlist(gregexpr('(D|E)[0-9]{5}',cl)))
  cla$stop <- c(cla$start[-1]-1,nchar(cl))
  cla <- apply(cla, 1, function(x) {substr(cl, x['start'], x['stop'])})
  cla <- data.frame(code=substr(cla,1,6), descr=gsub('[[:space:]\r\n]+',' ',substr(cla,7,500)),stringsAsFactors = F)
  cla$descr <- gsub('(^ )|( $)','',cla$descr)
  cla$descr <- gsub('[^A-Za-z0-9, \\(\\)]+','-', cla$descr) # Non-ASCII causing trouble
  cla$PartOf <- gsub('(D|E)([1-9]*)[1-9](0*)','\\1\\20\\3',cla$code)
  cla$PartOf[grepl('(D|E)0+$',cla$PartOf)] <- NA
  cla$lvl <- sapply(cla$code,function(x) {length(gregexpr('[1-9]',x)[[1]])})
  return(cla)
}
simple_rapply <- function(x, fn) {
  if(is.list(x))
  {
    lapply(x, simple_rapply, fn)
  } else {
    fn(x)
  }
}

































