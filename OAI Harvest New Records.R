{ ## Helper functions 
  ### deCache (clean=F, quiet=F, checkTotal=F)
  deCache <- function(clean=F, quiet=F, checkTotal=T, save=ifelse(Params$harv=='didlmods','MaxMem',F), fileNames='auto') {
    ##### clean \t Should files that have been stored earlier be deleted? Caution: Only works with standard filenames
    ###### Also cleans Total_New en Total_New_Sets if save=='MaxMem'
    ##### quiet \t Suppress messages in console?
    ##### checkTotal \t Check if the number of records is equal for all measurements. If F, some checks are still performed
    ##### save \t Save files as well, or only decache. 'MaxMem' to only save if memory usage grows too large, otherwise T or F
    ##### fileNames \t Names to be used for saving, or 'auto'.
    ###### Vector of complete filenames for c('Total_New','Total_ID','Total_Del','MetaOut','Errors','Total_New_Sets')
    ###### Names being NA are not stored, with a warning if quiet=F
    ##### Cachefiles are emptied, not removed
    DoneAnything <- F
    if(exists('Total_New_Sets') && exists('Total_New_Sets_Cache')) {
      if(!quiet) print('Saving cached results dataframes')
      DoneAnything <- T
      for(i in Params$subdfs) {
        if(nrow(Total_New_Sets[[i]])!=nrow(Total_New)) {
          Total_New_Sets[[i]] <<- rbind.fill(Total_New_Sets[[i]], Total_New_Sets_Cache[[i]])
          Total_New_Sets_Cache[[i]] <<- Total_New_Sets_Cache[[i]][F,]
        }
        if(!nrow(Total_New_Sets[[i]]) %in% c(nrow(Total_New), MetaOut$NoRecords-MetaOut$NoSaved)) {
          stop('Unclear what values should be decached and which not')
        }
      }
    }
    if(exists('Total_New') && exists('Total_New_Cache')) {
      DoneAnything <- T
      if(!quiet) print('Saving cached results raw list')
      if(nrow(Total_New)<MetaOut$NoRecords-MetaOut$NoSaved && 
         nrow(Total_New)+nrow(Total_New_Cache)>=MetaOut$NoRecords-MetaOut$NoSaved &&
         !any(Total_New_Cache$ID %in% Total_New$ID)) {
        Total_New <<- rbind.fill(Total_New, Total_New_Cache)
        Total_ID <<- c(Total_ID, Total_New_Cache$ID)
        Total_New_Cache <<- Total_New_Cache[F,]
      }
      if(nrow(Total_New)<MetaOut$NoRecords-MetaOut$NoSaved || length(Total_ID)<MetaOut$NoRecords) {
        stop('Warning: number of records seems corrupt (too few records)')
      }
      if(checkTotal && (nrow(Total_New)!=MetaOut$NoRecords-MetaOut$NoSaved || length(Total_ID)!=MetaOut$NoRecords)) {
        stop('Warning: number of records seems corrupt (too many)')
      }
    }
    if(identical(save, T) || (save=='MaxMem' && 
                              object.size(Total_New)+object.size(Total_New_Sets)+object.size(Total_New_Sets_Cache)+object.size(Total_New_Cache)>Params$MaxMem)) {
      DoneAnything <- T
      if(!quiet) print('Saving files')
      if(class(Total_New_Sets)=='list' && class(Total_New_Sets[[1]])=='data.frame' && nrow(Total_New_Sets[[1]])>0) {
        if(all(fileNames=='auto')) {
          thisName <- paste0(Paths$Summaries,'/Chunks/Total_New_Sets (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
        } else if(!is.na(fileNames[6])) {
          thisName <- fileNames[6]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for Total_New_Sets. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(!is.na(thisName)) saveRDS(Total_New_Sets, file=thisName)
      }
      if(nrow(Total_New)>0) {
        if(all(fileNames=='auto')) {
          if(save=='MaxMem') {
            thisName <- paste0(Paths$Summaries,'/Chunks/Total_Part (file ',Total_New$filenr[1],'-',Total_New$filenr[nrow(Total_New)],'), (',
                               gsub('[^0-9]+','',as.character(Sys.time())),').rds')
          } else {
            thisName <- paste0(Paths$Summaries,'/Chunks/Total_New (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
          }
        } else if(!is.na(fileNames[1])) {
          thisName <- fileNames[1]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for Total_New. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(!is.na(thisName)) saveRDS(Total_New, file=thisName)
      }
      if(length(Total_ID)>0) {
        if(all(fileNames=='auto')) {
          thisName <- paste0(Paths$Summaries,'/Chunks/Total_ID (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
        } else if(!is.na(fileNames[2])) {
          thisName <- fileNames[2]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for Total_ID. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(!is.na(thisName)) saveRDS(Total_ID, file=thisName)
      }
      if(nrow(Total_Del)>0) {
        if(all(fileNames=='auto')) {
          thisName <- paste0(Paths$Summaries,'/Chunks/Total_Del (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
        } else if(!is.na(fileNames[3])) {
          thisName <- fileNames[3]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for Total_Del. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(!is.na(thisName)) saveRDS(Total_Del, file=thisName)
      }
      if(exists('MetaOut')) {
        if(all(fileNames=='auto')) {
          thisName <- paste0(Paths$Summaries,'/Chunks/MetaOut (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
        } else if(!is.na(fileNames[4])) {
          thisName <- fileNames[4]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for MetaOut. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(save=='MaxMem' && clean) MetaOut$NoSaved <<- MetaOut$NoSaved+nrow(Total_New)
        if(!is.na(thisName)) saveRDS(MetaOut, file=thisName)
      }
      if(!is.null(Errors$RecMerge)) {
        if(all(fileNames=='auto')) {
          thisName <- paste0(Paths$Summaries,'/Chunks/Err_RecMerge (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds')
        } else if(!is.na(fileNames[5])) {
          thisName <- fileNames[5]
          if(!grepl('/', thisName)) {
            print('Appending filename with standard-path')
            thisName <- paste0(Paths$Summaries,'/Chunks/', thisName)
          }
        } else {
          if (quiet || readline(prompt="deCache called without a filename for MetaOut. Continue?")=='y') {
            thisName <- NA
          } else {
            stop('Operation cancelled (deCache)')
          }
        }
        if(gsub('^.*/','',thisName) %in% list.files(sub('/[^/]*$','',thisName)) && 
           readline(prompt=paste0("File already exists (",thisName,"). Overwrite?"))!='y') stop('Operation cancelled (deCache)')
        if(!is.na(thisName)) saveRDS(Errors, file=thisName)
      }
      if(clean) {
        if(!quiet) print('Cleaning old files')
        RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Summaries,'/Chunks'), pattern='.*\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
        RDSfiles$time <- file.mtime(RDSfiles$name)
        if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
        RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing = T),]
        rmvtemp <- ifelse(RDSfiles$time[grepl('Total_New', RDSfiles$name)]>RDSfiles$time[grepl('Total_Part', RDSfiles$name)],T,-1)
        if((any(grepl('Total_Part', RDSfiles$name)) && any(grepl('Total_New \\(temp\\)', RDSfiles$name)) &&
            RDSfiles$time[grepl('Total_New \\(temp\\)', RDSfiles$name)][1]>RDSfiles$time[grepl('Total_Part', RDSfiles$name)][1]) ||
           !any(grepl('Total_Part', RDSfiles$name))) {
          rmvtemp <- -1
        } else {
          rmvtemp <- T
        }
        RDSfiles <- RDSfiles[!grepl('Total_Part', RDSfiles$name),]
        file.remove(RDSfiles$name[grepl('Total_New_Sets \\(temp\\)', RDSfiles$name)],
                    RDSfiles$name[grepl('Total_New \\(temp\\)', RDSfiles$name)][rmvtemp],
                    RDSfiles$name[grepl('Total_ID \\(temp\\)', RDSfiles$name)][-1],
                    RDSfiles$name[grepl('Total_Del \\(temp\\)', RDSfiles$name)][-1],
                    RDSfiles$name[grepl('MetaOut \\(temp\\)', RDSfiles$name)][-1],
                    RDSfiles$name[grepl('Err_RecMerge \\(temp\\)', RDSfiles$name)][-1])
        # Note that if you save in another folder, copies remain in Chunks
        rm(RDSfiles)
        if(save=='MaxMem') {
          Total_New <<- Total_New[F,]
          if(exists('Total_New_Sets') && class(Total_New_Sets)=='data.frame') Total_New_Sets <<- Total_New_Sets[F,]
          if(exists('Total_New_Sets') && class(Total_New_Sets)=='list') {
            for(i in Params$subdfs) {
              Total_New_Sets[[i]] <<- Total_New_Sets[[i]][F,]
            }
          }
        }
      }
    }
    if(!quiet && DoneAnything) print('DeCaching completed')
    gc(verbose=F)
    return(invisible(0))
  }
  .Last <- function() {
    if(exists('deCache') && 
       ((exists('Total_New') && nrow(Total_New)>0) ||
        (exists('Total_New_Cache') && nrow(Total_New_Cache)>0))) 
      deCache(checkTotal=F, save=T)
  }
  removeDupls <- function(mlite, borderdate) {
    libinstandload('fastmatch')
    Sys.setenv(TZ='UTC')
    if(class(borderdate)[1]=='character') borderdate <- as.POSIXct(borderdate)
    if(!'POSIXt' %in% class(borderdate) || is.na(borderdate)) {
      print('Invalid borderdate')
      return(NULL)
    }
    dbls <- mlite$aggregate(pipeline=paste0('[{"$group": {"_id": {"ID": "$ID"},',
                                            '"uniqueIds": {"$addToSet": "$_id"},',
                                            '"count":{"$sum": 1}}}, ',
                                            '{"$match": {"count": {"$gt": 1}}}]'),
                            options='{"allowDiskUse": true}')
    if(nrow(dbls)==0) {
      print('Nothing to remove')
      return(data.frame())
    }
    mids <- unlist(dbls$uniqueIds)
    qry <- paste0('{"_id": {"$in": [{"$oid":"', paste(mids, collapse = '"}, {"$oid":"'),'"}]}}')
    info <- mlite$find(qry, fields='{"date_header":true}', pagesize = 1e5)
    dbls$ID <- simplify2array(dbls$`_id`$ID)
    dbls$`_id` <- NULL
    dbls$dates <- lapply(dbls$uniqueIds, function(id) {
      info$date_header[fmatch(id, info$`_id`)]
    })
    if(any(sapply(dbls$dates, function(d) {any(is.na(d)) || min(d)>borderdate || max(d)<borderdate}))) {
      print('Warning: borderdate is not a splitting value. Not removing anything, returning  list of doubles')
      return(dbls)
    }
    rmvids <- mapply(function(id, date) {
      id[[which.min(date)]]
    }, dbls$uniqueIds, dbls$dates)
    qry <- paste0('{"_id": {"$in": [{"$oid":"', paste(rmvids, collapse = '"}, {"$oid":"'),'"}]}}')
    # temp <- mlite$find(qry, fields='{"ID":true, "date_header":true}') # Test query
    print('Removing doubles')
    result <- mlite$remove(qry)
    newdbls <- mlite$aggregate(pipeline=paste0('[{"$group": {"_id": {"ID": "$ID"},',
                                               '"uniqueIds": {"$addToSet": "$_id"},',
                                               '"count":{"$sum": 1}}}, ',
                                               '{"$match": {"count": {"$gt": 1}}}]'),
                               options='{"allowDiskUse": true}')
    if(isTRUE(result) && nrow(newdbls)==0) {
      print('Succesfully removed doubles')
      if(any(!attr(dbls$dates, 'tzone') %in% c('UTC',''))) {
        print('Warning: dates will be cast to UTC')
        print(paste0('There are ',sum(attr(dbls$dates, 'tzone')!='UTC'),' records with different timezones.'))
        print(paste0('Unique ones are: ', paste0(unique(attr(dbls$dates, 'tzone')[attr(dbls$dates, 'tzone')!='UTC']), collapse = ', ')))
      }
      return(data.frame(ID=dbls$ID, date=do.call(c, lapply(dbls$dates, min))))
    } else {
      print('Warning: removal was attempted, but seems unsuccesfull. Returning as much information as posisble')
      return(list(doubles=dbls, dateinfo=info, tryremove=rmvids, stillinDB=newdbls))
    }
  }
} # Helper functions
{ ## Initialize
  # Restore from old state
  if(exists('Step')) {
    StepBU <- Step
  } else {
    src <- getSrcDirectory(function(x) {x})
    if(src!='') setwd(src)
    rm(src)
    source(paste0(getwd(), '/SetLocal.r'))
    libinstandload('readr','plyr','dplyr','lubridate','stringr','XML','rvest','oai','profvis','fastmatch',
                   'RMongo','jsonlite','mongolite') #,'mvndeps')
    #temp <- get_dependency_jar(group='org.mongodb', dep='mongodb-driver', version='3.0.4')
    #temp <- get_dependency_jar(group='org.mongodb', dep='mongo-java-driver', version='3.0.4')
    #temp <- list.files(gsub('/org/mongodb/.*', '/org/mongodb', temp), pattern='.*\\.jar$', T,T,T,T)
    #file.copy(temp, paste0(.libPaths()[1],'/RMongo/java'))
    Sys.setenv(TZ='UTC')
    options(stringsAsFactors = T)
  }
  if(!exists('SubStep')) {SubStep <- 'Start'}
  if(exists('Params') && !is.null(Params$cla)) {
    Params <- list(cla=Params$cla, Resume=list())
  } else {
    Params <- list(Resume=list())
    Params$cla=readNARCIScla()
  }
  
} # Initialize

# General settings:
Params$harv <- 'dc'
Params$NameOfHarv <- '2018-02-12'
Step <- 'JustGo'
Params$debug <- F
Params$WayBack <- as.POSIXct('2018-12-31 23:59:59', tz='UTC')
Params$Summarize <- c('IDs') # Which summaries should be produced? 'Records' or 'IDs'/'alwaysIDs' (or combination). 'IDs' is ignored if no IDs are processed, 'alwaysIDs' is not.

{ ## Some more settings, not used too much
  Params$didlbeperkt <- T
  Params$mongoColl <- paste0('DirectFromNewHarvest_',Params$NameOfHarv)
  
  resdate <- NULL
  Params$reCalcIDs <- F # Should IDs be processed (again)? If true, summary is automatically produced as well
  # Date from which to assume all records have been harvested before, and records are stable:
  Params$oldfdate <- as.POSIXct('1990-01-01', tz='UTC') # You can set this later for efficiency, but debugging becomes harder.
  Params$nfiles <- 50
  
  Paths$dumproot <- paste0(Paths$Dumps,'/',Params$NameOfHarv)
  Paths$dcXML<- paste0(Paths$dumproot, '/dcXML')
  Paths$didlmodsXML <- paste0(Paths$dumproot, '/didlmodsXML')
  Paths$Parsed <- paste0(Paths$dumproot, '/Chunk')
  Paths$IDdcXML <- paste0(Paths$dumproot, '/dcIDs')
  Paths$IDdidlmodsXML <- paste0(Paths$dumproot, '/didlIDs')
  
  # See what is available on disk
  RDSfiles <- data.frame(name=list.files(path=Paths$BaseForNewHarvest,
                                         pattern=paste0(Params$harv,'.*\\.(rds|RDS)'), full.name=T, ignore.case=T), stringsAsFactors = F)
  if(nrow(RDSfiles)==0) {
    print('No old files found, creating new empty df')
    saveRDS(data.frame(), file=paste0(Paths$BaseForNewHarvest,'/EmptyTotal ',Params$harv,' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    saveRDS(data.frame(), file=paste0(Paths$BaseForNewHarvest,'/EmptyIDlist ',Params$harv,' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    RDSfiles <- data.frame(name=list.files(path=Paths$BaseForNewHarvest,
                                           pattern='.*\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
  }
  RDSfiles$time <- file.mtime(RDSfiles$name)
  if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
  RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
  Paths$OldTotal <- RDSfiles$name[grepl(paste0('Total.*',Params$harv),RDSfiles$name, ignore.case=T)][1]
  Paths$oldID <- RDSfiles$name[grepl(paste0('IDlist.*',Params$harv),RDSfiles$name, ignore.case=T)][1]
  Paths$Summaries <- paste0(Paths$dumproot, '')   # Hier wordt nog params$harv aan toegevoegd
  Paths$Encoding <- 'UTF-8'
  Params$MaxAge <- ifelse(Params$debug, 730, 14) # Maximum age of files in days
  
  Params$urls$dcoaiurl <- 'http://oai.narcis.nl/oai'
  Params$urls$gmhoaiurl <- 'http://oai.gharvester.dans.knaw.nl'
  Params$urls$gmhformat <- 'nl_didl_combined'
  Params$filesize <- ifelse(Params$harv=='dc',200,100)
  Params$MaxMem <- 1e9
  Params$JustMongo <- F
  if(Params$JustMongo) Params$mongoColl <- 'nldidlnorm'
  Params$MongoUser <- 'admin'
  Params$MongoPswd <- 'admin'
  Params$Mongoport <- 27017
  
} # Some more settings, not used too much
{ ## Settings paths and static variables
  
  
  if(Params$harv=='dc') {
    Paths$XML <- Paths$dcXML
    Paths$IDXML <- Paths$IDdcXML
    Params$subdfs <- c('setSpec','GlobalIDs','ppl.creator','subject','subjectcode','format','source.meta','ppl.contributor',
                       'isPartOf','relation','coverage')
    Params$SetsSet <- c('dataset','oa_publication','publication','ec_fundedresources','openaire','thesis')
  } else if(Params$harv=='didlmods') {
    Paths$XML <- Paths$didlmodsXML
    Paths$IDXML <- Paths$IDdidlmodsXML
    Params$subdfs <- c()
    Params$SetsSet <- c()
  } else {
    stop('Unknown format, Paths$XML <- unknown')
  }
  if(!dir.exists(Paths$XML)) {dir.create(Paths$XML, recursive = T)}
  if(!dir.exists(Paths$IDXML)) {dir.create(Paths$IDXML, recursive = T)}
  
  Params$tempvarnames <- c('endResTokens','i','j','l','lastfiles','lastResToken','longdelays','lastchecked',
                           'moreFiles','mrfiles','m','n','o','rawfile','rawlen','startResTokens','totalerrors','newfiles','rms',
                           'wrongi','wrong','parsed','ch','pRecs','xmlerrors','badstr','Rec','headatt',
                           'orig','OneRecdf','names','longnames','commonname','Recdf','Deleteddf','ul',
                           'ParsRecfiles','ParsDelfiles','fileborders','Deldf','Recnrs','FirstID','LastID',
                           'colinnames','colsMand','RDSfiles','cols','Outdf','OutDel','x','CummTotals','namen','lengths','wrongrepl',
                           'CorrErrors','NonDel','OneDeldf','Recs','extraIDs','fIDs',
                           'extramatchlength','frommatch','tomatch','matchlength','matchrange','rights')
  Params$keepvarnames <- c(as.character(lsf.str()), 'Paths','Params','resdate','Errors','Step','SubStep','mongo','mlite')
  Params$Reclsnames <- c("responseDate","request","ListRecords")
  Params$dateform <- c('%Y-%m-%dT%H:%M:%SZ', '%Y-%m-%dT%H:%M:%OSZ', '%Y-%m-%d') # Caution: for some purposes, only first format is tried
  Params$RegExErrors <- c('<U\\+[A-F0-9]{4}>',                                  # e.g <U+12A4>
                          '<([^>]*)<',                                          # e.g. <SomeKey="....", atrr=<AnotherKey?>. Replacement unsure
                          '<dc:[A-Za-z]+></dc\\:[A-Za-z]+>[[:space:]]*',        # Empty dc-tags. Note this does nothing for didl-mods
                          '((<dc:description>)|(<dc:title>))([^<>]*<[^<>:]*>)+[^<>]*((</dc:description>)|(</dc:title>))', # Only for dc: remove any tag-like text in description and title
                          '(<dc:([A-Za-z]+)>[^<>]+</dc:\\2>)\\s*(\\1\\s*)+')    # Dubbelen
  Params$MongoRegex <- '[^\\x{0000}-\\x{00FF}Ā-￮ ]+'
  # Code points:
  # ASCII
  # 256-65518
  
  if(exists('Params') && !is.null(Params$LastPath) && Params$LastPath!=Paths$dumproot) stop('If path is changed, restart first!')
  Params$LastPath <- Paths$dumproot
  Paths$Summaries <- paste0(Paths$Summaries,'/',Params$harv)
  Paths$Parsed <- paste0(Paths$Parsed,Params$harv)
  if(!dir.exists(paste0(Paths$Summaries,'/Chunks'))) {dir.create(paste0(Paths$Summaries,'/Chunks'), recursive = T)}
  if(!dir.exists(Paths$Parsed)) {dir.create(Paths$Parsed, recursive = T)}
  
} # Settings paths and static variables

#Params$Resume$RecordParse <- 1

if(Step=='JustGo' || Step=='InitOnly') {
  print(paste0('Script started at ', format(Sys.time(), usetz = T)))
  if(Params$debug) print("Debugging mode")
  deCache(clean = F, checkTotal=F, save=F)
  if(exists('StepBU') && Step!='InitOnly') {
    Step <- StepBU
    if(StepBU %in% c('harvnewRecs','SummariseRecords','ParseRecords','MergeRecords','Finalize','MakeIDfile')) {
      Params$Summarize <- Params$Summarize[Params$Summarize!='IDs']
    }
    if(StepBU %in% c('JustGo','harvIDs')) Step <- 'Start'
    rm(StepBU)
    resdate <- suppressWarnings(as.POSIXct(min(newIDs$ts, oldIDs$inNewTs[!(oldIDs$inNew %in% c('Never', 'Assumed', 'Unchanged','Disappeared'))],Sys.time())-1))
  } else {
    RDSfiles <- data.frame(name=list.files(path=c(Paths$Summaries, paste0(Paths$Summaries, '/Chunks')),
                                           pattern='.*\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
    RDSfiles$time <- file.mtime(RDSfiles$name)
    RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
    if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
    if(any(grepl('\\/IDlist', RDSfiles$name))) {
      Step <- 'CopyFiles'
    } else {
      temp <- difftime(Sys.time(), RDSfiles$time[grepl('oldIDs',RDSfiles$name)][1], units='days')<Params$MaxAge &&
        difftime(Sys.time(), RDSfiles$time[grepl('FileList IDFs',RDSfiles$name)][1],units='days')<Params$MaxAge &&
        difftime(Sys.time(), RDSfiles$time[grepl('newIDs',RDSfiles$name)][1],units='days')<Params$MaxAge &&
        difftime(Sys.time(), RDSfiles$time[grepl('Errors',RDSfiles$name)][1],units='days')<Params$MaxAge
      if(!is.na(temp) && temp && !Params$reCalcIDs) {
        print('Reading files')
        oldIDs <- readRDS(RDSfiles$name[grepl('oldIDs',RDSfiles$name)][1])
        IDfiles <- readRDS(RDSfiles$name[grepl('FileList IDFs',RDSfiles$name)][1])
        newIDs <- readRDS(RDSfiles$name[grepl('newIDs',RDSfiles$name)][1])
        if(!exists('Errors')) {
          Errors <- readRDS(RDSfiles$name[grepl('Errors|Err_RecMerge',RDSfiles$name)][1])
        }
        if(!exists('Total_New') && 
           isTRUE(difftime(Sys.time(), RDSfiles$time[grepl('Total_New \\(temp',RDSfiles$name)][1],units='days')<Params$MaxAge) &&
           !isTRUE(RDSfiles$time[grepl('Total_New \\(temp',RDSfiles$name)][1]<RDSfiles$time[grepl('Total_?New',RDSfiles$name)][1])) {
          Total_New <- readRDS(RDSfiles$name[grepl('Total_New \\(temp',RDSfiles$name)][1])
        }
        if(!exists('Total_New_Sets') &&
           isTRUE(difftime(Sys.time(), RDSfiles$time[grepl('Total_New_Sets \\(temp',RDSfiles$name)][1],units='days')<Params$MaxAge) &&
           !isTRUE(RDSfiles$time[grepl('Total_New_Sets \\(temp',RDSfiles$name)][1]<RDSfiles$time[grepl('Total.*Set',RDSfiles$name)][1])) {
          Total_New_Sets <- readRDS(RDSfiles$name[grepl('Total_New_Sets \\(temp',RDSfiles$name)][1])
        }
        if(!exists('Total_Del') &&
           isTRUE(difftime(Sys.time(), RDSfiles$time[grepl('Total_Del \\(temp',RDSfiles$name)][1],units='days')<Params$MaxAge) &&
           !isTRUE(RDSfiles$time[grepl('Total_Del \\(temp',RDSfiles$name)][1]<RDSfiles$time[grepl('Total.*Del',RDSfiles$name)][1])) {
          Total_Del <- readRDS(RDSfiles$name[grepl('Total_Del \\(temp',RDSfiles$name)][1])
        }
        if(!exists('MetaOut') &&
           isTRUE(difftime(Sys.time(), RDSfiles$time[grepl('MetaOut \\(temp',RDSfiles$name)][1],units='days')<Params$MaxAge) &&
           !isTRUE(RDSfiles$time[grepl('MetaOut \\(temp',RDSfiles$name)][1]<RDSfiles$time[grepl('Meta.*Out',RDSfiles$name)][1])) {
          MetaOut <- readRDS(RDSfiles$name[grepl('MetaOut \\(temp',RDSfiles$name)][1])
          if(!exists('Total_New') && any(grepl('Total_Part', RDSfiles$name))) {
            Total_New <- data.frame()
            Total_New_Cache <- data.frame()
            Total_New_Sets <- sapply(Params$subdfs, function(x) {data.frame()}, simplify=F)
            Total_New_Sets_Cache <- sapply(Params$subdfs, function(x) {data.frame()}, simplify=F)
          }
          if(!exists('Total_ID') && any(grepl('Total_ID',RDSfiles$name)) &&
             isTRUE(difftime(Sys.time(), RDSfiles$time[grepl('Total_ID \\(temp',RDSfiles$name)][1],units='days')<Params$MaxAge)) {
            Total_ID <- readRDS(RDSfiles$name[grepl('Total_ID \\(temp',RDSfiles$name)][1])
          }
        }
        resdate <- as.POSIXct(min(newIDs$ts, oldIDs$inNewTs[!(oldIDs$inNew %in% c('Never', 'Assumed', 'Unchanged','Disappeared'))],Sys.time())-1)
        if(is.na(resdate) || !any(oldIDs$thisHarv)) {resdate <- NULL}
        if(Step!='InitOnly') Step <- ifelse('alwaysIDs' %in% Params$Summarize,'showIDsumm','harvnewRecs')
      } else {
        if(Step!='InitOnly') Step <- 'Start'
      }
      if(exists('Errors') && Errors$UpToStep=='MergeRecords') {
        if(!exists('Total_New')) Total_New <- readRDS(RDSfiles$name[grepl('TotalNewNS',RDSfiles$name)][1])
        if(!exists('Total_New_Sets')) Total_New_Sets <- readRDS(RDSfiles$name[grepl('Total_N_Sets',RDSfiles$name)][1])
        if(!exists('Total_Del')) Total_Del <- readRDS(RDSfiles$name[grepl('Total_Del',RDSfiles$name)][1])
        if(!exists('MetaOut')) MetaOut <- readRDS(RDSfiles$name[grepl('Meta_Out',RDSfiles$name)][1])
        if(!exists('Recfiles')) Recfiles <- readRDS(RDSfiles$name[grepl('RecFileList', RDSfiles$name)][1])
        print('Reading finished')
        if(Step!='InitOnly') Step <- 'FinalizeMerge'
      }
      if(exists('Errors') && Errors$UpToStep=='Finalize') {
        if(!exists('NewTotal')) {
          print('Reading new dataset')
          NewTotal <- readRDS(RDSfiles$name[grepl('NewTotal',RDSfiles$name)][1])
        }
        if(!exists('Total_Del')) Total_Del <- readRDS(RDSfiles$name[grepl('Total_N_Del',RDSfiles$name)][1])
        if(!exists('MetaOut')) MetaOut <- readRDS(RDSfiles$name[grepl('Meta_Out',RDSfiles$name)][1])
        if(!exists('Recfiles')) Recfiles <- readRDS(RDSfiles$name[grepl('RecFileList', RDSfiles$name)][1])
        print('Reading finished')
        if(Step!='InitOnly') Step <- 'MakeIDfile'
      }
    }
    rm(RDSfiles)
  }
}
if(Step=='Start') {
  rm(list=ls()[!ls() %in% c(Params$keepvarnames)])
  Errors <- list(count=0, UpToStep='NoneYet')
  Params$reCalcIDs <- T
  Step <- 'harvIDs'
  SubStep <- 'Start'
  print('Not using any old values in environment, but starting harvesting IDs')
}
if(Step=='harvIDs') {
  if (any(!ls() %in% c(Params$keepvarnames))) {
    warning('Environment doesn\'t seem to be what it should be to start harvesting new IDs')
    stop()
  }
  #Kijk eerst of dit of een hervatting is
  IDfiles <- data.frame(full=list.files(Paths$IDXML,full.names = T),stringsAsFactors = F)
  IDfiles$date <- file.mtime(IDfiles$full)
  if(Params$debug) IDfiles <- IDfiles[IDfiles$date<Params$WayBack,]
  IDfiles <- IDfiles[order(IDfiles$date,decreasing = T),]
  if(nrow(IDfiles)>0) {
    lastfiles <- list(read_file(IDfiles$full[1]))
    for (mrfiles in IDfiles$full[-1][IDfiles$date[-1]==IDfiles$date[1]]) {
      lastfiles[[length(lastfiles)+1]] <- read_file(mrfiles)
    }
    i <- regexpr('<resumptionToken>[^<>]*</resumptionToken>',lastfiles)
    endResTokens <- sapply(1:length(lastfiles),function(n) {
      substr(lastfiles[[n]],i[[n]]+17,i[[n]]+attr(i,"match.length")[[n]]-19)
    })
    if (any(i==-1)) {endResTokens[i==-1] <- "Endfile"}
    i <- regexpr('request resumptionToken[^<>]*verb',lastfiles)
    startResTokens <- sapply(1:length(lastfiles),function(n) {
      substr(lastfiles[[n]],i[[n]]+25,i[[n]]+attr(i,"match.length")[[n]]-7)
    })                                                                        #Beperking: werkt niet als eerste file in set zit
    lastResToken <- endResTokens[!(endResTokens %in% startResTokens)]
    if(length(lastResToken)!=1) {
      stop('Multiple non-mathcing endResTokens found. Check fs')
    }
  }
  if (exists('lastResToken')&& nchar(lastResToken)>10) {
    if(Params$harv=='dc') {
      Params$reCalcIDs <- T
      print('Resuming harvesting IDs')
      print(paste0('Already ',nrow(IDfiles),' x ',Params$filesize,' = ',Params$filesize*nrow(IDfiles),' were done before'))
      dump <- list_identifiers(Params$urls$dcoaiurl, as='raw', token=lastResToken, dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$IDXML))
      rm(dump)
      lastResToken <- 'Endfile'
    } else if (Params$harv=='didlmods') {
      Params$reCalcIDs <- T
      print('Resuming harvesting IDs')
      print(paste0('Already ',nrow(IDfiles),' x ',Params$filesize,' = ',Params$filesize*nrow(IDfiles),' were done before'))
      dump <- list_identifiers(Params$urls$gmhoaiurl, prefix = Params$urls$gmhformat, as='raw', token=lastResToken, dumper=dump_raw_to_txt, 
                               dumper_args=list(file_dir=Paths$IDXML))
      rm(dump)
      lastResToken <- 'Endfile'
    } else {
      print("Unclear format")
      stop()
    }
  } else {
    #Verse start
    if(exists('lastResToken')&&lastResToken=='Endfile') {
      print("Alle files zijn opgehaald")
    } else if(exists('lastResToken')&&lastResToken!='') {
      print("Unclear lastResToken")
      stop()
    } else if(Params$harv=='dc') {
      Params$reCalcIDs <- T
      print('Starting harvesting IDs')
      if(is.null(resdate)) resdate <- as.POSIXct('1900-12-31')
      dump <- list_identifiers(Params$urls$dcoaiurl, as='raw', from=format(resdate, '%Y-%m-%dT%H:%M%SZ'),
                               dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$IDXML))
      rm(dump)
      lastResToken <- 'Endfile'
    } else if (Params$harv=='didlmods') {
      Params$reCalcIDs <- T
      print('Starting harvesting IDs')
      if(is.null(resdate)) resdate <- as.POSIXct('1900-12-31')
      dump <- list_identifiers(Params$urls$gmhoaiurl, prefix = Params$urls$gmhformat, as='raw', from=format(resdate, '%Y-%m-%dT%H:%M%SZ'),
                               dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$IDXML))
      rm(dump)
      lastResToken <- 'Endfile'
    } else {
      print("Unclear format")
      stop()
    }
  }
  print ("Harvesting identifiers completed")
  if(Params$reCalcIDs) {Step <- 'ProcIDs'} else if ('IDs' %in% Params$Summarize) {Step <- 'showIDsumm'} else {Step <- 'harvnewRecs'}
  SubStep <- 'Start'
  rm(list=ls()[!ls() %in% c(Params$keepvarnames,'lastResToken')])
}
if(Step=='ProcIDs') {
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'lastResToken',
                           'IDfiles','oldIDs','newIDs', 'bu_IDfiles','bu_oldIDs','n')]
  if (any(!rms %in% c(Params$tempvarnames) & !grepl('temp',rms)) ||
      (exists('lastResToken') && !lastResToken %in% c('Endfile',''))) {
    warning('Environment doesn\'t seem to be what it should be to start processing IDs')
    stop()
  } else rm(list=rms)
  if(SubStep=='ParseIDs' && is.null(Params$Resume$IDParse)) {
    Params$Resume$IDParse <- n
    if(any(apply(IDfiles[1:(Params$Resume$IDParse-1),], 1, function(x) {any(is.na(x[1:12])) || (x['NoDeleted']!=Params$filesize && any(is.na(x[13:16])))}))) {
      print('When resuming parsingIDs, an error was encountered: not all information in IDfiles is filled. Sleeping for 5 seconds, press <ESC> to stop execution')
      Sys.sleep(5)
      Params$Resume$IDParse <- 1
    }
  }
  if(SubStep %in% c('OrderIDs', 'IDChecks','IDsCompWithOld','Assume','SaveFiles','Finished') && any(is.na(IDfiles[1:12]))) {
    print('When resuming processing IDs, an error was encountered: not all information in IDfiles is filled. Sleeping for 5 seconds, press <ESC> to stop execution')
    Sys.sleep(5)
    Params$Resume$IDParse <- 1
    SubStep <- 'Start'
  }
  if(SubStep=='Start' && is.null(Params$Resume$IDParse)) Params$Resume$IDParse <- 1
  if(SubStep=='Start') SubStep <- 'ParseIDs'
  
  if(SubStep=='ParseIDs') {
    if(Params$Resume$IDParse==1) {
      IDfiles <- data.frame(fullname=list.files(Paths$IDXML, full.names=T), row.names = NULL, stringsAsFactors = F)
      IDfiles$name <- gsub('.*/(oaidump.*\\.xml)','\\1',IDfiles$fullname)
      IDfiles$lastmod <- file.mtime(IDfiles$fullname)
      if (any(is.na(IDfiles$lastmod))) {
        print("Not all file-info read well!")
      }
      if(Params$debug) IDfiles <- IDfiles[IDfiles$lastmod<Params$WayBack,]
      IDfiles <- IDfiles[order(IDfiles$lastmod),]
      IDfiles$ReqDelay <- NA
      IDfiles$ReqResTok <- NA
      IDfiles$EndResTok <- NA
      IDfiles$NoRecords <- NA
      IDfiles$NoDeleted <- NA
      IDfiles$FirstID <- NA
      IDfiles$LastID <- NA
      IDfiles$tsf <- as.POSIXct(NA, tz='UTC')    # Timestamp first
      IDfiles$tsl <- as.POSIXct(NA, tz='UTC')    # Timestamp last
      IDfiles$FirstRecID <- NA
      IDfiles$LastRecID <- NA
      IDfiles$tsRecf <- as.POSIXct(NA, tz='UTC')    # Timestamp first non-deleted records
      IDfiles$tsRecl <- as.POSIXct(NA,tz='UTC')    # Timestamp last non-deleted record
      Errors$longdelays <- data.frame(IDfilenr=numeric(),IDfilename=character(),IDfileTS=as.POSIXct(character()),ReqDelay=numeric(), stringsAsFactors = F)
      Errors$MultiIDs <- data.frame(idx=numeric(),ID=character(),LastUpdate=as.POSIXct(character()),inNew=factor())
    } # Initialize
    print(paste0('Parsing ID-records, starting at file ',Params$Resume$IDParse,' (of ',nrow(IDfiles),')'))
    for(n in Params$Resume$IDParse:nrow(IDfiles)) {
      rawfile <- read_file(IDfiles$fullname[n])
      rawlen <- nchar(rawfile)
      # Get some metadata, and check file integrity
      i <- regexpr(pattern = '<responseDate>.*?</responseDate>',text = rawfile)
      IDfiles$ReqDelay[n] <- abs(difftime(IDfiles$lastmod[n], 
                                          as.POSIXct(substr(rawfile,i+14,i+33)
                                                     , format=Params$dateform[1], tz='UTC'), units = 'secs'))
      # Het verschil tussen de HTTP-request en het wegschrijven van het bestand. Rond de paar seconden, max 30.
      i <- regexpr('<request resumptionToken.*?verb',rawfile)
      IDfiles$ReqResTok[n] <- substr(rawfile,i+26, i+attr(i,"match.length")-7)
      i <- regexpr('<resumptionToken>[^<>]*</resumptionToken>',rawfile)
      IDfiles$EndResTok[n] <- substr(rawfile,i+17,i+attr(i,"match.length")-19)
      moreFiles <- (IDfiles$EndResTok[n]!='')
      if (moreFiles&&(length(gregexpr('</header>',rawfile)[[1]])!=Params$filesize)) {
        print(paste("Error: File seems not to have",Params$filesize,"records. ResumptionToken is available,"))
        print(paste("but ListRecords has",length(gregexpr('</header>',rawfile)[[1]]),"records"))
        stop()
      }
      #Extra info in files:
      # Number of records
      # Number deleted
      # FirstID
      # LastID
      IDfiles$NoRecords[n] <- length(gregexpr('</header>',rawfile)[[1]])
      i <- gregexpr(text=rawfile,'status="deleted"')
      IDfiles$NoDeleted[n] <- length(i[[1]])-(i[[1]][[1]]==-1)
      i <- gregexpr('<header[^<>]*><identifier>[^<>]*</identifier>',rawfile)
      IDfiles$FirstID[n] <- substr(rawfile,i[[1]][[1]],i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]-14)
      IDfiles$FirstID[n] <- sub('<header[^<>]*><identifier>','',IDfiles$FirstID[n])
      IDfiles$LastID[n] <- substr(rawfile,
                                  start=i[[1]][[IDfiles$NoRecords[n]]],
                                  stop=i[[1]][[IDfiles$NoRecords[n]]]+attr(i[[1]],"match.length")[[IDfiles$NoRecords[n]]]-14)
      IDfiles$LastID[n] <- sub('<header[^<>]*><identifier>','',IDfiles$LastID[n])
      IDfiles$tsf[n] <- as.POSIXct(substr(rawfile,
                                          start=i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]+11,
                                          stop=i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]+30),
                                   format=Params$dateform[1], tz='UTC')
      IDfiles$tsl[n] <- as.POSIXct(substr(rawfile,
                                          start=i[[1]][[IDfiles$NoRecords[n]]]+attr(i[[1]],"match.length")[[IDfiles$NoRecords[n]]]+11,
                                          stop=i[[1]][[IDfiles$NoRecords[n]]]+attr(i[[1]],"match.length")[[IDfiles$NoRecords[n]]]+30),
                                   format=Params$dateform[1], tz='UTC')
      if(IDfiles$NoDeleted[n]>0 && IDfiles$NoDeleted[n]!=IDfiles$NoRecords[n]) {
        i <- gregexpr('<header><identifier>[^<>]*</identifier>',rawfile)
        IDfiles$FirstRecID[n] <- substr(rawfile,i[[1]][[1]]+20,i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]-14)
        IDfiles$LastRecID[n] <- substr(rawfile, i[[1]][[length(i[[1]])]]+20,
                                       i[[1]][[length(i[[1]])]]+attr(i[[1]],"match.length")[[length(i[[1]])]]-14)
        IDfiles$tsRecf[n] <- as.POSIXct(substr(rawfile,
                                               start=i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]+11,
                                               stop=i[[1]][[1]]+attr(i[[1]],"match.length")[[1]]+30),
                                        format=Params$dateform[1], tz='UTC')
        IDfiles$tsRecl[n] <- as.POSIXct(substr(rawfile,
                                               start=i[[1]][[length(i[[1]])]]+attr(i[[1]],"match.length")[[length(i[[1]])]]+11,
                                               stop=i[[1]][[length(i[[1]])]]+attr(i[[1]],"match.length")[[length(i[[1]])]]+30),
                                        format=Params$dateform[1], tz='UTC')
      } else if (IDfiles$NoDeleted[n]==0) {
        IDfiles$FirstRecID[n] <- IDfiles$FirstID[n]
        IDfiles$LastRecID[n] <- IDfiles$LastID[n]
        IDfiles$tsRecf[n] <- IDfiles$tsf[n]
        IDfiles$tsRecl[n] <- IDfiles$tsl[n]
      }
      if(n%%500==0 || n==nrow(IDfiles)) {
        print(paste('ID-records up to number',n,'parsed'))
      }
    } # Parse records to fill IDfiles
    IDfiles$NoRecords <- IDfiles$NoRecords-IDfiles$NoDeleted
    SubStep <- 'OrderIDs'
  }
  if(SubStep=='OrderIDs') {
    print('Deciding order')
    IDfiles$nr <- NA
    lastResToken <- ''
    n <- 1
    repeat {
      lastchecked <- fmatch(lastResToken, IDfiles$ReqResTok)
      if (length(lastchecked)==0) {
        print("Geen matching ResTok gevonden!")
        print(paste("Gezocht:",IDfiles))
        print(paste("Zou dan nummer",n,"geweest zijn"))
        stop()
      }
      if (length(lastchecked)>1) {
        print("Meer dan één file met matching ResTok!")
        print(paste("Gezocht:",IDfiles))
        print(paste("Zou dan nummer",n,"geweest zijn"))
        print(paste(length(lastchecked),"files, nrs",lastchecked,"in dataframe"))
        stop()
      }
      IDfiles$nr[lastchecked] <- n
      lastResToken <- IDfiles$EndResTok[lastchecked]
      if (lastResToken=='') {
        print(paste0('Endfile found, n=',n))
        break
      }
      if (n%%10000==0) {
        print(paste("Order decided up to",n))
      }
      n <- n+1
    }
    if(any(is.na(IDfiles$nr)|duplicated(IDfiles$nr))) {
      print("Not all IDfiles numbered well")
      stop()
    }
    IDfiles <- IDfiles[order(IDfiles$nr),]
    rownames(IDfiles) <- IDfiles$nr
    if (any(IDfiles$lastmod[1:(nrow(IDfiles)-1)]>IDfiles$lastmod[2:nrow(IDfiles)])) {
      print("Not in order!")
      stop()
    }
    SubStep <- 'IDChecks'
  }
  if(SubStep=='IDChecks') {
    if (any(IDfiles$ReqDelay>30)) {
      print(paste("Warning, there are",sum(IDfiles$ReqDelay>30),"files with long Requestdelay"))
      longdelays <- which(IDfiles$ReqDelay>30)
      print(paste("First 10 are:",longdelays[1:min(10,length(longdelays))]))
      Errors$count <- Errors$count+length(longdelays)
      if(nrow(longdelays)>0) {
        stop('Error in procIDs: longdelays-df already filled!')
      }
      Errors$longdelays <- rbind.fill(Errors$longdelays,data.frame(IDfilenr=IDfiles$nr[longdelays], 
                                                                   IDfilename=IDfiles$name[longdelays],
                                                                   IDfileTS=IDfiles$lastmod[longdelays],
                                                                   ReqDelay=IDfiles$ReqDelay[longdelays],
                                                                   stringsAsFactors = F))
      print("Waiting a bit before continuing (ProcIDs-Checks-ReqDelay>30)")
      Sys.sleep(1)
      print('Continuing')
    }
    # Even opruimen
    suppressWarnings(rm(list=c('rawfile','rawlen','i','n','o','longdelays','lastResToken','lastchecked','moreFiles')))
    SubStep <- 'IDsCompWithOld'
  }
  if(SubStep=='IDsCompWithOld') {
    if(exists('oldIDs') && exists('newIDs') && exists('n', where=.GlobalEnv, inherits=F)) {
      if(!any(oldIDs$thisHarv)) {
        Params$oldfdate <- as.POSIXct('1902-01-01', tz='UTC')
      }
      tempcount <- table(c(oldIDs$inNewNr, newIDs$file))
      Params$Resume$IDComp <- suppressWarnings( 
        which(which(IDfiles$tsl>Params$oldfdate)[1]:nrow(IDfiles)!=
                as.integer(names(tempcount[tempcount==Params$filesize])))[1]) # Note that this fails if old IDs are duplicated
      rm(tempcount)
    } else {
      Params$Resume$IDComp <- 1
    }
    if(Params$Resume$IDComp==1) {
      print('Comparing with old IDs, first initializing')
      oldIDs <- readRDS(Paths$oldID)
      names(oldIDs) <- sub(paste0('^',Params$harv,'(.+)'), '\\1',names(oldIDs),ignore.case = T)
      names(oldIDs)[names(oldIDs)=='LastDCUpdate'] <- 'LastUpdate' # Backward compatibility
      names(oldIDs)[names(oldIDs)==Params$harv] <- 'thisHarv'
      if(is.null(oldIDs$thisHarv)) {
        oldIDs$thisHarv[] <- F
        oldIDs$LastUpdate <- as.POSIXct(rep('1902-01-01', times=nrow(oldIDs)), tz = 'UTC')
        oldIDs$File[] <- NA
        oldIDs$Filename[] <- NA
        Params$oldfdate <- as.POSIXct('1902-01-01', tz='UTC')
      }
      if(F) {
        attr(oldIDs$LastUpdate, 'tzone') <- 'Europe/Amsterdam'
        warning('Code in procIDs/read oldIDs is made for set dated 25-7. Check for date consistentcy otherwise')
      } # Kept for possible debuging dateformats
      oldIDs$inNew <- factor(x = ifelse(oldIDs$thisHarv, 'ToCheck', 'Never'), levels = c('Never','Deleted','Updated','Unchanged','Assumed','Disappeared','ToCheck','New'))  # Order is used when matching with oldIDs
      #if(Params$harv=='dc') {names(oldIDs)[names(oldIDs)=='LastDCUpdate'] <- 'LastUpdate'}
      oldIDs$inNewNr <- NA[nrow(oldIDs)>0]
      oldIDs$LastUpdate <- force_tz(oldIDs$LastUpdate, tz='UTC')[nrow(oldIDs)>0]
      oldIDs$inNewTs <- as.POSIXct(rep('1902-01-01', times=nrow(oldIDs)), tz='UTC')[nrow(oldIDs)>0]
      newIDs <- data.frame(ID=character(), del=logical(), ts=as.POSIXct(character(), tz='UTC'), file=numeric())
      # Opmerking: oldIDs hebben een nr-veld, maar dit zijn alleen de bestaande genummerd. Voor hier kijken we gewoon naar de index (which())
      Sys.setenv(TZ='UTC')
      Params$Resume$IDComp <- which(IDfiles$tsl>Params$oldfdate)[1]
    } else {
      Errors$MultiIDs <- Errors$MultiIDs[Errors$MultiIDs$nr<Params$Resume$IDComp]
      oldIDs$inNew[oldIDs$inNewNr>=Params$Resume$IDComp] <- 'ToCheck'
      oldIDs$inNewNr[oldIDs$inNewNr>=Params$Resume$IDComp] <- NA
      newIDs <- newIDs[newIDs$file<Params$Resume$IDComp,]
    }
    if(!any(oldIDs$thisHarv)) {
      Params$oldfdate <- as.POSIXct('1902-01-01', tz='UTC')
    } else {
      Params$oldfdate <- min(Params$oldfdate, max(oldIDs$LastUpdate[oldIDs$thisHarv]))
    }
    tempCacheVals <- data.frame(idx=integer(10000), inNew=factor(character(10000),levels=levels(oldIDs$inNew)), inNewNr=integer(10000), inNewTs=as.POSIXct(rep('1902-01-01', times=10000), tz='UTC'))  
    tempCacheidx <- 0
    tempcacheNewIDs <- data.frame(ID=character(), del=logical(), ts=as.POSIXct(character(), tz='UTC'), file=numeric())
    print(paste('Filling comparison from file',Params$Resume$IDComp))
    for (n in Params$Resume$IDComp:nrow(IDfiles)) {
      if (sum(tempCacheVals$idx==0)<Params$filesize || nrow(tempcacheNewIDs)>10000) {
        if((n-1)%/%500!=max(1, oldIDs$inNewNr, newIDs$file, na.rm=T)%/%500) { # That is: we have a crossed a border
          print(paste("File",n-1,"parsed"))
        }
        newIDs <- rbind(newIDs, tempcacheNewIDs)
        tempCacheVals <- tempCacheVals[tempCacheVals$idx!=0,]
        oldIDs$inNew[tempCacheVals$idx] <- tempCacheVals$inNew
        oldIDs$inNewNr[tempCacheVals$idx] <- tempCacheVals$inNewNr
        oldIDs$inNewTs[tempCacheVals$idx] <- tempCacheVals$inNewTs
        tempCacheVals <- data.frame(idx=integer(10000), inNew=factor(character(10000),levels=levels(oldIDs$inNew)), inNewNr=integer(10000), inNewTs=as.POSIXct(rep('1902-01-01', times=10000), tz='UTC'))
        tempCacheidx <- 0
        tempcacheNewIDs <- data.frame(ID=character(), del=logical(), ts=as.POSIXct(character(), tz='UTC'), file=numeric())
      }
      rawfile <- read_file(IDfiles$fullname[n])
      rawlen <- nchar(rawfile)
      i <- gregexpr('<identifier>[^<>]*</identifier>',rawfile)
      j <- attr(i[[1]], "match.length")
      i <- as.vector(i[[1]])
      j <- j+i
      fIDs <- data.frame(ID=substring(text=rawfile,
                                      first = i+12,
                                      last = j-14),
                         del = substring(rawfile, i-17, i-2)=='status="deleted"',
                         ts = as.POSIXct(substring(rawfile, j+11, j+30),
                                         format=Params$dateform[1], tz='UTC'),
                         file=n,
                         stringsAsFactors = F)
      if(nrow(oldIDs)>0) {
        tempidx <- fmatch(fIDs$ID, oldIDs$ID)
        tempcacheNewIDs <- rbind(tempcacheNewIDs, fIDs[is.na(tempidx),])
        tempidx <- tempidx[!is.na(tempidx)]
        temp <- oldIDs[tempidx,c('ID','LastUpdate','inNew')]
        temp$idx <- tempidx
        if (any((temp$inNew!='ToCheck' & !(temp$inNew=='Never' & temp$del)) | temp$idx %in% tempCacheVals$idx)) {
          print(paste0('IDs used multiple times in file ',n,', (stored in Errors$MultiIDs), nrs:\n',
                       paste(temp$idx[temp$inNew!='ToCheck'|temp$idx %in% tempCacheVals$idx], collapse=', ')))
          Errors$MultiIDs <- rbind.fill(Errors$MultiIDs, temp[temp$inNew!='ToCheck'|temp$idx %in% tempCacheVals$idx,])
          Errors$count <- Errors$count+sum(temp$inNew!='ToCheck'|temp$idx %in% tempCacheVals$idx)
          #readline(prompt="Press any key to continue")
          Sys.sleep(.2)
        }
        temp <- merge(temp, fIDs)
        temp <- temp[order(temp$idx),]
        if(any(temp$LastUpdate>temp$ts)) {
          print("Error: oldIDs updated later than current harvest!")
          stop()
        }
        temp$inNew[temp$LastUpdate<temp$ts] <- 'Updated'
        temp$inNew[temp$LastUpdate==temp$ts & temp$inNew!='Never'] <- 'Unchanged'
        temp$inNew[temp$del & temp$inNew!='Never'] <- 'Deleted'
        if(any(temp$inNew=='Never' & (!temp$del | temp$ts>temp$LastUpdate))) stop('Previously deleted file is now present again!')
        if(nrow(temp)!=0) {
          tempCacheidx <- (tempCacheidx+1):(tempCacheidx+nrow(temp))
          tempCacheVals$idx[tempCacheidx] <- temp$idx
          tempCacheVals$inNew[tempCacheidx] <- temp$inNew
          tempCacheVals$inNewNr[tempCacheidx] <- n
          tempCacheVals$inNewTs[tempCacheidx] <- temp$ts
          tempCacheidx <- max(tempCacheidx)
        }
      } else {
        tempcacheNewIDs <- rbind(tempcacheNewIDs, fIDs)
      }
    } # Fill oldIDs and newIDs with files from oldfdate
    tempCacheVals <- tempCacheVals[tempCacheVals$idx!=0,]
    oldIDs$inNew[tempCacheVals$idx] <- tempCacheVals$inNew
    oldIDs$inNewNr[tempCacheVals$idx] <- tempCacheVals$inNewNr
    oldIDs$inNewTs[tempCacheVals$idx] <- tempCacheVals$inNewTs
    newIDs <- rbind(newIDs, tempcacheNewIDs)
    rm(list=c('fIDs','temp','tempidx','tempcacheNewIDs','tempCacheVals','rawfile','rawlen','tempCacheidx','i','j','n'))
    bu_IDfiles <- IDfiles
    bu_oldIDs <- oldIDs
    SubStep <- 'Assume'
    print("Now at step ProcIDs, new data is parsed")
  }
  if(SubStep=='Assume') {
    print('Assuming files')
    IDfiles <- bu_IDfiles
    oldIDs <- bu_oldIDs
    
    # Assuming older files: first count values
    tempRecsToCheck <- IDfiles$tsl<=Params$oldfdate & IDfiles$NoRecords>0
    frommatch <- match(IDfiles$FirstRecID[tempRecsToCheck], oldIDs$ID) # Indices of first matches
    tomatch <- match(IDfiles$LastRecID[tempRecsToCheck], oldIDs$ID) # Indices of last matches
    if(any(is.na(frommatch)) || any(is.na(tomatch)) || any(frommatch>tomatch) || any(tomatch[1:length(tomatch)-1]>frommatch[2:length(frommatch)])) {
      stop('Can\'t match IDs from IDfiles and oldIDs properly, check values (procIDs/Assume)')
    } # Basic check
    if(any(oldIDs$LastUpdate[frommatch]!=IDfiles$tsRecf[tempRecsToCheck] | oldIDs$LastUpdate[tomatch]!=IDfiles$tsRecl[tempRecsToCheck])) {
      temperrors <- which(oldIDs$LastUpdate[frommatch]!=IDfiles$tsRecf[tempRecsToCheck] | oldIDs$LastUpdate[tomatch]!=IDfiles$tsRecl[tempRecsToCheck])
      warning(paste0('Non-matching time-stamps found, file(s) ',paste0(which(tempRecsToCheck)[temperrors], collapse=', '),'\n',
                     'These are removed from consideration'))
      tempRecsToCheck[which(tempRecsToCheck)[temperrors]] <- F
      frommatch <- frommatch[-temperrors]
      tomatch <- tomatch[-temperrors]
    } # Check if timestamps match
    matchrange <- mapply(`:`,frommatch, tomatch)
    matchrange <- data.frame(file=factor(rep(which(tempRecsToCheck), sapply(matchrange, length)), levels=which(tempRecsToCheck)),
                             IDnr=unlist(matchrange))
    matchrange$inNew <- oldIDs$inNew[matchrange$IDnr]
    if(any(!matchrange$inNew %in% c('ToCheck','Updated','Deleted'))) {
      temperrors <- matchrange[!matchrange$inNew %in% c('ToCheck','Updated','Deleted'),]
      warning(paste0('IDs with unexpected status found, file(s) ',paste0(temperrors$file, collapse=', '), '\n',
                     'IDnr(s) ', paste0(temperrors$IDnr, collapse=', '), '\n',
                     'Status: ',paste0(temperrors$inNew, collapse=', ')))
      tempRecsToCheck[as.integer(as.character(unique(temperrors$file)))] <- F
      frommatch <- frommatch[!frommatch %in% matchrange[matchrange$file %in% temperrors$file]]
      tomatch <- tomatch[!tomatch %in% matchrange[matchrange$file %in% temperrors$file]]
      matchrange <- matchrange[!matchrange$file %in% temperrors$file]
    }
    
    matchlength <- as.integer(table(matchrange[matchrange$inNew=='ToCheck',]$file))
    extramatchlength <- as.integer(table(matchrange[matchrange$inNew %in% c('Updated','Deleted'),]$file))
    
    if(any(!(matchlength<=IDfiles$NoRecords[tempRecsToCheck] & IDfiles$NoRecords[tempRecsToCheck]<=matchlength+extramatchlength))) {
      temperrors <- matchrange[!(matchlength <= IDfiles$NoRecords[tempRecsToCheck] & 
                                   IDfiles$NoRecords[tempRecsToCheck] <= matchlength+extramatchlength),]
      warning(paste0('Number of records doesn\'t match, file(s) ',paste0(temperrors$file, collapse=', '),'\n',
                     'These are removed from consideration'))
      tempRecsToCheck[as.integer(as.character(unique(temperrors$file)))] <- F
      frommatch <- frommatch[!frommatch %in% matchrange[matchrange$file %in% temperrors$file]]
      tomatch <- tomatch[!tomatch %in% matchrange[matchrange$file %in% temperrors$file]]
      matchrange <- matchrange[!matchrange$file %in% temperrors$file]
    } # Check lengths
    
    # And for the correct counts, write results to oldIDs
    matchrange <- matchrange[oldIDs$inNew[matchrange$IDnr]=='ToCheck',]
    oldIDs$inNew[matchrange$IDnr] <- 'Assumed'
    oldIDs$inNewTs[matchrange$IDnr] <- oldIDs$LastUpdate[matchrange$IDnr]
    oldIDs$inNewNr[matchrange$IDnr] <- matchrange$file
    oldIDs$inNew[c(frommatch, tomatch)] <- 'Unchanged'  
    
    oldIDs$inNewTs[oldIDs$inNewTs==as.POSIXct('1902-01-01', tz='UTC')] <- NA # This was a standard date. If it is still used, this means no replacement was found
    SubStep <- 'SaveFiles'
  }
  # Als deze stap incompleet is moeten problemen eerst worden opgelost
  if(!all(oldIDs$inNew %in% c('Deleted', 'Updated', 'Unchanged', 'Assumed','Never','Disappeared'))) {
    print('Warning: Some IDs seem to simply have disappeared')
    Errors$Disappeared <- oldIDs[!oldIDs$inNew %in% c('Deleted', 'Updated', 'Unchanged', 'Assumed','Never','Disappeared'),]
    oldIDs$inNew[oldIDs$inNew=='ToCheck'] <- 'Disappeared'
  } else {
    Errors$Disappeared <- data.frame()
  }
  if(SubStep=='SaveFiles') {
    print('Saving results')
    saveRDS(oldIDs, paste0(Paths$Summaries,'/oldIDs (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    saveRDS(IDfiles, paste0(Paths$Summaries,'/FileList IDFs (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    saveRDS(newIDs, paste0(Paths$Summaries,'/newIDs (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    if(Errors$count != nrow(Errors$longdelays) + nrow(Errors$MultiIDs)) {
      stop(paste0('Errorcount failure: count=',Errors$count,', longdelays=',nrow(Errors$longdelays),
                  ', MultiIDs=',nrow(Errors$MultiIDs)))
    }
    Errors$UpToStep <- 'procIDs'
    saveRDS(Errors, file=paste0(Paths$Summaries,'/ErrorsUpToProcIDs (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
    SubStep <- 'Finished'
  }
  if(any(!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','IDfiles','bu_IDfiles','bu_oldIDs','n','i','matchlength','extramatchlength',
                      'rms','frommatch','tomatch','matchrange','tempRecsToCheck'))) {
    warning('End of processing IDs: Unexpected variables present')
    stop()
  } else {
    resdate <- as.POSIXct(min(newIDs$ts, oldIDs$inNewTs[!(oldIDs$inNew %in% c('Assumed', 'Unchanged', 'Disappeared','Never'))],
                           ifelse(!all(is.na(oldIDs$LastUpdate)), max(oldIDs$LastUpdate),Sys.time()))-1)
    if(is.na(resdate) || !any(oldIDs$thisHarv)) {resdate <- NULL}
    print(paste('Resuming harvest at date', resdate))
    rm(list=ls()[!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','IDfiles')])
    Step <- ifelse(any(c('IDs','alwaysIDs') %in% Params$Summarize), 'showIDsumm', 'harvnewRecs')
  }
}
if(Step=='showIDsumm') {
  if (any(!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','IDfiles','RDSfiles','cnt','tstamps',
                       'lastchecked','lastResToken','moreFiles','Total_Kept') & !grepl('temp',ls())) ||
      (exists('lastResRoken') && !lastResToken %in% c('Endfile',''))) {
    warning('Environment doesn\'t seem to be what it should be to start showing ID summary')
    stop()
  }
  print('Showing ID-summary')
  newIDs$repo <- substr(newIDs$ID,1,regexpr(':',newIDs$ID))
  oldIDs$repo <- substr(oldIDs$ID,1,regexpr(':',oldIDs$ID))
  if(nrow(oldIDs)>0) {
    cnt <- plyr::count(oldIDs, vars=c('dataset', 'repo','thisHarv','inNew'))
    cnt$inNew[cnt$inNew=='Assumed'] <- 'Unchanged'
    cnt <- plyr::count(cnt, vars=c('dataset', 'repo','thisHarv','inNew'), wt_var='freq')
    cnt <- rbind.fill(cnt,data.frame(plyr::count(newIDs,vars='repo'),inNew='New'))
    (cnt <- cnt[order(cnt$repo),])
  } else {
    cnt <- data.frame(plyr::count(newIDs,vars='repo'),inNew='New')
    cnt$dataset <- NA
  }
  tstamps <- rbind(oldIDs[c('inNewTs','inNew')],data.frame(inNewTs=newIDs$ts, inNew=factor('New', levels=levels(oldIDs$inNew))))
  libinstandload('ggplot2')
  suppressWarnings({
    templimits <- c(as.POSIXct(min(newIDs$ts, 
                                   oldIDs$inNewTs[!(oldIDs$inNew %in% c('Assumed', 'Unchanged', 'Disappeared','Never'))],
                                   Sys.time(),
                                   resdate)-86400), Sys.time())
    tempbins <- 100
    print(ggplot(data=tstamps) +
            geom_histogram(aes(x=inNewTs, fill=inNew), breaks=as.numeric(c(templimits[1]+(0:tempbins*(templimits[2]-templimits[1])/tempbins)))) +
            scale_x_datetime(limits = templimits))
    if(nrow(oldIDs)>1) {
      readline(prompt='Press any key to show next plot')
      tempFirstTs <- min(oldIDs$LastUpdate[oldIDs$LastUpdate>as.POSIXct('1970-01-01')],
                         as.POSIXct('2017-01-01'))
      templimits <- c(as.POSIXct(tempFirstTs-years(1)-days(1)), Sys.time())
      
      print(ggplot(data=oldIDs) +
              geom_histogram(aes(x=as.POSIXct(if_else(LastUpdate>tempFirstTs, LastUpdate, tempFirstTs-years(1))), fill=inNew), 
                             breaks=as.numeric(c(templimits[1]+(0:tempbins*(templimits[2]-templimits[1])/tempbins)))) +
              scale_x_datetime(limits = templimits))
    }
  })
  print(paste('First new:',min(newIDs$ts)))
  print(paste('First changed old record:',min(oldIDs$inNewTs[!(oldIDs$inNew %in% c('Assumed', 'Unchanged', 'Disappeared','Never'))])))
  print(plyr::count(cnt, vars=c('dataset','inNew'), wt_var = 'freq'))
  rm(list=c('cnt','tstamps'))
  Step <- 'harvnewRecs'
}
if(Step=='harvnewRecs') {
  # Check sanity and clean environment
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','Total_Kept','Recfiles','Total_New','Total_New_Cache','Total_ID',
                           'Total_New_Sets','Total_New_Sets_Cache','MetaOut','Total_Del','IDfiles')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords') & !grepl('temp',rms)) ||
      any(!c('resdate','oldIDs','newIDs') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start harvesting new records')
    stop()
  } else rm(list=rms)
  
  #Kijk of dit een hervatting is, zo ja, ga verder
  newRecords <- F
  newfiles <- data.frame(full=list.files(Paths$XML,full.names = T),stringsAsFactors = F)
  newfiles$date <- file.mtime(newfiles$full)
  if(Params$debug) newfiles <- newfiles[newfiles$date<Params$WayBack,]
  newfiles <- newfiles[order(newfiles$date,decreasing = T),]
  if(nrow(newfiles)>0) {
    lastfiles <- list(read_file(newfiles$full[1]))
    for (mrfiles in newfiles$full[-1][newfiles$date[-1]==newfiles$date[1]]) {
      lastfiles[[length(lastfiles)+1]] <- read_file(mrfiles)
    }
    i <- regexpr('<resumptionToken>[^<>]*</resumptionToken>',lastfiles)
    endResTokens <- sapply(1:length(lastfiles),function(n) {
      substr(lastfiles[[n]],i[[n]]+17,i[[n]]+attr(i,"match.length")[[n]]-19)
    })
    if (any(i==-1)) {endResTokens[i==-1] <- "Endfile"}
    i <- regexpr('request resumptionToken[^<>]*verb',lastfiles)
    startResTokens <- sapply(1:length(lastfiles),function(n) {
      substr(lastfiles[[n]],i[[n]]+25,i[[n]]+attr(i,"match.length")[[n]]-7)
    })              #Beperking: werkt niet als eerste file in set zit
    lastResToken <- endResTokens[!(endResTokens %in% startResTokens)]
  }
  if (exists('lastResToken')&&nchar(lastResToken)>10) {
    if(Params$harv=='dc') {
      newRecords <- T
      print(paste0('Resuming harvesting records at ',Sys.time()))
      print(paste0('Expected number of records is ',nrow(newIDs)+sum(!is.na(oldIDs$inNewTs) & oldIDs$inNewTs>resdate)))
      print(paste0('Of which ',sum(oldIDs$inNew %in% c('Deleted'))+sum(newIDs$del),' are deleted (smaller)'))
      print(paste0('And already ',nrow(newfiles),' x ',Params$filesize,' = ',Params$filesize*nrow(newfiles),' were done before (',
                   format(100*Params$filesize*nrow(newfiles)/(nrow(newIDs)+sum(oldIDs$inNew %in% c('Deleted','Updated','New'))), digits=3),'%)'))
      dump <- list_records(Params$urls$dcoaiurl, as='raw', token=lastResToken, dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$XML))
      Params$SetsSet <- unique(c(Params$SetsSet, oai::list_sets(url=Params$urls$dcoaiurl)$setSpec))
      rm(dump)
      lastResToken <- 'Endfile'
    } else if (Params$harv=='didlmods') {
      newRecords <- T
      print(paste0('Resuming harvesting records at ',Sys.time()))
      print(paste0('Expected number of records is ',nrow(newIDs)+sum(!is.na(oldIDs$inNewTs) & oldIDs$inNewTs>resdate)))
      print(paste0('Of which ',sum(oldIDs$inNew %in% c('Deleted'))+sum(newIDs$del),' are deleted (smaller)'))
      print(paste0('And already ',nrow(newfiles),' x ',Params$filesize,' = ',Params$filesize*nrow(newfiles),' were done before (',
                   format(100*Params$filesize*nrow(newfiles)/(nrow(newIDs)+sum(oldIDs$inNew %in% c('Deleted','Updated','New'))), digits=3),'%)'))
      dump <- list_records(Params$urls$gmhoaiurl, prefix = Params$urls$gmhformat, as='raw', token=lastResToken, dumper=dump_raw_to_txt, 
                           dumper_args=list(file_dir=Paths$XML))
      Params$SetsSet <- unique(c(Params$SetsSet, oai::list_sets(url=Params$urls$gmhoaiurl)$setSpec))
      rm(dump)
      lastResToken <- 'Endfile'
    } else {
      print("Unclear format")
      stop()
    }
  } else {
    # Verse start of we zijn juist helemaal klaar
    if(exists('lastResToken')&&lastResToken=='Endfile') {
      print("Alle recordfiles zijn al eerder opgehaald")
    } else if(exists('lastResToken')&&lastResToken!='') {
      print("Unclear lastResToken")
      stop()
    } else if(Params$harv=='dc') {
      newRecords <- T
      print('Starting harvesting records.')
      print(paste0('Expected number of records is ',nrow(newIDs)+sum(!is.na(oldIDs$inNewTs) & oldIDs$inNewTs>resdate)))
      print(paste0('Of which ',sum(oldIDs$inNew %in% c('Deleted'))+sum(newIDs$del),' are deleted (smaller)'))
      if(is.null(resdate)) resdate <- as.POSIXct('1900-12-31')
      dump <- list_records(Params$urls$dcoaiurl, as='raw', from=format(resdate, '%Y-%m-%dT%H:%M:%SZ'),
                           dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$XML))
      Params$SetsSet <- unique(c(Params$SetsSet, oai::list_sets(url=Params$urls$dcoaiurl)$setSpec))
      rm(dump)
      lastResToken <- 'Endfile'
    } else if (Params$harv=='didlmods') {
      newRecords <- T
      print('Starting harvesting records.')
      print(paste0('Expected number of records is ',nrow(newIDs)+sum(!is.na(oldIDs$inNewTs) & oldIDs$inNewTs>resdate)))
      print(paste0('Of which ',sum(oldIDs$inNew %in% c('Deleted'))+sum(newIDs$del),' are deleted (smaller)'))
      if(is.null(resdate)) resdate <- as.POSIXct('1900-12-31')
      dump <- list_records(Params$urls$gmhoaiurl, prefix = Params$urls$gmhformat, as='raw', from=format(resdate, '%Y-%m-%dT%H:%M:%SZ'),
                           dumper=dump_raw_to_txt, dumper_args=list(file_dir=Paths$XML))
      Params$SetsSet <- unique(c(Params$SetsSet, oai::list_sets(url=Params$urls$gmhoaiurl)$setSpec))
      rm(dump)
      lastResToken <- 'Endfile'
    } else {
      print("Unclear format")
      stop()
    }
  }
  print('Harvesting records complete')
  if(!'Records' %in% Params$Summarize) {
    if(!exists('Recfiles')) {
      RDSfiles <- data.frame(name=list.files(path=Paths$Summaries, pattern='\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
      RDSfiles$time <- file.mtime(RDSfiles$name)
      if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
      RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
      RDSfiles <- RDSfiles[grepl('RecFileList',RDSfiles$name),]
      if(nrow(RDSfiles)>0) {
        Recfiles <- readRDS(RDSfiles$name[1])
      } else {
        Params$Summarize <- c(Params$Summarize, 'Records')
      }
      rm(RDSfiles)
    }
  }
  if(!'Records' %in% Params$Summarize && (any(is.na(Recfiles)) || nrow(Recfiles)!=length(list.files(path=Paths$XML)) || 
                                          is.null(Errors$RecSumm) || !Errors$UpToStep %in% c('SummariseRecords','ParseRecords','MergeRecords','MergeTotal','Finalize'))) {
    Params$Summarize <- c(Params$Summarize, 'Records')
  }
  Step <- ifelse(newRecords || ('Records' %in% Params$Summarize), 'SummariseRecords','ParseRecords')
}
if(Step=='SummariseRecords') {
  # Check sanity and clean environment
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','Total_Kept','Recfiles','IDfiles')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords','Total_New',
                      'Total_New_Sets','MetaOut','Total_Del','Total_ID') & !grepl('temp',rms)) ||
      (exists('lastResToken') && !lastResToken %in% c('','Endfile')) ||
      any(!c('oldIDs','newIDs') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start summarizing records')
    stop()
  } else rm(list=rms)
  if(is.null(Params$Resume$RecordSumm)) {Params$Resume$RecordSumm <- 1}
  if(exists('Recfiles') && !is.null(Errors$RecSumm) && nrow(Recfiles)==length(list.files(path=Paths$XML))) {
    Params$Resume$RecordSumm <- min(which(is.na(Recfiles), arr.ind = T, useNames = F)[,1],nrow(Recfiles))
    Params$Resume$RecordSumm <- ((Params$Resume$RecordSumm-1) %/% Params$nfiles) * Params$nfiles + 1
    if(!any(is.na(Recfiles))) {
      Params$Resume$RecordSumm <- nrow(Recfiles)
      if(sum(Recfiles$NoDeleted+Recfiles$NoRecords!=Params$filesize)<2) {
        Recfiles$NoRecords <- Recfiles$NoRecords+Recfiles$NoDeleted
      }
      if(sum(Recfiles$NoRecords!=Params$filesize)>1) {
        warning('Unclear number of records when checking if summary is completed')
        stop()
      }
    }
  }
  if(is.null(Errors$RecSumm)) {
    Params$Resume$RecordSumm <- 1
  } else {
    Errors$RecSumm <- Errors$RecSumm[Errors$RecSumm$fileno<Params$Resume$RecordSumm,]
    Errors$count <- nrow(Errors$longdelays) + nrow(Errors$MultiIDs) + nrow(Errors$RecSumm)
  }
  if(Params$Resume$RecordSumm==1) {
    Recfiles <- data.frame(fullname=list.files(path=Paths$XML, full.names=T), encoding=factor(Paths$Encoding), row.names = NULL, stringsAsFactors = F)
    Recfiles$name <- gsub('.*/(oaidump.*\\.xml)','\\1',Recfiles$fullname)
    Recfiles$lastmod <- file.mtime(Recfiles$fullname)
    if(Params$debug) Recfiles <- Recfiles[Recfiles$lastmod<Params$WayBack,]
    if (sum(is.na(Recfiles$lastmod))>0) {
      print("Not all file-info read well!")
    }
    Recfiles$size <- file.size(Recfiles$fullname)
    Recfiles <- Recfiles[order(Recfiles$lastmod),]
    Recfiles$ReqDelay <- NA
    Recfiles$ReqResTok <- NA
    Recfiles$EndResTok <- NA
    Recfiles$NoRecords <- NA
    Recfiles$NoDeleted <- NA
    Recfiles$FirstID <- NA
    Recfiles$LastID <- NA
    Errors$RecSumm <- data.frame(bad=character(), repl=character(),fileno=integer(), idx=integer())
  }
  
  print(paste0("Starting summarizing: ",nrow(Recfiles)," files total."))
  if(Params$Resume$RecordSumm>1) {
    print(paste('But resuming at file',Params$Resume$RecordSumm))
  }
  for (o in seq(from=Params$Resume$RecordSumm-1, to=nrow(Recfiles)-1, by=Params$nfiles)) {
    tempnto <- min(nrow(Recfiles), Params$nfiles+o)-o
    tempCacheRecs <- Recfiles[1:tempnto+o,]
    for (n in 1:tempnto) {
      rawfile <- read_file(tempCacheRecs$fullname[n], locale = locale(encoding = as.character(tempCacheRecs$encoding[n])))
      rawlen <- nchar(rawfile)
      tempstrcount <- str_count(rawfile, Params$RegExErrors)
      if(any(tempstrcount)>0) {
        tempNewErrors <- data.frame(bad=character(), repl=character(), fileno=integer(), idx=integer())
        if(tempstrcount[1]>0) {
          wrongi <- gregexpr(Params$RegExErrors[1],rawfile)[[1]]
          if (wrongi[[1]]!=-1) {
            lengths <- attr(wrongi,"match.length")
            wrong <- sapply(1:length(wrongi), function(m) {
              substr(rawfile,wrongi[m], wrongi[m]+lengths[m]-1)
            })
            wrongrepl <- rep('',times=length(wrong))
            Errors$count <- Errors$count+length(wrong)
            for(m in 1:length(wrong)) {
              rawfile <- sub(wrong[[m]],wrongrepl[[m]],rawfile, fixed=T)
            }
            wrongi <- wrongi-c(0,cumsum(lengths-nchar(wrongrepl))[1:length(lengths)-1]) # Om een consequente verwijzing naar de index te krijgen, zie documentatie
            tempNewErrors <- rbind.fill(tempNewErrors,data.frame(
              bad=wrong,
              repl=wrongrepl,
              fileno=n+o,
              idx=wrongi+0, # +0 to lose attributes
              class=1)) 
          }
        }
        if(tempstrcount[2]>0) {
          wrongi <- gregexpr(Params$RegExErrors[2],rawfile)[[1]]
          if (wrongi[[1]]!=-1) {
            lengths <- attr(wrongi,"match.length")
            wrong <- sapply(1:length(wrongi), function(m) {
              substr(rawfile,wrongi[m], wrongi[m]+lengths[m]-1)
            })
            wrongrepl <- sapply(wrong, gsub, pattern=Params$RegExErrors[2], replacement='\\1<')
            Errors$count <- Errors$count+length(wrong)
            for(m in 1:length(wrong)) {
              rawfile <- sub(wrong[[m]],wrongrepl[[m]],rawfile, fixed=T)
            }
            wrongi <- wrongi-c(0,cumsum(lengths-nchar(wrongrepl))[1:length(lengths)-1])
            tempNewErrors <- rbind.fill(tempNewErrors,data.frame(
              bad=wrong,
              repl=wrongrepl,
              fileno=n+o,
              idx=wrongi+0,
              class=2))
          }
        }
        if(tempstrcount[3]>0) {
          wrongi <- gregexpr(Params$RegExErrors[3],rawfile)[[1]]
          if (wrongi[[1]]!=-1) {
            lengths <- attr(wrongi,"match.length")
            wrong <- sapply(1:length(wrongi), function(m) {
              substr(rawfile,wrongi[m], wrongi[m]+lengths[m]-1)
            })
            wrongrepl <- rep('',times=length(wrong))
            Errors$count <- Errors$count+length(wrong)
            for(m in 1:length(wrong)) {
              rawfile <- sub(wrong[[m]],wrongrepl[[m]],rawfile, fixed=T)
            }
            wrongi <- wrongi-c(0,cumsum(lengths-nchar(wrongrepl))[1:length(lengths)-1])
            tempNewErrors <- rbind.fill(tempNewErrors,data.frame(
              bad=wrong,
              repl=wrongrepl,
              fileno=n+o,
              idx=wrongi+0,
              class=3))
          }
        }
        if(tempstrcount[4]>0) {
          wrongi <- gregexpr(Params$RegExErrors[4],rawfile)[[1]]
          if (wrongi[[1]]!=-1) {
            lengths <- attr(wrongi,"match.length")
            wrong <- sapply(1:length(wrongi), function(m) {
              substr(rawfile,wrongi[m], wrongi[m]+lengths[m])
            })
            wrongrepl <- sapply(wrong, gsub, pattern='<([^:<>]*)>', replacement='\\1')
            Errors$count <- Errors$count+length(wrong)
            for(m in 1:length(wrong)) {
              rawfile <- sub(wrong[[m]],wrongrepl[[m]],rawfile, fixed=T)
            }
            wrongi <- wrongi-c(0,cumsum(lengths-nchar(wrongrepl))[1:length(lengths)-1])
            tempNewErrors <- rbind.fill(tempNewErrors,data.frame(
              bad=wrong,
              repl=wrongrepl,
              fileno=n+o,
              idx=wrongi+0,
              class=4))
          }
        }
        if(tempstrcount[5]>0) {
          wrongi <- gregexpr(Params$RegExErrors[5],rawfile, perl=T)[[1]]
          if(length(wrongi)!=tempstrcount[5]) stop()
          if (wrongi[[1]]!=-1) {
            lengths <- attr(wrongi,"match.length")
            wrong <- sapply(1:length(wrongi), function(m) {
              substr(rawfile,wrongi[m], wrongi[m]+lengths[m]-1)
            })
            wrongrepl <- sapply(wrong, gsub, pattern=Params$RegExErrors[5], perl=T, replacement='\\1', USE.NAMES=F)
            Errors$count <- Errors$count+length(wrong)
            for(m in 1:length(wrong)) {
              rawfile <- sub(wrong[[m]],wrongrepl[[m]],rawfile, fixed=T)
            }
            wrongi <- wrongi-c(0,cumsum(lengths-nchar(wrongrepl))[1:length(lengths)-1])
            tempNewErrors <- rbind.fill(tempNewErrors,data.frame(
              bad=wrong,
              repl=wrongrepl,
              fileno=n+o,
              idx=wrongi+0,
              class=5))
          }
        }
        Errors$RecSumm <- rbind.fill(Errors$RecSumm, tempNewErrors)
        rm(tempNewErrors)
      }
      # Get some metadata, and check file integrity
      i <- regexpr(pattern = '<responseDate>.*?</responseDate>',text = substring(rawfile,1,1000))
      tempCacheRecs$ReqDelay[n] <- abs(difftime(tempCacheRecs$lastmod[n], 
                                                as.POSIXct(substr(rawfile,i+14,i+33)
                                                           , format=Params$dateform[1], tz='UTC'), units = 'secs'))
      # Het verschil tussen de HTTP-request en het wegschrijven van het bestand. Rond de paar seconden, max 300.
      i <- regexpr('<request resumptionToken.*?verb',substring(rawfile,1,1000))
      tempCacheRecs$ReqResTok[n] <- substr(rawfile,i+26, i+attr(i,"match.length")-7)
      i <- regexpr('<resumptionToken>[^<>]*</resumptionToken>',substring(rawfile,nchar(rawfile)-500,nchar(rawfile)))+nchar(rawfile)-501
      tempCacheRecs$EndResTok[n] <- substr(rawfile,i+17,i+attr(i,"match.length")-19)
      #Extra info in files:
      # Number of records
      # Number deleted
      # FirstID
      # LastID
      # And check if number of records is right
      tempCacheRecs$NoRecords[n] <- str_count(rawfile, '<record>')
      tempCacheRecs$NoDeleted[n] <- str_count(rawfile, 'status="deleted"')
      i <- regexpr('<header[^<>]*><identifier>[^<>]*</identifier>',substring(rawfile,1,1000))
      tempCacheRecs$FirstID[n] <- substr(rawfile,i[[1]],i[[1]]+attr(i,"match.length")[[1]]-14)
      tempCacheRecs$FirstID[n] <- sub('<header[^<>]*><identifier>','',tempCacheRecs$FirstID[n])
      templookbackl <- round(2*nchar(rawfile)/tempCacheRecs$NoRecords[n]+ifelse(Params$harv=='dc',500,1000))
      i <- gregexpr('<header[^<>]*><identifier>[^<>]*</identifier>',substring(rawfile,nchar(rawfile)-templookbackl,nchar(rawfile)))[[1]]
      if(i[1]==-1) {
        i <- gregexpr('<header[^<>]*><identifier>[^<>]*</identifier>',rawfile)[[1]]
        l <- attr(i,'match.length')[length(i)]
        i <- i[length(i)]
      } else {
        l <- attr(i,'match.length')[length(i)]
        i <- i[length(i)]+nchar(rawfile)-(templookbackl+1)
      }
      tempCacheRecs$LastID[n] <- substr(rawfile,
                                        start=i,
                                        stop=i+l-14)
      tempCacheRecs$LastID[n] <- sub('<header[^<>]*><identifier>','',tempCacheRecs$LastID[n])
      moreFiles <- (tempCacheRecs$EndResTok[n]!='')
      if (moreFiles&&(tempCacheRecs$NoRecords[n]!=Params$filesize)) {
        print(paste("Error: File seems not to have",Params$filesize,"records. ResumptionToken is available,"))
        print(paste("but ListRecords has",tempCacheRecs$NoRecords[n],"records"))
        stop()
      }
    }
    Recfiles[1:tempnto+o,] <- tempCacheRecs
    if(((n+o) %%100)==0) {
      print(paste('Files up to',n+o,'summarized'))
    }
  }
  Recfiles$NoRecords <- Recfiles$NoRecords-Recfiles$NoDeleted
  print("This was last file, now checking order...")
  Recfiles$nr <- NA
  lastResToken <- ''
  n <- 1
  
  repeat {
    if(!any(is.na(Recfiles$nr))) {
      lastResToken <- Recfiles$ReqResTok[which.max(Recfiles$nr)]
      n <- nrow(Recfiles)
    }
    lastchecked <- which(Recfiles$ReqResTok==lastResToken)
    if (length(lastchecked)==0) {
      print("Geen matching ResTok gevonden!")
      print(paste("Gezocht:",lastResToken))
      print(paste("Zou dan nummer",n,"geweest zijn"))
      stop()
    }
    if (length(lastchecked)>1) {
      print("Meer dan één file met matching ResTok!")
      print(paste("Gezocht:",lastResToken))
      print(paste("Zou dan nummer",n,"geweest zijn"))
      print(paste(length(lastchecked),"Recfiles, nrs",lastchecked,"in dataframe"))
      stop()
    }
    Recfiles$nr[lastchecked] <- n
    lastResToken <- Recfiles$EndResTok[lastchecked]
    if (lastResToken=='') {
      print(paste0('Endfile found, n=',n))
      break
    }
    if (n%%500==0) {
      print(paste("Order decided up to",n))
    }
    n <- n+1
  }
  if(any(is.na(Recfiles$nr)|duplicated(Recfiles$nr))) {
    print("Not all Recfiles numbered well")
    stop()
  }
  Errors$RecSumm$fileno <- Recfiles$nr[Errors$RecSumm$fileno] # Deze stap is omdat in eerste instantie bij Errors van een andere volgorde werd uitgegaan
  Recfiles <- Recfiles[order(Recfiles$nr),]
  if (any(Recfiles$lastmod[1:(nrow(Recfiles)-1)]>Recfiles$lastmod[2:nrow(Recfiles)])) {
    print("Not in order!")
    stop()
  }
  
  if(ncol(Recfiles)==13) {
    Recfiles <- Recfiles[,c(13,3,1,2,4:12)]
  }
  if(any(Recfiles$ReqDelay>60 | Recfiles$ReqDelay<(-2))) {
    Errors$count <- Errors$count+sum(Recfiles$ReqDelay>60)
    Errors$RecSumm <- rbind(Errors$RecSumm, data.frame(bad='Long ReqDelay',
                                                       idx='NA',
                                                       repl=as.character(Recfiles$ReqDelay[Recfiles$ReqDelay>60]),
                                                       fileno=Recfiles$nr[Recfiles$ReqDelay>60]))
  }
  saveRDS(Recfiles, file=paste0(Paths$Summaries,'/RecFileList (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  Errors$UpToStep <- 'SummariseRecords'
  saveRDS(Errors, file=paste0(Paths$Summaries,'/ErrorsUpToSummariseRecords (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  Step <- 'ParseRecords'
  # Recfiles$parsed <- F # Is dit nodig?
}
if(Step=='ParseRecords') {
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','Total_Kept','Recfiles','Total_New','Total_New_Cache',
                           'Total_New_Sets','Total_New_Sets_Cache','MetaOut','Total_Del','IDfiles','Total_ID')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords') & !grepl('temp',rms)) ||
      (exists('lastResToken') && !lastResToken %in% c('','Endfile')) ||
      !Errors$UpToStep %in% c('SummariseRecords','ParseRecords','MergeRecords','Finalize') ||
      any(!c('oldIDs','newIDs') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start parsing records')
    stop()
  } else rm(list=rms)
  
  if(is.null(Params$Resume$RecordParse)) {
    RDSfiles <- data.frame(name=list.files(path=Paths$Parsed, pattern='.*\\.(rds|RDS)', full.name=F), stringsAsFactors = F)
    if(Params$debug) {
      RDSfiles$time <- file.mtime(paste0(Paths$Parsed,'/',RDSfiles$name))
      RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
      RDSfiles$time <- NULL
    }
    if(nrow(RDSfiles)==0) {
      Params$Resume$RecordParse <- 0
      if(!dir.exists(Paths$Parsed)) {dir.create(Paths$Parsed, recursive=T)}
    } else {
      RDSfiles$time <- file.mtime(paste0(Paths$Parsed,'/',RDSfiles$name))
      if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
      RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
      #Check how far we've come
      Recnrs <- RDSfiles$name[grepl('^RecsUpToFile.*',RDSfiles$name)]
      Recnrs <- as.numeric(substring(Recnrs,13,regexpr('[^A-Za-z0-9]+',Recnrs)-1))
      if(any(is.na(Recnrs))) {
        warning('Filenames not as in template, ParseRecords/Recs')
        stop()
      }
      Recnrs <- Recnrs[order(Recnrs)]
      Recnrs <- Recnrs[!duplicated(Recnrs)]
      n <- c(seq(from=Params$nfiles, to=nrow(Recfiles)-1, by=Params$nfiles),nrow(Recfiles))
      suppressWarnings(Params$Resume$RecordParse <- max(0,n[n==Recnrs]))
      
      #Idem for Deleted
      Recnrs <- RDSfiles$name[grepl('^DeletedRecsUpToFile.*',RDSfiles$name)]
      Recnrs <- as.numeric(substring(Recnrs,20,regexpr('[^A-Za-z0-9]+',Recnrs)-1))
      if(any(is.na(Recnrs))) {
        warning('Filenames not as in template, ParseRecords/Dels')
        stop()
      }
      Recnrs <- Recnrs[order(Recnrs)]
      Recnrs <- Recnrs[!duplicated(Recnrs)]
      n <- c(seq(from=Params$nfiles, to=nrow(Recfiles)-1, by=Params$nfiles),nrow(Recfiles))
      suppressWarnings(Params$Resume$RecordParse <- min(Params$Resume$RecordParse, max(0,n[n==Recnrs]), na.rm = T))
      
      # and for errors of ParseRecords
      Recnrs <- RDSfiles$name[grepl('^ErrorsUpToFile.*',RDSfiles$name)]
      Recnrs <- as.numeric(substring(Recnrs,15,regexpr('[^A-Za-z0-9]+',Recnrs)-1))
      if(any(is.na(Recnrs))) {
        warning('Filenames not as in template, ParseRecords/Errors')
        stop()
      }
      Params$Resume$RecordParse <- min(Params$Resume$RecordParse, 
                                      max(Recnrs[Recnrs<=max(Params$Resume$RecordParse,0)],0), na.rm = T)
    }
    if(Params$Resume$RecordParse>0) {
      Errors$RecParse <- readRDS(paste0(Paths$Parsed,'/',
                                        RDSfiles$name[grepl(paste0('^ErrorsUpToFile[^0-9]*0*',Params$Resume$RecordParse,'[^0-9]+'),
                                                            RDSfiles$name)][1]))
      Errors$count <- nrow(Errors$longdelays) + nrow(Errors$MultiIDs) + nrow(Errors$RecSumm) + nrow(Errors$RecParse)
    } else {
      Errors$RecParse <- data.frame(bad=character(), repl=character(),fileno=integer(), recno=integer())
      Errors$count <- nrow(Errors$longdelays) + nrow(Errors$MultiIDs) + nrow(Errors$RecSumm)
    }
    Params$Resume$RecordParse <- Params$Resume$RecordParse+1
    suppressWarnings(rm(list=c('RDSfiles','Recnrs')))
  }
  if(Params$Resume$RecordParse>nrow(Recfiles) && !Errors$UpToStep %in% c('ParseRecords','MergeRecords','MergeTotal','Finalize')) {
    tempans <- readline(prompt='Errorfile not up to date, but all files are parsed. Skip errorfile (Y/N)? ')
    
    if(tempans %in% c('N','n')) {
      Params$Resume$RecordParse <- 1
    } else if (!tempans %in% c('Y','y')) {
      print('Invalid input, probably running file. Sleeping for 15 seconds, then continuing as if pressed N')
      Sys.sleep(15)
      Params$Resume$RecordParse <- 1
    }
  }
  
  #Start Parsing
  if(!(Params$Resume$RecordParse==nrow(Recfiles)+1)) {
    print(paste0('Start parsing records (',Params$Resume$RecordParse,'-',nrow(Recfiles),')'))
    for (o in seq(from=Params$Resume$RecordParse, to=min(nrow(Recfiles),1e6), by=Params$nfiles)) {
      #profvis({
      tempbroken <- F
      Recdf <- data.frame(header.identifier=character())
      Deleteddf <- data.frame(identifier=character())
      for (n in o:(min(nrow(Recfiles), Params$nfiles+o-1))) {
        rawfile <- read_file(Recfiles$fullname[n], locale = locale(encoding = as.character(Recfiles$encoding[n])))
        CorrErrors <- Errors$RecSumm[Errors$RecSumm$fileno==n,]
        for(m in seq(length.out=nrow(CorrErrors))) {
          if(regexpr(as.character(CorrErrors$bad[m]), rawfile, fixed=T)==CorrErrors$idx[m]) {
            rawfile <- sub(as.character(CorrErrors$bad[m]), CorrErrors$repl[m], rawfile, fixed=T)
          } else {
            print('Errorindex doesn\'t match!')
            warning('Errorindex doesn\'t match!')
            tempbroken <- T
            break
          }
        }
        rawfile <- read_xml(rawfile)
        parsed <- xmlParse(rawfile)
        ch <- xmlChildren(parsed)[[1]]
        pRecs <- xmlChildren(ch[[3]])
        # Get some metadata, and check file integrity
        if(!all(names(ch)==Params$Reclsnames)) {
          print("Error, file seems to be corrupt")
          warning("Error, file seems to be corrupt")
          tempbroken <- T
          break
        }
        moreFiles <- (nchar(Recfiles$EndResTok[n])>5) 
        if((n!=1)&&(Recfiles$ReqResTok[n]!=Recfiles$EndResTok[n-1])) {
          print("Error: Resumptiontokens don't match!")
          print(paste("File number:",n))
          print(paste("Expected:",Recfiles$EndResTok[n-1]))
          print(paste("Got:",Recfiles$ReqResTok[n]))
          if(n!=Params$Resume$RecordParse) {
            warning("Error: Resumptiontokens don't match!")
            tempbroken <- T
            break
          }
          print('But continuing first file')
        }
        if(xmlAttrs(ch[[2]])['verb']!='ListRecords') {
          print("Error: List is not a ListRecords reuqest")
          print(paste("Instead, verb=",xmlAttrs(ch[[2]])['verb']))
          warning("Error: List is not a ListRecords reuqest")
          tempbroken <- T
          break
        }
        if (!moreFiles&&(n!=nrow(Recfiles))) {
          print("Error: File seems endfile, but is not last in line")
          print(paste("File number", n, "out of",nrow(Recfiles)))
          print(paste("File name:",Recfiles$name[n],"last modified:",Recfiles$lastmod[n]))
          print(paste("File has no resumptiontoken, ListRecords has",length(ls$ListRecords),"items"))
          stop()
        }
        if (moreFiles&&(length(pRecs)!=Params$filesize+1)) {
          print("Error: File seems not to have Parmas$filesize records. ResumptionToken is available,")
          print(paste("but ListRecords has",length(pRecs),"elements"))
          warning("Error: File seems not to have Parmas$filesize records. ResumptionToken is available,")
          tempbroken <- T
          break
        }
        #Now to dataframe
        Recs <- lapply(pRecs[1:(length(pRecs)-1)],xmlChildren)
        headatt <- lapply(Recs, function(x) {xmlAttrs(x[[1]])})
        NonDel <- sapply(headatt, length)==0
        if(any(sapply(Recs[!NonDel], length)!=1) ||
           any(sapply(headatt[!NonDel], length)!=1) ||
           any(sapply(headatt[!NonDel], names)!='status') ||
           any(headatt[!NonDel]!='deleted')) {
          print("Error: Record is onduidelijk wat status is")
          warning("Error: Record is onduidelijk wat status is")
          tempbroken <- T
          break
        } else {
          OneDeldf <- data.frame(do.call(rbind.fill,lapply(Recs[!NonDel], function(x) {
            data.frame(lapply(xmlChildren(x[['header']]), xmlValue),stringsAsFactors = F)
          })))
          OneDeldf[grepl('setSpec', names(OneDeldf))] <- lapply(OneDeldf[grepl('setSpec', names(OneDeldf))], function(x) {as.factor(x)})
          if(nrow(OneDeldf)>0) {OneDeldf$filenr <- n}
        }
        if(Params$harv=='dc' && any(NonDel)) {
          orig <- lapply(Recs[NonDel], function(x) {xmlChildren(x[['about']][['provenance']])[[1]]})
          OneRecdf <- cbind(
            header=data.frame(do.call(rbind.fill,lapply(Recs[NonDel], function(x) {
              data.frame(lapply(xmlChildren(x[['header']]), xmlValue),stringsAsFactors = F)
            }))),
            meta=data.frame(do.call(rbind.fill,lapply(Recs[NonDel], function(x) {
              data.frame(lapply(xmlChildren(x[['metadata']]['dc'][[1]]),xmlValue),stringsAsFactors = F)
            }))),
            about.origin=data.frame(do.call(rbind.fill,lapply(orig, function(x) {
              data.frame(lapply(xmlAttrs(x), unlist),stringsAsFactors = F)
            }))),
            about=data.frame(do.call(rbind.fill,lapply(orig, function(x) {
              data.frame(lapply(xmlChildren(x), xmlValue), stringsAsFactors = F)
            })))
          )
          OneRecdf$filenr <- n
          
          if(ncol(OneRecdf)>100) {
            namen <- names(OneRecdf)
            longnames <- namen[grep('.*[0-9]{3,4}',namen)]
            while (length(longnames)>0) {
              commonname <- str_extract(longnames[1],'.*[a-z]\\.')
              longnames <- longnames[grep(commonname,longnames,fixed=T)]
              OneRecdf[,paste0(commonname,'many')] <- NA
              temprows <- !is.na(OneRecdf[,paste0(commonname,'99')])
              OneRecdf[temprows,paste0(commonname,'many')] <- apply(OneRecdf[temprows,longnames, drop=F],1, function(x) {
                paste(
                  sapply(x[!is.na(x)],as.character),
                  collapse = ';')
              })
              OneRecdf[,paste0(commonname,'many')][OneRecdf[,paste0(commonname,'many')]==''] <- NA
              OneRecdf <- OneRecdf[,!names(OneRecdf) %in% longnames]
              namen <- names(OneRecdf)
              longnames <- namen[grep('.*[0-9]{3,4}',namen)]
            }
          }
        } else if (Params$harv=='didlmods' && any(NonDel)) {
          Recs <- Recs[NonDel]
          # Check structure
          if(!all(sapply(Recs, function(x) {
            length(x)==3 && all(names(x)==c('header','metadata','about')) &&
              xmlSize(x[[2]])==1 && xmlSize(x[[2]][['nl_didl_combined']])==2 &&
              names(x[[2]][[1]])==c('nl_didl', 'nl_didl_norm') && xmlSize(x[[2]][[1]][['nl_didl']])==1 && 
              (xmlSize(x[[2]][[1]][[1]][['DIDL']])==1  || length(names(x[[2]][[1]][[1]][['DIDL']])[names(x[[2]][[1]][[1]][['DIDL']])!='comment'])==1) &&
              xmlSize(x[[2]][[1]][[1]][[1]][['Item']])>1 &&
              xmlSize(x[[2]][[1]][['nl_didl_norm']])==1 &&
              (xmlSize(x[[2]][[1]][[2]][['DIDL']])==1  || length(names(x[[2]][[1]][[2]][['DIDL']])[names(x[[2]][[1]][[2]][['DIDL']])!='comment'])==1)
          }))) {stop('Unexpected didl-mods format')}
          orig <- lapply(Recs, function(x) {xmlChildren(x[['about']][['provenance']])[[1]]})
          OneRecdf <- cbind(
            header=data.frame(do.call(rbind.fill,lapply(Recs, function(x) {
              data.frame(lapply(xmlChildren(x[['header']]), xmlValue),stringsAsFactors = F)
            }))),
            ID=sapply(Recs, function(x) {
              Attrs <- xmlAttrs(x[['metadata']][['nl_didl_combined']][['nl_didl']][['DIDL']])
              if('DIDLDocumentId' %in% names(Attrs)) {
                ID <- Attrs[['DIDLDocumentId']]
                Attrs <- xmlAttrs(x[['metadata']][['nl_didl_combined']][['nl_didl_norm']][['DIDL']])
                if('DIDLDocumentId' %in% names(Attrs) && ID!=Attrs[['DIDLDocumentId']]) stop('Differing IDs in nl_didl and nl_didl_norm')
              } else {
                Attrs <- xmlAttrs(x[['metadata']][['nl_didl_combined']][['nl_didl_norm']][['DIDL']])
                if('DIDLDocumentId' %in% names(Attrs)) {
                  ID <- Attrs[['DIDLDocumentId']]
                } else {
                  ID <- NA
                }
              }
              return(ID)
            }),
            nldidl=I(lapply(Recs, function(x) {
              xmlToList(x[[2]][[1]][[1]][[1]][['Item']])
            })),
            norm=I(simple_rapply(
              lapply(Recs, function(x) {
                xmlToList(x[[2]][[1]][[2]][[1]][['Item']])
              }),
              function(x) {
                if(is.null(x)) {
                  return(NA)
                } else {
                  return(x)
                }
              })),
            about.origin=data.frame(do.call(rbind.fill,lapply(orig, function(x) {
              data.frame(lapply(xmlAttrs(x), unlist),stringsAsFactors = F)
            }))),
            about=data.frame(do.call(rbind.fill,lapply(orig, function(x) {
              data.frame(lapply(xmlChildren(x), xmlValue), stringsAsFactors = F)
            }))),
            stringsAsFactors=F
          )
          ul <- lapply(OneRecdf$norm, unlist)
          if(Params$debug) stop('Debug-DAI')
          OneRecdf$DAI <- lapply(OneRecdf$norm, function(x) {
            auts <- x$Item$Component$Resource$mods
            auts <- auts[names(auts)=='name']
            return(lapply(auts, function(a) {
              DAIs <- sapply(a[names(a)=='nameIdentifier'], function(id) {
                if('info:eu-repo/dai/nl' %in% id$.attrs) {
                  return(id$text)
                } else {
                  return(NA)
                }
              })
              DAIs <- DAIs[!is.na(DAIs)]
              if(length(DAIs)==0) return(NA) else return(DAIs)
            }))
            
            
            idcs <- which(x=='dai-nl')-1
            idcs <- idcs-ifelse(x[idcs+2]=='info:eu-repo/dai/nl',0,1)
            namech <- names(x[rep(idcs, each=3)+0:2])
            if(length(namech)>0) {
              temporder <- c(sapply(3*1:(length(namech)/3), function(i) {
                order(namech[(i-2):i])[c(3,1,2)]
              })) + rep(idcs, each=3)-1
              x <- x[temporder]
              idcs <- 1:length(idcs)*3-2
              namech <- names(x[rep(idcs, each=3)+0:2])
              namech <- substring(namech,c(34,56,56),99999)
            } # Putting individual labelnames in alphabetical order, and discarding non-dai info
            if(any(namech!=c('.nameIdentifier.text','.type','.typeURI')) ||
               any(x[rep(idcs, each=2)+1:2]!=c('dai-nl', 'info:eu-repo/dai/nl'))) {
              if(any(namech!=c('.nameIdentifier.text','.type','.typeURI'))) {
                tempWhichError <- (which(namech!=c('.nameIdentifier.text','.type','.typeURI')) %/% 3) +1
                tempError <- paste0('DAIError: Name is ',
                                    namech[namech!=c('.nameIdentifier.text','.type','.typeURI')],
                                    ' instead of ',
                                    rep(c('.nameIdentifier.text','.type','.typeURI'), length.out=length(namech))[namech!=c('.nameIdentifier.text','.type','.typeURI')],
                                    ' in record with ID ',
                                    x[1])
              } else {
                tempError <- NULL
                tempWhichError <- NULL
              }
              if(any(x[rep(idcs, each=2)+1:2]!=c('dai-nl', 'info:eu-repo/dai/nl'))) {
                tempWhichError <- c(tempWhichError, (which(x[rep(idcs, each=2)+1:2]!=c('dai-nl', 'info:eu-repo/dai/nl')) %/% 2) +1)
                tempError <- c(tempError, paste0('DAIError: Content is ',
                                    x[rep(idcs, each=2)+1:2][x[rep(idcs, each=2)+1:2]!=c('dai-nl', 'info:eu-repo/dai/nl')],
                                    ' instead of ',
                                    c('dai-nl', 'info:eu-repo/dai/nl')[rep(idcs, each=2)+1:2][x[rep(idcs, each=2)+1:2]!=c('dai-nl', 'info:eu-repo/dai/nl')],
                                    ' in record with ID ',
                                    x[1]))
              }
              idcs <- idcs[-tempWhichError]
              print(paste('Errors in looking for DAIs in file',n))
              Errors$RecParse <<- rbind.fill(Errors$RecParse, data.frame(
                bad=tempError,
                repl='removed from DAI-list',
                fileno=n,
                recNo=which(sapply(ul, function(y) {y['Descriptor.Statement.Identifier']})==x['Descriptor.Statement.Identifier'])))
              rm(tempWhichError)
              rm(tempError)
            }
            dai <- as.character(x[idcs])
            if(any(is.na(dai))) stop("DAI can't be converted to character")
            return(dai)
          })
          OneRecdf$Journal <- lapply(OneRecdf$norm, function(x) {
            rels <- which(names(x[['Item']][['Component']][['Resource']][['mods']])=='relatedItem')
            rels <- x[['Item']][['Component']][['Resource']][['mods']][rels]
            hosts <- rels[vapply(rels, function(r) {
              all(r[['.attrs']][names(r[['.attrs']])=='type']=='host')
            }, logical(1))]
          })
          OneRecdf$Subject <- lapply(OneRecdf$norm, function(x) {
            x[['Item']][['Component']][['Resource']][['mods']][['subject']]
          })
          # OneRecdf$Type <- lapply(OneRecdf$norm, function(x) { ----
          #   x[['Item']][['Component']][['Resource']][['mods']][['genre']]
          # })
          OneRecdf$filenr <- n
        } else {
          OneRecdf <- data.frame()
        }
        if(nrow(OneRecdf)!=Recfiles$NoRecords[n] || nrow(OneDeldf)!=Recfiles$NoDeleted[n]) {
          print('Number of records error')
          warning('Number of records error')
          tempbroken <- T
          break
        }
        Recdf <- rbind.fill(Recdf, OneRecdf)
        Deleteddf <- rbind.fill(Deleteddf, OneDeldf)
        if(n%%10==0) {
          print(paste0("File number ",n," read (",Recfiles$name[n],"), now ",ncol(Recdf)," variables. Time=",format(Sys.time(),'%H:%M:%S')))
        }
      }  # End of loop over files to fill Recdf
      if(!tempbroken) {
        saveRDS(Recdf, file=paste0(Paths$Parsed,'/RecsUpToFile',formatC(n,flag='00000', width=5),' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
        saveRDS(Deleteddf, file=paste0(Paths$Parsed,'/DeletedRecsUpToFile',formatC(n,flag='00000', width=5),' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
        saveRDS(Errors$RecParse, file=paste0(Paths$Parsed,'/ErrorsUpToFile',formatC(n,flag='00000', width=5),' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
      }
      #}) # Closing profvis
    }
    print('Parsing complete')
    
    if(nrow(Errors$RecParse)>0) {
      Errors$SummaryOfRecParse <- data.frame(type=as.character(NA), subtype=as.character(NA), bad=unique(Errors$RecParse$bad), count=as.numeric(NA), stringsAsFactors = F)
      Errors$SummaryOfRecParse$bad <- as.character(Errors$SummaryOfRecParse$bad)
      Errors$SummaryOfRecParse$type[grepl('<U\\+[0-9A-F]{4}>',Errors$SummaryOfRecParse$bad)] <- 'UTF8-fout'
      Errors$SummaryOfRecParse$type[grepl('<(dc:.*)>([^<>]*<[^<>:]+>)+[^<>]*</\\1>',Errors$SummaryOfRecParse$bad)] <- 'Halve tag binnen tag'
      Errors$SummaryOfRecParse$type[grepl('<[^>]*$',Errors$SummaryOfRecParse$bad)] <- 'Vishaken zonder tag'
      Errors$SummaryOfRecParse$type[grepl('<(dc:[A-Za-z0-9]*)></\\1>',Errors$SummaryOfRecParse$bad)] <- 'Lege tag'
      Errors$SummaryOfRecParse$type[grepl('DAIError:',Errors$SummaryOfRecParse$bad)] <- 'DAI'
      Errors$SummaryOfRecParse$subtype[Errors$SummaryOfRecParse$type=='DAI'] <- gsub(' in record with ID.*$','',Errors$SummaryOfRecParse$bad)
      Errors$SummaryOfRecParse$count <- sapply(Errors$SummaryOfRecParse$bad, function(x) {sum(x==Errors$RecParse$bad)})
    } else {
      Errors$SummaryOfRecParse <- data.frame(type=character(), subtype=factor(), count=integer(), stringsAsFactors = F)
    }
    Errors$UpToStep <- 'ParseRecords'
    saveRDS(Errors, file=paste0(Paths$Summaries,'/ErrorsUpToParseRecords (temp) (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  }
  Step <- 'MergeRecords'
}
if(Step=='MergeRecords') {
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'oldIDs','newIDs','Total_Kept','Recfiles','Total_New','Total_New_Cache',
                           'Total_New_Sets','Total_New_Sets_Cache','MetaOut','Total_Del', 'IDfiles','Total_ID')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords') & !grepl('temp',rms)) ||
      (exists('lastResToken') && !lastResToken %in% c('','Endfile')) ||
      !Errors$UpToStep %in% c('ParseRecords','MergeRecords','Finalize') ||
      any(!c('oldIDs','newIDs') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start merging records')
    stop()
  } else rm(list=rms)
  
  print('Start merging step, first reading file info')
  {
    if(Params$harv=='didlmods') {
      mongo <- OpenMongo('NARCIS', dockername = Params$mongoColl, imagename='mongo', preOnly = T, quiet=T, port=Params$Mongoport)
    }
    
    ParsRecfiles <- data.frame(fullname=list.files(path=Paths$Parsed, pattern='^Rec.*', recursive = T,full.names = T),stringsAsFactors = F)
    ParsRecfiles$name <- gsub('.*/','',ParsRecfiles$fullname)
    ParsRecfiles$lastmod <- file.mtime(ParsRecfiles$fullname)
    if(Params$debug) ParsRecfiles <- ParsRecfiles[ParsRecfiles$lastmod<Params$WayBack,]
    ParsRecfiles <- ParsRecfiles[order(ParsRecfiles$lastmod),]
    ParsRecfiles$version <- (ParsRecfiles$lastmod > as.POSIXct('2099-12-30 10:30:00'))+1
    
    ParsDelfiles <- data.frame(fullname=list.files(path=Paths$Parsed, pattern='^Deleted.*', recursive = T,full.names = T),stringsAsFactors = F)
    ParsDelfiles$name <- gsub('.*/','',ParsDelfiles$fullname)
    ParsDelfiles$lastmod <- file.mtime(ParsDelfiles$fullname)
    if(Params$debug) ParsDelfiles <- ParsDelfiles[ParsDelfiles$lastmod<Params$WayBack,]
    ParsDelfiles <- ParsDelfiles[order(ParsDelfiles$lastmod),]
    ParsDelfiles$version <- (ParsDelfiles$lastmod > as.POSIXct('2099-12-30 10:30:00'))+1
    
    fileborders <- str_extract(as.character(ParsRecfiles$name),'File\\s*[0-9]*')
    if(!all(fileborders==str_extract(as.character(ParsDelfiles$name),'File\\s*[0-9]*'))||
       !all(ParsRecfiles$version==ParsDelfiles$version)){
      print("ParsRecfiles and ParsDelfiles mismatch, check first")
      stop()
    }
    fileborders <- as.numeric(str_extract(fileborders,'[0-9]+'))
    ParsRecfiles <- ParsRecfiles[!duplicated(fileborders, fromLast = T),]
    ParsDelfiles <- ParsDelfiles[!duplicated(fileborders, fromLast = T),]
    fileborders <- fileborders[!duplicated(fileborders, fromLast = T)]
    ParsRecfiles <- ParsRecfiles[order(fileborders),]
    ParsDelfiles <- ParsDelfiles[order(fileborders),]
    fileborders <- c(0,fileborders[order(fileborders)])
    ParsRecfiles$from <- ParsDelfiles$from <- fileborders[1:length(fileborders)-1]+1
    ParsRecfiles$to <- ParsDelfiles$to <- fileborders[2:length(fileborders)]
    
    row.names(ParsRecfiles) <- 1:nrow(ParsRecfiles)
    row.names(ParsDelfiles) <- 1:nrow(ParsDelfiles)
  }
  if(is.null(Params$Resume$RecordMerge)) {
    if(exists('Total_New') && !is.null(Errors$skippedFiles) && !is.null(Errors$RecMerge)) {
      #Het zou zo moeten zijn dat Total_new totaal aantal regels gelijk is aan een aantal Records in Recfiles[1:50X]
      print('Start calculating how far we\'ve already come')
      CummTotals <- cumsum(Recfiles$NoRecords[-c(Errors$skippedFiles,999999)])
      print('Calculating complete, filling dfs')
      if(MetaOut$NoRecords==CummTotals[fileborders[MetaOut$MergedUpTo+1]] && 
         ParsRecfiles$to[MetaOut$MergedUpTo] %in% Recfiles$nr[-c(Errors$skippedFiles,999999)][CummTotals==MetaOut$NoRecords]) {
        Params$Resume$RecordMerge <- ParsRecfiles$to[MetaOut$MergedUpTo]
      }
      if(is.null(Params$Resume$RecordMerge) || is.na(Params$Resume$RecordMerge) || !Params$Resume$RecordMerge %in% fileborders || MetaOut$NoRecords==0) {
        readline('Restarting. Continue?')
        Params$Resume$RecordMerge <- 1
      } else {
        Params$Resume$RecordMerge <- which(ParsRecfiles$to==Params$Resume$RecordMerge) + 1
      }
    } else {
      Params$Resume$RecordMerge <- 1
    }
  }
  if(Params$harv=='didlmods') {
    mongo <- OpenMongo('NARCIS', dockername = Params$mongoColl, quiet=T, port=Params$Mongoport)
    if(F) mlite <- mongo(collection=Params$mongoColl, db='NARCIS', url=paste0('mongodb://',Params$MongoUser,':',Params$MongoPswd,'@localhost:',Params$Mongoport))
    if(T) mlite <- mongo(collection=Params$mongoColl, db='NARCIS', url=paste0('mongodb://localhost:',Params$Mongoport))
    print('Connected to MongoDB')
  } # Connect to Mongo
  if(Params$Resume$RecordMerge!=1) {
    print('Pruning R-dataframes of extra records')
    if(nrow(Total_New)+MetaOut$NoSaved>MetaOut$NoRecords) Total_New <- Total_New[1:(MetaOut$NoRecords-MetaOut$NoSaved),]
    if(length(Total_ID)>MetaOut$NoRecords) Total_ID <- Total_ID[1:MetaOut$NoRecords]
    Total_New_Sets <- sapply(Params$subdfs, function (x) {
      if(nrow(Total_New_Sets[[x]])+MetaOut$NoSaved!=MetaOut$NoRecords) {
        Total_New_Sets[[x]][1:(MetaOut$NoRecords-MetaOut$NoSaved),]
      } else {
        Total_New_Sets[[x]]
      }
    })
    Total_Del <- Total_Del[Total_Del$filenr<ParsRecfiles$from[Params$Resume$RecordMerge],]
    Total_New_Sets_Cache <- sapply(Params$subdfs, function(x) {data.frame()}, simplify=F)
    Total_New_Cache <- data.frame()
    MetaOut$NoAltered <- MetaOut$NoAltered[1:(Params$Resume$RecordMerge-1),]
    if(Params$harv=='didlmods' && mlite$count()>MetaOut$NoRecords) {
      print('Removing extra records from mongo')
      tempmonqry <- simplify2array(mlite$find(fields='{"_id": 0, "ID":true}', skip=0, limit=2e9)$ID)
      if(length(tempmonqry)>MetaOut$NoRecords+50000) {
        temp <- readline(prompt=paste0('Warning: About to remove a lot of rows (',
                                       ifelse(nrow(tempmonqry)>2e5-1, '200000 or more', nrow(tempmonqry)),
                                       '). Continue (y/n)?'))
        if(temp!='y') stop('Operation aborted (cleaning DB)')
      }
      print('Querying mongo complete')
      temprmv <- !(tempmonqry %in% Total_ID[1:(MetaOut$NoRecords)])
      if(any(temprmv[1:MetaOut$NoRecords])) {
        stop('Unclear what records should be deleted from DB')
      } else if(length(temprmv)>MetaOut$NoRecords && 
          !all(temprmv[(MetaOut$NoRecords+1):length(temprmv)])) {
        print('Complication: there are doubled records. Doing careful removal.')
        tempdbldate <- min(Recfiles$lastmod-120)
        tempmonqry <- mlite$find(fields='{"_id": true, "ID":true, "date_harv":true}',
                     skip=MetaOut$NoRecords, limit=2e9)
        tempmonqry$date_harv <- sapply(tempmonqry$date_harv, as.POSIXct, tz='UTC')
        if(any(tempmonqry$date_harv<tempdbldate)) stop('Double records from before first datestamp')
        if(any(sapply(1:(((nrow(tempmonqry)-1) %/% 1000)+1), function(n) {
          # This is more unreadable, because we can't just specify a value: it needs an ObjectId. Example of a correct query:
          # {_id: {$in: [{'$oid': '5a699eaa77c858ea1a3f67ed'}, {'$oid': '5a699eaa77c858ea1a3f67ee'}]}}
          dbRemoveQuery(mongo, Params$mongoColl, paste0('{_id: {$in: [{\'$oid\': \'',paste0(
            tempmonqry$`_id`[(1000*(n-1)+1):min(1000*n, nrow(tempmonqry))], collapse = '\'}, {\'$oid\': \''),'\'}]}}'))
        })!='ok') ||
        mlite$count()!=MetaOut$NoRecords) {
          stop('Cleaning DB unsuccesful.')
        }
      } else {
        print('And computed which ones should be removed')
        if(any(temprmv) &&
           any(sapply(1:(((sum(temprmv)-1)%/%1000) +1), function(n) {
             dbRemoveQuery(mongo, Params$mongoColl, paste0('{ID: {$in: ["',paste0(tempmonqry[temprmv][(1000*(n-1)+1):min(1000*n, sum(temprmv))], collapse = '", "'),'"]}}'))
           })!='ok') ||
           mlite$count()!=MetaOut$NoRecords) {
          stop('Cleaning DB unsuccesful.')
        }
      }
      print('Removing extra records from mongo completed')
    }
    temprmv <- Errors$RecMerge$filenr>ParsRecfiles$to[Params$Resume$RecordMerge]
    if(any(temprmv) && any(!temprmv) && which(temprmv)[1]<max(which(!temprmv))) { # The any(!temprmv) is to prevent warning from max(NULL)
      warning('There are some IDs in Errors$RecMerge which are not in Total_New, but they are not all at the end')
      stop()
    } else {
      #print(paste('Errorfilesize is now', nrow(Errors$RecMerge)))
      Errors$RecMerge <- Errors$RecMerge[!temprmv,]
      Errors$skippedFiles <- Errors$skippedFiles[Errors$skippedFiles<Params$Resume$RecordMerge]
      Errors$ColumnsSkipped <- Errors$ColumnsSkipped[!Errors$ColumnsSkipped$filename %in% ParsRecfiles$name[ParsRecfiles$to<Params$Resume$RecordMerge],]
      Errors$count <- nrow(Errors$longdelays) + nrow(Errors$MultiIDs) + nrow(Errors$RecSumm) + nrow(Errors$RecParse) +
        nrow(Errors$RecMerge) + length(Errors$skippedFiles) + nrow(Errors$ColumnsSkipped)
      #print(paste('Errorfilesize is now', nrow(Errors$RecMerge)))
    }
  } else {
    Errors$skippedFiles <- numeric()
    Errors$RecMerge <- data.frame(ID=character(),filenr=numeric(), descr=factor(),bad=factor(), repl=factor(), stringsAsFactors = F)
    Errors$ColumnsSkipped <- data.frame(filename=character(), columnname=character(), nofilled=numeric(), stringsAsFactors = F)
    Errors$count <- nrow(Errors$longdelays) + nrow(Errors$MultiIDs) + nrow(Errors$RecSumm) + nrow(Errors$RecParse)
    MetaOut <- list(NoAltered=data.frame(True=integer(), False=integer()), MergedUpTo=0, NoRecords=0, NoDeleted=0, NoSaved=0)
    Total_ID <- character(0)
    Total_New <- data.frame()
    Total_New_Cache <- data.frame()
    Total_New_Sets <- sapply(Params$subdfs, function(x) {data.frame()}, simplify=F)
    Total_New_Sets_Cache <- sapply(Params$subdfs, function(x) {data.frame()}, simplify=F)
    Total_Del <- data.frame()
    if(Params$harv=='didlmods') {
      if(mlite$count()>0) {
        print(paste0('Warning: About to remove all content of mongo-databse (',
                     Params$mongoColl, ', containing ',mlite$count(), ' records).'))
        temp <- readline(prompt='Are you sure you want to continue (y/n)? ')
        if(temp!='y') stop('Operation aborted (cleaning DB)')
      }
      if(dbRemoveQuery(mongo, Params$mongoColl, '{}')!='ok' || 
         nrow(dbGetQuery(mongo, Params$mongoColl, '{}'))>0) {
        stop('Cleaning DB unsuccesful')
      }
    }
  }
  print(paste0('Start real merging process from record ',MetaOut$NoRecords+1, ' / file ',Params$Resume$RecordMerge, ' (of ',nrow(ParsRecfiles),') (',Sys.time(),')'))
  if(Params$Resume$RecordMerge<=nrow(ParsRecfiles)) {
    for (n in Params$Resume$RecordMerge:nrow(ParsRecfiles)) {
      Recdf <- readRDS(ParsRecfiles$fullname[n])
      Deldf <- readRDS(ParsDelfiles$fullname[n])
      #First check ID's
      if((ParsRecfiles$to[n]-ParsRecfiles$from[n])!=Params$nfiles-1 && ParsRecfiles$to[n]!=max(fileborders)){
        warning(paste('Recordfilesize is unusual.\nRecord from',ParsRecfiles$from[n],'to',ParsRecfiles$to[n]))
        print('Warning generated (MergeRecords-inloop-prechecks)')
      }
      if(!((nrow(Recdf)+nrow(Deldf)==(ParsRecfiles$to[n]-ParsRecfiles$from[n]+1)*Params$filesize) ||     # Als er precies Params$filesize * aantalfiles zijn óf:
           (ParsRecfiles$to[n] == max(fileborders)) &&                    # Het de laatste, niet-ronde file is                  
           (nrow(Recdf)+nrow(Deldf)<(ParsRecfiles$to[n]-ParsRecfiles$from[n]+1)*Params$filesize) &&        # En het minder files zijn
           (nrow(Recdf)+nrow(Deldf)>(ParsRecfiles$to[n]-ParsRecfiles$from[n])*Params$filesize))) {         # maar meer dan in een file minder had gepast
        print("Wrong number of records in total")
        stop()
      }
      if(nrow(Deldf)!=sum(Recfiles$NoDeleted[ParsRecfiles$from[n]:ParsRecfiles$to[n]])) {
        print("Wrong number of deleted records")
        stop()
      }
      if(nrow(Recdf)!=sum(Recfiles$NoRecords[ParsRecfiles$from[n]:ParsRecfiles$to[n]])) {
        print("Wrong number of good records")
        stop()
      }
      
      if(nrow(Deldf)==0) {
        FirstID <- as.character(Recdf$header.identifier[1])
        LastID <- as.character(Recdf$header.identifier[nrow(Recdf)])
      } else if (nrow(Recdf)==0) {
        FirstID <- as.character(Deldf$identifier[1])
        LastID <- as.character(Deldf$identifier[nrow(Deldf)])
      } else {
        FirstID <- as.POSIXct(Recdf$header.datestamp[1],format=Params$dateform[1], tz='UTC')<as.POSIXct(Deldf$datestamp[1],format=Params$dateform[1], tz='UTC')   #Stores if Recdf is firstID
        if(FirstID) {
          FirstID <- as.character(Recdf$header.identifier[1])
        } else {
          FirstID <- as.character(Deldf$identifier[1])
        }
        LastID <- as.POSIXct(Recdf$header.datestamp[nrow(Recdf)],format=Params$dateform[1],tz='UTC')<as.POSIXct(Deldf$datestamp[nrow(Deldf)],format=Params$dateform[1], tz='UTC')   #Stores if Recdf is firstID
        if(LastID) {
          LastID <- as.character(Recdf$header.identifier[nrow(Recdf)])
        } else {
          LastID <- as.character(Deldf$identifier[nrow(Deldf)])
        }
      } #Store FirstID and LastID
      
      if(!(((nrow(Recdf)>0)&&(Recfiles$FirstID[ParsRecfiles$from[n]]==Recdf$header.identifier[1]))||
           ((nrow(Deldf)>0)&&(Recfiles$FirstID[ParsRecfiles$from[n]]==Deldf$identifier[1])))) {
        print("First IDs don't match")
        stop()
      }
      if(!(((nrow(Recdf)>0)&&(Recfiles$LastID[ParsRecfiles$to[n]]==Recdf$header.identifier[nrow(Recdf)]))||
           ((ncol(Deldf)>0)&&(Recfiles$LastID[ParsRecfiles$to[n]]==Deldf$identifier[nrow(Deldf)])))) {
        print("Last IDs don't match")
        stop()
      }
      if(!(all(((Recfiles$FirstID[ParsRecfiles$from[n]:ParsRecfiles$to[n]] %in% Recdf$header.identifier) |
                (Recfiles$FirstID[ParsRecfiles$from[n]:ParsRecfiles$to[n]] %in% Deldf$identifier)) &
               ((Recfiles$LastID[ParsRecfiles$from[n]:ParsRecfiles$to[n]] %in% Recdf$header.identifier) |
                (Recfiles$LastID[ParsRecfiles$from[n]:ParsRecfiles$to[n]] %in% Deldf$identifier))))) {
        print("Not all IDs in file")
        stop()
      }
      if(Params$harv=='dc') {
        if (ParsRecfiles$version[n]==99) {
          colinnames <- c('header.identifier',
                          'header.datestamp',
                          'about.provenance.originDescription..attrs.harvestDate',
                          '.*setSpec.*',
                          'metadata.dc.identifier.*',                        #5
                          'metadata.dc.publisher',
                          'metadata.dc.description',
                          'metadata.dc.rights',
                          'metadata.dc.title',
                          'about.provenance.originDescription.identifier',   #10
                          'about.provenance.originDescription.baseURL',
                          'about.provenance.originDescription.metadataNamespace',
                          'about.provenance.schemaLocation',
                          'about.provenance.originDescription.datestamp',
                          'about.provenance.originDescription..attrs.altered',       #15
                          'metadata.dc.creator',
                          'metadata.dc.subject',
                          'metadata.dc.date',
                          'metadata.dc.language',
                          'metadata.dc.type',                                #20
                          'metadata.dc.format',
                          'metadata.dc.source',
                          'metadata.dc.contributor',
                          'metadata.dc.isPartOf',
                          'metadata.dc.relation',                                 #25
                          'metadata.dc.coverage',
                          '\\.(many|more)')
        } #Kept for sometime compatibility
        if (ParsRecfiles$version[n]==1) {
          colinnames <- c('header.identifier',
                          'header.datestamp',
                          'about.origin.harvestDate',
                          '.*setSpec.*',
                          'meta.identifier.*',                           #5
                          'meta.publisher',
                          'meta.description',
                          'meta.rights',
                          'meta.title',
                          'about.identifier',                            #10
                          'about.baseURL',
                          'about.metadataNamespace',
                          'about.schemaLocation',
                          'about.datestamp',
                          'about.origin.altered',                         #15
                          'meta.creator',
                          'meta.subject',
                          'meta.date',
                          'meta.language',
                          'meta.type',                                   #20
                          'meta.format',
                          'meta.source',
                          'meta.contributor',
                          'meta.isPartOf',
                          'meta.relation',                                 #25
                          'meta.coverage',
                          '\\.(many|more)',
                          'filenr')
        }
        colsMand <- colinnames[c(1:5,8:11,14:16,18)]
      } else if(Params$harv=='didlmods') {
        if (ParsRecfiles$version[n]==1) {
          colinnames <- c('header.identifier',
                          'header.datestamp',
                          'about.origin.harvestDate',
                          '.*setSpec.*',
                          'ID',                                           #5
                          'nldidl',
                          'norm',
                          'about.origin.altered',
                          'about.baseURL',
                          'about.identifier',                             #10
                          'about.datestamp',
                          'about.metadataNamespace',
                          'DAI',
                          'Journal',
                          'Subject',                                      #15
                          '\\.(many|more)',
                          'filenr')
        }
        colsMand <- colinnames[c(1:15, 17)]
      }
      if ((nrow(Recdf)>0)&& !all(sapply(colsMand,function(x) {any(grepl(pattern=x, x=names(Recdf)))}))) {
        Errors$count <- Errors$count+1
        Errors$skippedFiles <- c(Errors$skippedFiles,n)
      }
      if ((nrow(Recdf)>0)&& !(n %in% Errors$skippedFiles) && Params$harv=='dc') {
        Outdf <- data.frame(ID=as.character(Recdf[,colinnames[1]]),stringsAsFactors = F)
        Outdf$filenr <- Recdf[,grepl(colinnames[28],colnames(Recdf))]
        Outdf$date.header <- as.POSIXct(Recdf[,colinnames[2]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.header))) {
          Outdf$date.header[is.na(Outdf$date.header)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=Params$dateform[1], tz = 'UTC')-7200 # Vanwege problemen met zomer/winterijd
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.header[is.na(Outdf$date.header)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=i, tz = 'UTC')
            Outdf$date.header[is.na(Outdf$date.header)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=i, tz = 'UTC')-7200
          }
        }
        Outdf$date.harv <- as.POSIXct(Recdf[,colinnames[3]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.harv))) {                     
          Outdf$date.harv[is.na(Outdf$date.harv)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=Params$dateform[1], tz = 'UTC')-7200 # Vanwege problemen met switch bij winter/zomertijd
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.harv[is.na(Outdf$date.harv)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=i, tz = 'UTC')
            Outdf$date.harv[is.na(Outdf$date.harv)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=i, tz = 'UTC')-7200
          }
        }
        Outdf$setSpec <- Recdf[grep(x =  colnames(Recdf), pattern = colinnames[4], ignore.case = T)]
        Outdf$GlobalIDs <- Recdf[grepl(colinnames[5],colnames(Recdf))|grepl(colinnames[10],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$GlobalIDs<- data.frame(aperm(apply(Outdf$GlobalIDs, 1, function(x) {
          c(unique(x[!is.na(x)]),rep(NA,max(length(x),2)-length(unique(x[!is.na(x)]))))})), stringsAsFactors = F)
        Outdf$NumberofIDs <- apply(Outdf$GlobalIDs, 1, function(x){sum(!is.na(x))})
        cols <- grep(colinnames[6],colnames(Recdf))
        if(length(cols)>0) {
          Outdf$Publisherfull <- Recdf[,cols]
          temp <- regexpr('([,\\.] [0-9]{4})|
                          |([,[:space:]]*[\\(\\[][0-9]{4}\\??[\\)\\]])|
                          |([,[:space:]]*Vol\\.)|
                          |([,[:space:]]*[0-9]+e jaargang)|
                          |([,[:space:]]*[Nn][RrOo]\\.)|
                          |([,[:space:]]*[0-9]{0,2}(\\-[0-9]+)?[[:space:]]*(([Jj]an)|([Ff]eb)|([Mm]aa)|([Mm]rt)|([Aa]pr)|
                          |([Mm]ei)|([Mm]ay)|([Jj]u[nl])|([Aa]ug)|
                          |([Ss]ep)|([Oo][ck]t)|([Nn]ov)|([Dd]ec))[a-z-]* [0-9]{4})|
                          |([[:space:]][0-9]{4}[[:space:]]*[,\\.])', Outdf$Publisherfull)
          temp[is.na(temp)] <- -1
          temp[temp==-1] <- nchar(as.character(Outdf$Publisherfull)[temp==-1])+1
          Outdf$Publisher <- as.factor(substr(Outdf$Publisherfull,1,temp-1))
        }
        cols <- grep(colinnames[7],colnames(Recdf))
        if(length(cols)>0) {
          Outdf$description <- as.character(Recdf[,cols])
          Outdf$descriptionlengths <- sapply(Outdf$description,nchar,USE.NAMES = F)
        }
        rights <- data.frame(X=Recdf[,grep(colinnames[8],colnames(Recdf))])
        Outdf$access <- as.factor(apply(rights,1,function(x) {
          if("info:eu-repo/semantics/openAccess" %in% x) {tempOpen <- T} else {tempOpen <- F}
          if("info:eu-repo/semantics/closedAccess" %in% x) {tempClosed <- T} else {tempClosed <- F}
          if("info:eu-repo/semantics/embargoedAccess" %in% x) {tempEmbargo <- T} else {tempEmbargo <- F}
          if(tempOpen+tempClosed+tempEmbargo>1) {
            print("Record is Open/closed/embargoed at the same time")
            stop()
          }
          if(tempOpen) {return('Open')}
          if(tempClosed) {return('Closed')}
          if(tempEmbargo) {return('Embargoed')}
          if(all(is.na(x))) {return('Unknown')} else {return(paste('Other:',paste(x[!is.na(x)],collapse=' ; ')))}
        }))
        rm(rights)
        Outdf$title <- as.character(Recdf[,grep(colinnames[9],colnames(Recdf))])
        Outdf$originURL <- Recdf[,grep(colinnames[11],colnames(Recdf))]
        MetaOut$origin <- rbind.fill(MetaOut$origin, unique(data.frame(baseURL=Recdf[,grep(colinnames[11],colnames(Recdf))],
                                                                       NameSp=Recdf[,grep(colinnames[12],colnames(Recdf))],
                                                                       Schema=Recdf[,grep(colinnames[13],colnames(Recdf))])))
        Outdf$date.orig <- as.POSIXct(Recdf[,colinnames[14]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.orig))) {
          Outdf$date.orig[is.na(Outdf$date.orig)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[14]], format=Params$dateform[1], tz = 'UTC')-7200
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.orig[is.na(Outdf$date.orig)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[14]], format=i, tz = 'UTC')
            Outdf$date.orig[is.na(Outdf$date.orig)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[14]], format=i, tz = 'UTC')-7200
          }
        }
        tempna <- is.na(Outdf$date.orig)
        Outdf$date.orig[tempna] <- as.POSIXct(Recdf[tempna,colinnames[14]], format='%Y-%m-%d',tz = 'Europe/Amsterdam')
        if(any(is.na(Outdf$date.orig))) {
          print("Not alle origindates interpretable as date")
          Errors$count <- Errors$count+sum(is.na(Outdf$date.orig))
          Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[is.na(Outdf$date.orig)],
                                                                    filenr=Outdf$filenr[is.na(Outdf$date.orig)],
                                                                    descr='Bad Origindate',
                                                                    bad=Outdf[is.na(Outdf$date.orig), colinnames[14]],
                                                                    repl='NA'))
        }
        MetaOut$NoAltered <- rbind(MetaOut$NoAltered, data.frame(True=sum(Recdf[,colinnames[15]]=='true'),
                                                                 False=sum(Recdf[,colinnames[15]]=='false')))
        Outdf$ppl.creator <- Recdf[grepl(colinnames[16],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        tempdupl <- apply(Outdf$ppl.creator,1,function(x) {any(!is.na(x)&duplicated(x))})
        if(any(tempdupl)) {
          Errors$count <- Errors$count+sum(tempdupl)
          Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[tempdupl],
                                                                    filenr=Outdf$filenr[tempdupl],
                                                                    descr='Duplicate authors/creators',
                                                                    bad=apply(Outdf$ppl.creator[tempdupl,],1,function(x) {
                                                                      paste(x[!is.na(x)&duplicated(x)],collapse='; ')}),
                                                                    repl=''))
          Outdf$ppl.creator<- data.frame(aperm(apply(Outdf$ppl.creator, 1, function(x) {
            c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        }
        tempmore <- grepl(colinnames[16],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)) {Outdf$ppl.creator$more <- Recdf[,tempmore]}
        Outdf$subject <- Recdf[grepl(colinnames[17],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        tempdupl <- apply(Outdf$subject,1,function(x) {any(!is.na(x)&duplicated(x))})
        if(any(tempdupl)) {
          Errors$count <- Errors$count+sum(tempdupl)
          Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[tempdupl],
                                                                    filenr=Outdf$filenr[tempdupl],
                                                                    descr='Duplicate subjects',
                                                                    bad=apply(Outdf$subject[tempdupl,],1,function(x) {
                                                                      paste(x[!is.na(x)&duplicated(x)],collapse='; ')}),
                                                                    repl=''))
          Outdf$subject<- data.frame(aperm(apply(Outdf$subject, 1, function(x) {
            c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        }
        tempmore <- grepl(colinnames[17],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)) {Outdf$subject$more <- Recdf[,tempmore]}
        Outdf$subjectcode <- data.frame(codes=apply(Outdf$subject,c(1,2),function(x) {if(is.na(x)){return(NA)} else {
          if(x %in% Params$cla$descr) {return(Params$cla$code[Params$cla$descr==x])} else {return('-')}}}))
        #To-Do: Meer subjectcodes toevoegen
        Outdf$date.meta.text <- Recdf[,grepl(colinnames[18],colnames(Recdf))]
        Outdf$date.meta.date <- as.POSIXct(Outdf$date.meta.text,format='%Y-%m-%d',tz = 'Europe/Amsterdam')
        tempincompl <- (!is.na(Outdf$date.meta.text))&(is.na(Outdf$date.meta.date))
        Outdf$date.meta.date[tempincompl] <- as.POSIXct(Outdf$date.meta.text[tempincompl],format='%Y-%m',tz = 'Europe/Amsterdam')
        tempincompl <- (!is.na(Outdf$date.meta.text))&(is.na(Outdf$date.meta.date))
        Outdf$date.meta.date[tempincompl] <- as.POSIXct(Outdf$date.meta.text[tempincompl],format='%Y',tz = 'Europe/Amsterdam')
        tempincompl <- (!is.na(Outdf$date.meta.text))&(is.na(Outdf$date.meta.date))
        if(any(tempincompl)) {
          Errors$count <- Errors$count+sum(tempincompl)
          Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[tempincompl],
                                                                    filenr=Outdf$filenr[tempincompl],
                                                                    descr='Non-parseable metadata-date',
                                                                    bad=Outdf$date.meta.text[tempincompl],
                                                                    repl='NA'))
        }
        Outdf <- Outdf[!names(Outdf)=='date.meta.text']
        cols <- grep(colinnames[19],colnames(Recdf))
        if(length(cols)>0) {
          Outdf$language <- Recdf[,cols]
        }
        cols <- grep(colinnames[20],colnames(Recdf))
        if(length(cols)>0) {
          Outdf$type.meta <- Recdf[,cols]
        }
        Outdf$format <- Recdf[grepl(colinnames[21],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$format <- data.frame(aperm(apply(Outdf$format, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[21],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$format$more <- Recdf[,tempmore]}
        Outdf$source.meta <- Recdf[grepl(colinnames[22],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$source.meta <- data.frame(aperm(apply(Outdf$source.meta, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[22],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$source.meta$more <- Recdf[,tempmore]}
        Outdf$ppl.contributor <- Recdf[grepl(colinnames[23],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$ppl.contributor <- data.frame(aperm(apply(Outdf$ppl.contributor, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[23],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$ppl.contributors$more <- Recdf[,tempmore]}
        Outdf$isPartOf <- Recdf[grepl(colinnames[24],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$isPartOf <- data.frame(aperm(apply(Outdf$isPartOf, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[24],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$isPartOf$more <- Recdf[,tempmore]}
        Outdf$relation <- Recdf[grepl(colinnames[25],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$relation <- data.frame(aperm(apply(Outdf$relation, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[25],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$relation$more <- Recdf[,tempmore]}
        Outdf$coverage <- Recdf[grepl(colinnames[26],colnames(Recdf))&!grepl(colinnames[27],colnames(Recdf))]
        Outdf$coverage <- data.frame(aperm(apply(Outdf$coverage, 1, function(x) {
          c(unique(x),rep(NA,max(length(x),2)-length(unique(x))))} )))
        tempmore <- grepl(colinnames[26],colnames(Recdf))&grepl(colinnames[27],colnames(Recdf))
        if(any(tempmore)){Outdf$coverage$more <- Recdf[,tempmore]}
        Outdf$filenr <- Recdf[,grepl(colinnames[28],colnames(Recdf))]
        
        #Add to Total_new
        cols <- colnames(Outdf) %in% Params$subdfs
        Total_New <- rbind.fill(Total_New, Outdf[,!cols])
        Total_ID <- c(Total_ID, Outdf$ID)
        
        for (i in Params$subdfs) {
          if(ncol(Outdf[[i]])>101) {
            warning(paste0('MergeRecords is collapsing variables, this should already have been done by ParseRecords',
                           'Variable = ',i,', has ',ncol(Outdf[[i]]),' columns'))
            print('Warning thrown: MergeRecords is collapsing variables. Sleeping for 10 seconds')
            Sys.sleep(10)
            Outdf[[i]]$more <- apply(Outdf[[i]][101:ncol(Outdf[[i]])], 1, function(x) {
              if(all(is.na(x))) {return(NA)}
              y <- x[!is.na(x)]
              return(paste(y,collapse=';'))
            })
            Outdf[[i]] <- Outdf[[i]][c(1:100,ncol(Outdf[[i]]))]
          } # Mostly kept to make sure, in practice this should be done by ParseRecords
          if(is.null(Outdf[[i]])) {
            Outdf[[i]] <- NA
            Outdf[[i]] <- data.frame(Outdf[[i]])
            names(Outdf[[i]]) <- names(Total_New_Sets[[i]])[[1]]
          }
          temp <- Outdf[[i]]
          if(length(temp)>0 && !all(is.na(temp))) {
            temp <- temp[sapply(temp, function(x) {!all(is.na(x) | is.null(x) | x=='')})]
            temp <- data.frame(lapply(temp, as.factor))
          } # if-loop omdat anders aantal rijen 0 wordt bij niet-voorkomen
          names(temp)[names(temp)!='more'] <- paste0('X',seq(from=1, length.out=ncol(temp[names(temp)!='more'])))
          Total_New_Sets_Cache[[i]] <- rbind.fill(Total_New_Sets_Cache[[i]], temp)
          if(ncol(Total_New_Sets_Cache[[i]])>101 || any(!substr(unlist(sapply(Total_New_Sets_Cache, names)),1,1) %in% c('X','m'))) stop('Too many columns')
        }
        if(all(sapply(Total_New_Sets, nrow)+sapply(Total_New_Sets_Cache, nrow)==nrow(Total_New))) {
          print(paste0("Records from file ",n," parsed (",nrow(Outdf),") (",Sys.time(),")"))
          MetaOut$NoRecords <- MetaOut$NoRecords+nrow(Outdf)
        } else {
          stop('Not all dataframes merged well, start over')
        }
        
        #Check if columns are skipped
        tempcolsskip <- names(Recdf)[sapply(names(Recdf), function(x) {
          !any(sapply(colinnames[c(4:5,8,10,16:18,21:26)],grepl,x=x)) && !x %in% colinnames})]
        if(length(tempcolsskip>0)) {
          Errors$ColumnsSkipped <- rbind.fill(Errors$ColumnsSkipped, data.frame(filename=ParsRecfiles$fullname[n],
                                                                                columnname=tempcolsskip,
                                                                                nofilled=sum(!is.na(Recdf[,tempcolsskip]))))
        }
      }
      if ((nrow(Recdf)>0)&& !(n %in% Errors$skippedFiles) && Params$harv=='didlmods' && !Params$JustMongo) {
        Outdf <- data.frame(ID=as.character(Recdf[,colinnames[1]]),stringsAsFactors = F)
        Outdf$filenr <- Recdf[,colinnames[17]]
        Outdf$date.header <- as.POSIXct(Recdf[,colinnames[2]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.header))) {
          Outdf$date.header[is.na(Outdf$date.header)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=Params$dateform[1], tz = 'Europe/Amsterdam')+7200 # Vanwege problemen met zomer/winterijd
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.header[is.na(Outdf$date.header)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=i, tz = 'UTC')
            Outdf$date.header[is.na(Outdf$date.header)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.header),colinnames[2]], format=i, tz = 'Europe/Amsterdam')+7200
          }
          if(any(is.na(Outdf$date.header))) {
            print("Not alle origindates interpretable as date")
            Errors$count <- Errors$count+sum(is.na(Outdf$date.header))
            Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[is.na(Outdf$date.header)],
                                                                      filenr=Outdf$filenr[is.na(Outdf$date.header)],
                                                                      descr='Bad Origindate',
                                                                      bad=Recdf[is.na(Outdf$date.header), colinnames[2]],
                                                                      repl='NA'))
          }
        }
        Outdf$date.harv <- as.POSIXct(Recdf[,colinnames[3]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.harv))) {                     
          Outdf$date.harv[is.na(Outdf$date.harv)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=Params$dateform[1], tz = 'Europe/Amsterdam')+7200 # Vanwege problemen met switch bij winter/zomertijd
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.harv[is.na(Outdf$date.harv)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=i, tz = 'UTC')
            Outdf$date.harv[is.na(Outdf$date.harv)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.harv),colinnames[3]], format=i, tz = 'Europe/Amsterdam')+7200
          }
          if(any(is.na(Outdf$date.harv))) {
            print("Not alle origindates interpretable as date")
            Errors$count <- Errors$count+sum(is.na(Outdf$date.harv))
            Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[is.na(Outdf$date.harv)],
                                                                      filenr=Outdf$filenr[is.na(Outdf$date.harv)],
                                                                      descr='Bad Origindate',
                                                                      bad=Recdf[is.na(Outdf$date.harv), colinnames[3]],
                                                                      repl='NA'))
          }
        }
        Outdf$date.orig <- as.POSIXct(Recdf[,colinnames[11]], format=Params$dateform[1], tz='UTC')
        if(any(is.na(Outdf$date.orig))) {                     
          Outdf$date.orig[is.na(Outdf$date.orig)] <-
            as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[11]], format=Params$dateform[1], tz = 'Europe/Amsterdam')+7200 # Vanwege problemen met switch bij winter/zomertijd
          for(i in Params$dateform[-1]) {       # Verschillende formaten
            Outdf$date.orig[is.na(Outdf$date.orig)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[11]], format=i, tz = 'UTC')
            Outdf$date.orig[is.na(Outdf$date.orig)] <-
              as.POSIXct(Recdf[is.na(Outdf$date.orig),colinnames[11]], format=i, tz = 'Europe/Amsterdam')+7200
          }
          if(any(is.na(Outdf$date.orig))) {
            print("Not alle origindates interpretable as date")
            Errors$count <- Errors$count+sum(is.na(Outdf$date.orig))
            Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(ID=Outdf$ID[is.na(Outdf$date.orig)],
                                                                      filenr=Outdf$filenr[is.na(Outdf$date.orig)],
                                                                      descr='Bad Origindate',
                                                                      bad=Recdf[is.na(Outdf$date.orig), colinnames[11]],
                                                                      repl='NA'))
          }
        }
        Outdf$setSpec <- lapply(unname(split(Recdf[grep(x =  colnames(Recdf), pattern = colinnames[4], ignore.case = T)],1:nrow(Recdf))), function(x) {
          unname(x[!is.na(x)])
        })
        Outdf$GlobalIDs <- lapply(unname(split(Recdf[grepl(colinnames[5],colnames(Recdf))|
                                                       grepl(colinnames[10],colnames(Recdf))],1:nrow(Recdf))), function(x) {
          unname(x[!is.na(x)])
        })
        Outdf$NumberofIDs <- sapply(Outdf$GlobalIDs, length)
        Outdf$originURL <- Recdf[,grep(colinnames[9],colnames(Recdf))]
        rights <- lapply(Recdf[,grep(colinnames[7],colnames(Recdf))], function(x) {
          x <- unlist(x)
          unname(x[grepl('accessRights$', names(x))])
        })
        Params$Conflictdesr <- 'Other:conflicting'
        Outdf$access <- as.factor(sapply(rights,function(x) {
          if(any(grepl('Open.?Access', x, ignore.case = T))) {tempOpen <- T} else {tempOpen <- F}
          if(any(grepl('Closed.?Access', x, ignore.case = T))) {tempClosed <- T} else {tempClosed <- F}
          if(any(grepl('Embargoed.?Access', x, ignore.case = T))) {tempEmbargo <- T} else {tempEmbargo <- F}
          if(tempOpen+tempClosed+tempEmbargo>1) {
            #print("Record is Open/closed/embargoed at the same time")
            return(Params$Conflictdesr)
          }
          if(tempOpen) {return('Open')}
          if(tempClosed) {return('Closed')}
          if(tempEmbargo) {return('Embargoed')}
          if(length(x)==0 || all(is.na(x))) {return('Unknown')} else {return(paste('Other:',paste(x[!is.na(x)],collapse=' ; ')))}
        }))
        if(any(Outdf$access==Params$Conflictdesr)) {
          Errors$RecMerge <- rbind.fill(Errors$RecMerge, data.frame(
            ID=Outdf$ID[Outdf$access==Params$Conflictdesr],
            filenr=Outdf$filenr[Outdf$access==Params$Conflictdesr],
            descr="Record is Open/closed/embargoed at the same time",
            bad=I(rights[Outdf$access==Params$Conflictdesr]),
            repl='Other:conflicting'
          ))
        }
        rm(rights)
        MetaOut$NoAltered <- rbind(MetaOut$NoAltered, data.frame(True=sum(Recdf[,colinnames[8]]=='true'),
                                                                 False=sum(Recdf[,colinnames[8]]=='false')))
        MetaOut$origin <- unique(rbind.fill(MetaOut$origin, data.frame(baseURL=Recdf[,grep(colinnames[9],colnames(Recdf))],
                                                                       NameSp=Recdf[,grep(colinnames[12],colnames(Recdf))])))
        Outdf$DAI <- Recdf[,colinnames[13]]
        Outdf$Journal <- Recdf[,colinnames[14]]
        Outdf$Keywords <- Recdf[,colinnames[15]]
        if(!Params$didlbeperkt) Outdf$nldidlnorm <- Recdf[,colinnames[7]]
        # Note that not keeping track of entire nldidlcombined is on purpose
        
        #Add to Total_new and MongoDB
        Total_New_Cache <- rbind.fill(Total_New_Cache, Outdf)
        if(Params$didlbeperkt) Outdf$nldidlnorm <- Recdf[,colinnames[7]]
        Outdf <- lapply(split(Outdf, 1:nrow(Outdf)), as.list)
        names(Outdf) <- NULL
        
        Outdf <- adjustnestednames(Outdf, gsub, pattern='\\.', replacement='_')
        if(!all(lapply(Outdf, function(x) {
          if(any(unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T)))) {
            Errors$RecMerge <<- rbind.fill(Errors$RecMerge, data.frame(
              ID=x$ID,
              filenr=x$filenr,
              descr="Record contains Non-standard characters",
              bad=unname(paste0(names(which(unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T)))),': ', 
                         unlist(x)[unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T))])),
              repl=gsub(Params$MongoRegex,'',unlist(x)[unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T))], perl=T)
            ))
            x <- simple_rapply(x, gsub, pattern=Params$MongoRegex, replacement='', perl=T, classes = c('character', 'factor'))
          }
          dbInsertDocument(mongo, Params$mongoColl, gsub('\\.([0-9]+)','\\1',as.character(jsonlite::toJSON(x, POSIXt = 'mongo', raw='mongo'))))
        })=='ok')) {
          stop('Error in insertion to MongoDB')
        }
        #print(paste0('Batch nr ',n,' successfully inserted.'))
        
        print(paste0("Records from file ",n," parsed (",length(Outdf),") (",Sys.time(),")"))
        MetaOut$NoRecords <- MetaOut$NoRecords+length(Outdf)
        if(nrow(Total_New)+nrow(Total_New_Cache)!=MetaOut$NoRecords-MetaOut$NoSaved || MetaOut$MergedUpTo!=n-1) {
          stop('Merging failed: number of records unclear')
        }
        
        #Check if columns are skipped
        tempcolsskip <- names(Recdf)[sapply(names(Recdf), function(x) {
          !any(sapply(colinnames[c(4:5,8,10,16:18,21:26)],grepl,x=x)) && !x %in% colinnames})]
        if(length(tempcolsskip>0)) {
          Errors$ColumnsSkipped <- rbind.fill(Errors$ColumnsSkipped, data.frame(filename=ParsRecfiles$fullname[n],
                                                                                columnname=tempcolsskip,
                                                                                nofilled=sum(!is.na(Recdf[,tempcolsskip]))))
        }
      }
      if ((nrow(Recdf)>0)&& !(n %in% Errors$skippedFiles) && Params$harv=='didlmods' && Params$JustMongo) {
        Outdf <- data.frame(ID=as.character(Recdf[,colinnames[1]]),stringsAsFactors = F)
        Outdf$setSpec <- lapply(unname(split(Recdf[grep(x =  colnames(Recdf), pattern = colinnames[4], ignore.case = T)],1:nrow(Recdf))), function(x) {
          unname(x[!is.na(x)])
        })
        Outdf$nldidlnorm <- Recdf[,colinnames[7]]
        Outdf <- lapply(split(Outdf, 1:nrow(Outdf)), as.list)
        names(Outdf) <- NULL
        
        Outdf <- adjustnestednames(Outdf, gsub, pattern='\\.', replacement='_')
        if(!all(lapply(Outdf, function(x) {
          if(any(unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T)))) {
            Errors$RecMerge <<- rbind.fill(Errors$RecMerge, data.frame(
              ID=x$ID,
              filenr=NA,
              descr="Record contains Non-standard characters",
              bad=unname(paste0(names(which(unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T)))),': ', 
                                unlist(x)[unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T))])),
              repl=gsub(Params$MongoRegex,'',unlist(x)[unlist(simple_rapply(x, grepl, pattern=Params$MongoRegex, perl=T))], perl=T)
            ))
            x <- simple_rapply(x, gsub, pattern=Params$MongoRegex, replace='', perl=T, classes = c('character','factor'))
          }
          dbInsertDocument(mongo, Params$mongoColl, gsub('\\.([0-9])+','\\1',as.character(jsonlite::toJSON(x, POSIXt = 'mongo', raw='mongo'))))
        })=='ok')) {
          stop('Error in insertion to MongoDB')
        }
        print(paste0("Records from file ",n," parsed (",length(Outdf),") (",Sys.time(),")"))
      }
      if ((nrow(Recdf)==0)||(n %in% Errors$skippedFiles)) {
        MetaOut$NoAltered <- rbind(MetaOut$NoAltered, data.frame(True=0,
                                                                 False=0))
      }  # In feite een else-lus bij de vorige 2
      if((nrow(Deldf)>0)&&!(n %in% Errors$skippedFiles)) {
        OutDel <- Deldf
        Total_Del <- rbind.fill(Total_Del,OutDel)
        MetaOut$NoDeleted <- MetaOut$NoDeleted+nrow(Deldf)
        print(paste("Deleted Records from file",n,"parsed"))
      }
      if(n %in% Errors$skippedFiles) {
        print(paste("File",n,"skipped"))
      }
      MetaOut$MergedUpTo <- MetaOut$MergedUpTo+1
      if(((n %% 10 == 0) || Params$harv=='didlmods' && object.size(Total_New)+object.size(Total_New_Sets)+object.size(Total_New_Sets_Cache)+object.size(Total_New_Cache)>Params$MaxMem) 
         && (nrow(Total_New_Cache)>0 || (length(Total_New_Sets)>0 && nrow(Total_New_Sets_Cache[[1]])>0))) deCache(clean = T, checkTotal=T)
    }
    Errors$UpToStep <- 'MergeRecords'
    deCache(clean=T, checkTotal = T, save=T, fileNames=c(paste0(Paths$Summaries,'/',
                            c('TotalNewNS (temp)', 'Total_ID (temp)', 'Total_Del (temp)', 'Meta_Out', 'ErrorsUpToStepMerge (temp)', if(Params$harv=='didlmods') NA else 'Total_N_Sets (temp)'),
                            ' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'),NA))
    if(nrow(Total_New_Cache)==0) rm(Total_New_Cache)
    if(exists('Total_New_Sets_Cache') && (
      (class(Total_New_Sets_Cache)=='data.frame' && nrow(Total_New_Sets_Cache)==0) ||
      (class(Total_New_Sets_Cache)=='list' && all(sapply(Total_New_Sets_Cache, class)=='data.frame') && 
      all(sapply(Total_New_Sets_Cache, nrow)==0))
    )) rm(Total_New_Sets_Cache)
  }
  print('Merging new records among themselves completed.')
  Step <- 'FinalizeMerge'
}
if(Step=='FinalizeMerge') {
  rms <- ls()[!ls() %in% c(Params$keepvarnames, 'waardeomzet','oldIDs','newIDs','Total_Kept','Recfiles',
                           'Total_New','Total_New_Sets','MetaOut','Total_Del', 'IDfiles')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords','Total_ID','NewTotal','OldTotal') & !grepl('temp',rms)) ||
      !Errors$UpToStep %in% c('MergeRecords','Finalize') ||
      any(!c('oldIDs','newIDs','Total_New','Total_New_Sets','MetaOut','Total_Del','Recfiles') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start finalizing merge')
    stop()
  } else rm(list=rms)
  
  print('Starting finalization, first filling dependent fields')
  tempwaitonerror <- F
  if(!exists('waardeomzet')) waardeomzet <- read.csv2(paste0(Paths$Params,'/Waardeomzet.csv'), stringsAsFactors = F)
  if(!is.null(Errors$RecMergeFinal)) {Errors$count <- Errors$count-length(Errors$RecMergeFinal)+1}
  Errors$RecMergeFinal <- c('Initialized, but not finished') # Used as warning
  for(n in 1:nrow(waardeomzet)) {
    if(!duplicated(waardeomzet$FieldTo)[n]) {    
      Total_New[waardeomzet$FieldTo[n]] <- NA
    }# Initialize field
    Total_New[!is.na(Total_New[waardeomzet$FieldFrom[n]]) &
                Total_New[waardeomzet$FieldFrom[n]]==waardeomzet$ValFrom[n],
              waardeomzet$FieldTo[n]] <- waardeomzet$ValTo[n]
    if(!duplicated(waardeomzet$FieldTo, fromLast = T)[n]) {
      Total_New[,waardeomzet$FieldTo[n]] <- as.factor(Total_New[,waardeomzet$FieldTo[n]])
      if(any(is.na(Total_New[waardeomzet$FieldTo[n]]) & !is.na(Total_New[waardeomzet$FieldFrom[n]]))) {
        temperrors <- unique(Total_New[waardeomzet$FieldFrom[n]][is.na(Total_New[waardeomzet$FieldTo[n]]) & !is.na(Total_New[waardeomzet$FieldFrom[n]])])
        temperrors <- paste0('Not all values in waardeomzet have been covered, field from ',waardeomzet$FieldFrom[n],
                             " to ",waardeomzet$FieldTo[n],', value `',temperrors,'` (',sapply(temperrors, function(x) {
                               sum(!is.na(Total_New[waardeomzet$FieldFrom[n]]) & Total_New[waardeomzet$FieldFrom[n]]==x)
                             }),' occurences).')
        Errors$RecMergeFinal <- c(Errors$RecMergeFinal, temperrors)
        Errors$count <- Errors$count+length(temperrors)
        tempwaitonerror <- T
        print('Errors in filling field:')
        sapply(temperrors[1:min(5,length(temperrors))], print)
      } else {
        #print(paste('Field filled:',waardeomzet$FieldTo[n]))
      }
    } # Transform to factor and check for completeness
  }
  if(tempwaitonerror) {
    if(readline(prompt="There were errors. Continue anyway (y/n)? ")!='y') stop('Operation cancelled by user')
    tempwaitonerror <- F
  }
  print('General cleaning up')
  for(i in names(Total_New)[sapply(Total_New, function(x) {class(x)[1]}) %in% c('character') &
                            !names(Total_New) %in% c('ID','title','description')]) {
    Total_New[,i] <- as.factor(Total_New[,i])
  }    # Convert to factors
  Total_New <- droplevels(Total_New)
  Total_Del[grepl('setSpec', names(Total_Del))] <- lapply(Total_Del[grepl('setSpec', names(Total_Del))], function(x) {as.factor(x)}) # Just to make sure
  if('character' %in% class(Total_Del$datestamp)) {
    Total_Del$datestamptime <- as.POSIXct(Total_Del$datestamp, tz='UTC', format=Params$dateform[1])
    if(any(is.na(Total_Del$datestamptime))) {stop('Error in end-RecMerge: Total_Del  has invalid timestamps')}
    Total_Del$datestamp <- Total_Del$datestamptime
    Total_Del$datestamptime <- NULL
  } # Convert to POSIXct
  Errors$RecMerge <- droplevels(Errors$RecMerge)
  print('Setting sets to T/F')
  Params$SetsSet <- unique(c(Params$SetsSet, unlist(sapply(Total_New_Sets$setSpec, levels))))
  Params$SetsSet <- unique(c(Params$SetsSet, unlist(sapply(Total_Del[grepl('setSpec',names(Total_Del))], levels))))
  if(length(Params$SetsSet)>10) {
    stop('Too many sets to keep track off. Check and adapt code.')
  }
  n <- ncol(Total_New_Sets$setSpec[!names(Total_New_Sets$setSpec) %in% Params$SetsSet])
  for(i in Params$SetsSet) {
    Total_New_Sets$setSpec[i] <- apply(Total_New_Sets$setSpec[,1:n], 1, function(x) {any(x==i, na.rm = T)})
    Total_Del[i] <- apply(Total_Del[grepl('setSpec',names(Total_Del))], 1, function(x) {any(x==i, na.rm = T)})
    #print(paste('Decided for',i))
  }
  if(!exists('Total_Kept')) {
    print('Reading old TotalFile might take a while')
    OldTotal <- readRDS(Paths$OldTotal)
    if(any(oldIDs$ID[oldIDs$thisHarv]!=OldTotal$ID) || any(oldIDs$inNew %in% c('ToCheck','New'))) {
      warning('OldTotalfile different from oldIDfile, or still unclear files in OldIDs!')
      stop()
    } else {
      OldTotal$NewStatus <- oldIDs$inNew[oldIDs$thisHarv]
      #saveRDS(OldTotal, paste0(substr(Paths$OldTotal,1,nchar(Paths$OldTotal)-4),'_oldcopy.rds'))
      OldTotal <- OldTotal[oldIDs$inNew[oldIDs$thisHarv] %in% c('Assumed','Unchanged'),,drop=F]
      Total_Kept <- OldTotal
      rm(OldTotal)
    }
    print('Loading Total finished')
  }
  Total_Kept <- Total_Kept[!Total_Kept$ID %in% Total_New$ID,,drop=F]
  if(nrow(Total_Kept)>0) Total_Kept$filenr <- NA # These are now invalid. FileNAMES can be kept
  NewTotal <- rbind.fill(Total_New[sapply(Total_New, class)!='data.frame'], Total_Kept[sapply(Total_Kept, class)!='data.frame' &
                                                                                         !names(Total_Kept) %in% c('errors','NewStatus')])
  print('And combination succesfull, now filling subframes')
  for(n in Params$subdfs) {
    if(!all(grepl(paste0('(X[0-9]+)|(more)|(',paste0(unique(unlist(sapply(Total_Kept$setSpec,levels))), collapse=')|('),')'),names(Total_Kept[[n]])))) {
      if(nrow(Total_Kept[[n]])==0) {
        NewTotal[[n]] <- Total_New_Sets[[n]]
      } else {
        stop('Check the columns of Total_Kept')
      }
    } else {
      NewTotal[[n]] <- rbind.fill(Total_New_Sets[[n]], Total_Kept[[n]])
    }
    print(paste("Frame filled:",n))
  }
  if(any(duplicated(c(NewTotal$ID, Total_Del$identifier)))) {
    if(readline(prompt=paste('There are',sum(duplicated(c(NewTotal$ID, Total_Del$identifier))),'duplicates. Continue by removing them? '))!='y')
      stop('Operation cancelled by user')
    NewTotal <- NewTotal[!duplicated(c(NewTotal$ID, Total_Del$identifier), fromLast = T)[1:nrow(NewTotal)],]
    print('Removal completed')
  }
  if(any(sapply(NewTotal, function(x) {class(x)=='data.frame' && ncol(x)<1}))) {
    print('Not all data.frames have actual results. Dropping:')
    print(names(NewTotal)[sapply(NewTotal, function(x) {class(x)=='data.frame' && ncol(x)<1})])
  }
  print('Saving endresult')
  saveRDS(NewTotal, file=paste0(Paths$Summaries,'/NewTotal (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  saveRDS(Total_Del, file=paste0(Paths$Summaries,'/Total_N_Del',' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  Errors$RecMergeFinal <- Errors$RecMergeFinal[-1] #Removal of incompleteness warning
  Errors$UpToStep <- 'Finalize'
  saveRDS(Errors, file=paste0(Paths$Summaries,'/ErrorsUpToStepMergeFinal',' (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  Step <- 'MakeIDfile'
}
if(Step=='MakeIDfile') {
  rms <- ls()[!ls() %in% c(Params$keepvarnames,'NewTotal','oldIDs','newIDs','MetaOut','Recfiles','Total_Del')]
  if (any(!rms %in% c(Params$tempvarnames,'lastchecked','lastResToken','newRecords','Total_Kept','Total_New',
                      'Total_New_Sets', 'Total_New_Sets_Cache','waardeomzet','IDfiles', 'IDs') & !grepl('temp',rms)) ||
      (exists('lastResToken') && !lastResToken %in% c('','Endfile')) ||
      !Errors$UpToStep %in% c('Finalize') ||
      any(!c('oldIDs','newIDs','MetaOut','NewTotal', 'Total_Del') %in% ls())) {
    warning('Environment doesn\'t seem to be what it should be to start making IDfile')
    stop()
  } else rm(list=rms)
  print('Start ID-making step')
  if(!exists('Recfiles')) {
    RDSfiles <- data.frame(name=list.files(path=Paths$Summaries, pattern='\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
    RDSfiles$time <- file.mtime(RDSfiles$name)
    if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
    RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
    RDSfiles <- RDSfiles[grepl('RecFileList',RDSfiles$name),]
    if(nrow(RDSfiles)>0) {
      Recfiles <- readRDS(RDSfiles$name[1])
    } else {
      stop('No Recfiles-file found and also not already in environment!')
    }
    rm(RDSfiles)
  }
  
  # First check completeness
  if(!all(newIDs$ID %in% c(NewTotal$ID, Total_Del$identifier)) ||
     any(duplicated(c(NewTotal$ID, Total_Del$identifier))) ||
     any(oldIDs$ID %in% newIDs$ID) ||
     !all(oldIDs$ID[oldIDs$thisHarv & oldIDs$inNew %in% c('Deleted', 'Updated', 'Unchanged', 'Assumed')] %in% c(NewTotal$ID, Total_Del$identifier))) {
    stop('Errors found in trying to make new ID set. Check first')
  }
  IDs <- data.frame(ID=NewTotal$ID, 
                    dataset=NewTotal$setSpec$dataset, 
                    originURL=as.factor(NewTotal$originURL), 
                    thisHarv=T, # Deze naam wordt later veranderd in 'dc' of 'didlmods'
                    LastUpdate=NewTotal$date.header,
                    File=NewTotal$filenr, # Wordt later dus veranderd in 'dcFile' ... Merk verder op dat deze alleen is ingevuld voor de nieuw gevondenen. Bij oudere files blijft deze NA, maar wordt de Filename uit oldIDs gehaald
                    Filename=as.factor(NA), stringsAsFactors = F)
  if(nrow(Total_Del)>0) {
    extraIDs <- data.frame(ID=Total_Del$identifier,
                           dataset=Total_Del$dataset,
                           originURL=as.factor(NA), # Geen echte URL, meer identifier
                           thisHarv=F,
                           LastUpdate=Total_Del$datestamp,
                           File=Total_Del$filenr,
                           Filename=as.factor(NA))
    IDs <- rbind.fill(IDs, extraIDs)
  }
  if(is.null(oldIDs$thisHarv)) {oldIDs$thisHarv <- F}
  names(oldIDs)[names(oldIDs)==Params$harv] <- 'thisHarv'
  names(oldIDs) <- sub(paste0('^',Params$harv), '', names(oldIDs), ignore.case = T)
  names(newIDs) <- c('ID','del','inNewTs','inNewNr')
  newIDs$inNew <- factor(ifelse(newIDs$del, 'Deleted','New'))
  if(nrow(oldIDs)==0) {
    oldIDs$dataset <- rep(NA,0)
    oldIDs$originURL <- rep(NA,0)
  }
  oldIDs <- rbind.fill(oldIDs, newIDs[c('ID','inNew','inNewNr','inNewTs')])
  IDs <- merge(IDs, oldIDs[names(oldIDs)!='nr'], by='ID', all=T, suffixes = c('.NewT','.old'))
  print('Data.frame made, now filling values:')
  print('dataset (T/F)')
  if(any(IDs$dataset.NewT!=IDs$dataset.old, na.rm=T)) {
    stop('Error in making IDS, after merge: IDs have datasets T and F at same time')
  }
  IDs$dataset <- ifelse(is.na(IDs$dataset.NewT), IDs$dataset.old, IDs$dataset.NewT)
  if(any(is.na(IDs$dataset))) stop('Error in making IDs, datasets-field is NA')
  IDs$dataset.NewT <- NULL
  IDs$dataset.old <- NULL
  
  # Bijhouden wat wanneer gezet is:
  # Er zijn in totaal 3 meetmomenten:
  # 1. De oude dataset. De variabelen zijn met .old gemarkeerd. Waarden worden alleen gepakt als er verder geen beschikbaar zijn
  # 2. De ID-harvest. Variabelen inNew, inNewNr, inNewTs, del, ts
  # 3. De Record-harvest. Variabelen .NewT, 
  #       Merk wel op dat originURL niet per definitie gevuld is.
  
  # Decide most recent harvest:
  print('Deciding when was the most recent update')
  IDs$MostRecent <- factor(NA, levels=c('Never','old','IDs','RecHarv'))
  IDs$MostRecent[!is.na(IDs$inNew) & IDs$inNew=='Never'] <- 'Never'
  IDs$MostRecent[!is.na(IDs$inNew) & IDs$inNew %in% c('Unchanged','Assumed','Disappeared')] <- 'old'
  IDs$MostRecent[!is.na(IDs$inNew) & IDs$inNew %in% c('Deleted','Updated','New')] <- 'IDs'
  # In between, check timestamps
  print('And with what timestamps')
  if(any(!is.na(IDs$MostRecent) &IDs$MostRecent=='IDs' & 
         !is.na(IDs$LastUpdate.old) & IDs$LastUpdate.old>=IDs$inNewTs)) {
    stop('Conflicting timestamps (1)')
  }
  IDs$MostRecent[!is.na(IDs$thisHarv.NewT) & !is.na(IDs$MostRecent) & IDs$LastUpdate.NewT>max(IDs$ts, IDs$LastUpdate.old, IDs$inNewTs, na.rm=T)] <- 'RecHarv'
  IDs$MostRecent[!is.na(IDs$thisHarv.NewT) & is.na(IDs$MostRecent)] <- 'RecHarv'        # Records die we nog niet eerder hadden gezien
  
  #And fill values based on most recent
  print('Which records have content')
  IDs$thisHarv <- NA
  IDs$thisHarv[IDs$MostRecent=='Never'] <- F
  IDs$thisHarv[IDs$MostRecent=='old'] <- (IDs$inNew[IDs$MostRecent=='old'] %in% c('Unchanged','Assumed'))
  IDs$thisHarv[IDs$MostRecent=='IDs'] <- (IDs$inNew[IDs$MostRecent=='IDs'] %in% c('Updated','New'))
  IDs$thisHarv[IDs$MostRecent=='RecHarv'] <- IDs$thisHarv.NewT[IDs$MostRecent=='RecHarv']
  if(any(is.na(IDs$thisHarv))) {stop('Error in making IDfile: IDs$thisHarv==NA')}
  
  print('When was the last update')
  IDs$LastUpdate <- as.POSIXct(NA)
  IDs$LastUpdate[IDs$MostRecent %in% c('Never','old')] <- IDs$LastUpdate.old[IDs$MostRecent %in% c('Never','old')]
  IDs$LastUpdate[IDs$MostRecent=='IDs'] <- IDs$inNewTs[IDs$MostRecent=='IDs']
  IDs$LastUpdate[IDs$MostRecent=='RecHarv'] <- IDs$LastUpdate.NewT[IDs$MostRecent=='RecHarv']
  if(any(is.na(IDs$LastUpdate))) {stop('Error in making IDfile: dates unclear')}
  
  print('What are the repos')
  levels(IDs$originURL.NewT)[levels(IDs$originURL.NewT)=='http://pure.knaw.nl/ws/oai'] <- 'https://pure.knaw.nl/ws/oai'
  #levels(IDs$originURL)[levels(IDs$originURL=='http://pure.knaw.nl/ws/oai')] <- 'https://pure.knaw.nl/ws/oai'
  IDs$originURL.NewT <- droplevels(IDs$originURL.NewT)
  IDs$originURL.old <- droplevels(as.factor(IDs$originURL.old))
  IDs$originURL <- factor(NA, levels=unique(c(levels(IDs$originURL.old), levels(IDs$originURL.NewT))))
  IDs$originURL.old <- factor(IDs$originURL.old, levels=unique(c(levels(IDs$originURL.old), levels(IDs$originURL.NewT))))
  IDs$originURL.NewT <- factor(IDs$originURL.NewT, levels=unique(c(levels(IDs$originURL.old), levels(IDs$originURL.NewT))))
  IDs$originURL[!is.na(IDs$originURL.old)] <- IDs$originURL.old[!is.na(IDs$originURL.old)]
  IDs$originURL[!is.na(IDs$originURL.NewT)] <- IDs$originURL.NewT[!is.na(IDs$originURL.NewT)]
  if(any(is.na(IDs$originURL) & IDs$thisHarv)) {stop('Error in making IDfile: originURL unclear')}
  
  IDs$File <- IDs$File.NewT
  if(!all(Recfiles$nr==1:nrow(Recfiles))) stop('Error in Recfiles, not numbered well')
  IDs$Filename.NewT <- Recfiles$fullname[IDs$File]
  IDs$Filename <- as.character(IDs$Filename.NewT)
  IDs$Filename[is.na(IDs$File)] <- as.character(IDs$Filename.old[is.na(IDs$File)])
  if(any(is.na(IDs$Filename))) {
    readline(prompt='Not all filenames could be resolved. Continue?')
  }
  
  # Checks
  if(any((IDs$inNew %in% c('Deleted','Disappeared'))>!IDs$thisHarv.NewT, na.rm=T)) { # Check deletion status
    print('Error in making IDs: Unclear deletion status')
    readline('Press any key to continue based on newest record')
    IDs$inNew[which((IDs$inNew %in% c('Deleted','Disappeared'))>!IDs$thisHarv.NewT)] <- 'Updated'
  }
  if(any(IDs$thisHarv & (!IDs$ID %in% NewTotal$ID), na.rm=T)) {
    stop('Error in making IDs: record in thisHarv are not present in NewTotal')
  }
  if(any(IDs$LastUpdate.old>IDs$inNewTs, IDs$inNewTs>IDs$LastUpdate.NewT, na.rm=T)) {
    stop('Conflicting timestamps (2)')
  }
  if(any(IDs$inNew %in% c('Deleted','Updated') & !is.na(IDs$LastUpdate.old) & IDs$LastUpdate.old==IDs$LastUpdate.NewT)) {
    stop('Conflicting timestamps (4)')
  } # Check if newer record is harvested
  if(!is.null(resdate) && any(IDs$inNew %in% c('Deleted','Updated','New') & IDs$LastUpdate<resdate)) {
    stop('Error in making IDs: Record was modified, but LastUpdate before resdate')
  }
  if(any(IDs$originURL!=IDs$originURL.old, IDs$originURL!=IDs$originURL.NewT, na.rm = TRUE)) {
    stop('Error in making IDfile: Conflicting URLs!')
  }
  
  #And removal of old info
  IDs$thisHarv.old <- NULL
  IDs$thisHarv.NewT <- NULL
  IDs$LastUpdate.old <- NULL
  IDs$LastUpdate.NewT <- NULL
  IDs$inNewTs <- NULL
  IDs$originURL.old <- NULL
  IDs$originURL.NewT <- NULL
  IDs$File.old <- NULL
  IDs$File.NewT <- NULL
  IDs$Filename.old <- NULL
  IDs$Filename.NewT <- NULL
  IDs$inNew <- NULL
  IDs$inNewNr <- NULL
  IDs$MostRecent <- NULL
  IDs$Filename <- as.factor(IDs$Filename)
  names(IDs)[names(IDs)=='thisHarv'] <- Params$harv
  names(IDs)[names(IDs) %in% c('LastUpdate', 'File','Filename')] <- paste0(Params$harv,names(IDs)[names(IDs) %in% c('LastUpdate', 'File','Filename')])
  IDs$nr <- match(IDs$ID, NewTotal$ID)
  IDs <- IDs[order(IDs$nr),]
  print('Saving IDfile')
  saveRDS(IDs, paste0(Paths$Summaries,'/IDlist (',gsub('[^0-9]+','',as.character(Sys.time())),').rds'))
  Step <- 'CopyFiles'
}
if(Step=='CopyFiles') {
  tempcopy <- 'ask'
  while(tempcopy=='ask') {
    tempcopy <- readline(prompt='Do you want to copy the result to other folders (use this dataset for future reference) (y/n)? ')
    if(!tempcopy %in% c('y','n')) tempcopy <- 'ask' else tempcopy <- tempcopy=='y'
  }
  if(tempcopy) {
    RDSfiles <- data.frame(name=list.files(path=Paths$Summaries,
                                           pattern='.*\\.(rds|RDS)', full.name=T), stringsAsFactors = F)
    RDSfiles$time <- file.mtime(RDSfiles$name)
    RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
    if(Params$debug) RDSfiles <- RDSfiles[RDSfiles$time<Params$WayBack,]
    tempcopycount <- sum(
      file.copy(from=RDSfiles$name[grepl('NewTotal', RDSfiles$name)][1],
                to=c(paste0(Paths$Dumps,'/Werkset/Total dc',
                            str_extract(RDSfiles$name[grepl('NewTotal', RDSfiles$name)][1], ' \\([0-9]+\\)\\.rds')),
                     paste0(Paths$BaseForNewHarvest,'/Total dc',
                            str_extract(RDSfiles$name[grepl('NewTotal', RDSfiles$name)][1], ' \\([0-9]+\\)\\.rds')))))
    tempcopycount <- tempcopycount+sum(
      file.copy(from=RDSfiles$name[grepl('IDlist', RDSfiles$name)][1],
                to=paste0(Paths$BaseForNewHarvest,'/IDlist dc',
                          str_extract(RDSfiles$name[grepl('IDlist', RDSfiles$name)][1], ' \\([0-9]+\\)\\.rds'))))
    cat(tempcopycount, 'of 3 succesfully copied\n')
  }
  print('Finished!')
} # Copy files to next locations?



































