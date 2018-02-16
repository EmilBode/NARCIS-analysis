# Check if SetLocal has been executed well, and if paths exist
if(!exists('Paths')) {
  print('Running SetLocal.R first (which calls this script)')
  src <- getSrcDirectory(function(x) {x})
  if(src=='') src <- getwd()
  source(paste0(src, '/SetLocal.r'))
  rm(src)
} else {
  # First check if all paths exist
  for(dir in Paths[!names(Paths) %in% c('ExpectedInit', "initial") & 
                   sapply(Paths, class)=='character' &
                   sapply(Paths, length)==1 &
                   !grepl('(\\.)|(Mongo)', Paths) &
                   grepl('^/', Paths)]) {
    if(!dir.exists(dir)) {
      if(readline(prompt=paste0('Directory \"',dir,'", given in Paths$',names(Paths)[which(Paths==dir)],' does not exist. Create it (y/n)? '))=='y')
        dir.create(dir, recursive=T)
    }
  }
  rm(dir)
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
  ReadForAnalysisfromTotal <- function(Summarize=FALSE, FilePath='Auto', set='auto', ForceReload=F, silent=F, DropCols=NULL, KeepCols=NULL, KeepSets=NULL, KeepMulti=NULL) { # Padding
    libinstandload('plyr','lubridate')
    tz <- Sys.getenv('TZ', unset=NA)
    Sys.setenv(TZ='Europe/Amsterdam')
    
    if(FilePath=='Auto') {
      if(set=='auto') {
        RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                               pattern='.*Total.*\\.rds', full.name=T, ignore.case=T), stringsAsFactors = F)
        RDSfiles$time <- file.mtime(RDSfiles$name)
        RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
        FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
        if(is.na(FilePath) || is.null(FilePath)) {
          FilePath <- paste0(Paths$Dumps,'/TotalSet.rds')
        }
      } else if(set=='dc') {
        RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                               pattern='.*Total.*dc.*\\.rds', full.name=T, ignore.case=T), stringsAsFactors = F)
        RDSfiles$time <- file.mtime(RDSfiles$name)
        RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
        FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
      } else if(set %in% c('didl', 'didlmods')) {
        RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                               pattern='.*Total.*didl.*\\.rds', full.name=T, ignore.case=T), stringsAsFactors = F)
        RDSfiles$time <- file.mtime(RDSfiles$name)
        RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=T),]
        FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
      } else stop('Unknown set parameter given to ReadForAnalysisfromTotal')
    }
    if(!exists('Total')||ForceReload||is.null(Paths$WerksetTotal)||Paths$WerksetTotal!=FilePath) {
      if(!silent) print('Reading rds-file, which might take a while')
      Paths$WerksetTotal <<- NA
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
    } # Nakijken: dit werkt nu alleen met SetSpec factor/dataframe
    if(!is.null(KeepSets)) {Total <- Total[apply(Total$setSpec[KeepSets],1,any),]}
    if(!is.null(KeepMulti)) {
      for (n in 1:(length(KeepPlotList)-1)) {
        Total <- Total[Total[,names(KeepMulti)[[n]]] %in% KeepMulti[[n]],]
      }
    }
    if(!set %in% c('didl', 'didlmods')) {
      Total$year <- year(Total$date.meta.date)
      Total$Type[Total$Bron=='UvA'&Total$date.header<as.POSIXct('2017-08-20')&Total$Type=='Other'] <- 'Doctoral Thesis'
      levels(Total$Type) <- c(levels(Total$Type),'Unknown')
      Total$Type[is.na(Total$Type)] <- 'Unknown'
    }
    if(!is.na(tz)) {
      Sys.setenv(TZ=tz)
    }
    if (Summarize==FALSE) {
      assign('Total', Total, pos=.GlobalEnv)
      if(!silent) {
        print('Reading Total completed')
      }
      Paths$WerksetTotal <<- FilePath
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
      Paths$WerksetTotal <<- FilePath
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
    cla$PartOf <- gsub('(D|E)([0-9]*)[1-9](0*)$','\\1\\20\\3',cla$code)
    cla$PartOf[grepl('(D|E)0+$',cla$PartOf)] <- NA
    cla$lvl <- sapply(cla$code,function(x) {length(gregexpr('[1-9]',x)[[1]])+length(gregexpr('[1-9]0+[1-9]',x)[[1]])})
    return(cla)
  }
  simple_rapply <- function(x, fn, ..., classes='ANY') {
    if(is.list(x))
    {
      lapply(x, simple_rapply, fn, ..., classes=classes) # Don't sapply this, return value must be consistent (thus list)
    } else {
      if(classes=='ANY' || class(x) %in% classes) {
        fn(x, ...)
      } else {
        x
      }
    }
  } # The difference between rapply is that this one CAN return NULL
  nestednames <- function(lobj, unlist=T) {
    if(!unlist) {
      c(list(names(lobj)), lapply(lobj, function(x) {if(is.list(x)) nestednames(x, unlist=F) else names(x)}))
    } else {
      c(names(lobj), unlist(sapply(lobj, function(x) {if(is.list(x)) nestednames(x, unlist=T) else names(x)}), use.names = F))
    }
  }
  adjustnestednames <- function(lobj, fn, ...) {
    if(!is.null(names(lobj))) names(lobj) <- fn(names(lobj), ...)
    if(is.list(lobj)) {
      return(lapply(lobj, adjustnestednames, fn, ...))
    } else {
      return(lobj)
    }
  }
  OpenMongo <- function(DBName, collection='none', dockername=NULL, imagename=NULL, path=NULL, host="127.0.0.1", port=27017, 
                        inclView=T, preOnly=F, updateImage=F, newView=preOnly, quiet=F, user=NULL, pswd=NULL, echo=F, kickport=T) {
    libinstandload('RMongo')
    if(!is.numeric(port)) {
      if(!grepl('\\+', port)) stop('Unclear port')
      port <- as.numeric(gsub('\\+','',port))
      if(is.null(port) || is.na(port) || !is.numeric(port) || port==0) stop('Unclear port')
      checkport <- T
    } else checkport <- F
    if(!is.null(user) && is.na(user)) {
      if(exists('Params') && !is.null(Params$MongoUser)) {
        user <- Params$MongoUser
      } else {
        user <- 'Guest'
      }
    }
    if(!is.null(user) && is.na(pswd)) {
      if(exists('Params') && !is.null(Params$MongoPswd)) {
        pswd <- Params$MongoPswd
      } else {
        pswd <- 'Guest'
      }
    }
    if(is.null(dockername)) {
      success <- F
      tries <- 0
      if(echo) print(paste('System call: "nc -zvv',host, port))
      output <- suppressWarnings(system2('nc', args=c('-zvv', paste(host, port)), stderr=T, stdout=T)[10])
      if(!grepl(paste0('Connection to.* port ',port,'.*succeeded\\!'),output)) {
        print(paste('Warning: OpenMongo was called without a docker name, but there doesn\'t seem to be a process listening on port',port))
        print('Script will fail if no process has initialized in 100 seconds')
      }
      
      mongo <- mongoDbConnect(DBName, host, port)
      answer <- 'n'
      while(!success && tries<100) {
        tryCatch({
          # Note that if mongo is not yet ready, dbAuthenticate throws an error before print is called
          if(tries==98) {
            invisible('Debug here')
          }
          if(!is.null(user)) {
            print(paste0('Authenticate output: ', dbAuthenticate(mongo, user,pswd)))
            print(paste0('Supplied credentials: user=`',user,'`, password=`',pswd,'`'))
          }
          dbInsertDocument(mongo, collection, '{ID: "ConnectionTesting_r78qfuy8asfhaksfhajklsfhajksl"}')
          if(nrow(dbGetQuery(mongo, collection, '{}', skip=0, limit=10))<1)  stop('Error in connecting mongo')
          if(dbRemoveQuery(mongo, collection, '{ID: "ConnectionTesting_r78qfuy8asfhaksfhajklsfhajksl"}')!='ok') stop('Error in connecting mongo')
          success <- T
        },error=function(e) {
          if(!preOnly) {
            goOn <- F
            while((is.na(answer) || answer=='y') && !goOn) {
              if(is.na(answer)) answer <<- readline(prompt='Including pause to wait for mongo to get ready. Load mongoview first (y/n)? ')
              if(answer=='y') {
                syscall <- 'docker start mongoview'
                if(echo) print(paste('System call:', syscall))
                output <- system(syscall, intern=T)
                if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
                if(output!='mongoview') {
                  stop('Error in starting mongoview') 
                }
                Sys.sleep(5)
                syscall <- 'docker ps'
                if(echo) print(paste('System call:', syscall))
                output <- system(syscall, intern=T)
                if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
                goOn <- any(grepl(' mongo-express ', output))
              }
            }
            if(answer!='y') {
              if(tries==0) cat('Waiting for mongo to get ready')
              cat('.')
              tries <<- tries+1
              mongo <- mongoDbConnect(DBName, host, port)
              Sys.sleep(1)
            }
          } else {
            success <- T
          }
        }) 
      }
      if(!success) stop('Mongo seems not to get ready')
      if(!preOnly && tries>0) cat('\n')
      if(!quiet && !preOnly) print('Succesfully connected')
    } else {
      # Check if docker is running, otherwise start it
      {
        if(!quiet) print('Trying if Docker is running')
        syscall <- 'ps aux'
        if(echo) print(paste('System call:', syscall))
        output <- system(syscall, intern=T)
        if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        if(!any(grepl('Docker.app', output))) {
          syscall <- 'open --background -a Docker'
          if(echo) print(paste('System call:', syscall))
          if(system(syscall)!=0) stop('Error in starting docker')
          if(preOnly) return(NULL)
        }
        success <- F
        tries <- 0
        while(!success && tries<500) {
          tryCatch({
            if(echo) print(paste('System call:', 'docker ps'))
            output <- suppressWarnings(system2('docker', args=c('ps'), stderr=T, stdout=T))
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(substring(output[1],1,12)!='CONTAINER ID') stop('ToCatchError')
            success <- T
          }, error=function(e) {
            if(preOnly) {
              print('Warning: Docker process was running, but seems not to respond')
              tries <<- 1001
            } else {
              if(tries==0) {
                print('Waiting for docker process to get ready')
              } else {
                cat('.')
              }
              tries <<- tries+1
              Sys.sleep(1)
            }
          })
        }
        if(tries==1001) return(NULL) # No connection, but preOnly
        if(!success) {
          stop('Docker seems not to get ready')
        } else if (tries>0) {
          cat('\n')
        }
      }
      if(kickport && !checkport) {
        if(suppressWarnings(length(system(paste0('lsof -n | grep vpnkit.*TCP.*',port,'.*LISTEN'), intern = T)))!=0) {
          output <- fromJSON(system('docker inspect $(docker ps -q)', intern=T))
          toStop <- output$Id[sapply(1:nrow(output),function(x) {isTRUE(output[x,]$HostConfig$PortBindings$`27017/tcp`[[1]]$HostPort=='27017' &&
                                                                          output[x,]$Name!=paste0('/',dockername))})]
          if(length(toStop)>1) stop('According to system call, already multiple processes are listening on this port. This can\'t be right, investigate first')
          if(length(toStop)==1) {
            syscall <- paste('docker stop', toStop)
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!=toStop) stop('Stopping containers to free port unsuccesful')
          }
        }
      }
      if(is.null(imagename)) {
        syscall <- 'docker ps'
        if(echo) print(paste('System call:', syscall))
        output <- system(syscall, intern=T)
        if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        if(!any(grepl(paste0(' ',dockername,'$'), output))) {
          syscall <- paste('docker start', dockername)
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(output!=dockername) stop('Error in starting mongo')
        }
      } else { # Imagename en dockername provided
        if(!updateImage) {
          syscall <- 'docker image ls'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!any(grepl(paste0('^',imagename,' '), output))) updateImage <- T
        } # Check if image is available, set if F
        if(updateImage) {
          syscall <- 'docker ps -a'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(any(grepl(paste0(' ',dockername,'$'), output))) {
            syscall <- paste('docker stop', dockername) # May be overkill, but doesn't harm
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!=dockername) stop(paste('Error in stopping',dockername,'container'))
            syscall <- paste('docker rm',dockername)
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!=dockername) stop(paste('Error in stopping',dockername,'container'))
          }
          
          # And remove any instances of mongoview, otherwise linkage causes problems
          syscall <- 'docker ps -a'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(any(grepl(paste0(' mongoview$'), output))) {
            syscall <- 'docker stop mongoview' # May be overkill, but doesn't harm
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!='mongoview') stop('Error in stopping mongoview container')
            syscall <- paste('docker rm mongoview')
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!='mongoview') stop(paste('Error in stopping mongoview container'))
          }
          
          print(paste0('Pulling new image (',imagename,'), might take a while.'))
          syscall <- paste('docker pull',imagename)
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!grepl(paste0('(Status: Image is up to date for ',imagename,')|(Status: Downloaded newer image for ',imagename,')'),
                    output[length(output)])) {
            stop('Error in docker pulling image')
          } else {
            if(!quiet) print('Pulling completed successfully')
          }
        }
        syscall <- 'docker ps -a'
        if(echo) print(paste('System call:', syscall))
        output <- system(syscall, intern=T)
        if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        if(any(grepl(paste0(' ',dockername,'$'), output))) {
          syscall <- 'docker ps'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!any(grepl(paste0(' ',dockername,'$'), output))) {
            syscall <- paste('docker start', dockername)
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!=dockername) stop('Error in starting mongo')
          }
        } else {
          if(is.null(path)) path <- paste0(Paths$MongoData, dockername) # Omission of / is intentional
          if(checkport) {
            while(suppressWarnings(length(system(paste0('lsof -n | grep TCP.*',port,'.*LISTEN'), intern = T))!=0)) port <- port+1
          }
          syscall <- paste0('docker run --name ',dockername,' -v ',path,':/data/db -p ',port,':27017 -d '
                            ,imagename, ' --logpath /data/db/log.log',ifelse(is.null(user),'',' --auth'))
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          syscall <- 'docker ps'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!any(grepl(paste0(' ',dockername,'$'), output))) stop('Error in starting mongo')
        }
      }
      if(inclView) {
        if(!newView) {
          syscall <- 'docker ps -a'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(any(grepl(' mongoview$', output))) {
            syscall <- 'docker inspect mongoview'
            if(echo) print(paste('System call:', syscall))
            output <- fromJSON(system(syscall, intern=T))
            if(echo) print(paste0('Returned JSON-object of ',length(output),' fields.'))
            newView <- !any(grepl(dockername, output$HostConfig$Links, fixed = T))
          }
        } # Check first if runnning mongoview-container is linking to the right mongo
        if(newView) {
          syscall <- 'docker ps'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(any(grepl(' mongoview$', output))) {
            syscall <- 'docker stop mongoview'
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!='mongoview') stop('Error in stopping View to mongoDB')
          }
          syscall <- 'docker ps -a'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(any(grepl(' mongoview$', output))) {
            syscall <- 'docker rm mongoview'
            if(echo) print(paste('System call:', syscall))
            output <- system(syscall, intern=T)
            if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
            if(output!='mongoview') stop('Error in stopping view to mongoDB')
          }
        }
        if(!updateImage) {
          syscall <- 'docker image ls'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!any(grepl('^mongo-express ', output))) updateImage <- T
        }
        if(updateImage) {
          print(paste0('Pulling new image (mongo-express), might take a while.'))
          syscall <- 'docker pull mongo-express'
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
          if(!grepl('(Status: Image is up to date for mongo-express)|(Status: Downloaded newer image for mongo-express)',
                    output[length(output)])) {
            stop('Error in docker pulling image mongo-express')
          } else {
            if(!quiet) print('Pulling completed successfully (mongo-express)')
          }
        }
        syscall <- 'docker ps -a'
        if(echo) print(paste('System call:', syscall))
        output <- system(syscall, intern=T)
        if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        if(!any(grepl('mongoview *$', output))) {
          imagename <- substring(output[grepl(paste0(' ',dockername,'$'), output)],21,40)
          imagename <- gsub(' *$','', imagename)
          if(!is.null(user)) {
            syscall <- paste0('docker run --link ',dockername,':',imagename,
                              ' -p 8081:8081 --name mongoview -e ME_CONFIG_MONGODB_ADMINUSERNAME="',user,
                              '" -e ME_CONFIG_MONGODB_ADMINPASSWORD="', pswd,
                              '" -e ME_CONFIG_BASICAUTH_USERNAME="', user,
                              '" -e ME_CONFIG_BASICAUTH_PASSWORD="', pswd,
                              '" -d mongo-express')
          } else {
            syscall <- paste0('docker run --link ',dockername,':',imagename,' -p 8081:8081 --name mongoview -d mongo-express')
          }
          if(echo) print(paste('System call:', syscall))
          output <- system(syscall, intern=T)
          if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        }
      } # Initializing mongoview, must be done here because we need imagename or dockername
    }
    if(inclView) {
      syscall <- 'docker ps'
      if(echo) print(paste('System call:', syscall))
      output <- system(syscall, intern=T)
      if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
      if(!any(grepl(' mongoview$', output))) {
        syscall <- 'docker start mongoview'
        if(echo) print(paste('System call:', syscall))
        output <- system(syscall, intern=T)
        if(echo) print(paste('Output:', output)[if(is.numeric(echo)) {1:min(echo,length(output))} else {T}])
        if(output!='mongoview') {
          #This means staring mongo was unsuccesful. Most probable reason: linking to wrong container
          stop('Error in starting mongoview')
        }
      }
    }
    if(preOnly) {
      return(NULL)
    } else {
      if(is.null(dockername)) {
        return(mongo)
      } else {
        return(OpenMongo(DBName, collection, port=port, quiet=quiet, echo=echo))
      }
    }
  }
  extractComments <- function(FileName, max=9, fromLine=1, ToLine=-1, tab='\t') {
    libinstandload('stringr')
    file <- readLines(FileName, n=ToLine)
    file <- str_extract(file[fromLine:length(file)], paste0('(##[0-',max,'].*)|(#{2,',max+1,'}[^#0-9]+)'))
    file <- file[!is.na(file)]
    for(n in 9:1) {
      file <- gsub(pattern=paste0(paste0(rep('#', times=n+1), collapse=''), '[^#0-9]'),
                   replace=paste0('##',n),
                   x=file)
    }
    nrs <- as.numeric(substring(str_extract(file, '##[0-9]'),3,4))-1
    file <- paste0(sapply(nrs, function(n) {paste0(rep(tab, times=n), collapse='')}),'- ',gsub('##[0-9][[:space:]]*','',file))
    file <- gsub('\\\\t',tab,file)
    return(file)
  }
  '%!in%' <- function(x,y)!('%in%'(x,y))
}










































