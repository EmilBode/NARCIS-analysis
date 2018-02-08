# Algemene intro
src <- getSrcDirectory(function(x) {x})
if(src!='') setwd(src)
rm(src)
source(paste0(getwd(), '/SetLocal.r'))
libinstandload('readxl','fastmatch', 'RMongo','mongolite')

## Verwerking van de startlijsten naar de 3 werklijsten
### Auteurs. Hier hoeft maar een ding te gebeuren, het bepalen van een start-weegfactor. Dit gebeurt alleen op basis van het aantal vakgebieden, en het parameter-document
#### We gaan er hier van uit dat we een compleet document klaar hebben staan van de databasedump
if(F) {
  Aut <- read.csv2(paste0(Paths$Params,'/Qry_DAI-subject.csv'))
  Weeg <- read_excel(paste0(Paths$Params,'/PubSub.xlsx'), sheet='Weegfac')
  Weeg <- data.frame(n=unique(Aut$Aantal), w=sapply(unique(Aut$Aantal), function(x) {
    Weeg$WeegfactorTotaal[Weeg$Categorie=='AuteurVakken' & Weeg$AantalVan<=x & x<=Weeg$AantalTot]/x+
      Weeg$Weegfactor[Weeg$Categorie=='AuteurVakken' & Weeg$AantalVan<=x & x<=Weeg$AantalTot]
  }))
  Weeg <- Weeg[order(Weeg$n),]
  Aut <- merge(Aut, Weeg, by.x='Aantal', by.y='n', sort=F)
  Aut <- Aut[order(Aut$extern_id),c('extern_id', 'term','w')]
  names(Aut) <- c('ID','class','weeg')
  write.csv2(Aut, paste0(Paths$output,'/PubSubWerkSets/Authors.csv'))
  rm(Aut)
  rm(Weeg)
}
### Na het eerste inlezen van de geharveste lijst gaan we direct wat afslanken, om geheugengebruik binnen de perken te houden
{
  #if(!exists('Total') && ReadForAnalysisfromTotal(set='didl')!=0) stop('Error in reading new set')
  if(!exists('Total')) {
    Total <- readRDS("/Users/emilbode/Documents/BigFiles/NARCISdumps/Testing/Total_Part (file 11901-13800), (20180121003620).rds")
  }
  urls <- data.frame(url=c('http://library.wur.nl/oai',
                           'http://easy.dans.knaw.nl/oai'
                     ), dataset=T, stringsAsFactors = F)
  urls <- rbind.fill(urls, data.frame(url=c('http://repository.ubn.ru.nl/dspace-oai/request',
                                            'https://dspace.library.uu.nl/oai/dare',
                                            'https://ir.cwi.nl/oai',
                                            'https://cris.maastrichtuniversity.nl/ws/oai',
                                            'http://library.tue.nl/csp/narcis/DARE.Repository.cls',
                                            'http://repub.eur.nl/oai',
                                            'http://dare.uva.nl/oai_narcis',
                                            'https://pure.uvt.nl/ws/oai',
                                            'http://oai.tudelft.nl/tno',
                                            'http://dare.uva.nl/cgi/arno/oai/naturalis',
                                            'http://publications.beeldengeluid.nl/oai',
                                            'http://dare.uva.nl/cgi/arno/oai/aup',
                                            'http://repository.uvh.nl/uvhoai/request',
                                            'https://ris.utwente.nl/ws/oai'
                                            ), dataset=F, stringsAsFactors = F))
  mongo <- OpenMongo('NARCIS','DirectFromNewHarvest_Dec17_test')
  if(F) {
    temp <-  dbGetQueryForKeys(mongo, 'DirectFromNewHarvest_Dec17_test',
                               '{nldidlnorm: {Item: {Component: {Resource: {mods: {genre: "info:eu-repo/semantics/article"}}}}} }',
                               '{"ID": true}')
    temp <-  dbGetQueryForKeys(mongo, 'DirectFromNewHarvest_Dec17_test',
                               '{"nldidlnorm": {}}',
                               '{"ID": true}')
  }
  
  
  if(!all(unique(Total$originURL) %in% urls$url)) stop('Not all urls present')
  Total <- Total[!urls$dataset[fmatch(Total$originURL, urls$url)],!names(Total) %in% c('date.header', 'date.harv','date.orig', 'setSpec','GlobalIDs','NumberofIDs','originURL','access')]
}
### Koppeling van journal-title en ISSN. We houden ook een lijst bij van gevonden combinaties: NARCIS-journaltitle met ISSNs en welke ISSNs samen voorkomen.
#### Als in NARCIS een ISSN wordt genoemd gaan we ervan uit dat dit correct is. Staat het betreffende ISSN niet in de lijst van scimagojr, dan gaat het weg
{
  Errors <- data.frame()
  ISSNCheckSum <- function(candidates) {
    valid <- sapply(candidates, function(x) {
      ch <- sum(sapply(2:8, function(n) {as.numeric(substring(x,9-n,9-n))*n})) %% 11
      if(((11-ch) %% 11)==substring(x,8,8)||(ch==1&&(substring(x,8,8) %in% c('X','x')))) {
        return(T)
      } else {
        return(F)
      }
    })
  }
  ExtractISSN <- function(Journal, ID, unlist=F, checksum=F) {
    ret <- unname(sapply(Journal, simplify=F, function(y) {
      unname(sapply(y[names(y)=='identifier'], simplify=F,  function(z) {
        if(F&&(names(z$.attrs)=='type' && tolower(z$.attrs)!='issn')) return(NULL)
        if(!grepl('^ur.:issn:(( ?[0-9] ?){4}[^0-9X]?( ?[0-9X] ?){4})([^0-9X]+.*)?$', z$text, ignore.case = T)) {
          if(grepl('(ISBN)|(^URN:ISSN:$)', z$text, ignore.case = T)) {
            ISSN <- NA
            fillError <- F
          } else if(z$text=='URN:ISSN:0031-05850') {
            ISSN <- '00315850'
          } else if(z$text=='URN:ISSN:-169-2453') {
            ISSN <- '01692543'
          } else if(z$text=='URN:ISSN:004074961') {
            ISSN <- '00407496'
          } else if(grepl('^[0-9]{7}[0-9Xx]$', z$text)) {
            if(ISSNCheckSum(z$text)) {
              ISSN <- z$text
            } else {
              ISSN <- NA
              NonISSNs <- c(NonISSNs,'checksum:')
              fillError <- T
            }
          } else if(F) {
            
          } else if(F) {
            
          } else {
            ISSN <- NA
            fillError <- T
          }
          if(is.na(ISSN) && fillError) {
            Errors <<- rbind.fill(Errors, data.frame(ID=ID, descr='Invalid ISSN', wrong=z$text))
          }
        } else {
          ISSN <- gsub('[^0-9X]','', z$text)
        }
        if(unlist) {
          return(ISSN[!suppressWarnings(is.na(ISSN))])
        } else {
          return(ISSN)
        }
      }))
    }))
    if(unlist) {
      ret <- unlist(ret)
      suppressWarnings(ret <- ret[!is.na(ret)])
      if(length(ret)==0) {
        return(NULL)
      } else {
        return(ret)
      }
    } else {
      return(ret)
    }
  }
  stop('Debug')
  
  Total$Journal <- data.frame(
    ISSN=I(apply(Total, 1, function(x) {
      ID <- x$ID
      if(F&&ID=='uu:oai:dspace.library.uu.nl:1874/294491') {
        print('Entering debug')
      }
      x <- unname(sapply(x$Journal, function(y) {
        y <- y[names(y)=='identifier']
        if(F&&ID=='uu:oai:dspace.library.uu.nl:1874/294491') {
          print('Entering debug')
        }
        y <- unname(unlist(sapply(y, function(z) {
          if(z$.attrs[names(z$.attrs)=='type']=='uri' && grepl('^ur.:issn:', z$text, ignore.case = T)) {
            if(grepl('^ur.:issn:(( ?[0-9] ?){4}[^0-9X]?( ?[0-9X] ?){4}).*$', z$text, ignore.case = T)) {
              z <- gsub('[^0-9X]','', z$text)
            }
            if(F&&ID=='uu:oai:dspace.library.uu.nl:1874/294491') {
              print('Entering debug')
            }
            if(!all(nchar(z) %in% c(0,8))) {
              # Handmatige aanpassingen
              if(grepl('URN:ISSN:0031-05850', z)) {
                z <- gsub('URN:ISSN:0031-05850','00315850', z)
              } else if (grepl('URN:ISSN:-169-2453', z)) {
                z <- gsub('URN:ISSN:-169-2453', '01692453', z)
              } else if(z %in% c('URN:ISSN:','urn:issn:-')) {
                z <- NA
              } else if(z=='URN:ISSN:004074961') {
                z <- '00407496'
              } else if(F) {
                
              } else if(F) {
                
              } else if(F) {
                
              } else {
                print(paste('Unknown ISSN-format',z))
                Errors <<- rbind.fill(Errors, data.frame(
                  ID=ID,
                  ISSN=z,
                  Title=sapply(x$Journal, function(x2) {x2$titleInfo$title}),
                  Subtitle=sapply(x$Journal, function(x2) {ifelse(is.null(x2$titleInfo$subTitle), NA, x2$titleInfo$subTitle)})
                ))
                z <- NA
              }
            }
            return(z)
          }
        }, USE.NAMES = F)))
        if(is.null(y)) {
          return(as.character(NA))
        } else {
          return(y)
        }
      }, USE.NAMES = F))
      if(is.null(x)) {
        return(as.character(NA))
      } else if(length(x)==0) {
        return(NULL)
      } else {
        return(unname(x))
      }
    })),
    title=I(sapply(Total$Journal, function(x) {
      unname(lapply(x, function(x) {x[['titleInfo']][['title']]}))
    })),
    subTitle=I(sapply(Total$Journal, function(x) {
      unname(lapply(x, function(x) {x[['titleInfo']][['subTitle']]}))
    })),
    role=I(sapply(Total$Journal, function(x) {
      as.character(lapply(x, function(x) {x[['.attrs']]['role']}))
    })))
  Total$Journal$title <- simple_rapply(Total$Journal$title, function(x) {
    if(is.null(x)) {
      return(NA)
    } else {
      return(x)
    }
  })
  Total$Journal$subTitle <- simple_rapply(Total$Journal$subTitle, function(x) {
    if(is.null(x)) {
      return(NA)
    } else {
      return(x)
    }
  })
  temp <- rbind.fill(lapply(Total$Journal, function(x) {data.frame(x)}))
  
}
stop('Scheduled')
### Daarna kunnen de journals weggeschreven worden: voor de uiteindelijke koppeling zijn alleen ISSNs, classificatie en weegfactor van belang. Heeft een journal meerdere ISSNs, dan wordt alleen de eerste bewaard, wat betekent dat bij de koppeling dus ook het eerste ISSN gebruikt dient te worden. Weegfactor is in eerste weer alleen gebaseerd op het aantal vakgebieden wat hoort bij het betreffende journal (vergelijkbaar met de weegfactor van een auteur)
### Bij de lijst met publicaties moet nog de weegfactor van elke auteur worden bepaald, op basis van de positie. Dit is iets voor een latere uitbreiding. Op dit moment kijken we alleen naar het aantal DAIs dat we hebben gevonden.
### Daarna mag de lijst worden afgeslankt.
## Het koppelen van een publicatie aan een onderwerp op basis van de werklijsten. 
### Voor elke publicatie worden de volgende stappen doorlopen:
#### De DAIs worden opgezocht, gekoppeld aan de auteurslijst --> onderwerp
##### Weging is de weegfactor van de DAI, maal die van het onderwerp
#### Het ISSN wordt opgezocht, gekoppeld aan publicatielijst --> onderwerp
##### Weging is nu alleen de weegfactor van het journal
### Hoe de technische implementatie er uit gaat zien (serieel per publicatie, of serieel per stap, combineren met volgende hoofdstap) bepalen we later.
#### De architectuur blijft ruimte geven voor latere aanpassingen (aanvullingen)
## Het aggregeren van de resultaten
### De classificatie wordt omhoog bijgewerkt: een gevonden classificatie van A12345 wordt bijgewerkt tot 5 gevonden classificaties: A12345, A12340, A12300, A12000 en A10000. Wel blijft er een veld bijhouden wat nog een specifiekere classificatie heeft
### Alle gevonden classificaties worden opgeteld op basis van weegfactor: als een auteur en journal matchen hebben we meer zekerheid
### Gevonden classificaties die niet boven een drempelwaarde uitkomen worden verwijderd
## Output en terugkoppeling
### De lijst met publicaties wordt weggeschreven
### Voor elke auteur wordt bekeken welke publicaties ze op hun naam hebben staan, en over welke onderwerpen die gingen.
#### Deze lijst wordt weggeschreven om later te kunnen bekijken.
#### Als een nieuw onderwerp wordt gevonden dan komt dit in een nieuwe werklijst.
#### Weegfactoren worden ook op basis hiervan aangepast.
##### Voor elk journal wordt ook gekeken over welke onderwerpen ze publiceerden, vergelijkbaar met de auteurs
