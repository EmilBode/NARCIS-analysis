src <- getSrcDirectory(function(x) {x})
if(src!='') setwd(src)
rm(src)
source(paste0(getwd(), '/SetLocal.r'))
libinstandload('fastmatch', 'RMongo','mongolite')

Recfiles <- readRDS("/Users/emilbode/Documents/BigFiles/NARCISdumps/Dec17_test/didlmods/RecFileList (temp) (20171207161907).rds")
if(!exists('Total')) {
  #Total <- readRDS("/Users/emilbode/Documents/BigFiles/NARCISdumps/Testing/Total_Part (file 11901-13800), (20180121003620).rds")
  print('Opening DB')
  RMon <- OpenMongo('NARCIS','Dec2017','NARCIS','mongo',paste0(Paths$MongoData,'DB-NARCIS-Dec17'), quiet = T)
  print('DB started succesfully')
  mlite <- mongo('Dec2017', 'NARCIS')
  print('And mongolite connection succesfull')
  qry <- paste0('{"nldidlnorm.0.Item.Component.Resource.mods.genre": "info:eu-repo/semantics/article",',
                '"nldidlnorm.0.Item.Component.Resource.mods.identifier._attrs": "doi"}')
  Total <- mlite$aggregate(paste0('[{"$match":',qry,'}, {"$sample": {"size": ',10000,'}}]'))
  print('Sample retrieved')
  #Total <- mlite$find(qry)
  if(any(duplicated(Total$ID))) {
    if(readline(paste0('Warning: there were ',sum(duplicated(Total$ID)), ' duplicates. Continue (y/n)? '))!='y') stop()
    Total <- Total[!duplicated(Total$ID)]
  }
}
output <- function(text) {
  sink('/Users/EmilBode/surfdrive/Documents/R-IO/Messages from PubSub-stats.R.txt', append=T)
  if(class(text)=='character') {
    cat(paste0(text,'\n'))
  } else {
    print(text)
  }
  sink()
  if(class(text)=='character') {
    cat(paste0(text,'\n'))
  } else {
    print(text)
  }
}
isJournal <- function(item) {
  if('type' %in% names(item$.attrs)) {
    return(item$.attrs[names(item$.attrs)=='type']=='host')
  } else {
    return(NA)
  }
}
stop('Debug')
# We splitsen wat op
Journals <- Total$Journal
relItems <- unlist(Journals, recursive=F, use.names = F)
sink('/Users/EmilBode/surfdrive/Documents/R-IO/Messages from PubSub-stats.R.txt') #To clean file
if(sink.number()>0) for(n in 1:sink.number()) sink()

output('Informatie over losse relatedItems')
output(paste('Dit alles is gebaseerd op',length(relItems),'RelatedItems'))
output('Hoeveel velden')
output(table(sapply(relItems, length)))
output('Welke hebben dubbele velden?')
output(table(sapply(relItems, function(x) {sum(duplicated(names(x)))})))
output('Welke zijn dat?')
output(table(unlist(sapply(relItems, function(x) {names(x)[duplicated(names(x))]}))))
output('Welke velden komen voor en hoe vaak?')
output('Met dubbelen: ')
output(table(unlist(sapply(relItems, names))))
output('Zonder:')
output(table(unlist(sapply(relItems, function(x) {unique(names(x))}))))
for(field in c('physicalDescription','_attrs')) {
  output(paste0('What is in ',field,'?'))
  fields <- unlist(sapply(relItems[sapply(relItems, function(x) {field %in% names(x)})], function(x) {unname(x[names(x)==field])}), recursive = F)
  output(table(data.frame(name=names(fields), value=as.character(fields))))
}
field <- 'part'
output(paste0('What is in ',field,'?'))
fields <- unlist(sapply(relItems[sapply(relItems, function(x) {field %in% names(x)})], function(x) {unname(x[names(x)==field])}), recursive = F)
output(table(names(fields)))
temp <- relItems[sapply(relItems, function(x) {
  y <- x[names(x)=='part']
  if(length(y)==0) return(F)
  names <- unlist(sapply(y, names))
  return(is.null(names) || '' %in% names)
})]
output('What is in the empty ones?')
output(table(sapply(temp, function(x) {x$part}), useNA='ifany'))
output('And in the other ones?')
for(subf in unique(names(fields))[unique(names(fields))!='']) {
  output(paste0(subf,' (',sum(names(fields)==subf),'):'))
  output(table(names(unlist(unname(fields[names(fields)==subf])))))
}
output('Opmerkingen:')
output('detail number is supposed to be a journal numbering but is quite dirty')
output('detail caption is a prefix, with the following counts:')
temp <- unlist(unname(fields[names(fields)=='detail']), recursive=F)
output(table(unlist(temp[names(temp)=='caption'])))
output('detail attributes are, with the following counts:')
temp <- unlist(unname(fields[names(fields)=='detail']), recursive=F)
temp <- unlist(unname(temp[names(temp)=='.attrs']))
temp <- data.frame(name=names(temp), value=unname(temp))
output(plyr::count(temp))
output('So it seems safe to classify it as the same as with caption')
output('Extent is to what extent a publication is part of a host, so e.g. page 200-210 (This will be coded as start 200, end 210, total 11, unit=page)')
output('text can have the following text attributes:')
temp <- unlist(unname(fields[names(fields)=='text']), recursive=F)
output(table(unlist(temp[names(temp)=='.attrs'])))
output('with the following text:')
output(table(unlist(temp[names(temp)=='text'])))
output('Next field: originInfo')
field <- 'originInfo'
fields <- unlist(sapply(relItems[sapply(relItems, function(x) {field %in% names(x)})], function(x) {unname(x[names(x)==field])}), recursive = F)
output('It has the next subfields:')
output(table(names(fields)))
output('dateIssued has the following different formats of subfields')
output(unique(lapply(fields[names(fields)=='dateIssued'], names)))
temp <- sapply(unname(fields[names(fields)=='dateIssued']), function(x) {x$.attrs})
output(paste0('With .attrs having names `', unique(names(temp)), '` and content `', paste(unique(temp), collapse='; '),'`'))
temp <- sapply(unname(fields[names(fields)=='dateIssued']), function(x) {x$text})
output('And text being a date, sometimes only a year. We could later use this, nut not now. Most frequent:')
output(table(substring(temp,1,1e3))[order(table(temp), decreasing = T)][1:10])

subf1 <- 'place'
output(paste0(subf1,' has the following different formats of subfields'))
subfields <- fields[names(fields)==subf1]
output(unique(lapply(fields[names(fields)==subf1], names)))
temp <- sapply(unname(fields[names(fields)==subf1]), function(x) {x$placeTerm})
output(c('With placeTerm things like: ', sample(temp, size=10)))
temp <- sapply(unname(fields[names(fields)==subf1]), function(x) {x$text})
output('And text being a date, sometimes only a year. We could later use this, nut not now. Most frequent:')
output(table(substring(temp,1,1e3))[order(table(temp), decreasing = T)][1:10])

stop('Scheduled')

print(results <- table(sapply(Journals, length)))
print(results <- table(sapply(unlist(Total$Journal, recursive = F), isJournal)))
temp <- sample_n(Total[sapply(Journals, length)>1,c('ID','filenr')], size=20)
temp$file <- Recfiles$name[temp$filenr]
temp <- unname(sapply(unlist(Journals, recursive = F), function(x) {
  x$.attrs
}))
unique(unlist(sapply(temp, names)))
table(sapply(temp, function(x) {sum(names(x)=='type')}))
table(unlist(temp)[names(unlist(temp))=='type'])
table(sapply(temp, function(x) {sum(names(x)=='role')}))
table(unlist(temp)[names(unlist(temp))=='role'])

table(sapply(Journals, function(x) {sum(names(x)=='identifier')}))
temp <- unname(sapply(unlist(Journals, recursive = F), function(x) {
  x$.attrs
}))
unique(unlist(sapply(temp, names)))
table(sapply(temp, function(x) {sum(names(x)=='type')}))
table(unlist(temp)[names(unlist(temp))=='type'])
table(sapply(temp, function(x) {sum(names(x)=='role')}))
table(unlist(temp)[names(unlist(temp))=='role'])



idx <- which(sapply(Journals, function(x1) {
  any(sapply(x1, function(x2) {
    field %in% names(x2) && sapply(x2[names(x2)==field], function(x3) {
      subf1 %in% names(x3) && sapply(x3[names(x3)==subf1], function(x4) {
        subf2 %in% names(x4) && sapply(x4[names(x4)==subf2], function(x5) {
          is.null(names(x5))
        })
      })
    })
  }))
}))
             
             
             
