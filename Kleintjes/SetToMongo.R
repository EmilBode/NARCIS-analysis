src <- getSrcDirectory(function(x) {x})
if(src!='') setwd(paste0(src,'/..'))
rm(src)
source(paste0(getwd(), '/SetLocal.r'))
libinstandload('RMongo','rjson','mongolite','jsonlite','RJSONIO')
Sys.setenv(TZ='UTC')
Params <- list(insertstep=1000,
               CollectionName='Total3')

if(!exists('Total') && ReadForAnalysisfromTotal()!=0) stop('Error in loading file')
part <- 1:10000
dcAsList <- Total[part,sapply(Total, class)!='data.frame']
print('Basic structure complete')
for(c in names(Total[part,sapply(Total, class)=='data.frame'])) {
  if(c=='setSpec') {
    dcAsList[c] <- I(list(apply(Total[part,][[c]][,!names(Total[[c]]) %in% 
                                               c('dataset','oa_publication','publication','ec_fundedresources','openaire','thesis')], 1, function(x) {
      as.character(x[!is.na(x)])
    })))
  } else {
    dcAsList[c] <- I(list(apply(Total[part,][[c]], 1, function(x) {
      as.character(x[!is.na(x)])
    })))
  }
  print(paste0(c, ' filled'))
}
names(dcAsList) <- gsub('\\.','_',names(dcAsList))

mongo <- mongoDbConnect('NARCIS')
for(n in 1:(((nrow(dcAsList)-1) %/% Params$insertstep)+1)) {
  if(!all(apply(dcAsList[(Params$insertstep*(n-1)+1):(Params$insertstep*n),], 1, function(x) {
    dbInsertDocument(mongo, Params$CollectionName, toJSON(x))
  })=='ok')) {
    stop('Error in insertion')
  }
  print(paste0('Batch (of ',Params$insertstep,') nr ',n,' successfully inserted.'))
}
    
    
    
















































