source(paste0(getSrcDirectory(function(x) {x}), '/../SetLocal.r'))
libinstandload('plyr')
if(!exists('TotalVals')) TotalVals <- ReadForAnalysisfromTotal(T)

ToVSNU <- plyr::count(TotalVals, vars = c('Access','Jaar','Bron','BronSoort','Type'), wt_var = 'freq')
rangeList <- c(1900,1920,1930,1940,1950,1960,1970,1980,1990,1995,2000:2018,2025)
ToVSNU$yearrange <- factor(sapply(ToVSNU$Jaar, function(x) {
  if(is.na(x)) {return('Unknown')}
  if(x<rangeList[1]) {return (paste0('Pre',rangeList[1]))}
  if(!any(rangeList>x)) {return (paste0('Post',rangeList[length(rangeList)]))}
  temp <- which(rangeList>x)[1]
  if(rangeList[temp]-rangeList[temp-1]==1) {
    return(x)
  } else {
    return(paste0(rangeList[temp-1],'-',rangeList[temp]-1))
  }
}))
ToVSNU <- plyr::count(ToVSNU, vars = c('Access','yearrange','Bron','BronSoort','Type'), wt_var = 'freq')
ToVSNU$yearrange <- relevel(ToVSNU$yearrange, 'Pre1900')
ToVSNU$BronSoort[ToVSNU$BronSoort %in% c('Universiteit (alg)','Universiteit (4TU)')] <- 'Universiteit'
ToVSNU$BronSoort <- relevel(ToVSNU$BronSoort,'Universiteit')
ToVSNU <- ToVSNU[order(ToVSNU$BronSoort, ToVSNU$yearrange),]
write.csv2(ToVSNU, file = paste0(Paths$output,'/NARCIS-cijfers voor VSNU per access.csv'),row.names = F)
ToVSNU <- plyr::count(ToVSNU, vars = c('yearrange','Bron','BronSoort','Type'), wt_var = 'freq')
ToVSNU <- ToVSNU[order(ToVSNU$BronSoort, ToVSNU$yearrange),]
write.csv2(ToVSNU, file=paste0(Paths$output,'/NARCIS-cijfers voor VSNU samengevat.csv'),row.names = F)




