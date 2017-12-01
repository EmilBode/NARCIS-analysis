Sys <- lapply(Sys.info(), function(x) {x})
if(Sys$sysname=='Darwin') {
  MySrcPth <- getSrcDirectory(function(x) {x})
  for(i in 1:10) {
    MySrcPth <- gsub('/[^/]+/\\.\\.', '', MySrcPth) # Resolve path: ~/Documents/Gitted/NARCIS/huppelepup/.. is ook goed
  }
  rm(i)
  if(substring(Sys$release,1,2)!='17' || tolower(Sys$login)!='emilbode' || 
     tolower(MySrcPth)!=tolower('/Users/emilbode/Documents/Gitted/NARCIS')) {
    print('Warning: Unexpected Sys.info(), check SetLocal-file for details')
    print(paste('Path is',MySrcPth))
    readline('Press any key to continue anyway')
  }
  options("java.home"="/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home")
  Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home')
  Sys.setenv(LD_LIBRARY='/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server')
  Sys.setenv(TZ='Europe/Amsterdam')
  setwd('/users/emilbode/Documents/Gitted/NARCIS')
  Paths <- list(RCode=getwd(),
                Params=paste0(getwd(),'/Params'),
                Dumps='/users/emilbode/Documents/BigFiles/NARCISdumps',
                input='/users/emilbode/surfdrive/Documents/R-IO',
                output='/users/emilbode/surfdrive/Documents/R-IO',
                plots='/users/emilbode/surfdrive/Documents/R-IO/plots',
                initial=paste0(getwd(),'/Help.R'))
}
rm(Sys)
rm(MySrcPth)
source(Paths$initial)