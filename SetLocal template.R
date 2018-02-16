# Everything that needs to be adjusted is marked *This way*. In some cases something like a standard directory is suggested
# Note that paths are best specified absolutely, because the user might change their working directory
# These paths are used when harvesting new records, and plotting them. Also some helperfunctions are dependent in them
# And a lot of code is just checks and standard settings.

Sys <- as.list(Sys.info())
if(Sys$sysname=='Darwin') {
  MySrcPth <- getSrcDirectory(function(x) {x})
  for(i in 1:10) {
    MySrcPth <- sub('/[^/]+/\\.\\.', '', MySrcPth) # Resolve path: ~/a/../b  is the same as ~/b
  }
  rm(i)
  
  # Check if system information is what we expect
  if(substring(Sys$release,1,2)!='17' || tolower(Sys$user)!=tolower('*YourUserName*') || 
     tolower(MySrcPth)!=tolower('*Expected directory THIS file is in*')) {
    print('Warning: Unexpected Sys.info(), check SetLocal-file for details')
    print(paste('Path is',MySrcPth))
    readline('Press any key but <Esc> to continue anyway ')
  }
  if(!exists('Paths')) Paths <- list() # If paths does exist, leave additional entries
  options("java.home"="*/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home*")
  Sys.setenv(JAVA_HOME=options()$java.home)
  Sys.setenv(LD_LIBRARY=paste0(*options()$java.home, '/lib/server')*)
  Sys.setenv(TZ='*Europe/Amsterdam*')
  Paths$RCode <- MySrcPth
  Paths$Params <- paste0(Paths$RCode,'/Params')
  Paths$Dumps <- '*Path/To/Store/Large/Files*'                                    # Used for harvesting via OAI-PMH
  Paths$BaseForNewHarvest <- paste0(Paths$Dumps,'/InputForNew')
  Paths$input <- '*Local path for smaller inputfiles*'                       # Not all input, name should have been something else
  Paths$output <- Paths$input
  Paths$IO <- Paths$input                                                         # This one is for scripts reading their own output again
  Paths$plots <- paste0(Paths$output,'/plots')
  Paths$MongoData <- '*/Path/to/Mongo*'                                     # This one is case-sensitive
  
  # Checks and other standard settings, no need to adjust these
  Paths$ExpectedInit <- c("adjustnestednames",                            # We expect thes variables/functions to be present after initialisation
                          "extractComments", 
                          "libinstandload",
                          "OpenMongo",
                          "ReadForAnalysisfromTotal", 
                          "readNARCIScla",
                          "nestednames",
                          "simple_rapply",
                          "%!in%"
                          "Paths")
  Paths$initial <- if(!all(Paths$ExpectedInit %in% ls())) {             # These commands are executed at the end of te script. It is reset to source this very script ath the end
    c(
      lapply(paste0(Paths$RCode,c('/Help.R')), function(x) {list(what='source', args=list(x))}),
      list(list(what='libinstandload', args=list('plyr','dplyr','XML'))))
  } else {
    list()
  }
  # Miscellaneous settings, feel free to delete where needed
  options('roadoi_email'=tolower('*Your mailadress*'))
} # Darwin is the internal name for OSX
for(s in Paths$initial) {
  do.call(do.call, s)
}
rm(s, Sys, MySrcPth)
Paths$initial <- list(list(what='source', args=list(paste0(Paths$RCode,'/SetLocal.R'))))
