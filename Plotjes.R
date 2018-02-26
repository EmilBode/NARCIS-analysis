{
  source(paste0(getSrcDirectory(function(x) {x}), '/SetLocal.r'))
  libinstandload('plyr','lubridate','ggplot2','plotly','scales','tidyr','reshape2','RColorBrewer','googleVis','readr','extrafont')
  options(OutDec = '.') # Put here for consistency. Should be overriden later, but not setting here led to initialization issues
  ColContrast <- function(bg, contrast='BW30') {
    if(nchar(bg)!=7 || substr(bg,1,1)!='#') {
      print(paste('Warning: ColContrast called with invalid color:',bg))
      return('#000000')
    }
    bgr <- substr(bg,2,3)
    bgg <- substr(bg,4,5)
    bgb <- substr(bg,6,7)
    if (substr(contrast,1,3)=='Add') {
      if(nchar(contrast)==3) {
        contrast <- 128
      } else {contrast <- 2.56*as.numeric(substr(contrast,4,nchar(contrast)))}
      bgr <- as.hexmode((strtoi(bgr, 16L)+128) %% 256)
      bgg <- as.hexmode((strtoi(bgg, 16L)+128) %% 256)
      bgb <- as.hexmode((strtoi(bgb, 16L)+128) %% 256)
      return(paste0('#',bgr,bgg,bgb))
    }
    if (substr(contrast,1,2)=='BW') {
      if(nchar(contrast)==2) {cutoff <- 448} else {cutoff <- 8.96*as.numeric(substr(contrast,3,nchar(contrast)))}
      coltotal <- strtoi(bgr, 16L)+2*strtoi(bgg, 16L)+strtoi(bgb, 16L)/2
      return(ifelse(coltotal>cutoff,'#000000','#FFFFFF'))
    }
    return(paste0('#',bgr,bgg,bgb))
  }
  if(!exists('TotalVals')) {
    TotalVals <- ReadForAnalysisfromTotal(Summarize = 'Both')
  }
} # Initialization
part <- T
multiPlot <- lapply(rep(c(7, 2, 22, 14), times=1), function(x) {x})[part]
multiPlotParams <- list(GglPlotGlobLinks=rep(c(T,T,F,F),each=4)[part],
                        LabelsLang=rep(c('nl','en','en','nl'), each=4)[part])
multiPlotParams <- multiPlotParams[1:4]

for (ng in 1:length(multiPlot)) {
  {
    StandardPlot <- multiPlot[[ng]]
    PP <- list()
    if(exists('Jaren')) {rm(Jaren)}
    plotData <- TotalVals
    levels(plotData$Type) <- c(levels(plotData$Type),'Unknown')
    plotData$Type[is.na(plotData$Type)] <- 'Unknown'
    VSNU <- readRDS(paste0(Paths$Params,'/VSNU.rds'))
    temp <- read.csv2(paste0(Paths$Params,'/UniNaamOmzet.csv'), stringsAsFactors = F)
    levels(VSNU$Universiteit) <- vapply(levels(VSNU$Universiteit), function(x) {temp$NARCISNaam[temp$VSNUNaam==x]}, character(1))
    rm(temp)
    
    VSNU$Universiteit <- factor(VSNU$Universiteit, levels = levels(plotData$Bron))
    names(VSNU) <- c('Categorie','Bron','Jaar','freq')
  } # Get clean data
  
  PP$PrintPlot <- FALSE
  PP$PausePlot <- FALSE
  PP$SavePlot <- TRUE
  PP$Zoomfactor <- .5
  PP$SaveGglPlotStd <- F
  
  PP$SaveGglPlot <- PP$SaveGglPlotStd && StandardPlot %in% c(2,7,22,14)
  PP$GglPlotGlobLinks <- FALSE
  PP$LabelsLang <- 'en'
  if(exists('multiPlotParams')) {
    for(par in names(multiPlotParams)) {
      PP[par] <- multiPlotParams[[par]][ng]
    }
  }
  PP$GglPlotFileName <- paste0('GoogleChartsCode',
                               ifelse(PP$GglPlotGlobLinks, ' for local viewing ',' for website '),
                               PP$LabelsLang,'.htm')
  if(0 %in% StandardPlot) {
    plotData <- plotData[plotData$set.publication|plotData$set.dataset,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2017,F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    plotData <- plotData[plotData$Type %in% c('Doctoral Thesis', 'Article'),]
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Access'
    
  } # Template
  if(1 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2016, F)
    plotData <- plotData[plotData$Access %in% c('Open','Closed'),]
    plotData <- plotData[plotData$Type %in% c('Article'),]
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- 'Jaar'
    PP$xlab <- 'Year of publication'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed', 'Open')
    PP$kleuren <- 'FromFile'
    #PP$plotPerc <- 'Open'
    #PP$ylab2 <- 'Percentage Open Access'
    PP$LegTitle <- ''
    PP$Titel <- 'Open access of articles'
    
    PP$SecLine <- list(What='VSNUTotals', Total=c('Wetensch', 'Vak'))
    names(PP$SecLine) <- c('What','Total (VSNU)')
    PP$extrakleuren <- list(VSNULine='#D02020')
    PP$LegLblLinetype <- 'Total amount of\nresearch output\n(VSNU)'
    PP$LegLblAlpha <- PP$LegLblLinetype
    
  } # Oud nr 3: Open Access voor alle artikelen
  if(2 %in% StandardPlot) {
    #Selectie
    PP$plotType <- 'Histo'
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2016, F)
    plotData <- plotData[plotData$Type %in% c('Doctoral Thesis'),]
    
    #Plotparameters
    PP$plotX <- 'Jaar'
    PP$XOrder <- '-'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed', 'Open')
    PP$kleuren <- 'FromFile'
    PP$SecLine <- list(What='VSNUTotals', Theses=c('Thesis'))
    PP$LegTitle <- '' #Bewust lege regel, lijnt mooier uit
    PP$extrakleuren <- list(VSNULine='#D02020')
    
    if(PP$LabelsLang=='en') {
      PP$xlab <- 'Year of publication'
      PP$ylab <- 'Number of PhD dissertations'
      PP$Titel <- 'Open access of PhD dissertations'
      PP$LegLblLinetype <- 'Total\naccording\nto VSNU'
    } else {
      PP$xlab <- 'Jaar van publicatie'
      PP$ylab <- 'Aantal proefschriften'
      PP$Titel <- 'Open access voor PhD proefschriften'
      PP$LegLblLinetype <- 'Totaal\nvolgens\nde VSNU'
    }
    PP$LegLblAlpha <- PP$LegLblLinetype
  } # NR 2: Open Access voor theses
  if(3 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2015, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    plotData <- plotData[!is.na(plotData$Type) & plotData$Type == 'Doctoral Thesis',]
    plotData <- plotData[plotData$Access=='Open',]
    
    PP$PercBase <- VSNU[VSNU$Categorie=='Thesis',c(3,2,4)]
    names(PP$PercBase) <- c('x', 'group', 'freq')
    
    #Plotparameters
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$SecLine <- list(What='RefPerc',Percentage=100)
    PP$ylab <- 'Percentage'
    PP$xlab <- 'Year of publication'
    PP$Titel <- 'Ratio of theses in NARCIS vs. VSNU'
    PP$TextSizeTitle <- 18
    PP$AddRange <- list(What='SD',which='Average',range=1, alpha=.2)
    PP$Anonimize <- T
    PP$LegTitle <- ifelse(PP$Anonimize,'','University')
  } # Percentage Theses in NARCIS
  if(4 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2015, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    plotData <- plotData[plotData$Access=='Open',]
    plotData <- plotData[plotData$Type=='Article',]
    
    PP$PercBase <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    names(PP$PercBase) <- c('x', 'group', 'freq')
    
    #Plotparameters
    PP$Zoomfactor <- PP$Zoomfactor*.8
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$AddRange <- list(What='SD',which='Average',range=1, alpha=.333)
    PP$LegTitle <- list(NULL)
    PP$Titel <- 'Open access of articles across Dutch universities'
    PP$xlab <- 'Year of publication'
    PP$ylab <- expression(frac('Number of open access articles','Total amount of research output (VSNU)'))
    PP$Anonimize <- T
    PP$LegDynamicHeight <- F
    PP$LegLbl <- c('',' ','Different',substring('                 ',1,2:8), 'Universities',substring('               ',1,9:10),'\n\nAverage\n\n')
    PP$LegLblSD <- 'Area within\n1 standard\ndeviation from\naverage'
    PP$TextSizeLblY <- 12
    PP$TextSizeTitle <- 18
    
  } # Oud nr 4: Open Access totalen als percentage VSNU
  if(5 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    plotData <- plotData[plotData$Access!='Other',]
    #Jaren <- c(2000,2017,F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    plotData <- plotData[!plotData$Type %in% c('Bachelor Thesis', 'Master Thesis'),]
    plotData <- plotData[plotData$Access!='Open',]
    
    temp <- read.csv2(paste0(Paths$Params,'/TypeCats.csv'))
    #temp$count <- sapply(temp$Orig, function(i) {sum(TotalVals$freq[as.character(TotalVals$Type)==as.character(i)], na.rm=T)})
    #temp$count[temp$Orig=='Unknown'] <- sum(TotalVals$freq[is.na(TotalVals$Type)])
    #write.csv2(temp, paste0(Paths$Params,'/TypeCats.csv'))
    temp <- temp[temp$Version=='TypeCat1',2:4]
    plotData$TypeCat <- sapply(plotData$Type, function(i) {temp$Cat[temp$Orig==i]})
    rm(temp)
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- c('TypeCat')
    #PP$plotX <- c(PP$plotX,'Jaar')
    PP$plotCol <- 'TypeCat'
    PP$BarPosit <- 'fill'
    PP$BarWidth <- 'root1'
    PP$BarGap <- 0
    PP$XOrder <- 'PropVal'
    PP$XOrder <- NULL
    PP$ColOrder <- c('Closed','Open')
    #PP$ColOrder <- c('Open','Closed')
    PP$kleuren <- 'Auto'
    PP$coord <- 'polar'
    PP$polarArea <- F
    PP$polarRadius <- 'TextAllign1.2'
    PP$BackgroundColorPlot <- 'transparent'
    PP$Titel <- 'Closed categories'
    PP$LegTitle <- 'Type'
    
    #PP$Categorise <- list(Conference=c('Conference Object', 'Conference item', 'Conference Proceedings','Conference Paper'),
    #                      Paper=c('Article', 'Working Paper','Preprint'),
    #                      Print=c('Book','Book part', 'Book Review','Review', 'Technical Documentation','Contribution to periodical','Doctoral Thesis'),
    #                      Rest=c('Lecture','Report', 'Annotation','Other','Unknown','Patent'))
    
  } # PieChart
  if(6 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2015,F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    plotData <- plotData[!plotData$Type %in% c('Bachelor Thesis', 'Master Thesis'),]
    PP$PercBase <- VSNU[VSNU$Categorie %in% c('Wetensch'),c(3,2,4)]
    
    #Plotparameters
    PP$plotType <- 'Scatter'
    PP$plotX <- 'Total'
    PP$plotY <- c('Access','Open','NARCISTotal')
    PP$plotGroups <- c('Bron','Jaar')
    PP$plotCol <- 'Bron'
    PP$plotAlpha <- 'Jaar'
    PP$plotSymbol <- 'Bron'
    PP$pointsize <- 4
  } # Scatter voor Open vs. compleetheid
  if(7 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    plotData <- plotData[plotData$Access!='Other',]
    Jaren <- c(2000,2017,F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)') | plotData$Bron=='Hogescholen',]
    plotData <- plotData[!plotData$Type %in% c('Bachelor Thesis', 'Master Thesis'),]
  
    temp <- read.csv2(paste0(Paths$Params,'/TypeCats.csv'), stringsAsFactors = F)
    #temp$count <- sapply(temp$Orig, function(i) {sum(TotalVals$freq[as.character(TotalVals$Type)==as.character(i)], na.rm=T)})
    #temp$count[temp$Orig=='Unknown'] <- sum(TotalVals$freq[is.na(TotalVals$Type)])
    #write.csv2(temp, paste0(Paths$Params,'/TypeCats.csv'))
    temp <- temp[temp$Version=='TypeCat1',2:4]
    plotData$Type <- droplevels(plotData$Type)
    temp <- temp[temp$Orig %in% levels(plotData$Type),]
    temp$Orig <- factor(temp$Orig)
    plotData$TypeCat <- sapply(plotData$Type, function(i) {temp$Cat[temp$Orig==i]})
    rm(temp)
    
    #Plotparameters
    
    PP$plotType <- 'Histo'
    PP$plotX <- c('TypeCat')
    #PP$plotX <- 'Type'
    #PP$plotX <- c(PP$plotX,'Jaar')
    PP$plotCol <- 'Access'
    PP$BarPosit <- 'fill'
    PP$BarWidth <- 'root1'
    PP$FileTitle <- paste0('Open access for different scholarly publication types (linear width) (',PP$LabelsLang,')')
    PP$BarGap <- .1
    PP$XOrder <- 'ValProp'
    if(PP$LabelsLang=='en') {
      PP$Titel <- 'Open access for different scholarly publication types'
      PP$LegLbl <- c('Closed\naccess','Open\naccess')
      PP$XOrder <- c('Article','Book (part)','Doctoral Thesis','Rest')
      PP$xvallabs <- c('All','Articles','Book (chapter)','PhD dissertations','Other types')
    } else {
      PP$Titel <- 'Open access voor verschillende publicatietypen'
      PP$LegLbl <- c('Closed\naccess','Open\naccess')
      PP$XOrder <- c('Article','Book (part)','Doctoral Thesis','Rest')
      PP$xvallabs <- c('Alle','Artikelen','Boeken (delen)','Proefschriften','Overig')
    }
    PP$ColOrder <- c('Closed','Open')
    #PP$ColOrder <- c('Open','Closed')
    PP$kleuren <- 'FromFile'
    PP$xlab <- ''
    PP$ylab <- ''
    PP$LegTitle <- list(NULL)
    PP$TextSizeTitle <- 15
    PP$TextFont <- 'Verdana'
    PP$InclSummPos <- 'left'
    #PP$plotPerc <- 'Open'
    
    #PP$Categorise <- list(Conference=c('Conference Object', 'Conference item', 'Conference Proceedings','Conference Paper'),
    #                      Paper=c('Article', 'Working Paper','Preprint'),
    #                      Print=c('Book','Book part', 'Book Review','Review', 'Technical Documentation','Contribution to periodical','Doctoral Thesis'),
    #                      Rest=c('Lecture','Report', 'Annotation','Other','Unknown','Patent'))
  } # NR 1: Access per Type
  if(11 %in% StandardPlot) {
    #Selectie
    if(is.null(Jaren)) {
      Jaren <- c(2000,2017,F)
    } else {
      Jaren[1] <- max(Jaren[1],2000)
      Jaren[2] <- min(Jaren[2],2017)
    }
    plotData <- plotData[plotData$Bron %in% c('Wageningen UR'),]
    PP$PercBase <- PP$PercBase[PP$PercBase$group %in% c('Wageningen UR'),]
    VSNU <- VSNU[VSNU$Bron %in% c('Wageningen UR'),]
    PP$Titel <- paste('Wageningen UR:',PP$Titel)
    if(!is.null(PP$TextSizeTitle)) {
      PP$TextSizeTitle <- PP$TextSizeTitle/1.5
    } else {
      PP$TextSizeTitle <- 18
    }
    PP$includeSumm <- F
    
    #Plotparameters
  } # Voor Wageningen UR, altijd combineren met andere param
  if(12 %in% StandardPlot) {
    #Selectie
    Jaren <- c(2000,2016, F)
    
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    PP$PercBase <- plotData
    PP$PercBase$freq <- PP$PercBase$freq*sapply(PP$PercBase$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$Jaar) & PP$PercBase$Jaar>=Jaren[1] & PP$PercBase$Jaar<=Jaren[2],c(2,3,5,7)]
    PP$PercBaseArticles <- plyr::count(PP$PercBase[PP$PercBase$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseTotals <- plyr::count(PP$PercBase, vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    PP$PercBaseVSNU <- plyr::count(PP$PercBaseVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    PP$PercBaseWeigh <- merge(PP$PercBaseArticles, PP$PercBaseTotals, by=c('Jaar','Bron'))
    names(PP$PercBaseWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    PP$PercBaseWeigh <- merge(PP$PercBaseWeigh, PP$PercBaseVSNU, by=c('Jaar','Bron'))
    PP$PercBaseWeigh$weigh <- PP$PercBaseWeigh$Articles/sapply(1:nrow(PP$PercBaseWeigh), function(n) {
      min(PP$PercBaseWeigh[n,4:5])
    })
    PP$PercBase <- merge(PP$PercBaseVSNU, PP$PercBaseWeigh[c(1,2,6)])
    PP$PercBase$adjfreq <- PP$PercBase$freq*PP$PercBase$weigh
    PP$SecLine <- PP$PercBase[c(1,2,5)]
    names(PP$SecLine) <- c('Jaar','Bron','freq')
    PP$SecLine$Categorie <- factor('Articles')
    VSNU <- rbind.fill(VSNU, PP$SecLine)
    PP$PercBase <- NULL
    
    plotData <- plotData[plotData$Access %in% c('Open','Closed'),]
    plotData <- plotData[plotData$Type %in% c('Article'),]
    
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- 'Jaar'
    PP$xlab <- 'Year of publication'
    PP$ylab <- 'Number of articles'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed', 'Open')
    PP$kleuren <- 'FromFile'
    #PP$plotPerc <- 'Open'
    #PP$ylab2 <- 'Percentage Open Access'
    PP$LegTitle <- list(NULL)
    PP$Titel <- 'Open access of articles'
    
    PP$SecLine <- list(What='VSNUTotals', Total=c('Wetensch', 'Vak'), Articleestimate=c('Articles'))
    #names(PP$SecLine) <- c('What','Total (VSNU)')
    PP$extrakleuren <- list(VSNULine=c('#D02020','#00FF00'))
    PP$LegLblLinetype <- c('Total amount of\nresearch output\n(VSNU)', 'Estimated number\nof articles\n(based on VSNU)')
    PP$LegLblAlpha <- PP$LegLblLinetype
    PP$LegLblLineColor <- PP$LegLblLinetype
    PP$LegWidth <- list(fill=1, rest=2)
    PP$LegHeight <- list(fill=2, rest=4)
    
  } # NR 3: Open Access voor artikelen
  if(13 %in% StandardPlot) {
    #Selectie
    plotData <- plotData[plotData$set.publication|plotData$set.dataset,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- NULL
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    
    #Plotparameters
    PP$Zoomfactor <- PP$Zoomfactor*.667
    PP$plotType <- 'Histo'
    PP$includeSumm <- F
    PP$coord <- 'flip'
    PP$plotX <- 'Bron'
    PP$XOrder <- 'Count'
    PP$BarWidth <- 'const.333'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed','Open')
    PP$LegLblFill <- c('Closed  ','Open   ')
    PP$LegTitle <- list(NULL)
    PP$kleuren <- c('#F07000','#3030FF')
    PP$Titel <- 'Open en closed access publicaties per instituut in NARCIS, juli 2017'
    PP$BackgroundColorPlot <- 'transparent'
    PP$TextSizeTitle <- 16
    PP$SaveResolution <- c(1600,800)
  }
  if(14 %in% StandardPlot) {
    
    # Uitleg van de weging:
    #------------------------------
    # We kijken eerst hoeveel publicaties er zijn die vallen binnen de VSNU-definitie van publicatie
    # Gebaseerd op Param/PubWeigh.csv (PhD=altijd publ, boek= 90% publicatie, ...)
    # Dan kijken we eerst of dit meer is dan de VSNU zegt. Zo ja, dan gaan we er van uit dat we alle artikelen in NARCIS hebben,
    # en wordt dit onze schatting van het daadwerkelijk aantal artikelen.
    # Zijn het er minder, dan gaan we er van uit dat de verhouding artikelen/publicaties in NARCIS klopt, deze verhouding passen we toe op VSNU.
    # Voorbeeld: NARCIS heeft 100 artikelen (50 OA), 100 PhDs. VSNU zegt 150 publicaties.
    # Schatting is dan daadwerkelijk 100 artikelen. Percentage in grafiek is dan 50 %
    # Voorbeeld 2: NARCIS heeft 100 artikelen (50 OA), 50 PhDs. VSNU zegt 200 publicaties.
    # Schatting is dan 66.7% van alle pubs is een artikel --> er zijn in werkelijkheid 133.3 artikelen.
    # Percentage in grafiek is dan 50/133.3 = 37.5 %
    
    # Eerst 2016 KUOZ-cijfers
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    KUOZ <- ReadVSNUpdf()$doc1
    KUOZ <- KUOZ[KUOZ$Bron=='KUOZ' & KUOZ$Type=='Totaal_Artikel' & KUOZ$jaar==2016 & KUOZ$Uni!='OU',]
    KUOZ$jaar <- as.numeric(as.character(KUOZ$jaar))
    temp <- read.csv2(paste0(Paths$Params,'/UniNaamOmzet.csv'), stringsAsFactors = F)
    KUOZ$Uni <- droplevels(KUOZ$Uni)
    levels(KUOZ$Uni) <- vapply(levels(KUOZ$Uni), function(x) {temp$NARCISNaam[temp$VSNUNaam==x]}, character(1))
    rm(temp)
    KUOZ <- KUOZ[,4:6]
    names(KUOZ) <- c("Jaar","Bron","adjfreq")
    
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2016, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    PP$PercBase <- plotData
    PP$PercBase$freq <- PP$PercBase$freq*sapply(PP$PercBase$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$Jaar) & PP$PercBase$Jaar>=Jaren[1] & PP$PercBase$Jaar<=Jaren[2],c(2,3,5,7:ncol(PP$PercBase))]
    PP$PercBaseArticles <- plyr::count(PP$PercBase[PP$PercBase$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseTotals <- plyr::count(PP$PercBase, vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    PP$PercBaseVSNU <- plyr::count(PP$PercBaseVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    PP$PercBaseWeigh <- merge(PP$PercBaseArticles, PP$PercBaseTotals, by=c('Jaar','Bron'))
    names(PP$PercBaseWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    PP$PercBaseWeigh <- merge(PP$PercBaseWeigh, PP$PercBaseVSNU, by=c('Jaar','Bron'))
    PP$PercBaseWeigh$weigh <- PP$PercBaseWeigh$Articles/sapply(1:nrow(PP$PercBaseWeigh), function(n) {
      min(PP$PercBaseWeigh[n,4:5])
    })
    PP$PercBase <- merge(PP$PercBaseVSNU, PP$PercBaseWeigh[c(1,2,6)])
    PP$PercBase$adjfreq <- PP$PercBase$freq*PP$PercBase$weigh
    PP$PercBase <- PP$PercBase[c(1,2,5)]
    PP$PercBase <- rbind.fill(PP$PercBase, KUOZ)
    
    plotData <- plotData[plotData$Access=='Open',]
    plotData <- plotData[plotData$Type=='Article',]
    #plotData$freq <- plotData$freq*sapply(plotData$Type,function(x) {
    #  PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    #})
    names(PP$PercBase) <- c('x', 'group', 'freq')
    
    
    #Plotparameters
    PP$Zoomfactor <- PP$Zoomfactor*.8
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$epsFriendly <- F
    PP$AddRange <- list(What='SD',which='Average',range=1, alpha=.333)
    PP$LegTitle <- list(NULL)
    PP$Anonimize <- T
    PP$LegDynamicHeight <- F
    PP$TextSizeLblY <- 12
    PP$TextSizeTitle <- 18
    
    if(PP$LabelsLang=='en') {
      PP$Titel <- 'Open Access of articles across Dutch universities'
      PP$xlab <- 'Year of publication'
      PP$ylab <- expression(paste(over('OA articles (NARCIS)','Total articles (estim. based on VSNU)'),'  %'))
      PP$LegLbl <- c('',' ','Individual',substring('                 ',1,2:8), 'Universities',substring('               ',1,9:10),'\n\nAverage\n\n')
      PP$LegLblSD <- 'Area within\n1 standard\ndeviation from\naverage'
    } else {
      PP$Titel <- 'Open Access voor artikelen, voor verschillende universiteiten'
      PP$xlab <- 'Jaar van publicatie'
      PP$ylab <- expression(paste(over('OA artikelen (NARCIS)','Schatting artikelen - o.b.v VSNU'),'  %'))
      PP$LegLbl <- c('',' ','Individuele',substring('                 ',1,2:8), 'Universiteiten',substring('               ',1,9:10),'\n\nGemiddelde\n\n')
      PP$LegLblSD <- 'Gebied binnen\n1 standaard\ndeviatie van\nhetgemiddelde'
    }
  } # NR 4: Open Access totalen als percentage VSNU met schatting
  if(15 %in% StandardPlot) {
    # Uitleg van de weging:
    #------------------------------
    # We kijken eerst hoeveel publicaties er zijn die vallen binnen de VSNU-definitie van publicatie
    # Gebaseerd op Param/PubWeigh.csv (PhD=altijd publ, boek= 90% publicatie, ...)
    # Dan kijken we eerst of dit meer is dan de VSNU zegt. Zo ja, dan gaan we er van uit dat we alle artikelen in NARCIS hebben,
    # en wordt dit onze schatting van het daadwerkelijk aantal artikelen.
    # Zijn het er minder, dan gaan we er van uit dat de verhouding artikelen/publicaties in NARCIS klopt, deze verhouding passen we toe op VSNU.
    # Voorbeeld: NARCIS heeft 100 artikelen (50 OA), 100 PhDs. VSNU zegt 150 publicaties.
    # Schatting is dan daadwerkelijk 100 artikelen. Percentage in grafiek is dan 50 %
    # Voorbeeld 2: NARCIS heeft 100 artikelen (50 OA), 50 PhDs. VSNU zegt 200 publicaties.
    # Schatting is dan 66.7% van alle pubs is een artikel --> er zijn in werkelijkheid 133.3 artikelen.
    # Percentage in grafiek is dan 50/133.3 = 37.5 %
    
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2015, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    PP$PercBase <- plotData
    PP$PercBase$freq <- PP$PercBase$freq*sapply(PP$PercBase$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==as.character(x)]
    })
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$Jaar) & PP$PercBase$Jaar>=Jaren[1] & PP$PercBase$Jaar<=Jaren[2],c(2,3,5,7)]
    PP$PercBaseArticles <- plyr::count(PP$PercBase[PP$PercBase$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseTotals <- plyr::count(PP$PercBase, vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    PP$PercBaseVSNU <- plyr::count(PP$PercBaseVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    PP$PercBaseWeigh <- merge(PP$PercBaseArticles, PP$PercBaseTotals, by=c('Jaar','Bron'))
    names(PP$PercBaseWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    PP$PercBaseWeigh <- merge(PP$PercBaseWeigh, PP$PercBaseVSNU, by=c('Jaar','Bron'))
    PP$PercBaseWeigh$weigh <- PP$PercBaseWeigh$Articles/sapply(1:nrow(PP$PercBaseWeigh), function(n) {
      min(c(1,2)*PP$PercBaseWeigh[n,4:5])
    })
    PP$PercBase <- merge(PP$PercBaseVSNU, PP$PercBaseWeigh[c(1,2,6)])
    PP$PercBase$adjfreq <- PP$PercBase$freq*PP$PercBase$weigh
    PP$PercBase <- PP$PercBase[c(1,2,5)]
    
    #plotData <- plotData[plotData$Access=='Open',]
    plotData <- plotData[plotData$Type=='Article',]
    #plotData$freq <- plotData$freq*sapply(plotData$Type,function(x) {
    #  PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    #})
    names(PP$PercBase) <- c('x', 'group', 'freq')
    
    
    #Plotparameters
    PP$Zoomfactor <- PP$Zoomfactor*.8
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$AddRange <- list(What='SD',which='Average',range=1, alpha=.333)
    PP$LegTitle <- list(NULL)
    PP$Titel <- 'Completeness-estimate of NARCIS'
    PP$xlab <- 'Year of publication'
    PP$ylab <- 'Completeness-estimate'
    PP$Anonimize <- F
    PP$LegDynamicHeight <- F
    PP$LegLbl <- c('',' ','Different',substring('                 ',1,2:8), 'Universities',substring('               ',1,9:10),'\n\nAverage\n\n')
    PP$LegLblSD <- 'Area within\n1 standard\ndeviation from\naverage'
    PP$TextSizeLblY <- 12
    PP$TextSizeTitle <- 18
    
    # Write csv values
    temp <- TotalVals[TotalVals$set.publication,]
    levels(temp$Type) <- c(levels(temp$Type),'Unknown')
    temp$Type[is.na(temp$Type)] <- 'Unknown'
    
    temp <- temp[c(1:(ncol(temp)-11),ncol(temp))]
    Jaren <- c(2000,2015, F)
    temp <- temp[temp$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    temp$Jaar[temp$Jaar<Jaren[1]] <- Jaren[1]-1
    temp$Jaar[temp$Jaar>Jaren[2]] <- Jaren[2]+1
    temp$Jaar[is.na(temp$Jaar)] <- Jaren[2]+2
    if (Jaren[3]) {      # TRUE voor restjaren, False voor selectie
      temp$Jaar <- factor(temp$Jaar)
      levels(temp$Jaar) <- c('Before', levels(temp$Jaar)[2:(nlevels(temp$Jaar)-2)],'After','Unknown')
    } else {
      temp <- temp[temp$Jaar>=Jaren[1] & temp$Jaar<=Jaren[2],]
      temp$Jaar <- factor(temp$Jaar)
    }
    
    temp <- plyr::count(temp[temp$BronSoort %in% c('Universiteit (4TU)', 'Universiteit (alg)')&
                             as.numeric(as.character(temp$Jaar))>1999,], 
                        vars=c('Bron','Jaar','Type'), wt_var = 'freq')
    temp$weighfreq <- temp$freq*sapply(1:nrow(temp), function(n) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==as.character(temp$Type[n])]
    })
    temp <- merge(plyr::count(temp, vars=c('Bron','Jaar'), wt_var = 'weighfreq'), spread(temp[1:4], Type, freq))
    names(temp)[3] <- 'Totalestimate'
    VSNU2 <- spread(VSNU, Categorie,freq)
    VSNU2$VSNUTotal <- VSNU2$Vak+VSNU2$Wetensch
    VSNU2 <- VSNU2[c(1,2,6)]
    temp <- merge(temp, VSNU2)
    temp$PercEstim <- 100*temp$Totalestimate/temp$VSNUTotal
    temp$EmptyCol <- NA
    temp <- temp[c(1,2,ncol(temp)-2,3,ncol(temp)-1:0,4:(ncol(temp)-3))]
    write.csv2(temp, paste0(Paths$output,'/Completenessestimate.csv'),na='')
  } # Check hoe compleet NARCIS is
  if(16 %in% StandardPlot) {
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    VSNUcmpr <- ReadVSNUpdf()
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2014,2016, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    
    Estim <- plotData
    Estim$weighfreq <- Estim$freq*sapply(Estim$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    Estim <- Estim[!is.na(Estim$Jaar) & Estim$Jaar>=Jaren[1] & Estim$Jaar<=Jaren[2],c(2,3,5,7:ncol(Estim))]
    EstimArticles <- plyr::count(Estim[Estim$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    EstimTotals <- plyr::count(Estim, vars=c('Jaar','Bron'), wt_var = 'weighfreq')
    EstimVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    EstimVSNU <- plyr::count(EstimVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    EstimWeigh <- merge(EstimArticles, EstimTotals, by=c('Jaar','Bron'))
    names(EstimWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    EstimWeigh <- merge(EstimWeigh, EstimVSNU, by=c('Jaar','Bron'))
    EstimWeigh$weigh <- EstimWeigh$Articles/sapply(1:nrow(EstimWeigh), function(n) {
      min(EstimWeigh[n,4:5])
    })
    Estim <- merge(EstimVSNU, EstimWeigh[c(1,2,6)])
    Estim$adjfreq <- Estim$freq*Estim$weigh
    Estim <- Estim[c(1,2,5)]
    
    plotData <- plotData[plotData$Type=='Article',]
    plotData <- droplevels(plotData)
    plotData <- rbind.fill(plotData, data.frame(Access=as.factor(NA), 
                                                Jaar=Estim$Jaar,
                                                Bron=Estim$Bron,
                                                BronSoort=as.factor(NA),
                                                Type='Estimate',
                                                lang=as.factor(NA),
                                                freq=Estim$adjfreq))
    KUOZ <- VSNUcmpr$doc1[VSNUcmpr$doc1$Type %in% c('Article', 'Totaal_Artikel')&VSNUcmpr$doc1$Uni!='OU',]
    temp <- read.csv2(paste0(Paths$Params,'/UniNaamOmzet.csv'), stringsAsFactors = F)
    plotData <- rbind.fill(plotData, data.frame(Access=as.factor(NA),
                                                Jaar=as.numeric(as.character(KUOZ$jaar)),
                                                Bron=as.factor(vapply(KUOZ$Uni, function(x) {temp$NARCISNaam[temp$VSNUNaam==x]}, character(1))),
                                                Type=KUOZ$Bron,
                                                lang=as.factor(NA),
                                                freq=KUOZ$count))
    PP$PercBase <- plyr::count(KUOZ[KUOZ$Bron=='KUOZ',], vars=c('jaar','Uni'), wt_var = 'count')
    PP$PercBase$group <- vapply(PP$PercBase$Uni, function(x) {temp$NARCISNaam[temp$VSNUNaam==x]}, character(1))
    PP$PercBase <- PP$PercBase[c('jaar', 'group', 'freq')]
    temp <- data.frame(x=unique(PP$PercBase$jaar),
                       group='Average',
                       freq=sapply(unique(PP$PercBase$jaar), function(x) {sum(PP$PercBase$freq[PP$PercBase$jaar==x])}))
    PP$PercBase <- data.frame(x=rep(PP$PercBase$jaar, times=4),
                              group=paste0(rep(PP$PercBase$group, times=4), rep(levels(plotData$Type), each=nrow(PP$PercBase))),
                              freq=rep(PP$PercBase$freq, times=4))
    PP$PercBase <- rbind.fill(PP$PercBase, temp)
    rm(temp)
    
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$plotLineType <- 'Type'
    PP$includeSumm <- T
    PP$Titel <- 'Compare estimate and VSNU 1'
    PP$LegDynamicHeight <- F
    PP$LegWidth <- list(linetype=2, rest=1)
    plotData <- plotData[plotData$Type=='Estimate',]
    
  } # Compare to KUOZ doc 1
  if(17 %in% StandardPlot) {
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    VSNUcmpr <- ReadVSNUpdf()
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2008,2016, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)') | plotData$Bron=='Open Universiteit',]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))

    Estim <- plotData
    Estim$weighfreq <- Estim$freq*sapply(Estim$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    Estim <- Estim[!is.na(Estim$Jaar) & Estim$Jaar>=Jaren[1] & Estim$Jaar<=Jaren[2],c(2,3,5,7:ncol(Estim))]
    EstimArticles <- plyr::count(Estim[Estim$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    EstimTotals <- plyr::count(Estim, vars=c('Jaar','Bron'), wt_var = 'weighfreq')
    EstimVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    EstimVSNU <- plyr::count(EstimVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    EstimWeigh <- merge(EstimArticles, EstimTotals, by=c('Jaar','Bron'))
    names(EstimWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    EstimWeigh <- merge(EstimWeigh, EstimVSNU, by=c('Jaar','Bron'))
    EstimWeigh$weigh <- EstimWeigh$Articles/sapply(1:nrow(EstimWeigh), function(n) {
      min(EstimWeigh[n,4:5])
    })
    Estim <- merge(EstimVSNU, EstimWeigh[c(1,2,6)])
    Estim$adjfreq <- Estim$freq*Estim$weigh
    Estim <- Estim[c(1,2,5)]
    
    plotData <- data.frame(Jaar=unique(Estim$Jaar),
                           Type='Estimate',
                           Bron='Estimate',
                           freq=sapply(unique(Estim$Jaar), function(x) {sum(Estim$adjfreq[Estim$Jaar==x])}))
    KUOZ <- VSNUcmpr$doc2[VSNUcmpr$doc2$HoofdType=='Article',]
    KUOZ <- droplevels(KUOZ)
    levels(KUOZ$Type)[levels(KUOZ$Type)=='Article'] <- 'Articles in NARCIS'
    KUOZ$count <- as.numeric(as.character(KUOZ$count))
    plotData <- rbind.fill(plotData, data.frame(Jaar=as.numeric(as.character(KUOZ$jaar)),
                                                Type=KUOZ$Type,
                                                Bron=KUOZ$Bron,
                                                freq=KUOZ$count))
    PP$PercBase <- plyr::count(KUOZ[KUOZ$Bron=='KUOZ',], vars=c('jaar'), wt_var = 'count')
    PP$PercBase$jaar <- as.numeric(PP$PercBase$jaar)
    plotData <- rbind.fill(plotData, data.frame(Jaar=PP$PercBase$jaar,
                                                Type='Total KUOZ (100%)',
                                                Bron='KUOZ',
                                                freq=PP$PercBase$freq))
    PP$PercBase <- data.frame(x=rep(PP$PercBase$jaar, times=6),
                              group=rep(levels(plotData$Type), each=nrow(PP$PercBase)),
                              freq=rep(PP$PercBase$freq, times=6))
    
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Type'
    PP$plotSymbol <- 'Type'
    PP$includeSumm <- F
    PP$kleuren <- c('#FF0000','#00CC00','#D0D000','#0000FF','#FF00FF','#000000')
    PP$Titel <- 'Compare estimate and VSNU 2'
    
  } # Compare to KUOZ doc 2
  if(22 %in% StandardPlot) {
    #Selectie
    Jaren <- c(2000,2016, F)
    
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    PP$PercBase <- plotData
    PP$PercBase$freq <- PP$PercBase$freq*sapply(PP$PercBase$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$Jaar) & PP$PercBase$Jaar>=Jaren[1] & PP$PercBase$Jaar<=Jaren[2],c(2,3,5,7)]
    PP$PercBaseArticles <- plyr::count(PP$PercBase[PP$PercBase$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseTotals <- plyr::count(PP$PercBase, vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    PP$PercBaseVSNU <- plyr::count(PP$PercBaseVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    PP$PercBaseWeigh <- merge(PP$PercBaseArticles, PP$PercBaseTotals, by=c('Jaar','Bron'))
    names(PP$PercBaseWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    PP$PercBaseWeigh <- merge(PP$PercBaseWeigh, PP$PercBaseVSNU, by=c('Jaar','Bron'))
    PP$PercBaseWeigh$weigh <- PP$PercBaseWeigh$Articles/sapply(1:nrow(PP$PercBaseWeigh), function(n) {
      min(PP$PercBaseWeigh[n,4:5])
    })
    PP$PercBase <- merge(PP$PercBaseVSNU, PP$PercBaseWeigh[c(1,2,6)])
    PP$PercBase$adjfreq <- PP$PercBase$freq*PP$PercBase$weigh
    PP$SecLine <- PP$PercBase[c(1,2,5)]
    names(PP$SecLine) <- c('Jaar','Bron','freq')
    PP$SecLine$Categorie <- factor('Estimate')
    VSNU <- rbind.fill(VSNU, PP$SecLine)
    
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    KUOZ <- ReadVSNUpdf()$doc2
    KUOZ <- KUOZ[KUOZ$Bron=='KUOZ' & KUOZ$HoofdType=='Article',]
    KUOZ <- plyr::count(KUOZ, vars='jaar', wt_var = 'count')
    KUOZ$Categorie <- factor('KUOZ')
    KUOZ$Bron <- factor(NA)
    KUOZ$Jaar <- as.numeric(KUOZ$jaar)
    KUOZ$jaar <- NULL
    VSNU <- rbind.fill(VSNU, KUOZ)
    
    PP$PercBase <- NULL
    
    plotData <- plotData[plotData$Access %in% c('Open','Closed'),]
    plotData <- plotData[plotData$Type %in% c('Article'),]
    
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed', 'Open')
    PP$kleuren <- 'FromFile'
    #PP$plotPerc <- 'Open'
    #PP$ylab2 <- 'Percentage Open Access'
    PP$LegTitle <- list(NULL)
    
    if(PP$LabelsLang=='en') {
      PP$xlab <- 'Year of publication'
      PP$ylab <- 'Number of articles'
      PP$Titel <- 'Open access of articles with different totals and estimates'
      PP$LegLblAlpha <- c('Total amount of\nresearch output\n(VSNU)',
                          'Estimated number\nof articles\n(based on VSNU)',
                          'KUOZ (VSNU):\nTotal number\nof articles')
    } else {
      PP$xlab <- 'Jaar van publicatie'
      PP$ylab <- 'Aantal artikelen'
      PP$Titel <- 'Open access voor artikelen, met totalen en schatting'
      PP$LegLblAlpha <- c('Totale wetensch.\noutput',
                          'Schatting\nartikelen -\no.b.v VSNU',
                          'Totaal aantal\nartikelen\n(VSNU)')
    }
    
    PP$SecLine <- list(What='VSNUTotals', Total=c('Wetensch', 'Vak'), Articleestimate=c('Estimate'), KUOZ=c('KUOZ'))
    #names(PP$SecLine) <- c('What','Total (VSNU)')
    PP$extrakleuren <- list(VSNULine=c('#D02020','#00FF00','#0000FF'))
    PP$LegLblLineColor <- PP$LegLblAlpha
    PP$LegLblLinetype <- rep('constant',3)
    PP$LegWidth <- list(fill=1, rest=2)
    PP$LegHeight <- list(fill=2, rest=4)
    
  } # NR 3B: Open Access voor artikelen met KUOZ
  if(23 %in% StandardPlot) {
    #Selectie
    Jaren <- c(2008,2016, F)
    
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    KUOZ <- ReadVSNUpdf()$doc2
    KUOZ <- KUOZ[KUOZ$Bron=='KUOZ' & KUOZ$HoofdType=='Article',]
    KUOZ <- plyr::count(KUOZ, vars='jaar', wt_var = 'count')
    KUOZ$Categorie <- factor('KUOZ')
    KUOZ$Bron <- factor(NA)
    KUOZ$Jaar <- as.numeric(KUOZ$jaar)
    KUOZ$jaar <- NULL
    VSNU <- KUOZ
    
    plotData <- plotData[plotData$Access %in% c('Open','Closed'),]
    plotData <- plotData[plotData$Type %in% c('Article'),]
    
    
    #Plotparameters
    PP$plotType <- 'Histo'
    PP$plotX <- 'Jaar'
    PP$xlab <- 'Year of publication'
    PP$ylab <- 'Number of articles'
    PP$plotCol <- 'Access'
    PP$ColOrder <- c('Closed', 'Open')
    PP$kleuren <- 'FromFile'
    #PP$plotPerc <- 'Open'
    #PP$ylab2 <- 'Percentage Open Access'
    PP$LegTitle <- list(NULL)
    PP$Titel <- 'Open access of articles'
    
    PP$SecLine <- list(What='VSNUTotals', KUOZ=c('KUOZ'))
    #names(PP$SecLine) <- c('What','Total (VSNU)')
    PP$extrakleuren <- list(VSNULine=c('#D02020'))
    PP$LegLblLinetype <- c('KUOZ (VSNU):\nTotal number\nof articles')
    PP$LegLblAlpha <- PP$LegLblLinetype
    PP$LegLblLineColor <- PP$LegLblLinetype
    PP$LegWidth <- list(fill=1, rest=2)
    PP$LegHeight <- list(fill=2, rest=4)
    
  } # NR 3C: Open Access voor artikelen met KUOZ, zonder schatting, 2008-2016
  if(24 %in% StandardPlot) {
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2016, F)
    plotData <- plotData[plotData$Type %in% c('Doctoral Thesis') & plotData$BronSoort %in% c('Universiteit (4TU)', 'Universiteit (alg)'),]
    PP$PercBase <- VSNU[VSNU$Categorie=='Thesis',2:4]
    names(PP$PercBase) <- c('group','x','freq')
    
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$includeSumm <- F
  } # Compleetheid theses
  if(34 %in% StandardPlot) {
    
    # Uitleg van de weging:
    #------------------------------
    # We kijken eerst hoeveel publicaties er zijn die vallen binnen de VSNU-definitie van publicatie
    # Gebaseerd op Param/PubWeigh.csv (PhD=altijd publ, boek= 90% publicatie, ...)
    # Dan kijken we eerst of dit meer is dan de VSNU zegt. Zo ja, dan gaan we er van uit dat we alle artikelen in NARCIS hebben,
    # en wordt dit onze schatting van het daadwerkelijk aantal artikelen.
    # Zijn het er minder, dan gaan we er van uit dat de verhouding artikelen/publicaties in NARCIS klopt, deze verhouding passen we toe op VSNU.
    # Voorbeeld: NARCIS heeft 100 artikelen (50 OA), 100 PhDs. VSNU zegt 150 publicaties.
    # Schatting is dan daadwerkelijk 100 artikelen. Percentage in grafiek is dan 50 %
    # Voorbeeld 2: NARCIS heeft 100 artikelen (50 OA), 50 PhDs. VSNU zegt 200 publicaties.
    # Schatting is dan 66.7% van alle pubs is een artikel --> er zijn in werkelijkheid 133.3 artikelen.
    # Percentage in grafiek is dan 50/133.3 = 37.5 %
    
    # Eerst 2016 KUOZ-cijfers
    source(paste0(Paths$RCode,'/Help/VSNU-pdf.R'))
    KUOZ <- ReadVSNUpdf()$doc1
    KUOZ <- KUOZ[KUOZ$Bron=='KUOZ' & KUOZ$Type=='Totaal_Artikel' & KUOZ$jaar==2016 & KUOZ$Uni!='OU',]
    KUOZ$jaar <- as.numeric(as.character(KUOZ$jaar))
    temp <- read.csv2(paste0(Paths$Params,'/UniNaamOmzet.csv'), stringsAsFactors = F)
    KUOZ$Uni <- droplevels(KUOZ$Uni)
    levels(KUOZ$Uni) <- vapply(levels(KUOZ$Uni), function(x) {temp$NARCISNaam[temp$VSNUNaam==x]}, character(1))
    rm(temp)
    KUOZ <- KUOZ[,4:6]
    names(KUOZ) <- c("Jaar","Bron","adjfreq")
    
    #Selectie
    plotData <- plotData[plotData$set.publication,]
    plotData <- plotData[c(1:(ncol(plotData)-11),ncol(plotData))]
    Jaren <- c(2000,2016, F)
    plotData <- plotData[plotData$BronSoort %in% c('Universiteit (4TU)','Universiteit (alg)'),]
    PP$PubWeigh <- read.csv2(paste0(Paths$Params,'/PubWeigh.csv'))
    PP$PercBase <- plotData
    PP$PercBase$freq <- PP$PercBase$freq*sapply(PP$PercBase$Type,function(x) {
      PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    })
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$Jaar) & PP$PercBase$Jaar>=Jaren[1] & PP$PercBase$Jaar<=Jaren[2],c(2,3,5,7:ncol(PP$PercBase))]
    PP$PercBaseArticles <- plyr::count(PP$PercBase[PP$PercBase$Type=='Article',], vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseTotals <- plyr::count(PP$PercBase, vars=c('Jaar','Bron'), wt_var = 'freq')
    PP$PercBaseVSNU <- VSNU[VSNU$Categorie %in% c('Wetensch', 'Vak'),c(3,2,4)]
    PP$PercBaseVSNU <- plyr::count(PP$PercBaseVSNU, vars=c('Jaar','Bron'), wt_var='freq')
    PP$PercBaseWeigh <- merge(PP$PercBaseArticles, PP$PercBaseTotals, by=c('Jaar','Bron'))
    names(PP$PercBaseWeigh) <- c('Jaar','Bron','Articles','NARCTot')
    PP$PercBaseWeigh <- merge(PP$PercBaseWeigh, PP$PercBaseVSNU, by=c('Jaar','Bron'))
    PP$PercBaseWeigh$weigh <- PP$PercBaseWeigh$Articles/sapply(1:nrow(PP$PercBaseWeigh), function(n) {
      min(PP$PercBaseWeigh[n,4:5])
    })
    PP$PercBase <- merge(PP$PercBaseVSNU, PP$PercBaseWeigh[c(1,2,6)])
    PP$PercBase$adjfreq <- PP$PercBase$freq*PP$PercBase$weigh
    PP$PercBase <- PP$PercBase[c(1,2,5)]
    PP$PercBase <- rbind.fill(PP$PercBase, KUOZ)
    
    plotData <- plotData[plotData$Type=='Article',]
    #plotData$freq <- plotData$freq*sapply(plotData$Type,function(x) {
    #  PP$PubWeigh$WeighPerc[PP$PubWeigh$Type==x]
    #})
    names(PP$PercBase) <- c('x', 'group', 'freq')
    
    
    #Plotparameters
    PP$Zoomfactor <- PP$Zoomfactor*.8
    PP$plotType <- 'Lijn1'
    PP$plotX <- 'Jaar'
    PP$plotCol <- 'Bron'
    PP$plotSymbol <- 'Bron'
    PP$epsFriendly <- F
    PP$AddRange <- list(What='SD',which='Average',range=1, alpha=.333)
    PP$LegTitle <- list(NULL)
    PP$Anonimize <- T
    PP$LegDynamicHeight <- F
    PP$TextSizeLblY <- 12
    PP$TextSizeTitle <- 18
    
    if(PP$LabelsLang=='en') {
      PP$Titel <- 'Open Access of articles across Dutch universities'
      PP$xlab <- 'Year of publication'
      PP$ylab <- expression(paste(over('OA articles (NARCIS)','Total articles (estim. based on VSNU)'),'  %'))
      PP$LegLbl <- c('',' ','Individual',substring('                 ',1,2:8), 'Universities',substring('               ',1,9:10),'\n\nAverage\n\n')
      PP$LegLblSD <- 'Area within\n1 standard\ndeviation from\naverage'
    } else {
      PP$Titel <- 'Open Access voor artikelen, voor verschillende universiteiten'
      PP$xlab <- 'Jaar van publicatie'
      PP$ylab <- expression(paste(over('OA artikelen (NARCIS)','Schatting artikelen - o.b.v VSNU'),'  %'))
      PP$LegLbl <- c('',' ','Individuele',substring('                 ',1,2:8), 'Universiteiten',substring('               ',1,9:10),'\n\nGemiddelde\n\n')
      PP$LegLblSD <- 'Gebied binnen\n1 standaard\ndeviatie van\nhetgemiddelde'
    }
  } # NR 4: Open Access totalen als percentage VSNU met schatting
  if(99 %in% StandardPlot) {
    PP$plotType <- 'ColorCheck'
  } # ColorCheck
  
  if (exists('Jaren') && !is.null(Jaren)) {
    plotData$Jaar[plotData$Jaar<Jaren[1]] <- Jaren[1]-1
    plotData$Jaar[plotData$Jaar>Jaren[2]] <- Jaren[2]+1
    plotData$Jaar[is.na(plotData$Jaar)] <- Jaren[2]+2
    if (Jaren[3]) {      # TRUE voor restjaren, False voor selectie
      plotData$Jaar <- factor(plotData$Jaar)
      levels(plotData$Jaar) <- c('Before', levels(plotData$Jaar)[2:(nlevels(plotData$Jaar)-2)],'After','Unknown')
    } else {
      plotData <- plotData[plotData$Jaar>=Jaren[1] & plotData$Jaar<=Jaren[2],]
      plotData$Jaar <- factor(plotData$Jaar)
    }
    if(PP$plotX=='Jaar' && is.null(PP$includeSumm)) {PP$includeSumm <- as.logical(Jaren[3]) || PP$plotType!='Histo'}
  } #Verdere selectie
  if(T) {
    if(is.null(PP$plotType)) {} # Throw Error!
    if(is.null(PP$plotX)) {} # Throw Error!
    if(is.null(PP$plotCol)) {} # Throw Error!
    if(is.null(PP$plotSymbol)) {} # Is fine
    if(is.null(PP$plotLineType)) {} # Is fine
    if(is.null(PP$SecLine)) {} # Is fine
    if(is.null(PP$xlims)) {} # Is fine
    if(is.null(PP$SubTitle)) {} # Is fine
    if(is.null(PP$AddRange)) {} # Is fine
    if(is.null(PP$Anonimize)) {PP$Anonimize <- F}
    if(is.null(PP$SaveResolution)) {PP$SaveResolution <- c(1600,1200)}
    if(is.null(PP$Savedpi)) {PP$Savedpi <- 300}
    if(is.null(PP$coord)) {PP$coord <- 'Normal'}
    if(is.null(PP$includeSumm)) {PP$includeSumm <- (PP$coord!='polar' && length(PP$plotX)==1)}
    if(is.null(PP$InclSummPos) && PP$includeSumm) {PP$InclSummPos <- 'right'}
    if(is.null(PP$kleuren)) {PP$kleuren <- 'Auto'}
    if(is.null(PP$extrakleuren$VSNULine) && !is.null(PP$SecLine$What) && PP$SecLine$What=='VSNUTotals') {PP$extrakleuren$VSNULine <- '#FF0000'}
    if(is.null(PP$extrakleuren$NormalLineThickness) && PP$plotType %in% c('Lijn1')) {PP$extrakleuren$NormalLineThickness <- .6}
    if(is.null(PP$extrakleuren$AvgLineCol) && PP$includeSumm && PP$plotType=='Lijn1') {PP$extrakleuren$AvgLineCol <- '#000000'}
    if(is.null(PP$extrakleuren$AvgLineShape) && PP$includeSumm && PP$plotType=='Lijn1') {PP$extrakleuren$AvgLineShape <- 15}
    if(is.null(PP$extrakleuren$AvgLineThickness) && PP$plotType %in% c('Lijn1') && PP$includeSumm) {PP$extrakleuren$AvgLineThickness <- 1.2}
    if(is.null(PP$extrakleuren$AvgLineLinetype) && PP$plotType %in% c('Lijn1') && PP$includeSumm) {PP$extrakleuren$AvgLineLinetype <- 1}
    if(is.null(PP$extrakleuren$RefLineCol) && !is.null(PP$SecLine) && PP$plotType=='Lijn1') {PP$extrakleuren$RefLineCol <- '#000000'}
    if(is.null(PP$extrakleuren$RefLineShape) && !is.null(PP$SecLine) && PP$plotType=='Lijn1') {PP$extrakleuren$RefLineShape <- NA}
    if(is.null(PP$extrakleuren$RefLineThickness) && PP$plotType %in% c('Lijn1') && !is.null(PP$SecLine)) {PP$extrakleuren$RefLineThickness <- 1}
    if(is.null(PP$extrakleuren$RefLineLinetype) && PP$plotType %in% c('Lijn1') && !is.null(PP$SecLine)) {PP$extrakleuren$RefLineLinetype <- 3}
    if(is.null(PP$colstep) && PP$kleuren=='Auto') {PP$colstep <- nlevels(plotData[PP$plotCol]) %/% 5 +1}
    if(is.null(PP$XOrder) && PP$plotType %in% c('Histo')) {PP$XOrder <- '-'}
    if((!is.null(PP$XOrder))&&length(PP$XOrder)==1&&(PP$XOrder %in% c('ValProp','RevValProp')) && is.null(PP$XOrderPropVal)) {PP$XOrderPropVal <- 'AutoLow'}
    if(PP$plotType %in% c('Histo') && is.null(PP$ColOrder)) {PP$ColOrder <- '-'} else if (length(PP$ColOrder)>1 && all(unique(plotData[,PP$plotCol]) %in% PP$ColOrder)) {
      plotData[PP$plotCol] <- factor(plotData[,PP$plotCol], levels=PP$ColOrder)
    }
    if(length(PP$kleuren)==1 && PP$kleuren=='FromFile') {
      standklr <- read.csv2(paste0(Paths$Params,'/PlotKleuren.csv'))
      standklr <- standklr[standklr$Hoofdcat=='eScience',c(2,3,4)]
      PP$kleuren <- c(sapply(levels(plotData[,PP$plotCol]), function(x) {
        as.character(standklr$col[standklr$var==PP$plotCol & standklr$val==as.character(x)])
      }), sapply(levels(plotData[,PP$plotCol]), function(x) {
        as.character(standklr$col[standklr$var==paste0(PP$plotCol,'-bg') & standklr$val==as.character(x)])
      }), use.names=F)
      PP$kleuren <- sapply(PP$kleuren, function(x) {
        if(length(x)==1) {return(x)} else {return('Auto')}
      })
      if(!is.null(PP$plotPerc)) {
        PP$kleuren <- c(PP$kleuren, 
                        as.character(standklr$col[standklr$var=='percLine' & standklr$val=='OnBg']),
                        as.character(standklr$col[standklr$var=='percLine' & standklr$val=='Line']))
      }
    }
    if(PP$plotType %in% c('Histo') && is.null(PP$BarPosit)) {PP$BarPosit <- 'stack'}
    if(PP$plotType %in% c('Histo') && is.null(PP$BarWidth)) {PP$BarWidth <- ifelse(PP$BarPosit=='fill','log','const.9')}
    if(PP$plotType %in% c('Histo') && is.null(PP$BarGap)) {PP$BarGap <- ifelse(substr(PP$BarWidth,1,5)=='const', 1-as.numeric(substring(PP$BarWidth,6)),(PP$coord!='polar')*.1)}
    if(is.null(PP$TotalXLab) && PP$plotType %in% c('Histo')) {PP$TotalXLab <- ifelse(PP$BarPosit=='fill', 'All','All')}
    if(is.null(PP$polarArea) && PP$coord=='polar') {PP$polarArea <- T}
    if(is.null(PP$polarRadius) && PP$coord=='polar') {PP$polarRadius <- 'TextAllign1.25'}
    if(is.null(PP$plotPerc)) {} else {if(is.null(PP$ylab2)) {PP$ylab2 <- paste('Percentage',PP$plotPerc)}}
    if(PP$plotType %in% c('Lijn1') && is.null(PP$PercBase)) {
      PP$PercBase <- VSNU[VSNU$Categorie=='Wetensch' & VSNU$Bron %in% unique(plotData$Bron),c(3,2,4)]
      names(PP$PercBase) <- c('x', 'group', 'freq')
    }
    if(is.null(PP$AddRange$color) && (PP$plotType %in% c('Lijn1')) && !is.null(PP$AddRange)) {PP$AddRange$color <- 'black'}
    
    if(PP$plotType %in% c('Scatter') && is.null(PP$plotGroups)) {PP$plotGroups <- c('Bron')}
    if(PP$plotType %in% c('Scatter') && is.null(PP$plotCol)) {PP$plotCol <- c('-')}
    if(PP$plotType %in% c('Scatter') && is.null(PP$plotSymbol)) {PP$plotSymbol <- c('-')}
    if(PP$plotType %in% c('Scatter') && is.null(PP$plotAlpha)) {PP$plotAlpha <- c('-')}
    if(PP$plotType %in% c('Scatter') && is.null(PP$pointsize)) {PP$pointsize <- 4}
    if(is.null(PP$xlab) && PP$coord!='polar') {PP$xlab <- PP$plotX}
    if(is.null(PP[['ylab']]) && PP$coord!='polar') {
      if((!is.null(PP$BarPosit) && PP$BarPosit=='fill') || PP$plotType=='Lijn1') {
        PP$ylab <- 'Percentage'
      } else {
        PP$ylab <- 'Total amount'
      }
    }
    if(is.null(PP$Titel)) {
      PP$Titel <- ''
    }
    if(is.null(PP$FileTitle)) {PP$FileTitle <- PP$Titel}
    if(PP$FileTitle=='') {
      PP$FileTitle <- 'NoName'
    }
    if(is.null(PP$LegTitleCol) && is.null(PP[['LegTitle']]) && !is.null(PP$plotCol)) {PP$LegTitleCol <- paste0(toupper(substr(PP$plotCol, 1, 1)), tolower(substring(PP$plotCol, 2)))}
    if(is.null(PP$LegTitleLineType) && is.null(PP[['LegTitle']]) && !is.null(PP$plotLineType)) {PP$LegTitleLineType <- paste0(toupper(substr(PP$plotLineType, 1, 1)), tolower(substring(PP$plotLineType, 2)))}
    if(is.null(PP$LegTitleSymbol) && is.null(PP[['LegTitle']]) && !is.null(PP$plotSymbol)) {PP$LegTitleSymbol <- paste0(toupper(substr(PP$plotSymbol, 1, 1)), tolower(substring(PP$plotSymbol, 2)))}
    if(is.null(PP$LegTitleSize) && is.null(PP[['LegTitle']]) && !is.null(PP$plotSize)) {PP$LegTitleSize <- paste0(toupper(substr(PP$plotSize, 1, 1)), tolower(substring(PP$plotSize, 2)))}
    if(is.null(PP$LegTitleAlpha) && is.null(PP[['LegTitle']]) && !is.null(PP$plotAlpha)) {PP$LegTitleAlpha <- paste0(toupper(substr(PP$plotAlpha, 1, 1)), tolower(substring(PP$plotAlpha, 2)))}
    if(is.null(PP[['LegTitle']])) {PP$LegTitle <- paste0(toupper(substr(PP$plotCol, 1, 1)), tolower(substring(PP$plotCol, 2)))}
    if(is.null(PP$LegTitleCol)) {PP$LegTitleCol <- PP$LegTitle}
    if(is.null(PP$LegTitleLineType)) {PP$LegTitleLineType <- PP$LegTitle}
    if(is.null(PP$LegTitleSymbol)) {PP$LegTitleSymbol <- PP$LegTitle}
    if(is.null(PP$LegTitleSize)) {PP$LegTitleSize <- PP$LegTitle}
    if(is.null(PP$LegTitleAlpha)) {PP$LegTitleAlpha <- PP$LegTitle}
    if(is.null(PP[['LegTitleLine']])) {} # Is fine, NULL is no title, '' is empty line
    if(is.null(PP[['LegLbl']])) {} # Repeated later, to fill from others
    if(is.null(PP$LegLblFill) && is.null(PP[['LegLbl']]) && PP$plotCol=='Access') {
      PP$LegLblFill <- paste0(as.character(levels(plotData$Access)[levels(plotData$Access) %in% unique(plotData[,PP$plotCol])]),'\naccess')
    }
    if(is.null(PP$LegLblFill) && !is.null(PP[['LegLbl']])) {
      PP$LegLblFill <- PP[['LegLbl']]
    }
    if(is.null(PP$LegLblCol) && !is.null(PP[['LegLbl']])) {
      PP$LegLblCol <- PP[['LegLbl']]
    }
    if(is.null(PP$LegLblLinetype) && !is.null(PP[['LegLbl']])) {
      PP$LegLblLineType <- PP[['LegLbl']]
    }
    if(is.null(PP$LegLblAlpha)  && !is.null(PP[['LegLbl']])) {
      PP$LegLblAlpha <- PP[['LegLbl']]
    }
    if(is.null(PP$LegLblSymbol)  && !is.null(PP[['LegLbl']])) {
      PP$LegLblSymbol <- PP[['LegLbl']]
    }
    if(is.null(PP$LegLblLineColor)  && !is.null(PP[['LegLbl']])) {
      PP$LegLblLineColor <- PP[['LegLbl']]
    }
    if(is.null(PP$LegDynamicHeight)) {PP$LegDynamicHeight <- is.null(PP$LegHeight)}
    if(is.null(PP$LegHeight) && !PP$LegDynamicHeight) {PP$LegHeight <- list(rest=1)}
    if(is.null(PP$LegWidth)) {PP$LegWidth <- list(rest=1)}
    if(is.null(PP$TextSizeTickX)) {PP$TextSizeTickX <- 12}
    if(is.null(PP$TextSizeTickY)) {PP$TextSizeTickY <- PP$TextSizeTickX}
    if(is.null(PP$TextSizeTick2)) {PP$TextSizeTick2 <- PP$TextSizeTickX}
    if(is.null(PP$TextSizeTitle)) {PP$TextSizeTitle <- 24}
    if(is.null(PP$TextSizeSubTitle) && !is.null(PP$SubTitle)) {PP$TextSizeSubTitle <- 12}
    if(is.null(PP$TextSizeLblX)) {PP$TextSizeLblX <- 18}
    if(is.null(PP$TextSizeLblY)) {PP$TextSizeLblY <- PP$TextSizeLblX}
    if(is.null(PP$TextSizeLegTitle)) {PP$TextSizeLegTitle <- 18}
    if(is.null(PP$TextSizeLegLbls)) {PP$TextSizeLegLbls <- 12}
    if(is.null(PP$TextSizeFacetHead)) {PP$TextSizeFacetHead <- 12}
    if(is.null(PP$TextFont)) {PP$TextFont <- 'Verdana'}
    if(is.null(PP$Zoomfactor)) {PP$Zoomfactor <- 1}
    if(is.null(PP$BackgroundColorPlot) && PP$coord=='polar')  {}
    if(PP$coord=='polar' && substr(PP$polarRadius,1,10)=='TextAllign') {
      PP$polarRadius <- max((strwidth(unique(plotData[,PP$plotX]), units='inches')*PP$TextSizeTickX/15)+1.02)/as.numeric(substring(PP$polarRadius,11))
    }
    if(!is.null(PP$polarRadius)) {PP$Zoomfactor <- 1.5*PP$Zoomfactor/PP$polarRadius}
    if(is.null(PP$SavePlot)) {PP$SavePlot <- F}
    if(is.null(PP$SaveGglPlot)) {PP$SaveGglPlot <- F}
    if(is.null(PP$WerksetDate)) {
      temp <- regexpr('[0-9]{8,}', Paths$WerksetTotal)
      temp <- substr(Paths$WerksetTotal, temp, temp+7)
      PP$WerksetDate <- as.Date(temp, format='%Y%m%d')
    }
    PP$extrakleuren$NormalLineThickness <- PP$extrakleuren$NormalLineThickness*PP$Zoomfactor
    PP$extrakleuren$AvgLineThickness <- PP$extrakleuren$AvgLineThickness*PP$Zoomfactor
    PP$extrakleuren$RefLineThickness <- PP$extrakleuren$RefLineThickness*PP$Zoomfactor
    PP$LegWidth <- lapply(PP$LegWidth, `*`, PP$Zoomfactor)
    PP$LegHeight <- lapply(PP$LegHeight, `*`, PP$Zoomfactor)
    PP$TextSizeTitle <- PP$TextSizeTitle*PP$Zoomfactor
    PP$TextSizeSubTitle <- PP$TextSizeSubTitle*PP$Zoomfactor
    PP$TextSizeLblX <- PP$TextSizeLblX*PP$Zoomfactor
    PP$TextSizeLblY <- PP$TextSizeLblY*PP$Zoomfactor
    PP$TextSizeLegLbls <- PP$TextSizeLegLbls*PP$Zoomfactor
    PP$TextSizeLegTitle <- PP$TextSizeLegTitle*PP$Zoomfactor
    PP$TextSizeTick2 <- PP$TextSizeTick2*PP$Zoomfactor
    PP$TextSizeTickX <- PP$TextSizeTickX*PP$Zoomfactor
    PP$TextSizeTickY <- PP$TextSizeTickY*PP$Zoomfactor
    PP$TextSizeFacetHead <- PP$TextSizeFacetHead*PP$Zoomfactor
  } # Standard values
  
  if (PP$plotType=='Histo') {
    plotVals <- plyr::count(plotData, vars=c(PP$plotX,PP$plotCol), wt_var='freq')
    names(plotVals) <- c('x',paste0(rep('x',length(PP$plotX)-1),seq(from=2,to=length(PP$plotX), length.out=length(PP$plotX)-1)),'col','freq')
    droplvls <- !(levels(plotVals$col) %in% unique(plotVals$col))
    if(any(droplvls)) {
      if(length(PP$kleuren)>nlevels(plotVals$col)) {
        PP$kleuren <- PP$kleuren[c(rep(T,nlevels(plotVals$col)),!droplvls,T,T)]
      }
      if(length(PP$kleuren)>1) {
        PP$kleuren <- PP$kleuren[-which(droplvls)]
      }
      plotVals$col <- factor(plotVals$col, levels=levels(plotVals$col)[!droplvls])
    }
    plotVals <- droplevels(plotVals)
    if(length(PP$XOrder)>1) {
      if(all(levels(plotVals$x) %in% PP$XOrder)) {
        plotVals$x <- factor(plotVals$x, levels=PP$XOrder)
      } else {
        print("Manual ordering for x-axis is incomplete")
        readline("Press any key to continue (natural ordering is used)")
      }
    }
    if(!is.null(PP$Categorise)) {
      plotVals$facet <- sapply(plotVals$x,function(n) {
        names(PP$Categorise)[sapply(PP$Categorise, function(m) {n %in% m})]
      })
      if(is.list(plotVals$facet)) {
        plotVals$facet[sapply(plotVals$facet, length)==0] <- 'Unspecified'
        plotVals$facet[sapply(plotVals$facet, length)>1] <- 'Specified more then once'
        plotVals$facet <- sapply(plotVals$facet, function(n) {n})
      }
      for(j in 1:length(PP$Categorise)) {
        i <- PP$Categorise[[j]]
        plotValsCats <- data.frame(x=PP$TotalXLab, col=unique(plotVals$col[plotVals$x %in% i]))
        plotValsCats$freq=sapply(plotValsCats$col, function(n) {sum(plotVals$freq[plotVals$x %in% i & plotVals$col==n])})
        plotValsCats$facet <- names(PP$Categorise)[j]
        plotVals <- rbind(plotVals, plotValsCats)
        plotValsCats$x <- names(PP$Categorise)[j]
        plotValsCats$facet <- PP$TotalXLab
        plotVals <- rbind(plotVals, plotValsCats)
      }
      plotValsCats <- data.frame(x=PP$TotalXLab, col=unique(plotVals$col))
      plotValsCats$freq=sapply(plotValsCats$col, function(n) {sum(plotVals$freq[plotVals$col==n])})
      plotValsCats$facet <- PP$TotalXLab
      plotVals <- rbind(plotVals, plotValsCats)
    } else if (PP$includeSumm) {
      plotValsCats <- data.frame(x=PP$TotalXLab, col=unique(plotVals$col))
      plotValsCats$freq=sapply(plotValsCats$col, function(n) {sum(plotVals$freq[plotVals$col==n])})
      if(PP$BarPosit=='fill') {plotValsCats$freq <- plotValsCats$freq/length(unique(plotVals$x))}
      if(PP$InclSummPos=='left') {
        plotVals <- rbind.fill(plotValsCats, plotVals)
      } else {
        plotVals <- rbind.fill(plotVals, plotValsCats)
      }
    }
    plotPercVals <- data.frame(unique(plotVals[!names(plotVals) %in% c('col','freq')]))
    if(length(PP$ColOrder)==1 && PP$ColOrder=='Count') {
      order <- order(sapply(levels(plotVals$col),function(x) {sum(plotVals$freq[!is.na(plotVals$col)&plotVals$col==x])}), decreasing=F)
      plotVals$col <- factor(plotVals$col, levels=c(levels(plotVals$col)[order],'Other/Unknown'))
      plotVals$col[is.na(plotVals$col)] <- 'Other/Unknown'
      PP$LegLblFill <- PP$LegLblFill[order]
    }
    plotVals$cum <- NA   #To-Do: Verder gaan met x2 categoriseren
    plotVals$cum <- sapply(1:nrow(plotVals), function(n) {
      sum(plotVals$freq[plotVals$x==plotVals$x[n] & as.numeric(plotVals$col)>=as.numeric(plotVals$col[n])])
    })
    plotPercVals$total <- sapply(1:nrow(plotPercVals), function(n) {
      sum(plotVals$freq[plotVals$x==plotPercVals$x[n]])
    })
    if(length(PP$XOrder)==1 && (PP$XOrder=='Count' || PP$XOrder=='RevCount')) {
      #Order on number of counts
      plotPercVals$x <- factor(plotPercVals$x, levels=
                               c(ifelse(PP$includeSumm && PP$InclSummPos=='left',PP$TotalXLab,NA),
                                 as.character(unique(plotPercVals$x[plotVals$x!=PP$TotalXLab])[
                                   order(plotPercVals$total[plotPercVals$x!=PP$TotalXLab], decreasing = (PP$XOrder=='Count'))]), 
                                 ifelse(PP$includeSumm && PP$InclSummPos!='left',PP$TotalXLab,NA)))
    }
    if(length(PP$XOrder)==1 && (PP$XOrder %in% c('ValProp','RevValProp'))) {
      if(PP$XOrderPropVal=='AutoLow') {
        PP$XOrderPropVal <- levels(plotVals$col)[nlevels(plotVals$col)]
      }
      plotPercVals$perc <- sapply(1:nrow(plotPercVals), function(i) {
        sum(plotVals$freq[plotVals$x==plotPercVals$x[i] & plotVals$col==PP$XOrderPropVal])/plotPercVals$total[i]
      })
      plotPercVals$x <- factor(plotPercVals$x, levels=
                               c(ifelse(PP$includeSumm && PP$InclSummPos=='left',PP$TotalXLab,NA),
                                 as.character(plotPercVals$x[plotPercVals$x!=PP$TotalXLab][
                                   order(plotPercVals$perc[plotPercVals$x!=PP$TotalXLab], decreasing = PP$XOrder=='ValProp')]),
                                 ifelse(PP$includeSumm && PP$InclSummPos!='left',PP$TotalXLab,NA)))
    }
    if (!is.null(PP$Categorise)) {
      plotVals$facet <- factor(plotVals$facet, levels=levels(plotVals$x)[levels(plotVals$x) %in% unique(plotVals$x[plotVals$facet==PP$TotalXLab])])
    }
    if(length(PP$kleuren)==1 && PP$kleuren=='Auto') {
      PP$kleuren <- colorRampPalette(rainbow(n=nlevels(plotVals$col)))
      PP$kleuren <- PP$kleuren(nlevels(plotVals$col))
      PP$colseq <- seq(from=1, by=PP$colstep, length.out = nlevels(plotVals$col)) %% nlevels(plotVals$col)+1
      PP$kleuren <- c(PP$kleuren[PP$colseq], sapply(PP$kleuren[PP$colseq],ColContrast, USE.NAMES=F),'#000000','#202020')
    }
    if(length(PP$kleuren)>1 && any(PP$kleuren=='Auto')) {
      PP$kleuren[PP$kleuren=='Auto'] <- ColContrast(PP$kleuren[which(PP$kleuren=='Auto')-nlevels(plotVals$col)])
    }
    if(PP$BarPosit=='stack') {
      if(!is.null(PP$Categorise) || !PP$includeSumm) {
        plotVals <- plotVals[plotVals$x!=PP$TotalXLab,]
      }
      if(!is.null(PP$Categorise)) {
        plotVals <- plotVals[plotVals$facet!=PP$TotalXLab,]
      }
      ScaleFac <- max(plotPercVals$total[plotPercVals$x!=PP$TotalXLab])/100
      plotVals$size <- .9
      plotPercVals$size <- .9
    }
    if(PP$BarPosit=='fill') {
      ScaleFac <- .01
    }
    if(substr(PP$BarWidth,1,5)=='const') {
      plotPercVals$size <- as.numeric(substr(PP$BarWidth,6,nchar(PP$BarWidth)))
    } else {
      plotPercVals$size <- plotPercVals$total
    }
    if(PP$BarWidth=='log') {
      plotPercVals$size <- log(plotPercVals$size*exp(1))
      plotPercVals$size <- plotPercVals$size/max(plotPercVals$size)
    }
    if(substr(PP$BarWidth,1,4)=='root') {
      plotPercVals$size <- (plotPercVals$size)^(1/as.numeric(substring(PP$BarWidth,5)))
      plotPercVals$size <- plotPercVals$size/max(plotPercVals$size)
    }
    plotVals$size <- sapply(plotVals$x, function(x) {
      plotPercVals$size[plotPercVals$x==x]
    })
    plotPercVals$xpos <- sapply(plotPercVals$x, function(x) {
      sum(plotPercVals$size[as.numeric(plotPercVals$x)<as.numeric(x)]+PP$BarGap)+
        plotPercVals$size[as.numeric(plotPercVals$x)==as.numeric(x)]/2
    })
    plotVals$xpos <- sapply(plotVals$x, function(x) {
      plotPercVals$xpos[plotPercVals$x==x]
    })
    if(!is.null(PP$plotPerc)) {
      plotPercVals$perc <- sapply(1:nrow(plotPercVals), function(n) {
        sum(plotVals$freq[plotVals$x==plotPercVals$x[n] & plotVals$col==PP$plotPerc])/plotPercVals$total[n]
      })
      plotPercVals$col <- sapply(1:nrow(plotPercVals), function(n) {
        w <- as.numeric(plotVals$col[plotVals$x==plotPercVals$x[n] & plotVals$cum>(plotPercVals$perc[n]*ScaleFac*100)])
        w2 <- as.numeric(plotVals$col[plotVals$x==plotPercVals$x[n] & abs(plotVals$cum-(plotPercVals$perc[n]*ScaleFac*100))<max(1,ScaleFac/10)])
        if(length(w2)!=0) {return(max(1, w2-.5))}
        if(length(w)==0) {return(nlevels(plotVals$col)+1)} else {return(max(w))}
      })
      temp <- which(plotPercVals$col %% 1 !=0)
      plotPercVals$col[temp] <- round(plotPercVals$col[temp]*.9 + plotPercVals$col[max(temp-1,1)]*.05 + plotPercVals$col[min(temp+1,nrow(plotPercVals))]*.05)
      plotPercVals$col <- factor(plotPercVals$col, levels=1:(nlevels(plotVals$col)+1))
    }
    if(!is.null(PP$SecLine)) {
      if(PP$SecLine[[1]]!='VSNUTotals') {
        warning('Second line only implemented for VSNU')
        PP$SecLine <- NULL
      } else {
        plotValsSec <- data.frame(x=rep(unique(plotVals$x), each=length(PP$SecLine)-1), 
                                  xpos=rep(unique(plotVals$xpos), each=length(PP$SecLine)-1), 
                                  whichline=factor(names(PP$SecLine)[-1], levels=names(PP$SecLine)[-1]))
        plotValsSec$freq <- sapply(1:nrow(plotValsSec), function(x) {
          sum(VSNU$freq[VSNU$Categorie %in% PP$SecLine[[as.character(plotValsSec$whichline[x])]] & VSNU[,PP$plotX]==plotValsSec$x[x]])
        })
        plotValsSec$col <- sapply(1:nrow(plotValsSec), function(n) {
          w <- as.numeric(plotVals$col[plotVals$x==plotValsSec$x[n] & plotVals$cum>plotValsSec$freq[n]])
          w2 <- as.numeric(plotVals$col[plotVals$x==plotValsSec$x[n] & plotVals$cum==plotValsSec$freq[n]])
          if(length(w2)!=0) {return(max(1, w2-.5))}
          if(length(w)==0) {return(nlevels(plotVals$col)+1)} else {return(max(w))}
        })
        temp <- which(plotValsSec$col %% 1 !=0)
        plotValsSec$col[temp] <- round(plotValsSec$col[temp]*.9 + plotValsSec$col[max(temp-1,1)]*.05 + plotValsSec$col[min(temp+1,nrow(plotValsSec))]*.05)
        plotValsSec$col <- factor(plotValsSec$col, levels=1:(nlevels(plotVals$col)+1))
        plotValsSec$freq[apply(plotValsSec, 1, function(x) {!any(VSNU[,PP$plotX]==as.character(x['x']) & 
                                                                   VSNU$Categorie %in% PP$SecLine[[as.character(x['whichline'])]])})] <- NA
        ScaleFac <- max(ScaleFac, plotValsSec$freq/100, na.rm=T)
      }
    }
    if(PP$coord=='polar' && PP$polarArea) {
      plotVals$y <- sapply(1:nrow(plotVals), function(i) {
        sqrt(plotVals$cum[i])-sqrt(plotVals$cum[i]-plotVals$freq[i])
      })
      plotPercVals$total <- sapply(plotPercVals$x, function(i) {
        sum(plotVals$y[plotVals$x==i])
      })
      if (PP$BarPosit!='fill') {ScaleFac <- max(plotPercVals$total)/100}
    } else {
      plotVals$y <- plotVals$freq
    }
    if(is.null(PP[['LegLbl']])) {
      if(is.null(PP$LegLblFill)) {
        PP$LegLblFill <- as.character(unique(plotVals$col))
      }
      PP$LegLbl <- PP$LegLblFill
    }
    if(is.null(PP$xvallabs)) {
      PP$xvallabs <- plotPercVals$x
    }
    if(PP$LegDynamicHeight) {
      PP$LegHeight <- list(fill=PP$Zoomfactor*sapply(gregexpr('\\n',PP$LegLblFill), function(n) {sum(n!=-1)+1}),
                           rest=PP$Zoomfactor*sapply(gregexpr('\\n',PP$LegLbl), function(n) {sum(n!=-1)+1}))
    }
    plotRes <- ggplot(data=plotVals) +
      geom_bar(mapping = aes(x=xpos,y=y, fill=col, width=size), stat='identity', position=PP$BarPosit) +
      scale_x_continuous(breaks=plotPercVals$xpos, labels=PP$xvallabs, limits = c(0,max(plotVals$xpos+plotVals$size/2+ifelse(PP$coord=='polar',PP$BarGap,0)))) +
      scale_fill_manual(values=PP$kleuren, labels=PP$LegLblFill) +
      scale_size_identity() +
      labs(y=PP$ylab,x=PP$xlab,title=PP$Titel, subtitle=PP$SubTitle)+
      guides(fill=guide_legend(title=unlist(PP$LegTitle), 
                               keywidth=ifelse(is.null(PP$LegWidth$fill), PP$LegWidth$rest, PP$LegWidth$fill), 
                               keyheight=ifelse(is.null(PP$LegHeight$fill), PP$LegHeight$rest, PP$LegHeight$fill),
                               order=1, reverse=PP$coord=='flip'))
    if(PP$BarPosit=='stack') {
      options(OutDec = ',')
      if(PP$coord=='polar') {
        plotRes <- plotRes + scale_y_continuous(breaks=sqrt(c(0,2e4,1e5,2.5e5,5e5,1e6)), labels = function(x) {format(x^2, big.mark='.', scientific = F)})
      } else {
        plotRes <- plotRes + coord_cartesian(ylim=c(0,ScaleFac*105), expand=F)
      }
    }
    if(PP$BarPosit=='fill') {
      options(OutDec = '.')
      plotRes <- plotRes +
        coord_cartesian(ylim=c(0,ScaleFac*100), expand=F)
      if(PP$coord=='polar') {
        plotRes <- plotRes + 
          scale_y_continuous(labels=NULL) +
          theme(axis.ticks.y = element_blank())
      } else {
        plotRes <- plotRes + scale_y_continuous(labels=scales::percent)
      }
    }
    if(!is.null(PP$plotPerc)) {
      plotRes <- plotRes +
        geom_line(data=plotPercVals, aes(x=xpos, y=perc*100*ScaleFac, group=1, linetype=paste('Percentage',PP$plotPerc)), color=PP$kleuren[length(PP$kleuren)], size=PP$Zoomfactor/1.5, na.rm = T)+ 
        geom_point(data=plotPercVals, aes(x=xpos, y=perc*100*ScaleFac, color=col, alpha='Percentage'), size=PP$Zoomfactor*1.5, na.rm=T)+
        scale_alpha_discrete(limits=c(1,1), labels=paste('Percentage',PP$plotPerc)) +
        scale_color_manual(values=PP$kleuren[(nlevels(plotVals$col)+1):(nlevels(plotVals$col)*2+1)][levels(plotPercVals$col) %in% plotPercVals$col]) +
        scale_y_continuous(sec.axis = sec_axis(~./ScaleFac, name=PP$ylab2, labels=function(x) {paste(x,'%')}),
                           labels= function(x) {format(x, big.mark='.', scientific = F, trim=T)}) +
        guides(linetype=guide_legend(title=unlist(PP[['LegTitleLine']]), order=2), 
               alpha=guide_legend(title=unlist(PP[['LegTitleLine']]), order=2, override.aes = list(alpha=1, size=PP$Zoomfactor*1.5)),
               color='none')
    }
    if(!is.null(PP$Categorise)) {
      if(PP$coord=='flip') {
        plotRes <- plotRes + facet_grid(plotVals$facet~., drop=T, scales='free_y', space='free_y')
      } else {
        plotRes <- plotRes + facet_grid(~plotVals$facet, drop=T, scales='free_x', space='free_x')
      }
    }
    if(!is.null(PP$SecLine)) {
      plotRes <- plotRes +
        geom_line(data=plotValsSec, aes(x=xpos, y=freq, group=whichline, linetype=PP$LegLblLinetype[as.numeric(whichline)], color=sapply(whichline, function(n) {PP$extrakleuren$VSNULine[[n]]})), size=PP$Zoomfactor, na.rm=T) +
        geom_point(data=plotValsSec, aes(x=xpos, y=freq, alpha=whichline, color=sapply(whichline, function(n) {PP$extrakleuren$VSNULine[[n]]})), size=2*PP$Zoomfactor, na.rm=T) +
        scale_linetype(labels=PP$LegLblLinetype) +
        scale_alpha_manual(values=rep(1,nlevels(plotValsSec$whichline)), breaks=levels(plotValsSec$whichline), labels=PP$LegLblAlpha) +
        scale_color_identity(breaks=PP$extrakleuren$VSNULine, labels=PP$LegLblAlpha, guide='legend') +
        guides(shape='none', 
               linetype='none',
               alpha=guide_legend(title=unlist(PP['LegTitleAlpha'][[1]]), order=2),
               color=guide_legend(title=unlist(PP['LegTitleCol'][[1]]), order=2))
      if(length(unique(PP$LegLblLinetype))>1) {
        plotRes <- plotRes +
          guides(linetype=guide_legend(title=unlist(PP['LegTitleLineType'][[1]]), order=2))
      }
    }
  }
  if (PP$plotType=='Lijn1') {
    options(OutDec = '.')
    plotVals <- plyr::count(plotData, vars=c(PP$plotX,PP$plotCol, PP$plotSymbol, PP$plotLineType), wt_var='freq')
    names(plotVals)[1] <- 'x'
    n <- 2
    plotVals$group <- ''
    if(!is.null(PP$plotCol)) {
      names(plotVals)[n] <- 'col'
      n <- n+1
      plotVals$group <- plotVals$col
    } else {
      plotVals$col <- factor('Constant')
    }
    if(!is.null(PP$plotSymbol)) {
      names(plotVals)[n] <- 'symbol'
      n <- n+1
      if(is.null(PP$plotCol) || PP$plotCol!=PP$plotSymbol) {plotVals$group <- paste0(plotVals$group,plotVals$symbol)}
    } else {
      plotVals$symbol <- factor('Constant')
    }
    if(!is.null(PP$plotLineType)) {
      names(plotVals)[n] <- 'linetype'
      n <- n+1
      PP$linetypes <- rep(c(1:6),10)[1:length(unique(plotVals$symbol))]
      if((is.null(PP$plotCol) || PP$plotCol!=PP$plotLineType) && (is.null(PP$plotSymbol) || PP$plotSymbol!=PP$plotLineType))
      {plotVals$group <- paste0(plotVals$group,plotVals$linetype)}
    } else {
      plotVals$linetype <- plotVals$col
      PP$linetypes <- rep(1,100)[1:length(unique(plotVals$col))]
    }
    plotVals$linesize <- factor(PP$extrakleuren$NormalLineThickness)
    names(plotVals)[n] <- 'freq'
    PP$PercBase$x <- factor(PP$PercBase$x, levels=levels(plotVals$x))
    PP$PercBase <- PP$PercBase[!is.na(PP$PercBase$x),]
    PP$PercBase$group <- as.character(PP$PercBase$group)
    plotVals$perc <- sapply(1:nrow(plotVals), function(i) {
      plotVals$freq[i]/sum(PP$PercBase$freq[PP$PercBase$x==plotVals$x[i] & PP$PercBase$group==plotVals$group[i]])
    })
    if(length(PP$kleuren)==1 && PP$kleuren=='Auto') {
      plotVals <- droplevels(plotVals)
      kleuren <- colorRampPalette(rainbow(n=nlevels(plotVals$col)))
      kleuren <- kleuren(nlevels(plotVals$col))
      colseq <- seq(from=1, by=PP$colstep, length.out = nlevels(plotVals$col)) %% nlevels(plotVals$col)+1
      PP$kleuren <- kleuren[colseq]
      rm(kleuren)
      rm(colseq)
    }
    PP$shapes <- rep(c(15,20,17:19),10)[1:length(unique(plotVals$symbol))]
    if(PP$includeSumm) {
      plotVals <- rbind.fill(plotVals, data.frame(x=unique(plotVals$x),
                                                  col=rep(ifelse(is.null(PP$plotCol),'Constant','Average'),nlevels(plotVals$x)),
                                                  symbol=rep(ifelse(is.null(PP$plotSymbol),'Constant','Average'),nlevels(plotVals$x)),
                                                  linetype=rep('Average',nlevels(plotVals$x)),
                                                  linesize=factor(rep(PP$extrakleuren$AvgLineThickness,nlevels(plotVals$x))),
                                                  group=factor(rep('Average',nlevels(plotVals$x)))))
      plotVals$freq[plotVals$group=='Average'] <- sapply(plotVals$x[plotVals$group=='Average'], function(x) {
        sum(plotVals$freq[plotVals$x==x], na.rm = T)
      })
      if(!any(substring(PP$PercBase$group,1,7)=='Average')) {
        plotVals$perc[plotVals$group=='Average'] <- plotVals$freq[plotVals$group=='Average']/sapply(plotVals$x[plotVals$group=='Average'], function(x) {
          sum(PP$PercBase$freq[PP$PercBase$x==x])
        })
      } else {
        plotVals$perc[plotVals$group=='Average'] <- plotVals$freq[plotVals$group=='Average']/sapply(plotVals$x[plotVals$group=='Average'], function(x) {
          sum(PP$PercBase$freq[PP$PercBase$x==x & PP$PercBase$group=='Average'])
        })
      }
      PP$kleuren <- c(PP$kleuren, PP$extrakleuren$AvgLineCol)
      PP$shapes <- c(PP$shapes, PP$extrakleuren$AvgLineShape)
      PP$linetypes <- c(PP$linetypes, PP$extrakleuren$AvgLineLinetype)
    }
    if(!is.null(PP$SecLine) && PP$SecLine[1]=='RefPerc') {
      if(length(PP$Secline)==1) {PP$SecLine <- list(What='RefPerc',Percentage=100)}
      plotVals <- rbind.fill(plotVals, data.frame(x=unique(plotVals$x),
                                                  col=rep(ifelse(is.null(PP$plotCol),'Constant',paste0('Reference (',PP$SecLine$Percentage,'%)')),nlevels(plotVals$x)),
                                                  symbol=rep(factor(paste0('Reference (',PP$SecLine$Percentage,'%)')),nlevels(plotVals$x)),
                                                  linetype=rep(factor(paste0('Reference (',PP$SecLine$Percentage,'%)')),nlevels(plotVals$x)),
                                                  linesize=factor(rep(PP$extrakleuren$RefLineThickness,nlevels(plotVals$x))),
                                                  group=factor(rep(factor(paste0('Reference (',PP$SecLine$Percentage,'%)')),nlevels(plotVals$x)))))
      plotVals$perc[plotVals$group==paste0('Reference (',PP$SecLine$Percentage,'%)')] <- PP$SecLine$Percentage/100
      PP$kleuren <- c(PP$kleuren, PP$extrakleuren$RefLineCol)
      PP$shapes <- c(PP$shapes, PP$extrakleuren$RefLineShape)
      PP$linetypes <- c(PP$linetypes, PP$extrakleuren$RefLineLinetype)
    }
    if(is.null(PP$xlims)) {
      if(is.numeric(plotVals$x)) {
        PP$xlims <- c(1.03*min(plotVals$x)-.03*max(plotVals$x), 1.03*max(plotVals$x)-.03*min(plotVals$x))
      } else {
        if(!is.factor(plotVals$x)) {plotVals$x <- as.factor(plotVals$x)}
        PP$xlims <- c(.5, nlevels(plotVals$x)+.5)
      }
    }
    if(!is.null(PP$AddRange) && PP$AddRange$What=='SD') {
      plotVals$sd <- 0
      plotVals$sd[plotVals$group==PP$AddRange$which] <- sapply(plotVals$x[plotVals$group==PP$AddRange$which], function(x) {
        sd(plotVals$perc[plotVals$x==x])
      })
    }
    plotVals$Origcol <- plotVals$col
    if(PP$Anonimize) {
      plotVals$col <- ifelse(plotVals$col %in% levels(TotalVals$Bron), sapply(as.numeric(plotVals$col),function(n) {paste0(rep(' ',n),collapse='')}), as.character(plotVals$col))
      plotVals$symbol <- ifelse(plotVals$symbol %in% levels(TotalVals$Bron), sapply(as.numeric(plotVals$symbol),function(n) {paste0(rep(' ',n),collapse='')}), as.character(plotVals$symbol))
      plotVals$group <- ifelse(plotVals$group %in% levels(TotalVals$Bron), sapply(as.numeric(plotVals$group),function(n) {paste0(rep(' ',n),collapse='')}), as.character(plotVals$group))
      plotVals$linetype <- ifelse(plotVals$linetype %in% levels(TotalVals$Bron), sapply(as.numeric(plotVals$linetype),function(n) {paste0(rep(' ',n),collapse='')}), as.character(plotVals$linetype))
    }
    plotRes <- ggplot(data=plotVals, mapping=aes(x=x, y=perc, col=col, group=group))
    if(!is.null(PP$AddRange) && PP$AddRange$What=='SD') {
      if(is.null(PP$LegLblSD)) {
        PP$LegLblSD <- paste(PP$AddRange$range, 'sd')
      }
      if(!is.null(PP$epsFriendly) && PP$epsFriendly && PP$AddRange$alpha<1) {
        if(is.null(PP$BackgroundColorPlot) ||is.na(PP$BackgroundColorPlot)) {PP$BackgroundColorPlot <- 'grey92'}
        PP$AddRange$color <- rgb(aperm(PP$AddRange$alpha*col2rgb(PP$AddRange$color)+(1-PP$AddRange$alpha)*col2rgb(PP$BackgroundColorPlot)), maxColorValue = 255)
        PP$AddRange$alpha <- 1
      }
      plotRes <- plotRes +
        geom_ribbon(aes(x=x, ymin=perc-PP$AddRange$range*sd, ymax=perc+PP$AddRange$range*sd, alpha=factor(PP$AddRange$alpha)),
                    color=NA,
                    fill=PP$AddRange$color) +
        scale_alpha_manual(values=PP$AddRange$alpha, 
                           labels=PP$LegLblSD) +
        guides(alpha=guide_legend(title=NULL, labels=T, order=2))
    }
    plotRes <- plotRes +
      geom_point(aes(shape=symbol),size=PP$Zoomfactor*3, alpha=ifelse(!is.null(PP$epsFriendly) && PP$epsFriendly,1,.5)) +
      geom_line(aes(linetype=linetype,size=group)) +
      coord_cartesian(xlim=PP$xlims, ylim=c(0,min(max(plotVals$perc)*1.1,2)), expand=F) +
      labs(y=PP$ylab,x=PP$xlab,title=PP$Titel)
    if(PP$Anonimize) {
      plotRes <- plotRes + 
        scale_size_manual(values=sapply(unique(plotVals$group), function(i) {
          mean(as.numeric(as.character(plotVals$linesize[plotVals$group==i])))
        }), labels=PP$LegLbl) +
        scale_shape_manual(values=PP$shapes, labels=PP$LegLbl) +
        scale_color_manual(values=PP$kleuren, labels=PP$LegLbl) +
        scale_linetype_manual(values=PP$linetypes, labels=PP$LegLbl) +
        scale_y_continuous(labels=scales::percent)
      if(is.null(PP$LegLbl)) {PP$LegLbl <- 'Average'}
    } else {
      plotRes <- plotRes + 
        scale_size_manual(breaks=unique(plotVals$group), values=sapply(unique(plotVals$group), function(i) {
          mean(as.numeric(as.character(plotVals$linesize[plotVals$group==i])))
        })) +
        scale_shape_manual(values=PP$shapes) +
        scale_color_manual(values=PP$kleuren) +
        scale_linetype_manual(values=PP$linetypes) +
        scale_y_continuous(labels=scales::percent)
      if(is.null(PP$LegLbl)) {
        if(!is.null(PP$plotCol)) {
          if(is.null(PP$LegLblCol)) {PP$LegLblCol <- as.character(unique(plotVals$col))}
          PP$LegLbl <- PP$LegLblCol
        }
        if(!is.null(PP$plotSymbol)) {
          if(is.null(PP$LegLblSymbol)) {PP$LegLblSymbol <- as.character(unique(plotVals$symbol))}
          if(is.null(PP$plotCol) || PP$plotCol!=PP$plotSymbol) {
            PP$LegLbl <- c(PP$LegLbl, PP$LegLblSymbol)
          }
        }
        if(!is.null(PP$plotLinetype)) {
          if(is.null(PP$LegLblLinetype)) {PP$LegLblLinetype <- as.character(unique(plotVals$linetype))}
          if((is.null(PP$plotCol) || PP$plotCol!=PP$plotLinetype) && (is.null(PP$plotSymbol) || PP$plotSymbol!=PP$plotLinetype)) {
            PP$LegLbl <- c(PP$LegLbl, PP$LegLblLinetype)
          }
        }
      }
    }
    
    # Zo nodig legenda's toevoegen:
    if(is.null(PP$plotCol)) {
      plotRes <- plotRes + guides(col='none')
    } else {
      plotRes <- plotRes + guides(col=guide_legend(title=unlist(PP$LegTitleCol), order=1))
    }
    plotRes <- plotRes + guides(linetype=guide_legend(title=unlist(PP$LegTitleLineType),
                                                      keywidth=ifelse(is.null(PP$LegWidth$linetype), PP$LegWidth$rest, PP$LegWidth$linetype),
                                                      order=1))
    if(is.null(PP$plotSymbol)) {
      plotRes <- plotRes + guides(shape='none')
    } else {
      plotRes <- plotRes + guides(shape=guide_legend(title=unlist(PP$LegTitleSymbol), order=1))
    }
    #plotRes <- plotRes + guides(size=guide_legend(title=unlist(PP$LegTitleSize), order=1))
    plotRes <- plotRes + guides(size='none')
  }
  if (PP$plotType=='Scatter') {
    PP$vars <- PP$plotGroups
    if(!(length(PP$plotX)==1 && PP$plotX=='Total')) {PP$vars <- c(PP$vars,PP$plotX[1])}
    if(!(length(PP$plotY)==1 && PP$plotY=='Total')) {PP$vars <- c(PP$vars,PP$plotY[1])}
    plotVals <- plyr::count(plotData, vars=c(PP$vars), wt_var = 'freq')
    plotVals$group <- apply(plotVals,1,function(n) {
      paste0(n[PP$plotGroups], collapse = '')
    })
    PP$PercBase$group <- apply(PP$PercBase,1,function(n) {
      paste0(n[PP$plotGroups], collapse = '')
    })
    plotVals$perc <- sapply(1:nrow(plotVals),function(n) {
      plotVals$freq[n]/sum(PP$PercBase$freq[PP$PercBase$group==plotVals$group[n]])
    })
    if(length(PP$plotX)==1 && PP$plotX=='Total') {
      plotVals$x <- sapply(1:nrow(plotVals), function(n) {
        sum(plotVals$perc[plotVals$group==plotVals$group[n]])
      })
    } else {
      plotVals$groupx <- apply(plotVals,1,function(n) {
        paste0(n[c(PP$plotGroups,PP$plotX[1])], collapse = '')
      })
      if(PP$plotX[3]=='Total') {
        plotVals$x <- sapply(1:nrow(plotVals), function(n) {
          sum(plotVals$perc[plotVals$groupx==plotVals$groupx[n]])
        })
      }
      if(PP$plotX[3]=='NARCISTotal') {
        plotVals$x <- sapply(1:nrow(plotVals), function(n) {
          sum(plotVals$perc[plotVals$groupx==plotVals$groupx[n]])/sum(plotVals$perc[plotVals$group==plotVals$group[n]])
        })
      }
    }
    if(length(PP$plotY)==1 && PP$plotY=='Total') {
      plotVals$y <- sapply(1:nrow(plotVals), function(n) {
        sum(plotVals$perc[plotVals$group==plotVals$group[n]])
      })
    } else {
      plotVals$groupy <- apply(plotVals,1,function(n) {
        paste0(n[c(PP$plotGroups,PP$plotY[1])], collapse = '')
      })
      if(PP$plotY[3]=='Total') {
        plotVals$y <- sapply(1:nrow(plotVals), function(n) {
          sum(plotVals$perc[plotVals$groupy==plotVals$groupy[n]])
        })
      }
      if(PP$plotY[3]=='NARCISTotal') {
        plotVals$y <- sapply(1:nrow(plotVals), function(n) {
          sum(plotVals$perc[plotVals$groupy==plotVals$groupy[n]])/sum(plotVals$perc[plotVals$group==plotVals$group[n]])
        })
      }
    }
    if(!(length(PP$plotX)==1 && PP$plotX=='Total')) {
      plotVals <- plotVals[plotVals[PP$plotX[1]]==PP$plotX[2],]
    }
    if(!(length(PP$plotY)==1 && PP$plotY=='Total')) {
      plotVals <- plotVals[plotVals[PP$plotY[1]]==PP$plotY[2],]
    }
    plotVals <- plotVals[c(PP$plotGroups,'x','y')]
    if(PP$plotCol!='-') {plotVals$col <- plotVals[,PP$plotCol]} else {plotVals$col <- 'Constant'}
    if(PP$plotAlpha=='-') {plotVals$alpha <- 1} else {
      plotVals$alpha <- plotVals[,PP$plotAlpha]
      if(!any(is.na(as.numeric(as.character((plotVals$alpha)))))) {plotVals$alpha <- as.numeric(as.character(plotVals$alpha))}
    }
    if(PP$plotSymbol=='-') {plotVals$shape <- 'Constant'} else {plotVals$shape <- plotVals[,PP$plotSymbol]}
    plotRes <- ggplot(data=plotVals) +
      geom_point(aes(x=x, y=y, col=col, alpha=alpha, shape=shape), size=PP$pointsize) +
      stat_function(fun=function(x) {1/x}, color='black', aes(size='100% van VSNU')) +
      scale_shape_manual(values=rep(15:19,3)) +
      scale_y_continuous(labels=scales::percent, limits = c(0,1.2)) +
      scale_x_continuous(labels=scales::percent, limits = c(0,3)) +
      scale_size_manual(values=c(1.5,1.5)) +
      guides(shape=guide_legend("Universiteit", order=1),
             color=guide_legend("Universiteit", order=1),
             alpha=guide_legend("Jaar", order=2),
             size=guide_legend("Maximum", order=3))
    if(is.numeric(plotVals$alpha)) {
      if(all(plotVals$alpha>=0 & plotVals$alpha<=1)) {
        plotRes <- plotRes + scale_alpha_identity()
      } else {
        plotRes <- plotRes + scale_alpha(range=c(.15,1))}
    }
  }
  if (PP$plotType=='ColorCheck') {
    temp <- data.frame(r=rep(1:8, times=64), g=rep(1:8, times=8, each=8), b=rep(1:8,each=64))
    temp$hex <- paste0('#',as.hexmode(temp$r*2-1),'0',as.hexmode(temp$g*2-1),'0',as.hexmode(temp$b*2-1),'0')
    ggplot(data=temp) +
      geom_point(aes(x=8*r+g, y=b, color=hex), size=6) +
      geom_point(aes(x=8*r+g, y=b, color=ColContrast(hex)), size=2) +
      scale_color_identity()
  }
  plotRes <- plotRes + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=.5, size=PP$TextSizeTickX, family=PP$TextFont),
                             axis.text.y = element_text(size=PP$TextSizeTickY, family=PP$TextFont),
                             axis.text.y.right = element_text(size=PP$TextSizeTick2, family=PP$TextFont),
                             axis.title.x = element_text(size=PP$TextSizeLblX, family=PP$TextFont),
                             axis.title.y = element_text(size=PP$TextSizeLblY, family=PP$TextFont),
                             legend.title = element_text(size=PP$TextSizeLegTitle, family=PP$TextFont),
                             legend.text = element_text(size=PP$TextSizeLegLbls, family=PP$TextFont),
                             legend.key.width = unit(PP$LegWidth$rest, 'lines'),
                             plot.title = element_text(size=PP$TextSizeTitle, family=PP$TextFont),
                             plot.subtitle = element_text(size=PP$TextSizeSubTitle, face='italic', family=PP$TextFont),
                             strip.text = element_text(size=PP$TextSizeFacetHead, family=PP$TextFont),
                             panel.background = element_rect(fill = PP$BackgroundColorPlot, color=PP$BackgroundColorPlot),
                             panel.grid.minor = element_blank(),
                             plot.background = element_rect(fill = "transparent", colour = NA))
  if(PP$coord=='flip') {
    plotRes <- plotRes + coord_flip(xlim=c(min(plotVals$xpos-.5*plotVals$size)-.5*PP$BarGap, 
                                           max(plotVals$xpos+.5*plotVals$size)+.5*PP$BarGap),
                                    ylim=c(0,max(plotVals$cum)*1.1), expand=F) #To-Do: adjust margins (xlim en ylim)
  } else if(PP$coord=='polar') {
    if(PP$BarPosit=='fill') {plotPercVals$y <- ScaleFac*102} else {plotPercVals$y <- plotPercVals$total*1.02}
    plotRes <- plotRes + 
      coord_polar() +
      geom_text(aes(x=xpos, y=y, label=as.character(x), angle=-xpos*360/max(xpos+size/2+PP$BarGap)+90), size=PP$TextSizeTickX*(25.4/72), hjust=0, vjust=.5, data=plotPercVals) +
      geom_text(aes(x=xpos, y=PP$polarRadius*ScaleFac/.01, label='', angle=-(xpos)*360/max(xpos+size/2)+90), data=plotPercVals, hjust=0) +
      theme(axis.text.x = element_blank())
    if(PP$plotX==PP$plotCol) {
      plotRes <- plotRes + guides(fill='none')
    }
    if(!is.null(PP$BackgroundColorPlot) && !is.na(PP$BackgroundColorPlot) && PP$BackgroundColorPlot=='transparent') {
      plotRes <- plotRes+theme(panel.grid.major=element_blank())
    }
  }
  if(PP$LegDynamicHeight) {
    plotRes <- plotRes + theme(legend.key.height = unit(PP$Zoomfactor*sapply(gregexpr('\\n',PP$LegLbl), function(n) {sum(n!=-1)+1}), 'lines'))
  } else {
    plotRes <- plotRes + theme(legend.key.height = unit(PP$LegHeight$rest, 'lines'))
  }
  if(13 %in% StandardPlot) {
    plotRes <- plotRes + theme(axis.text.x = element_text(angle=0, hjust=0.5, size=PP$TextSizeTickX, family=PP$TextFont),
                               axis.text.y = element_text(size=PP$TextSizeTickY, family=PP$TextFont),
                               axis.text.y.right = element_text(size=PP$TextSizeTick2, family=PP$TextFont),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks = element_blank(),
                               legend.title = element_text(size=PP$TextSizeLegTitle, family=PP$TextFont),
                               legend.text = element_text(size=PP$TextSizeLegLbls, family=PP$TextFont),
                               legend.key.width = unit(PP$LegWidth$rest, 'lines'),
                               legend.position = 'bottom',
                               plot.title = element_text(size=PP$TextSizeTitle, family=PP$TextFont, hjust=0, margin = margin(0,10000,0,50, unit='points')),
                               plot.subtitle = element_text(size=PP$TextSizeSubTitle, face='italic', family=PP$TextFont),
                               strip.text = element_text(size=PP$TextSizeFacetHead, family=PP$TextFont),
                               panel.background = element_rect(fill = PP$BackgroundColorPlot, color=PP$BackgroundColorPlot),
                               panel.grid.minor = element_blank(),
                               panel.grid.major.y = element_blank(),
                               panel.grid.major.x = element_line(colour = '#A0A0A0',size = .2),
                               plot.background = element_rect(fill = "transparent", colour = NA))
    library(grid)
    library(gridExtra)
    
    #plotResGrid <- ggplot_gtable(ggplot_build(plotRes))
    #plotResGrid$layout[which(plotResGrid$layout$name == "title"), c("l", "r")] <- c(1, max(plotResGrid$layout$r))
    #plotRes <- grid.arrange(plotResGrid)
    
  }
  if(PP$PrintPlot) print(plotRes)
  if(PP$SavePlot) {
    ggsave(paste0(Paths$plots, '/',PP$FileTitle,'.png'),
           plot=plotRes, device='png', width=PP$SaveResolution[1]/PP$Savedpi, height=PP$SaveResolution[2]/PP$Savedpi, dpi=PP$Savedpi, bg='transparent')
    ggsave(paste0(Paths$plots, '/',PP$FileTitle,'.pdf'),
           plot=plotRes, device='pdf', width=PP$SaveResolution[1]/PP$Savedpi, height=PP$SaveResolution[2]/PP$Savedpi, dpi=PP$Savedpi, bg='transparent')
    embed_fonts(paste0(Paths$plots, '/',PP$FileTitle,'.pdf'))
    ggsave(paste0(Paths$plots, '/',PP$FileTitle,'.svg'),
           plot=plotRes, device='svg', width=PP$SaveResolution[1]/PP$Savedpi, height=PP$SaveResolution[2]/PP$Savedpi, dpi=PP$Savedpi, bg='transparent')
    if (PP$plotType!='Lijn1' || is.null(PP$AddRange) || PP$AddRange$alpha==1) {
      ggsave(paste0(Paths$plots, '/',PP$FileTitle,'.eps'),
             plot=plotRes, device='eps', width=PP$SaveResolution[1]/PP$Savedpi, height=PP$SaveResolution[2]/PP$Savedpi, dpi=PP$Savedpi, bg='transparent', fonts=PP$TextFont)
    }
  }
  
  
  #EndSummary <- plotPercVals
  #EndSummary$PercFromTotal <- plotPercVals$total/sum(plotPercVals$total[plotPercVals$x!=PP$TotalXLab])*100
  #EndSummary <- EndSummary[order(EndSummary$x),]
  
  
  #To-Do: file bug report for combining legend growth when using multiple identic aesthetics labels
  if (PP$SaveGglPlot) {
    # Maten: Standaard bargraph heeft links+rechts marges 6px, bars 16px, gap 11px, dus breedte 27n+1 px
    GglLabels <- read.csv2(paste0(Paths$Params,'/GglLabels.csv'), stringsAsFactors = F)
    GglLabels <- GglLabels[GglLabels$Lang==PP$LabelsLang,]
    if(PP$LabelsLang=='en') {
      GglLabels$Tekst[GglLabels$Plotnr!=99] <- paste0(GglLabels$Tekst[GglLabels$Plotnr!=99],'<br>Used data is from ',PP$WerksetDate)
      GglLabels$Tekst[GglLabels$Plotnr==7] <- paste0(GglLabels$Tekst[GglLabels$Plotnr==7],', using publications since ',Jaren[1])
      GglLabels$Tekst[GglLabels$Plotnr!=99] <- paste0(GglLabels$Tekst[GglLabels$Plotnr!=99],'.')
    } else {
      GglLabels$Tekst[GglLabels$Plotnr!=99] <- paste0(GglLabels$Tekst[GglLabels$Plotnr!=99],'<br>Gebruikte data is per ',PP$WerksetDate)
      GglLabels$Tekst[GglLabels$Plotnr==7] <- paste0(GglLabels$Tekst[GglLabels$Plotnr==7],', voor publicaties sinds ',Jaren[1])
      GglLabels$Tekst[GglLabels$Plotnr!=99] <- paste0(GglLabels$Tekst[GglLabels$Plotnr!=99],'.')
    }
    if(exists('temp')) {rm(temp)}
    if(7 %in% StandardPlot) {
      googleVals <- spread(plotVals[1:3], col, freq)
      googleVals <- googleVals[order(googleVals$x),]
      google <- gvisColumnChart(googleVals,xvar='x',yvar=names(googleVals)[-1],
                                option=list(isStacked='percent'))
      gglfrm <- data.frame(Access=googleVals$x, Open=googleVals$Open, Closed=googleVals$Closed, annotate='', stringsAsFactors = F)
      gglfrm[gglfrm$Access=='All',c('Open', 'Closed')] <- gglfrm[gglfrm$Access=='All',c('Open', 'Closed')]*(nrow(gglfrm)-1)
      names(gglfrm)[4] <- '{ role: \'annotation\' }'
      gglfrm$Access <- PP$xvallabs
      temp <- list(pre='<script type="text/javascript" id="barchartscript',
                   pre2='">\n/*<![CDATA[*/\ngoogle.setOnLoadCallback(drawChart',
                   pre3=');\nfunction drawChart',
                   pre4='() {var barchart_data = google.visualization.arrayToDataTable(',
                   n=3,
                   post1=');var chart = new google.visualization.ColumnChart(document.getElementById(\'barchart_div_',
                   post2=paste0('\' ));chart.draw(barchart_data, { isStacked:\'percent\',',
                                'colors:[\'#248bb8\',\'#949294\'],',
                                'bars: \'horizontal\',',
                                'hAxis: { minValue:0, slantedText: true, slantedTextAngle: 90}, ',
                                'chartArea: {left: 105, top: 20, width: \'',136*2,'\', height: \'50%\'}',
                                '});}\n/*]]>*/\n</script>'),
                   header=GglLabels$header[GglLabels$Plotnr %in% StandardPlot],
                   graphtext=paste0(GglLabels$Tekst[GglLabels$Plotnr %in% StandardPlot]))
      arraytxt <- paste0('[[',paste0('\'', names(gglfrm)[-4], '\'',collapse=', '),', ',names(gglfrm[4]),'], ',
                         paste0(apply(gglfrm,1,function(x) {paste0('[\'',x[1],'\', ',x[2],', ',x[3],', \'\']')}), collapse=', '),']')
      (textR <- with(temp, paste0(pre,n,pre2,n,pre3,n,pre4, arraytxt, post1, n, post2)))
      textBodyR <- paste0('<br><br><br>\n',
                          '<h2 class="style01" style="margin-left:100px; width: 700px;">',temp$header,'</h2>\n',
                          '<div id="barchart_div_',temp$n,'" style="width: 500px; height: 300px;"></div>\n',
                          '<p style="margin-left:110px; width: 400px;"><label>',temp$graphtext,'</label></p>')
    }
    if(2 %in% StandardPlot) {
      googleVals <- spread(plotVals[1:3], col, freq)
      googleVals <- merge(googleVals, plotValsSec[,c('x','freq')])
      
      google <- gvisComboChart(googleVals, 'x', yvar=names(googleVals)[-1],
                               option=list(seriesType='bars', series="{3: {type: 'line'}}"))
      gglfrm <- data.frame(sapply(googleVals, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- 'null'
        return(x)
      }), stringsAsFactors = F)
      names(gglfrm)[1] <- 'Access'
      if(PP$LabelsLang=='en') {
        names(gglfrm)[4] <- 'Total - VSNU'
        gglfrm <- gglfrm[c('Access','Open','Closed','Total - VSNU')]
      } else {
        names(gglfrm)[4] <- 'Totalen - VSNU'
        gglfrm <- gglfrm[c('Access','Open','Closed','Totalen - VSNU')]
      }
      
      
      temp <- list(pre='<script type="text/javascript" id="barchartscript',
                   pre2='">\n/*<![CDATA[*/\ngoogle.setOnLoadCallback(drawChart',
                   pre3=');\nfunction drawChart',
                   pre4='() {var barchart_data = google.visualization.arrayToDataTable(',
                   n=4,
                   post1=');var chart = new google.visualization.ComboChart(document.getElementById(\'barchart_div_',
                   post2=paste0('\' ));chart.draw(barchart_data, {isStacked: true, ',
                                'colors:[\'#248bb8\',\'#949294\',\'#B02020\'],',
                                'seriesType: \'bars\', ',
                                'series: {2: {type:\'line\'}}, ',
                                'pointSize: 4, ',
                                'chartArea: {left: 105, top:60, width: ',463,', height: \'61.8%\'}',
                                '});}\n/*]]>*/\n</script>'),
                   header=GglLabels$header[GglLabels$Plotnr %in% StandardPlot],
                   graphtext=GglLabels$Tekst[GglLabels$Plotnr %in% StandardPlot])
      
      arraytxt <- paste0('[[',paste0('\'', names(gglfrm), '\'',collapse=', '),'], ',
                         paste0(apply(gglfrm,1,function(x) {paste0('[\'',x[1],'\', ',x[2],', ',x[3],', ',x[4],']')}), collapse=', '),']')
      (textR <- with(temp, paste0(pre,n,pre2,n,pre3,n,pre4, arraytxt, post1, n, post2)))
      textBodyR <- paste0('<br><br><br>\n',
                          '<h2 class="style01" style="margin-left:100px; width: 700px;">',temp$header,'</h2>\n',
                          '<div id="barchart_div_',temp$n,'" style="width: 700px; height: 300px;"></div>\n',
                          '<p style="margin-left:110px; width: 550px;"><label>',temp$graphtext,'</label></p>')
      
    }
    if(12 %in% StandardPlot) {
      googleVals <- spread(plotVals[1:3], col, freq)
      googleVals <- merge(googleVals, spread(plotValsSec[c(1,3,4)], whichline, freq))
      googleVals[sapply(googleVals, is.numeric)] <- round(googleVals[sapply(googleVals, is.numeric)])
      
      google <- gvisComboChart(googleVals, 'x', yvar=names(googleVals)[-1],
                               option=list(seriesType='bars', series="{3: {type: 'line'}, 2: {type:'line'}}"))
      gglfrm <- data.frame(sapply(googleVals, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- 'null'
        return(x)
      }), stringsAsFactors = F)
      names(gglfrm)[1] <- 'Access'
      names(gglfrm)[4] <- 'Total - VSNU'
      names(gglfrm)[5] <- 'Estimate of articles - based on VSNU'
      gglfrm <- gglfrm[c('Access','Open','Closed','Total - VSNU', 'Estimate of articles - based on VSNU')]
      
      
      temp <- list(pre='<script type="text/javascript" id="barchartscript',
                   pre2='">\n/*<![CDATA[*/\ngoogle.setOnLoadCallback(drawChart',
                   pre3=');\nfunction drawChart',
                   pre4='() {var barchart_data = google.visualization.arrayToDataTable(',
                   n=6,
                   post1=');var chart = new google.visualization.ComboChart(document.getElementById(\'barchart_div_',
                   post2=paste0('\' ));chart.draw(barchart_data, {isStacked: true, ',
                                'colors:[\'#248bb8\',\'#949294\',\'#B02020\',\'#B02020\'],',
                                'seriesType: \'bars\', ',
                                'series: {2: {type:\'line\'}, ',
                                '3: {type:\'line\', lineDashStyle: [6,3]}}, ',
                                'pointSize: 4, ',
                                'chartArea: {left: 105, top: 60, width: ',463,', height: \'61.8%\'}',
                                '});}\n/*]]>*/\n</script>'),
                   header=GglLabels$header[GglLabels$Plotnr %in% StandardPlot],
                   graphtext=GglLabels$Tekst[GglLabels$Plotnr %in% StandardPlot])
      
      arraytxt <- paste0('[[',paste0('\'', names(gglfrm), '\'',collapse=', '),'], ',
                         paste0(apply(gglfrm,1,function(x) {paste0('[\'',x[1],'\'',paste0(', ',x[2:5], collapse=''),']')}), collapse=', '),']')
      (textR <- with(temp, paste0(pre,n,pre2,n,pre3,n,pre4, arraytxt, post1, n, post2)))
      textBodyR <- paste0('<br><br><br>\n',
                          '<h2 class="style01" style="margin-left:100px; width: 700px;">',temp$header,'</h2>\n',
                          '<div id="barchart_div_',temp$n,'" style="width: 700px; height: 300px;"></div>\n',
                          '<p style="margin-left:110px; width: 550px;"><label>',temp$graphtext,'</label></p>')
      
    }
    if(22 %in% StandardPlot) {
      googleVals <- spread(plotVals[1:3], col, freq)
      googleVals <- merge(googleVals, spread(plotValsSec[c(1,3,4)], whichline, freq))
      googleVals[sapply(googleVals, is.numeric)] <- round(googleVals[sapply(googleVals, is.numeric)])
      
      google <- gvisComboChart(googleVals, 'x', yvar=names(googleVals)[-1],
                               option=list(seriesType='bars', series="{3: {type: 'line'}, 2: {type:'line'}, 4: {type:'line'}}"))
      gglfrm <- data.frame(sapply(googleVals, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- 'null'
        return(x)
      }), stringsAsFactors = F)
      if(PP$LabelsLang=='en') {
        names(gglfrm)[1] <- 'Access'
        names(gglfrm)[4] <- 'Total scholarly output'
        names(gglfrm)[5] <- 'Estimate of articles - based on VSNU'
        names(gglfrm)[6] <- 'Total no of articles (VSNU)'
        gglfrm <- gglfrm[c('Access','Open','Closed',names(gglfrm)[4:6])]
      } else {
        names(gglfrm)[1] <- 'Access'
        names(gglfrm)[4] <- 'Totale wetensch. output'
        names(gglfrm)[5] <- 'Schatting artikelen - o.b.v VSNU'
        names(gglfrm)[6] <- 'Totaal aantal artikelen (VSNU)'
        gglfrm <- gglfrm[c('Access','Open','Closed',names(gglfrm)[4:6])]
      }
      
      
      
      
      temp <- list(pre='<script type="text/javascript" id="barchartscript',
                   pre2='">\n/*<![CDATA[*/\ngoogle.setOnLoadCallback(drawChart',
                   pre3=');\nfunction drawChart',
                   pre4='() {var barchart_data = google.visualization.arrayToDataTable(',
                   n=6,
                   post1=');var chart = new google.visualization.ComboChart(document.getElementById(\'barchart_div_',
                   post2=paste0('\' ));chart.draw(barchart_data, {isStacked: true, ',
                                'colors:[\'#248bb8\',\'#949294\',\'#B02020\',\'#18A018\',\'#2020C0\'],',
                                'seriesType: \'bars\', ',
                                'series: {2: {type:\'line\'}, ',
                                '3: {type:\'line\'}, ',
                                '4: {type:\'line\'}}, ',
                                'pointSize: 4, ',
                                'chartArea: {left: 105, top: 60, width: ',440,', height: \'61.8%\'}',
                                '});}\n/*]]>*/\n</script>'),
                   header=GglLabels$header[GglLabels$Plotnr %in% StandardPlot],
                   graphtext=GglLabels$Tekst[GglLabels$Plotnr %in% StandardPlot])
      
      arraytxt <- paste0('[[',paste0('\'', names(gglfrm), '\'',collapse=', '),'], ',
                         paste0(apply(gglfrm,1,function(x) {paste0('[\'',x[1],'\'',paste0(', ',x[2:6], collapse=''),']')}), collapse=', '),']')
      (textR <- with(temp, paste0(pre,n,pre2,n,pre3,n,pre4, arraytxt, post1, n, post2)))
      textBodyR <- paste0('<br><br><br>\n',
                          '<h2 class="style01" style="margin-left:100px; width: 700px;">',temp$header,'</h2>\n',
                          '<div id="barchart_div_',temp$n,'" style="width: 700px; height: 300px;"></div>\n',
                          '<p style="margin-left:110px; width: 550px;"><label>',temp$graphtext,'</label></p>')
      
    }
    if(14 %in% StandardPlot) {
      plotVals$closed <- sapply(1:nrow(plotVals), function(n) {
        sum(TotalVals$freq[!is.na(TotalVals$Jaar) & TotalVals$Jaar==plotVals$x[n] &
                             ((as.character(TotalVals$Bron)==as.character(plotVals$Origcol[n])) | 
                                (TotalVals$BronSoort %in% c('Universiteit (alg)', 'Universiteit (4TU)') & plotVals$col[n]=='Average')) &
                             !is.na(TotalVals$Type) & TotalVals$Type=='Article' &
                             TotalVals$set.publication &
                             TotalVals$Access=='Closed'])
      })
      plotVals$col <- factor(plotVals$col)
      googleVals <- spread(plotVals[c(1,10,8)], Origcol, perc)
      googleTooltips <- spread(plotVals[c(1,10,4)],Origcol, freq)
      googleTooltipsCl <- spread(plotVals[c(1,10,11)], Origcol, closed)
      googleTooltips <- googleTooltips[c(1, order(names(googleTooltips)[2:14])+1,15)]
      googleTooltipsCl <- googleTooltipsCl[c(1, order(names(googleTooltipsCl)[2:14])+1,15)]
      googleTooltips <- data.frame(googleTooltips$x, sapply(2:length(googleTooltips), function(n) {
        paste0('\'', plotVals$col[plotVals$Origcol==names(googleTooltips)[n]][1],' (',
               googleTooltips$x, '), ',
               round(googleVals[,n]*100, digits = 2),
               '% :<br>', googleTooltips[,n],' OA (NARCIS),<br>',
               googleTooltipsCl[,n], ' CA (NARCIS),<br>',
               round(googleTooltips[,n]/googleVals[,n]),
               ifelse(PP$LabelsLang=='en',' Total (based on VSNU)\'', ' Totaal (schatting o.b.v. VSNU)\''))
      }), stringsAsFactors = F)
      
      names(googleTooltips) <- c('x', paste0(names(googleVals),'-tt')[-1])
      googleVals <- merge(googleVals, googleTooltips)
      googleVals <- googleVals[c(1,
                                 rep(2:nlevels(plotVals$col), each=2) +c(0, nlevels(plotVals$col)),
                                 nlevels(plotVals$col)+1, 
                                 nlevels(plotVals$col)*2+1
                                 )]
      googleVals <- merge(googleVals, plotVals[plotVals$col=='Average',c('x','sd')])
      googleVals$minsd <- googleVals$Average-googleVals$sd
      googleVals$minsd[googleVals$minsd<0] <- 0
      googleVals$`minsd-tt` <- ''
      googleVals$maxsd <- 2*googleVals$sd
      googleVals$maxsd[googleVals$minsd+googleVals$maxsd>1] <- 1
      googleVals$`maxsd-tt` <- ''
      
      gglfrm <- data.frame(sapply(googleVals, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- 'null'
        return(x)
      }), stringsAsFactors = F)
      names(gglfrm)[1] <- 'Universities'
      if(PP$Anonimize) {names(gglfrm)[2:27] <- sapply(2:27, function (n) {paste0(rep(' ',n), collapse='')})} else {
        names(gglfrm)[2:27] <- names(googleVals[2:27])
      }
      if(PP$LabelsLang=='en') {
        names(gglfrm)[31:34] <- c('Average - 1 sd','Average - 1 sd-tt','Average +/- 1 sd','Average +/- 1 sd-tt')
        gglfrm <- gglfrm[c(1,rep(13:1, each=2)*2+c(0,1),31:34,28,29)]
      } else {
        gglfrm$Average.tt <- sub('Average','Gemiddelde',gglfrm$Average.tt)
        names(gglfrm)[c(28, 33)] <- c('Gemiddelde', 'Gemiddelde +/- 1 sd')
        gglfrm <- gglfrm[c(1,rep(13:1, each=2)*2+c(0,1),31:34,28,29)]
      }
      
      
      PP$GglPointSize <- 4
      temp <- list(pre='<script type="text/javascript" id="barchartscript',
                   pre2='">\n/*<![CDATA[*/\ngoogle.setOnLoadCallback(drawChart',
                   pre3=');\nfunction drawChart',
                   pre4='() {var chart_data = google.visualization.arrayToDataTable(',
                   n=5,
                   post1=');var chart = new google.visualization.ComboChart(document.getElementById(\'barchart_div_',
                   post2='\' ));chart.draw(chart_data, {isStacked: true, colors:[\'',
                   post3=paste0('\'],seriesType: \'line\', ',
                                'lineWidth: 1, ',
                                'vAxis: {format: \'percent\'}, ',
                                'pointSize: ', PP$GglPointSize, ', ',
                                'chartArea: {left: 105, top: 60, width: ',436,', height: \'61.8%\'}, ',
                                'dataOpacity: .5, ',
                                'tooltip: {isHtml: true}, ',
                                'series: {'),
                   post4=paste0('13: {type:\'area\', lineWidth: .1, areaOpacity: 0, pointSize: 0, visibleInLegend:false, enableInteractivity: false}, ',
                                '15: {type:\'line\', lineWidth: 2, pointShape: \'square\'}, ',
                                '14: {type:\'area\', lineWidth:.1, areaOpacity: .5, pointSize: 0, visibleInLegend:true, enableInteractivity: false}}});}\n/*]]>*/\n</script>'),
                   header=GglLabels$header[GglLabels$Plotnr %in% StandardPlot],
                   graphtext=GglLabels$Tekst[GglLabels$Plotnr %in% StandardPlot])
      
      arraytxt <- paste0('[[\'',names(gglfrm)[1],'\', ',
                         paste0('\'', names(gglfrm)[seq(from=2, to=length(gglfrm), by=2)], '\', {role: \'tooltip\', p: {html: true}}',collapse=', '),'], ',
                         paste0(apply(gglfrm,1,function(x) {
                           paste0('[\'',x[1],'\'',paste0(', ',x[rep(1:16, each=2)*2+c(0,1)], collapse=''),']')
                         }), collapse=', '),']')
      colorartxt <- paste0(PP$kleuren[13:1], collapse='\', \'')
      colorartxt <- paste0(colorartxt, '\', \'#FFFFFF\', \'#7F7F7F\', \'', PP$kleuren[14])
      PP$shapesGgl <- factor(PP$shapes)
      levels(PP$shapesGgl) <- c('square','triangle','diamond','circle','star')
      PP$shapesGgl <- PP$shapesGgl[-length(PP$shapesGgl)]
      PP$GglPointSizeLvls <- c(1,1,1,2/3,1.5)*PP$GglPointSize
      PP$GglPointSize <- PP$shapesGgl
      levels(PP$GglPointSize) <- PP$GglPointSizeLvls
      PP$GglPointSize <- as.numeric(as.character(PP$GglPointSize))
      
      seriesspec <- paste0(12:0, ': {type:\'line\', ',
                           'pointShape: \'',PP$shapesGgl,'\', ',
                           'pointSize: ',PP$GglPointSize, ', ',
                           'visibleInLegend: ', tolower(!PP$Anonimize),
                           '}, ', collapse='')
      
      textR <- with(temp, paste0(pre,n,pre2,n,pre3,n,pre4, arraytxt, post1, n, post2,colorartxt,post3, seriesspec, post4))
      textBodyR <- paste0('<br><br><br>\n',
                          '<h2 class="style01" style="margin-left:100px; width: 700px;">',temp$header,'</h2>\n',
                          '<div id="barchart_div_',temp$n,'" style="width: 700px; height: 300px;"></div>\n',
                          '<p style="margin-left:110px; width: 550px;"><label>',temp$graphtext,'</label></p>')
    }
    if(exists('temp')) {
      if(PP$GglPlotFileName %in% list.files(Paths$IO)) {
        htmlfile <- read_file(paste0(Paths$IO,'/',PP$GglPlotFileName))
      } else {
        htmlfile <- read_file(paste0(Paths$IO,'/GoogleChartsCode orig',PP$LabelsLang,'.htm'))
      } # This is meant to be able to start with a clean version by removing GoogleChartsCode.htm.
      inspos <- gregexpr(paste0('<script type="text/javascript"[^>]*>[^>]*google\\.setOnLoadCallback\\(drawChart',temp$n,'.*?</script>'), htmlfile)[[1]]
      if(inspos!=-1) {
        print('Overwriting old data')
        htmlfile <- paste0(substr(htmlfile,1,inspos-1),
                           textR,
                           substr(htmlfile,inspos+attr(inspos, 'match.length'),nchar(htmlfile)))
      } else {
        inspos <- gregexpr('<script type="text/javascript" id="barchartscript[0-9]*">.*?</script>', htmlfile)[[1]]
        pos <- inspos[length(inspos)]+attr(inspos, 'match.length')[length(inspos)]-1
        htmlfile <- paste0(substr(htmlfile,1,pos),
                           '\n',
                           textR,
                           substr(htmlfile,pos+1,nchar(htmlfile)))
      }
      inspos <- gregexpr(paste0('<br><br><br>[^<>]*<h2[^<]*</h2>[^<>]*<div id="barchart_div_',temp$n,'"[^<>]*></div>.*?</label></p>'), htmlfile)[[1]]
      if(inspos!=-1) {
        htmlfile <- paste0(substr(htmlfile,1,inspos-1),
                           textBodyR,
                           substr(htmlfile,inspos+attr(inspos, 'match.length'),nchar(htmlfile)))
      } else {
        inspos <- gregexpr('<div id="barchart_div_?[0-9]*"[^<>]*></div>.*?</label></p>', htmlfile)[[1]]
        pos <- inspos[length(inspos)]+attr(inspos, 'match.length')[length(inspos)]-1
        htmlfile <- paste0(substr(htmlfile,1,pos),
                           '\n\n',
                           textBodyR,
                           substr(htmlfile,pos+1,nchar(htmlfile)))
      }
      write_file(htmlfile,path=paste0(Paths$IO,'/',PP$GglPlotFileName))
    }
    if(multiPlot[[1]]==StandardPlot) {
      inspos <- gregexpr(paste0('<br><br><br>[^<>]*<h2[^<]*</h2>[^<>]*<div id="barchart_div_.?"[^<>]*></div>[^<>]*<p[^<>]*><label>(([^<>]*)|(<br>))*</label></p>',
                                '[^<>]*<br><br>[^<>]*<p[^<>]*><label>(([^<>]*)|(<br>)|(<a[^<>]*>[^<>]*</a>))*</label></p>'), htmlfile)[[1]]
      inspos <- gregexpr('<label>(([^<>]*)|(<br>))*</label>', substr(htmlfile, inspos, inspos+attr(inspos, 'match.length')-1))[[1]]+inspos-1
      inspos <- c(inspos[length(inspos)],attr(inspos, 'match.length')[length(inspos)]) # Ugly but works: inspos[1]=index of last match, inspos[2]=match.length
      htmlfile <- paste0(substr(htmlfile,1,inspos[1]-1),
                         '<label>',
                         GglLabels$Tekst[GglLabels$Plotnr==99],
                         '</label>',
                         substr(htmlfile,inspos[1]+inspos[2],nchar(htmlfile)))
      if(PP$GglPlotGlobLinks) {
        htmlfile <- gsub('href="/','href="http://www.narcis.nl/', htmlfile)
      }
      # And delete graph 2, if present
      if(F) {
        inspos <- regexpr(paste0('<script[^<>]*barchartscript">[^<>]*',
                                 '\\/\\*<!\\[CDATA\\[\\*\\/[^<>]*',
                                 'barchart_div_2[^<>]*',
                                 '\\/\\*\\]\\]>\\*\\/[^<>]*',
                                 '</script>'), htmlfile)
        htmlfile <- paste0(substr(htmlfile, 1, inspos-1), substr(htmlfile, inspos+attr(inspos, 'match.length')+1, nchar(htmlfile)))
        inspos <- regexpr(paste0('<label>This chart shows the actual number of open and closed access articles in NARCIS, since 2000\\.<\\/label>'), htmlfile)
        inspos2 <- gregexpr('<\\/p>', substr(htmlfile, inspos-1000, inspos))
        inspos2 <- inspos2[[length(inspos2)]]+inspos-998
        inspos <- regexpr('<br>', substr(htmlfile, inspos+attr(inspos, 'match.length'), inspos+attr(inspos, 'match.length')+1000))+inspos+attr(inspos, 'match.length')-1
        htmlfile <- paste0(substr(htmlfile, 1, inspos2),'\n',substr(htmlfile, inspos, nchar(htmlfile)))
      }
      
      write_file(htmlfile,
                 path=paste0(Paths$IO,'/',PP$GglPlotFileName))
    }
  }
  print(paste('Graph',StandardPlot,'completed'))
  if(PP$PrintPlot && readline('Press any key to continue')!='StopNow') {}
}

  
  
  






























