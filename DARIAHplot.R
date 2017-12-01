if (!all(sapply(c('Paths','libinstandload'), exists))|| is.null(Paths$input)) source(paste0(getSrcDirectory(function(x) {x}), '/SetLocal.r'))

if(!exists('Paths')) Paths <- list()
Paths$exc <- paste0(Paths$input,'/DARIAH-timeline.xlsx')
{
  sigmoid <- function(x, reach=1, steep=10) {
  if(is.na(steep) || steep==0) steep <- 10
  reach/(1+exp(-steep*2*x+steep))
}
  getColorByName <- function(name) {
  name <- tolower(name)
  ifelse(name %in% colors() | grepl('^#([0-9A-Fa-f]{2}){3,4}$', name), name, '#000000')
}
  ColContrast <- function(bg, contrast='BW30') {
  if(!nchar(bg) %in% c(7,9) || substr(bg,1,1)!='#') {
    if(bg %in% colors()) {
      bg <- col2rgb(bg)
      bgr <- as.character(as.hexmode(bg[1,]))
      bgg <- as.character(as.hexmode(bg[2,]))
      bgb <- as.character(as.hexmode(bg[3,]))
      bga <- 'FF'
    } else {
      print(paste('Warning: ColContrast called with invalid color:',bg))
      return('#000000')
    }
  } else {
    bgr <- substr(bg,2,3)
    bgg <- substr(bg,4,5)
    bgb <- substr(bg,6,7)
    bga <- ifelse(nchar(bg)>7, substr(bg,8,9), 'FF')
  }
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
    coltotal <- (strtoi(bgr, 16L)+2*strtoi(bgg, 16L)+strtoi(bgb, 16L)/2)*strtoi(bga, 16L)/255+896*(1-strtoi(bga, 16L)/255)
    return(ifelse(coltotal>cutoff,'#000000','#FFFFFF'))
  }
  return(paste0('#',bgr,bgg,bgb))
  }
  if(!exists('libinstandload')) libinstandload <- function(..., order=F, quiet=T) {
    packages <- as.vector(list(...))
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
        install.packages(packages[install])
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
} # Helper functions
libinstandload('ggplot2', 'plyr', 'lubridate','readxl','tidyr','purrr') # extrafont is loaded later, beacuse we want the FontPath to be initialised first

{
  input <- read_excel(Paths$exc, sheet='Parameters', col_types='text')
  input <- input[!is.na(input$Name) & input$Name!='' & input$Name!=0 & substring(input$Name,1,1)!='#',]
  Params <- as.list(input$Value)
  names(Params) <- input$Name
  Params <- as.list(suppressWarnings(ifelse(!is.na(as.numeric(Params)) |is.na(Params), as.numeric(Params), Params)))
  Params$CircleWidthCorr <- Params$CircleWidthCorr/86400
  Params$DivColorXBrdr <- days(Params$DivColorXBrdr)
  Params$Xfrom <- as.Date(Params$Xfrom, origin='1899-12-30')
  Params$Xto <- as.Date(Params$Xto, origin='1899-12-30')
  Paths$plots <- Params$PlotPath
  Paths$Fonts <- Params$FontPath
  libinstandload('extrafont')
} # Read parameters
{
  input <- read_excel(Paths$exc, sheet = 'Lines', col_types='text')
  input <- as.data.frame(input[!is.na(input$Workgroup) & input$Workgroup!='' & input$Workgroup!=0 & substring(input$Workgroup,1,1)!='#',])
  input <- {data.frame(Workgroup=input[1],
                       FromDate=as.Date(as.numeric(input[,2]), origin='1899-12-30'),
                       ToDate=as.Date(as.numeric(input[,3]), origin='1899-12-30'),
                       SlopeFrom=tolower(input[,4]),
                       SlopeTo=tolower(input[,5]),
                       PosStart=as.numeric(input[,6]),
                       PosTo=as.numeric(input[,7]),
                       SizeStart=as.numeric(input[,8]),
                       SizeTo=as.numeric(input[,9]),
                       Color=tolower(input[,10]),
                       ColorTo=tolower(input[,11]),
                       ColorSteps=as.numeric(input[,12]),
                       CurveSteepness=as.numeric(input[,13]),
                       SizeLinPar=as.numeric(input[,14]), stringsAsFactors = F)}
  input$SlopeFrom[input$SlopeFrom=='free'] <- 999
  input$SlopeTo[input$SlopeTo=='free'] <- 999
  input$SlopeFrom <- as.numeric(input$SlopeFrom)
  input$SlopeTo <- as.numeric(input$SlopeTo)
  input$SizeLinPar[is.na(input$SizeLinPar)] <- 0
  lines <- data.frame(color=character(), stringsAsFactors = F)
  for(i in 1:nrow(input)) {
    nr <- 0:as.numeric(difftime(input$ToDate[i], input$FromDate[i], units='days'))
    nr <- nr/(length(nr)-1) # So it runs from 0 to 1 inclusive
    slopefrom <- input$SlopeFrom[i]*(as.numeric(difftime(input$ToDate[i], input$FromDate[i], units='days'))/365) #input$SlopeFrom is in lines/year, this is in lines per reach, because nr is from 0:1
    slopeto <- input$SlopeTo[i]*(as.numeric(difftime(input$ToDate[i], input$FromDate[i], units='days'))/365)
    slopefree <- 'none'
    if(input$SlopeFrom[i]==999) {
      slopefrom <- slopeto
      slopefree <- 'left'
    }
    if(input$SlopeTo[i]==999) {
      slopeto <- slopefrom
      slopefree <- 'right'
    }
    
    newline <- data.frame(date=seq(input$FromDate[i],input$ToDate[i], by=2*((input$FromDate[i]<input$ToDate[i])-.5)),
                          pos=input$PosStart[i]+slopefrom*nr+.5*(slopeto-slopefrom)*nr^2)
    lowerslopefrom <- slopefrom-input$SizeLinPar[i]*(input$SizeTo[i]-input$SizeStart[i])/2
    upperslopefrom <- slopefrom+input$SizeLinPar[i]*(input$SizeTo[i]-input$SizeStart[i])/2
    lowerslopeto <- slopeto-input$SizeLinPar[i]*(input$SizeTo[i]-input$SizeStart[i])/2
    upperslopeto <- slopeto+input$SizeLinPar[i]*(input$SizeTo[i]-input$SizeStart[i])/2
    
    newline$lower <- input$PosStart[i]-input$SizeStart[i]/2+lowerslopefrom*nr+.5*(lowerslopeto-lowerslopefrom)*nr^2
    newline$upper <- input$PosStart[i]+input$SizeStart[i]/2+upperslopefrom*nr+.5*(upperslopeto-upperslopefrom)*nr^2
    
    if(slopefree=='left') {
      reach <- input$PosTo[i]-newline$pos[nrow(newline)]
      newline$pos <- newline$pos+sigmoid(nr/2+.5, 2*reach, steep=input$CurveSteepness[i])-reach
      reach <- input$PosTo[i]-input$SizeTo[i]/2-newline$lower[nrow(newline)]
      newline$lower <- newline$lower+sigmoid(nr/2+.5, 2*reach, steep=input$CurveSteepness[i])-reach
      reach <- input$PosTo[i]+input$SizeTo[i]/2-newline$upper[nrow(newline)]
      newline$upper <- newline$upper+sigmoid(nr/2+.5, 2*reach, steep=input$CurveSteepness[i])-reach
    } else if(slopefree=='right') {
      reach <- input$PosTo[i]-newline$pos[nrow(newline)]
      newline$pos <- newline$pos+sigmoid(nr/2, 2*reach, steep=input$CurveSteepness[i])
      reach <- input$PosTo[i]-input$SizeTo[i]/2-newline$lower[nrow(newline)]
      newline$lower <- newline$lower+sigmoid(nr/2, 2*reach, steep=input$CurveSteepness[i])
      reach <- input$PosTo[i]+input$SizeTo[i]/2-newline$upper[nrow(newline)]
      newline$upper <- newline$upper+sigmoid(nr/2, 2*reach, steep=input$CurveSteepness[i])
    } else {
      newline$pos <- newline$pos+sigmoid(nr,reach=input$PosTo[i]-newline$pos[nrow(newline)], steep=input$CurveSteepness[i])
      newline$lower <- newline$lower+sigmoid(nr, reach=input$PosTo[i]-input$SizeTo[i]/2-newline$lower[nrow(newline)],
                                             steep=input$CurveSteepness[i])
      newline$upper <- newline$upper+sigmoid(nr, reach=input$PosTo[i]+input$SizeTo[i]/2-newline$upper[nrow(newline)],
                                             steep=input$CurveSteepness[i])
    }
    newline$WG <- input$Workgroup[i]
    newline$ColName <- input$Color[i]
    if(!is.na(input$ColorTo[i]) && input$ColorTo[i]!=input$Color[i]) {
      endpoints <- round(seq(from=1, to=nrow(newline), length.out=input$ColorSteps[i]))
      idcs <- c(1:nrow(newline), endpoints[2:(length(endpoints)-1)])
      idcs <- idcs[order(idcs)]
      clrs <- 2:(length(idcs)+1)-idcs
      clrs <- paste0('#', apply(colorRamp(c(input$Color[i], input$ColorTo[i]), alpha=T)((clrs-.5)/max(clrs)),
                                1,function(c) {paste0(format(as.hexmode(round(c)), width=2), collapse='')}))
      newline <- newline[idcs,]
      newline$color <- clrs
      newline$date[duplicated(idcs)] <- newline$date[duplicated(idcs)]-Params$DivColorXBrdr
    }
    lines <- rbind.fill(lines, newline)
  }
  lines$color[is.na(lines$color)] <- sapply(lines$ColName[is.na(lines$color)], getColorByName)
} # Make the lines themselves
{
  input <- read_excel(Paths$exc, sheet='Endpoints', col_types='text')
  input <- as.data.frame(input[!is.na(input$Workgroup) & input$Workgroup!='' & input$Workgroup!=0 & substring(input$Workgroup,1,1)!='#',])
  points <- {data.frame(WG=input[,1],
                        date=as.Date(as.numeric(input[,2]), origin='1899-12-30'),
                        high=as.numeric(input[,3]),
                        wid=as.numeric(input[,4]),
                        color=tolower(input[,5]),
                        shape=tolower(input[,6]),
                        offset=as.numeric(input[,7]),
                        edgeCol=tolower(input[,8]),
                        edgeSize=as.numeric(input[,9]),
                        stringsAsFactors = F)}
  input <- read_excel(Paths$exc, sheet='Notations', col_types='text')
  input <- as.data.frame(input[!is.na(input$Workgroup) & input$Workgroup!='' & input$Workgroup!=0 & substring(input$Workgroup,1,1)!='#',])
  points <- rbind.fill(points, {data.frame(
    WG=input[,1],
    date=as.Date(as.numeric(input[,2]), origin='1899-12-30'),
    high=as.numeric(input[,3]),
    wid=as.numeric(input[,4]),
    color=tolower(input[,5]),
    shape=tolower(input[,6]),
    offset=as.numeric(input[,7]),
    edgeCol=tolower(input[,8]),
    edgeSize=as.numeric(input[,9]),
    stringsAsFactors = F)})
  points <- points[!is.na(points$high) & points$high>0,]
  points$offset[is.na(points$offset)] <- 0
  input <- points[!is.na(points$shape)& points$shape!='none',]
  points <- data.frame()
  for(i in 1:nrow(input)) {
    if(input$shape[i]=='start') {
      newpoints <- data.frame(x=cos(seq(from=.5, to=1.5, by=2/Params$PointcircleSteps)*pi)*input$wid[i]*Params$EndPointWidthCorr,
                              y=sin(seq(from=.5, to=1.5, by=2/Params$PointcircleSteps)*pi)*input$high[i]/2)
    } else if (input$shape[i]=='end') {
      newpoints <- data.frame(x=cos(seq(from=-.5, to=.5, by=2/Params$PointcircleSteps)*pi)*input$wid[i]*Params$EndPointWidthCorr,
                              y=sin(seq(from=-.5, to=.5, by=2/Params$PointcircleSteps)*pi)*input$high[i]/2)
    } else if (input$shape[i]=='none') {
      newpoints <- newpoints[1,]
      newpoints$x <- NA
      newpoints$y <- NA
    } else if(input$shape[i]=='circle') {
      newpoints <- data.frame(x=cos(seq(from=0, to=2, by=2/Params$PointcircleSteps)[-1]*pi)*input$wid[i]*Params$CircleWidthCorr*86400,
                              y=sin(seq(from=0, to=2, by=2/Params$PointcircleSteps)[-1]*pi)*input$high[i]/2)
    } else if(input$shape[i]=='tristart') {
      newpoints <- data.frame(x=c(0,0,-input$wid[i])*Params$TriWidthCorr,    # Order is high-low-tip
                              y=c(.5,-.5,0)*input$high[i]) 
    } else if(input$shape[i]=='triend') {
      newpoints <- data.frame(x=c(0,0,input$wid[i])*Params$TriWidthCorr,    # Order is high-low-tip
                              y=c(.5,-.5,0)*input$high[i]) 
    } else {
      print(paste0('Warning: Endpoint shape \'',input$shape[i],'\' is undefined. Defaulting to circle'))
      newpoints <- data.frame(x=cos(seq(from=0, to=2, by=2/Params$PointcircleSteps)[-1]*pi)*input$wid[i]*Params$CircleWidthCorr*86400,
                              y=sin(seq(from=0, to=2, by=2/Params$PointcircleSteps)[-1]*pi)*input$high[i]/2)
    }
    newpoints$y <- newpoints$y+lines$pos[lines$WG==input$WG[i] & lines$date==input$date[i]][1]+input$offset[i]
    newpoints$date <- input$date[i]+newpoints$x
    newpoints$WG <- input$WG[i]
    newpoints$group <- paste(newpoints$WG, input$date[i])
    newpoints$colName <- input$color[i]
    newpoints$color <- NA
    newpoints$edge <- input$edgeCol[i]
    newpoints$size <- input$edgeSize[i]
    points <- rbind.fill(points, newpoints)
  }
  points$color[is.na(points$color)] <- sapply(points$colName[is.na(points$color)], getColorByName) # The is.na check is redundant at this point, but for future implementation
  points$edge[is.na(points$edge)] <- points$color[is.na(points$edge)]
  points$size[is.na(points$size)] <- 0
} # Add endpoints
{
  input <- read_excel(Paths$exc, sheet='Names', col_types = 'text')
  input <- as.data.frame(input[!is.na(input$Workgroup) & input$Workgroup!='' & input$Workgroup!=0 & substring(input$Workgroup,1,1)!='#',])
  input <- {data.frame(Workgroup=input[,1],
                       Date=as.Date(as.numeric(input[,2]), origin='1899-12-30'),
                       Text=input[,3],
                       AbsRotation=as.numeric(input[,4]),
                       RotateToSlope=as.numeric(input[,5]),
                       PosAdjust=as.numeric(input[,6]),
                       TextSize=as.numeric(input[,7]),
                       TextColor=tolower(input[,8]),
                       TextAllignHoriz=tolower(input[,9]),
                       TextAllignVert=tolower(input[,10]),
                       Font=tolower(input[,11]),
                       stringsAsFactors = F)}
  texts <- input
  input <- read_excel(Paths$exc, sheet='Notations', col_types = 'text')
  input <- as.data.frame(input[!is.na(input$Workgroup) & input$Workgroup!='' & input$Workgroup!=0 & substring(input$Workgroup,1,1)!='#'&!is.na(input$Text),])
  input <- {data.frame(Workgroup=input[,1],
                       Date=as.Date(as.numeric(input[,2]), origin='1899-12-30'),
                       Text=input[,10],
                       AbsRotation=as.numeric(input[,11]),
                       RotateToSlope=as.numeric(input[,12]),
                       PosAdjust=as.numeric(input[,13]),
                       TextSize=as.numeric(input[,14]),
                       TextColor=tolower(input[,15]),
                       TextAllignHoriz=tolower(input[,16]),
                       TextAllignVert=tolower(input[,17]),
                       Font=tolower(input[,18]),
                       stringsAsFactors = F)}
  input <- rbind.fill(texts, input)
  fonts <- fonts()[match(input$Font, tolower(fonts()))]
  if(any(is.na(fonts) & !is.na(input$Font))) {
    print(paste0('Unknown fontfamily: ', unique(input$Font[is.na(fonts) & !is.na(input$Font)]),'.\n'))
    reCheckFonts <- readline("Should we check if new fonts are installed recently (which may take some time)? No means the standard font (Verdana) will be used. [y/n] ")
    if(reCheckFonts=='y') {
      font_import()
      if(!is.na(Params$FontPath)) font_import(Params$FontPath)
      loadfonts()
      loadfonts(device='postscript')
      fonts <- fonts()[match(input$Font, tolower(fonts()))]
      if(any(is.na(fonts) & !is.na(input$Font))) {
        print('Still not all fonts are recognized:')
        print(unique(input$Font[is.na(fonts) & !is.na(input$Font)]))
        print('Defaulting to Verdana')
      }
    }
  }
  fonts[is.na(fonts)] <- 'Verdana'
  input$Font <- fonts
  texts <- data.frame()
  for(i in 1:nrow(input)) {
    lineidx <- which.min(abs(difftime(lines$date,input$Date[i], units='days'))+(lines$WG!=input$Workgroup[i])*1000)
    newtext <- {data.frame(WG=input$Workgroup[i],
                          date=input$Date[i],
                          text=gsub('\\\\n','\n',input$Text[i]),
                          size=ifelse(is.na(input$TextSize[i]),Params$StdTxtSize, as.numeric(as.character(input$TextSize[i]))),
                          color=ifelse(is.na(input$TextColor[i]), 
                                       ifelse(is.na(input$PosAdjust[i]) || input$PosAdjust[i]<lines$upper[lineidx]-lines$pos[lineidx],
                                              ColContrast(lines$color[lineidx], contrast = paste0('BW',Params$ColorToBlackCutoff)),
                                              'black'),
                                       getColorByName(input$TextColor[i])),
                          font=input$Font[i],
                          stringsAsFactors = F)}
    poss <- lines[lines$WG==newtext$WG,]
    p1 <- poss[poss$date<newtext$date,][which.max(poss$date[poss$date<newtext$date]),] # Closest smaller value
    if(nrow(p1)==0) p1 <- poss[poss$date==newtext$date,]
    if(nrow(p1)==0) p1 <- poss[which.min(poss$date),]
    p2 <- poss[poss$date>newtext$date,][which.min(poss$date[poss$date>newtext$date]),] # Closest larger value
    if(nrow(p2)==0) p2 <- poss[poss$date==newtext$date,]
    if(nrow(p2)==0) p2 <- poss[which.max(poss$date),]
    if(p1$date==p2$date) {
      slope <- 0
    } else {
      slope <- Params$AngleSlopeCorr*(p2$pos-p1$pos)/as.numeric(difftime(p2$date, p1$date, units='days'))
    }
    newtext$rotate <- atan(slope)*360/(2*pi)
    if(!is.na(input$RotateToSlope[i])) newtext$rotate <- newtext$rotate+input$RotateToSlope[i]
    if(!is.na(input$AbsRotation[i])) newtext$rotate <- newtext$rotate+input$AbsRotation[i]
    newtext$pos <- lines$pos[lineidx]+ifelse(is.na(input$PosAdjust[i]),0,input$PosAdjust[i])
    newtext$hjust <- .5
    if(tolower(input$TextAllignHoriz[i])=='left') newtext$hjust <- 0
    if(tolower(input$TextAllignHoriz[i])=='right') newtext$hjust <- 1
    suppressWarnings(if(!is.na(as.numeric(input$TextAllignHoriz[i]))) newtext$hjust <- as.numeric(input$TextAllignHoriz[i]))
    newtext$vjust <- 0
    if(tolower(input$TextAllignVert[i])=='mid') newtext$vjust <- 0.5
    if(tolower(input$TextAllignVert[i])=='top') newtext$vjust <- 1
    suppressWarnings(if(!is.na(as.numeric(input$TextAllignVert[i]))) newtext$vjust <- as.numeric(input$TextAllignVert[i]))
    texts <- rbind.fill(texts, newtext)
  }
  texts <- texts[!is.na(texts$text),]
} # Add textlabels
{
  input <- read_excel(Paths$exc, sheet = 'Legend', col_types = 'text')
  input <- as.data.frame(input[!is.na(input$Type) & input$Type!='' & input$Type!=0 & substring(input$Type,1,1)!='#',])
  legend <- {data.frame(Title=input[,1],
                       Type=input[,2],
                       Value=input[,3],
                       Description=input[,4],
                       order=as.numeric(input[,5]))}
  legend <- legend[legend$order>0,]
  legend <- legend[order(legend$order),]
} # Read legend info
{
  ribbon <- lines
  lines <- gather(lines, which, y, lower, upper)
  points$Type <- 'points'
  lines$Type <- 'lines'
  ribbon$Type <- 'ribbon'
  texts$Type <- 'text'
  data <- rbind.fill(lines, points, ribbon, texts)
  data$WG <- factor(data$WG, levels=unique(lines$WG)) # To keep the order intact, WGs without lines should give an error
} # Put everything in dataframe
plot <- {ggplot() +
  purrr::map(.x=split(data, data$WG), .f=function(d) {
    list(geom_ribbon(data=d[d$Type=='ribbon',], aes(x=date, ymin=lower, ymax=upper, group=paste(WG,color), color=color, fill=color), size=0),
         geom_line(data=d[d$Type=='lines',],aes(x=date, y=y, group=paste0(WG,which), color=color),size=0),
         geom_polygon(data=d[d$Type=='points',], aes(x=date, y=y, group=group, color=edge, fill=color, size=size)))
  }) +
  geom_text(data=data[data$Type=='text',], 
            aes(x=date, y=pos, label=text, angle=rotate, colour=color, size=size, hjust=hjust, vjust=vjust, family=font), 
            show.legend = F) + # Seperate to always keep text on top
  scale_color_identity(breaks=tolower(legend$Value[legend$Type=='Color']), 
                       labels=legend$Description[legend$Type=='Color'],
                       guide='legend') +
  scale_fill_identity(breaks=tolower(legend$Value[legend$Type=='Color']), 
                      labels=legend$Description[legend$Type=='Color'],
                      guide='legend') +
  scale_size_identity() +
  guides(color=guide_legend(title=paste(legend$Title[legend$Type=='Color' & !is.na(legend$Title)][1]),order=legend$order[legend$Type=='Color'][1]),
         fill=guide_legend(title=paste(legend$Title[legend$Type=='Color' & !is.na(legend$Title)][1]),order=legend$order[legend$Type=='Color'][1]))}
if(!is.na(Params$Xfrom) && !is.na(Params$Xto)) {
  if(!is.na(Params$Yfrom) && !is.na(Params$Yto)) {
    plot <- plot +
      coord_cartesian(xlim=c(Params$Xfrom, Params$Xto), 
                      ylim=c(Params$Yfrom, Params$Yto), expand=F)
  } else {
    plot <- plot +
      coord_cartesian(xlim=c(Params$Xfrom, Params$Xto)) +
      scale_x_date(expand = c(0,0))
  }
} else {
  if(!is.na(Params$Yfrom) && !is.na(Params$Yto)) {
    plot <- plot +
      coord_cartesian(ylim=c(Params$Yfrom, Params$Yto), expand=F)
  }
}
print(plot)
{ggsave(Paths$plots, 
       plot,
       scale=(Params$Saveplot.res.x+Params$Saveplot.res.y)/(500*Params$ScaleFac),
       width=Params$Saveplot.res.x/Params$Saveplot.dpi, 
       height=Params$Saveplot.res.y/Params$Saveplot.dpi,
       dpi=Params$Saveplot.dpi,
       units='in')} # Save































