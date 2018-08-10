#' Jones Datum object
#'
#' @field fileName Provide the full path to teh file
#' @field metaData no input. contains metadata and info for file
#' @field data no input. list of data types: Raw, Adjusted, Rescaled and Time vector
#' @field background no input. contains, if applicable, lum, roi and area of background roi
#' @field ares no input. data.frame of cell# and corresponding roi area
#' @field rois no input. named list for each roi in the dataset
#' @field image no input. Contains the image data, if applicable. 
#'
#' @export jonesDatum
#' @exportClass jonesDatum
#' @import grDevices
jonesDatum<- setRefClass('jonesDatum',
  fields=list(
    fileName='character',
    metaData='list',
    data='list',
    background='list',
    areas='data.frame',
    rois='list',
    image='list'
  ),
  methods=list(
    initialize = function(fileName='',...) {################################################################### init
      #handle copy case
      if (fileName == '') {
        return(.self)
      }
      
      .self$fileName = basename(fileName) %>%
        sub(x = .,pattern = '\\.[^\\.]*$',replacement = '')
      
      dat<- fileImporter(filePath = fileName,...)
      
      .self$areas <- dat[[1]]$Areas
      .self$rois <- dat[[1]]$ROIs
      .self$image <- dat[[1]]$Image
      .self$background <- dat[[1]]$Background
      
      ## parse metadata
      meta <- dat[[1]]$Info
      meta$nROI <- meta$nCells
      meta$nCells <- meta$nROI
      #exp date
      meta$ExperimentDate <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'date of experiment',2
        ] %>% unname()
      # celltype
      meta$CellType <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'cells:', 2
        ] %>% unname()
      meta$PassageNumber <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'Passage:', 2
        ] %>% unname()
      meta$MediaSupl <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'cultured in:', 2
        ] %>% unname()
      
      
      meta$StarvedLength <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'serum starved x :',2
        ] %>% unname()
      meta$Confluency <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'Density',2
        ] %>% unname() %>% as.numeric()
      meta$Treatment <- if (!is.na(meta$TreatmentStart)){'ET1'} else {'none'}
      
      meta$SecondaryTreatment<- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'other treatments',2
        ] %>% unname() %>% as.character()
      meta$CellIDs <- colnames(dat[[1]]$Data)
      # assign into .self
      .self$metaData <- meta
      
      ## parse data
      .self$data <- list(
        Time= list(data=dat[[1]]$Time,units='sec'),
        Raw=  list(data=dat[[1]]$Original,units='Lum'),
        Adjusted= list(data=dat[[1]]$Data,units=expression(frac(paste(Delta, F),F[0]))),
        Standardized= list(data=dat[[1]]$Standardized,units='AU'),
        Corrected= list(data=dat[[1]]$Corrected,units=expression(frac(paste(Delta, F),F[0]))),
        Background= list(data=dat[[1]]$Background$Data,units='Lum')
      )
      
      #return
      invisible(.self)
    },
    show= function ( ){################################################################### Show/copy/save/plotImage
      "Display important information from the file."
      cat (" Jones Data File \ n ")
      cat (" Exp. Date :" , .self$metaData$ExperimentDate , "\n")
      cat (" Cell Type :" , .self$metaData$CellType , "\n")
      cat (" Treatment :" , .self$metaData$Treatment , "\n")
      cat (" Other Treatments :" , .self$metaData$SecondaryTreatment , "\n")
      cat (" Confluency :", paste0(.self$metaData$Confluency*100, '% ') , "\n")
      cat (" Passage Number :" , .self$metaData$PassageNumber , "\n")
      cat (" Data Containes:\n     ", 
           paste(
             paste0(.self$metaData$nROI, ' ROIs,'),
             sprintf('%sbackground,', ifelse(.self$metaData$HasBackground,'including ', 'without ')),
             sprintf('%d samples long', length(.self$data$Time$data)) 
           ),
           "\n")
      cat ("\n")
    },
    copy = function() {
      "Return a copy of the object."
      theCopy <- jonesDatum('')
      theCopy$fileName=.self$fileName
      theCopy$metaData= .self$metaData
      theCopy$data= .self$data
      theCopy$background= .self$background
      theCopy$areas= .self$areas
      theCopy$rois= .self$rois
      theCopy$image= .self$image
      return(theCopy)
    },
    save = function(pathName,objectName='',...){
      "Save the current object on the file in R external object format."
      if (missing(pathName)){
        pathName = file.path(
          getwd(),
          paste0(
            'datum_',
            gsub(x = Sys.Date(), pattern = '[[:punct:]]',replacement = ''),
            '.rda'
          )
        )
      }
      if (objectName == ''){
        objectName <- paste(
          .self$fileName,
          gsub(x = Sys.Date(), pattern = '[[:punct:]]',replacement = ''),
          sep='_'
        )
      }
      #give obj a name
      objCall <- sprintf('%s <- .self$copy()', objectName)
      eval(parse(text=objCall))
      saveCall <- sprintf('base::save("%s", file = "%s", ...)', objectName,pathName)
      eval(parse(text=saveCall))
      cat('\n', sprintf('Saving object %s', objectName), '\n')
      cat(sprintf('located in %s', pathName),'\n\n')
    },
    plotImage = function(
      channel=c('green','all','red','blue'), 
      ofst=0, 
      greyscale= FALSE,
      equalize= TRUE,
      gamma=2.2,
      colors = NULL,
      texts = NULL,
      draw.roi = TRUE,
      roi.alpha  = 0.5,
      draw.text = TRUE,
      text.color = NULL,
      ...) {
      "Plots the corresponding image if applicable. \\cr
      \\code{channel}:    Choose a specific channel or 'all' for all channels.\\cr
      \\code{ofst}:       Value to set unchosen channels on [0,1] can reduce contrast\\cr
      \\code{greyscale}:  boolean, convert image to greyscale (2d indexed)\\cr
      \\code{equalize}:   boolean, Use histogram equalization\\cr
      \\code{gamma}:      double, used if greyscale or hist equalization\\cr
      \\code{colors}:     hex color matrix for ROIs (defaults to all red)\\cr
      \\code{draw.roi}    boolean, Draw the ROIs over the image \\cr
      \\code{roi.alpha}   double, Transparency level for ROI on [0,1]\\cr
      \\code{draw.text}   boolean, Draw text labels, provided or otheriwse, at the centroid of each ROI\\cr
      \\code{text.color}  array, Hex color vector. If scalar, color is applied to all text, otherwise
      it must be the same length as .self$nCells().\\cr
      \\code{...}:        Parameters passed to \\code{plot.window()}.
      "
      #"#9e0142" "#d53e4f" "#f46d43" "#fdae61" "#fee08b" "#ffffbf" "#e6f598" "#abdda4" "#66c2a5" "#3288bd" "#5e4fa2"
      channel <- match.arg(channel)
      stopifnot(length(ofst)==1,ofst >= 0, ofst <= 1)
      
      if (!.self$image$HasImage){
        writeLines('No Image file to be drawn')
        return()
      }
      draw.points <- if(draw.text){FALSE} else {TRUE}
      
      imageMatrix <- array(ofst,dim=c(.self$image$Data@size,3))
      switch(channel,
        green = {
          imageMatrix[,,2]<- .self$image$Data@green
          if(is.null(colors)){
            colors <- rep("#d53e4f", .self$nCells())
          }
          if(is.null(text.color)){
            text.color <- rep("#e6f598ff", .self$nCells())
          }
        },
        red = {
          imageMatrix[,,1]<- .self$image$Data@red
          if(is.null(colors)){
            colors <- rep("#3288bd", .self$nCells())
          }
          if(is.null(text.color)){
            text.color <- rep("#fdae61ff", .self$nCells())
          }
        },
        blue = {
          imageMatrix[,,3]<- .self$image$Data@blue
          if(is.null(colors)){
            colors <- rep("#d53e4f", .self$nCells())
          }
          if(is.null(text.color)){
            text.color <- rep("#e6f598ff", .self$nCells())
          }
        },
        {
          for (col in 1:3){
            imageMatrix[,,col] <- slot(.self$image$Data,c('red','green','blue')[col])
            if(is.null(colors)){
              colors <- rep("#e6f598", .self$nCells())
            }
            if(is.null(text.color)){
              text.color <- rep("#f46d43ff", .self$nCells())
            }
          }
        }
      )
      # greyscale
      colorIndex <- c('red','green','blue') %in% channel
      if(greyscale){
        #convert to 2D matrix
        if(!any(colorIndex)){
          # use all channels to compute intensity map compute Lum map
          imageMatrix <- getLum(imageMatrix,gamma)
        } else {
          # use selected channel
          if (max(imageMatrix)>1) { imageMatrix <- imageMatrix/256 }
          imageMatrix <- imageMatrix[,,which(colorIndex)]
        }
        # To keep output consistent, let's convert this to rgb
        imageMatrix <- array(imageMatrix,c(.self$image$Data@size,3))
      }
      # equalize
      if(equalize){
        # equalize forces greyscale
        
        lum <- getLum(imageMatrix,gamma)
        eqLum <- equalizeHist(lum)
        imageMatrix <- array(eqLum,dim=c(.self$image$Data@size,3))
      }
      
      # check texts for plotting
      if (is.null(texts)){
        texts <- sprintf('%d', 1:.self$nCells())
      }
      ##### Plot
      oldPar <- par(no.readonly = TRUE)
      par(mar = c(0,0,0,0)+0.1, family = 'serif', ps = 20)
      
      picDims <- .self$image$Data@size
      
      #draw the plot
      plot.new()
      plot.window(c(1,picDims[1]), c(1,picDims[2]),asp=1, ...)
      
      rasterImage(imageMatrix,1,1,picDims[1],picDims[2],interpolate = TRUE)
      
      if(draw.roi){
        alphaCols <- col2rgb(colors,alpha = TRUE)/255
        alphaCols['alpha', ] <- roi.alpha
        alphaCols <- rgb(
          alphaCols['red', ],
          alphaCols['green', ],
          alphaCols['blue', ],
          alphaCols['alpha', ]
        )
        alphaCols[is.na(colors)] <- NA
        #draw roi
        for (r in 1:.self$nCells()){
          Xs <- .self$rois[[r]]$X.Pixels
          Ys <- picDims[2] - .self$rois[[r]]$Y.Pixels
          polygon(
            x = Xs,
            y = Ys,
            col = alphaCols[r],
            border = colors[r],
            lwd = 3
          )
        }
      }
      if (length(text.color) < .self$nCells()){
        text.color <- rep(text.color, .self$nCells())[1:.self$nCells()]
      }
      if(draw.text){
        
        for (r in 1:.self$nCells()){
          #get center of best ellipsoid
          roiCenter <- fitEllipse(
            .self$rois[[r]]$X.Pixels,
            picDims[2] - .self$rois[[r]]$Y.Pixels
            )$center
          
          text(
            x = roiCenter['x'],
            y = roiCenter['y'],
            labels = texts[r],
            col = text.color[r],
            pos = 3,
            font = 4,
            offset = -0.3,
            cex= 1.2
          )
        }
      } else if(draw.points) {
        xys <- sapply(
          X = .self$rois,
          function(ROI) {
            fitEllipse(
              ROI$X.Pixels,
              picDims[2] - ROI$Y.Pixels
            )$center
          }
        )
        points(
          x = xys['x',],
          y = xys['y',],
          pch = 19,
          cex = 2,
          col=text.color
        )
          
      }
      #Return graphical parameters
      par(oldPar)
      
    },################################################################### Getters
    getConfluence = function(){
      "Report confluence as a percent (0-100)."
      return(.self$metaData$Confluency*100)
    },
    getTreatment = function(){
      "Report Treatment type, typically ET1."
      return(
        .self$metaData$Treatment
      )
    },
    getOtherTreatment = function(){
      "Check the other treatments, typically IL-1b."
      return(
        .self$metaData$SecondaryTreatment
      )
    },
    getMeta = function(whichField){
      if (all(whichField %in% names(.self$metaData))) {
        mDat <- sapply(
          X = whichField,
          FUN= function(X){
            .self$metaData[[X]]
          }
        )
        names(mDat) <- NULL
        return(mDat)
      }
      return(NULL)
    },
    nCells = function(){
      return(.self$metaData$nCells)
    },
    getIDs = function(){
      "Get the names of the cell IDs"
      return(colnames(.self$data$Adjusted$data))
      
    },
    getData = function(type=c('adjusted', 'standardized','raw','background','corrected')){
      "Extract full dataset of provided type."
      type = match.arg(type)
      output <- list(
        x = .self$data$Time$data,
        units = c(x=.self$data$Time$units)
      )
      switch(type,
             adjusted = {
               output$y = .self$data$Adjusted$data
               output$units <- append(output$units,c(y=.self$data$Adjusted$units))
             },
             standardized = {
               output$y = .self$data$Standardized$data
               output$units <- append(output$units,c(y=.self$data$Standardized$units))
             },
             raw = {
               output$y = .self$data$Raw$data
               output$units <- append(output$units,c(y=.self$data$Raw$units))
             },
             corrected = {
               output$y = .self$data$Corrected$data
               output$units <- append(output$units,c(y=.self$data$Corrected$units))
             },
             background = {
               output$y <- as.matrix(data.frame(background=.self$data$Background$data))
               output$units <- append(output$units,c(y=.self$data$Background$units))
             }
      )
      obj <- dataList(
        X = output$x,
        Y = output$y,
        META =  list(names=colnames(output$y), units = output$units)
      )
      return(obj)
    },
    getDataAsList = function(type=c('adjusted','standardized','raw','background','corected')){
      "Extract the data as a named list of X,Y datasets for each Cell."
      type = match.arg(type)
      ncells <- .self$metaData$nCells
      cellNames <- .self$metaData$CellIDs
      output <- vector('list', ncells)
      names(output) <- cellNames
      #get the corresponding data
      switch(type,
             adjusted = {
               for (cell in 1:ncells){
                 output[[cellNames[cell]]] <- list(
                   data = data.frame(
                     x = .self$data$Time$data,
                     y = .self$data$Adjusted$data[,cell]
                   ),
                   units = c(x=.self$data$Time$units, y=.self$data$Adjusted$units)
                 )
               }
             },
             standardized = {
               for (cell in 1:ncells){
                 output[[cellNames[cell]]] <- list(
                   data = data.frame(
                     x = .self$data$Time$data,
                     y = .self$data$Standardized$data[,cell]
                   ),
                   units = c(x=.self$data$Time$units, y=.self$data$Standardized$units)
                 )
               }
             },
             raw = {
               for (cell in 1:ncells){
                 output[[cellNames[cell]]] <- list(
                   data = data.frame(
                     x = .self$data$Time$data,
                     y = .self$data$Raw$data[,cell]
                   ),
                   units = c(x=.self$data$Time$units, y=.self$data$Raw$units)
                 )
               }
             },
             corrected = {
               for (cell in 1:ncells){
                 output[[cellNames[cell]]] <- list(
                   data = data.frame(
                     x = .self$data$Time$data,
                     y = .self$data$Corrected$data[,cell]
                   ),
                   units = c(x=.self$data$Time$units, y=.self$data$Corrected$units)
                 )
               }
             },
             background = {
               output <- .self$getData('background')
             },
             {
               output <- list()
             }
      )
      return(output)
    },################################################################### Logicals
    isConfluent = function(prcnt=100){
      "Check if confluence is == \\code{prcnt}."
      return(
        getConfluence() == prcnt
      )
    },
    isET1 = function(){
      "Check if ET1 is in the primary treatment."
      return(
        grepl('et[-]?1',tolower(getTreatment()))
      )
    },
    isIL1b = function(){
      "Check if IL1b is in other treatments."
      return(
        grepl('il[-]?1b', tolower(getOtherTreatment()))
      )
    }
  )
)



#' plot
#' 
#' @rdname jonesDatum-class
#' @param x jonesDatum object, required
#' @param y char of: 'raw', 'adjusted', 'scaled', 'background', 'corrected'
#' @param xlab X-axis label (units will be appended)
#' @param ylab Y-axis label (units will be appended)
#' @param axes bool, draw with or without axes (FALSE by default)
#' @param ... for plot: passed to plot.window. otherwise unused.
#' @exportMethod plot
setMethod("plot", 
  c(x="jonesDatum",y="character"),
  function(x,
    y=c('raw', 'adjusted', 'standardized', 'background', 'corrected'), 
    xlab ="" , ylab ="" , axes = FALSE ,  ...){
    y<- match.arg(y)
    plotData <- x$getData(y)
    
    if (x$isET1()){
      xadj <- x$getMeta('TreatmentStart')
    } else {
      xadj <- 0
    }
    
    oldPar = par(no.readonly = TRUE)
    par(
      family= 'serif', 
      ps= 16, 
      las= 1, 
      mar= par('mar')*c(1,1.15,1,1), 
      oma= c(2,2,1,0.5)
    )
    
    plot(plotData, y=NULL, xAdjust = xadj)
    
    if (x$isET1()){
      abline(v = 0, lwd = 4, col = 'red3')
    }
    # return params
    par(oldPar)
  })



#' subset
#' @rdname jonesDatum-class
#' @param subset logical test for rows
#' @param select logical test for columns
#' @param type type=c('adjusted', 'standardized','raw','background','corrected')
#' @param drop sent to indexing operation for \code{x[,,drop]}
#' @exportMethod subset
setMethod(
  "subset", 
  "jonesDatum",
  function(
    x,
    subset,
    select,
    type = c('adjusted','raw','standardized','background','corrected'),
    drop = FALSE,
    ...
  ){
    type <- match.arg(type)
    d <- x$getData(type)
    
    rows<- if(missing(subset)){
      rep_len(TRUE, nrow(d$y))
    } else {
      evalString <- substitute(subset)
      tCheck<- grepl('time',deparse(evalString),ignore.case = TRUE)
      if (tCheck){
        newStr <- gsub('time','d$x', deparse(evalString),ignore.case = TRUE) %>%
          parse(text=.)
        
        rowTmp <- eval(newStr, enclos = parent.frame())
      } else {
        rowTmp <- eval(evalString, as.data.frame(d$y), parent.frame())
      }
      if (!is.logical(rowTmp)){
        stop("'subset' must be logical!")
      }
      naCheck<- if(tCheck){!is.na(d$x)} else {!apply(d$y,1,function(x)sum(is.na(x))>0)}
      rowTmp & naCheck
    }
    vars <- if (missing(select)) 
      TRUE
    else {
      nl <- as.list(seq_along(d$y))
      names(nl) <- names(d$y)
      eval(substitute(select), nl)
    }
    return(
      list(x = d$x[rows,drop=drop],
           y = d$y[rows, vars, drop = drop]
       )
    )
  }
)


#' getLum
#' 
#' @param imgArr image array (2D or 3D ok)
#' @param gamma double gamma for lum calculation
getLum <- function(imgArr,gamma=2.2){
  if (max(imgArr) <= 1) { imgArr <- imgArr/256 }
  if ( length(dim(imgArr)) < 3 ){
    L <- imgArr^gamma
  } else {
    L <- apply(
      X = imgArr,
      MARGIN = c(1,2),
      FUN = function(X){
        0.2126 * X[1]^gamma + 0.7152 * X[2]^gamma + 0.0722 * X[3]^gamma
      }
    )
  }
  L <- 116*L^(1/3) - 16
  # scale to [0,1] just in case
  # (x - from[1])/diff(from) * diff(to) + to[1]
  return((L-min(L))/diff(range(L)))
}

#' equalizeHist
#' 
#' @param imgMat matrix, must be 2D
#' @param bitDepth int, number of bins for the hist
#' @param ... unsused
equalizeHist <- function(imgMat,bitDepth=2^8,...){
  #parse
  stopifnot(length(dim(imgMat))==2)
  if(max(imgMat)<=1){ imgMat <- imgMat * (bitDepth-1) }
  
  shape <- dim(imgMat)
  
  imgLimits <- range(imgMat)
  
  # create Bins for image histogram
  bL <- seq(imgLimits[1],imgLimits[2],length.out=bitDepth+1)
  dB <- mean(diff(bL))
  newRange <- sort(c(min(bL)-dB, c(max(bL)+dB)))
  bL <- (bL-min(bL))/diff(imgLimits) * diff(newRange) + newRange[1]
  
  h<- hist(imgMat,breaks=bL,plot=FALSE)
  
  nElm <- prod(shape)
  
  MapFunc <- cumsum(
      as.double(h$counts)/nElm
    ) * (bitDepth-1)
  
  eqImg <- MapFunc[as.integer(imgMat)+1]
  
  # convert back to [0,1] by clipping (shouldnt be needed)
  nr<- range(eqImg)
  return( matrix((eqImg - nr[1])/diff(nr),nrow = shape[1], ncol = shape[2]) )
}

#' fitEllipse
#' 
#' @param x vector of Xs or data frame with 2 columns
#' @param y vector of Ys or NOT
fitEllipse <- function (x, y = NULL) {
  # from:
  # http://r.789695.n4.nabble.com/Fitting-a-half-ellipse-curve-tp2719037p2720560.html
  # and:
  # http://lastresortsoftware.blogspot.com/2012/09/fitting-ellipse-to-point-data.html
  # Least squares fitting of an ellipse to point data
  # using the algorithm described in: 
  #   Radim Halir & Jan Flusser. 1998. 
  #   Numerically stable direct least squares fitting of ellipses. 
  #   Proceedings of the 6th International Conference in Central Europe 
  #   on Computer Graphics and Visualization. WSCG '98, p. 125-132 
  #
  # Adapted from the original Matlab code by Michael Bedward (2010)
  # michael.bedward@gmail.com
  #
  # Subsequently improved by John Minter (2012)
  # 
  # Arguments: 
  # x, y - x and y coordinates of the data points.
  #        If a single arg is provided it is assumed to be a
  #        two column matrix.
  #
  # Returns a list with the following elements: 
  #
  # coef - coefficients of the ellipse as described by the general 
  #        quadratic:  ax^2 + bxy + cy^2 + dx + ey + f = 0 
  #
  # center - center x and y
  #
  # major - major semi-axis length
  #
  # minor - minor semi-axis length
  #
  EPS <- 1.0e-8 
  dat <- xy.coords(x, y) 
  
  D1 <- cbind(dat$x * dat$x, dat$x * dat$y, dat$y * dat$y) 
  D2 <- cbind(dat$x, dat$y, 1) 
  S1 <- t(D1) %*% D1 
  S2 <- t(D1) %*% D2 
  S3 <- t(D2) %*% D2 
  T <- -solve(S3) %*% t(S2) 
  M <- S1 + S2 %*% T 
  M <- rbind(M[3,] / 2, -M[2,], M[1,] / 2) 
  evec <- eigen(M)$vec 
  cond <- 4 * evec[1,] * evec[3,] - evec[2,]^2 
  a1 <- evec[, which(cond > 0)] 
  f <- c(a1, T %*% a1) 
  names(f) <- letters[1:length(f)] 
  
  # calculate the center and lengths of the semi-axes 
  #
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2288654/
  # J. R. Minter
  # for the center, linear algebra to the rescue
  # center is the solution to the pair of equations
  # 2ax +  by + d = 0
  # bx  + 2cy + e = 0
  # or
  # | 2a   b |   |x|   |-d|
  # |  b  2c | * |y| = |-e|
  # or
  # A x = b
  # or
  # x = Ainv b
  # or
  # x = solve(A) %*% b
  A <- matrix(c(2*f[1], f[2], f[2], 2*f[3]), nrow=2, ncol=2, byrow=T )
  b <- matrix(c(-f[4], -f[5]), nrow=2, ncol=1, byrow=T)
  soln <- solve(A) %*% b
  
  b2 <- f[2]^2 / 4
  
  center <- c(soln[1], soln[2]) 
  names(center) <- c("x", "y") 
  
  num  <- 2 * (f[1] * f[5]^2 / 4 + f[3] * f[4]^2 / 4 + f[6] * b2 - f[2]*f[4]*f[5]/4 - f[1]*f[3]*f[6]) 
  den1 <- (b2 - f[1]*f[3]) 
  den2 <- sqrt((f[1] - f[3])^2 + 4*b2) 
  den3 <- f[1] + f[3] 
  
  semi.axes <- sqrt(c( num / (den1 * (den2 - den3)),  num / (den1 * (-den2 - den3)) )) 
  
  # calculate the angle of rotation 
  term <- (f[1] - f[3]) / f[2] 
  #angle <- atan(1 / term) / 2 #original
  angle <- atan2(f[2],f[1]-f[3]) / 2
  
  list(coef=f, center = center, major = max(semi.axes), minor = min(semi.axes), angle = unname(angle)) 
}

