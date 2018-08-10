#' dataList
#'
#' Class Definition for Storing formatted data with methods to access data, 
#' subest traces, append/remove files, 
#'
#' @field X vector or matrix
#' @field Y coerced to matrix, if no names present, CELL\%d will be applied
#' @field META stoarage of units and names
dataList<- setRefClass('dataList',
  fields=list(
    x = 'matrix',
    y = 'matrix',
    meta = 'list'
  ),
  methods=list(
    initialize = function(X,Y,META){
      # X
      if(!is.matrix(X)){
        X <- matrix(X,nrow = length(X))
      }
      .self$x <- X
      
      #Y
      switch(class(Y),
        data.frame = {
          names <- colnames(Y)
          Y <- as.matrix(Y)
        },
        matrix = {
          names <- colnames(Y)
        },
        numeric = {
          Y <- matrix(Y,nrow = length(Y))
          names <- 'y'
        },
        {stop("Undeterminable data type.")}
      )
      .self$y <- Y
      
      #META
      if ('names' %in% names(META)){
        names <- META$names
      }
      if ('units' %in% names(META)){
        Iunits <- META$units
      } else {
        Iunits <- c(x = 'sec', y='AU')
      }
      .self$meta <- list(
        names = names,
        units = Iunits
      )
      invisible(.self)
    },
    getUnits = function(dimension = c('x','y','xy')){
      dimension <- match.arg(dimension)
      if (dimension == 'xy'){ return(.self$meta$units) }
      return(.self$meta$units[dimension])
    },
    ID = function(){
      return(.self$meta$names)
    },
    shape = function(dimension = c('xy','x','y')){
      dimension <- match.arg(dimension)
      s <- dim(.self$y)
      names(s) <- c('x','y')
      switch(dimension,
          x = { return(s['x']) },
          y = { return(s['y']) },
          { return(s) }
      )
    }
  )
)


#' plot
#' 
#' @rdname dataList-class
#' @param x dataList object, required
#' @param y ANY: best if a vector of ints or names
#' @param xlab X-axis label (units will be appended)
#' @param ylab Y-axis label (units will be appended)
#' @param axes bool, draw with or without axes (FALSE by default)
#' @param ... for plot: passed to plot.window. otherwise unused.
#' @exportMethod plot
setMethod("plot", 
  c(x="dataList",y="ANY"),
  function(x,
    y=NULL, 
    xlab ="Time" , ylab ="" , axes = FALSE ,  ...
  ){
    #Parse Y
    if (is.null(y)){
      y <- 1:x$shape('y') 
    } else if (is.character(y)) {
      nameTest <- y %in% x$ID()
      stopifnot(!any(nameTest))
      y <- y[nameTest]
    } else if (is.numeric(y)){
      if(length(y)==1){
        y <- 1:y
      }
      y <- y[y<=x$shape('y')]
    } else {
      writeLines("Couldn't parse input 'y', using all.")
      y <- 1:x$shape('y')
    }
    if (requireNamespace("viridis", quietly = TRUE)) {
      theColors <- viridis::viridis(length(y)+1, alpha = 0.5)
    } else {
      theColors <- grDevices::terrain.colors(length(y),alpha = 0.5)
    }
    
    # setup plot parameters
    possiblePlotPar <- unique(
      c(
        names(formals('plot.window')), 
        names(formals('plot.default')),
        names(par())
      )
    )
    possiblePlotPar <- possiblePlotPar[!(possiblePlotPar %in% "...")]
    plotParams <- list(...)
    nonPlot <- plotParams[!(names(plotParams) %in% possiblePlotPar)]
    plotParams <- plotParams[(names(plotParams) %in% possiblePlotPar)]
    
    plotParams$xaxt <- plotParams$yaxt <- if(axes){'s'} else {'n'}
    
    
    if(!is.null(names(nonPlot)) && ('xAdjust' %in% names(nonPlot))){
      x$x <- x$x - nonPlot$xAdjust
    }
    
    if (!('xlim' %in% names(plotParams))){
      plotParams$xlim = range(x$x,na.rm=TRUE)
    }
    if (!('ylim' %in% names(plotParams))){
      plotParams$ylim = range(x$y,na.rm=TRUE)
    }
    plotParams$yaxs <- plotParams$xaxs <- 'i'
    
    #### MAKE FIGURE
    graphics::plot.new()
    
    do.call('plot.window',args = plotParams)
    
    for (pp in 1:length(y)){
      lines(
        x$x[ ,min(c(ncol(x$x), pp))],
        x$y[ ,y[pp]],
        lwd = 4,
        col = theColors[pp]
      )
      
    }
    
    axis(1, ps = 12)
    axis(2, ps = 12)
    
    title(
      xlab = paste(xlab,paste0(' ',x$getUnits('x'))),
      line = 2.25,
      cex.lab = 1.2
    )
    
    if (any(is.expression(ylab), is.expression(x$getUnits('y')))) {
      ylabel <- parse(
        text = paste(as.character(ylab), ' ', as.character(x$getUnits('y')))
      )
      parlas=1
    } else {
      ylabel = paste(ylab,paste0(' ',x$getUnits('y')))
      parlas=3
    }
    #draw ylab
    mtext(ylabel, 2, -1.5, outer=TRUE, las=parlas, cex=1.5)
    #make axes lines prettier
    abline(h = par('usr')[3], v = par('usr')[1],lwd=3)
    
    
  }
)