#Custom color library function
.color<- function(
  name = c('blue','sBlue1','sBlue2',
           'yellow','nYellow',
           'green','purple','pink',
           'cyan', 'black', 'gold',
           'custom'),
  opacity = 1,
  brightness = 1,#scale factor
  ...){
  name<- match.arg(name)
  opacity<- min(opacity,1)
  if (opacity < 0){opacity<- 0}
  switch(name,
         blue = {args<- list(   red=50 , green=132, blue=191)},
         sBlue1 = {args<- list( red=0  , green=85 , blue=166)},
         sBlue2 = {args<- list( red=0  , green=165, blue=229)},
         yellow = {args<- list( red=255, green=232, blue=0  )},
         nYellow = {args<- list(red=255, green=255, blue=0  )},
         green = {args<- list(  red=0  , green=255, blue=135)},
         purple = {args<- list( red=130, green=55 , blue=255)},
         pink = {args<- list(   red=255, green=0  , blue=165)},
         cyan = {args<- list(   red=0  , green=255, blue=255)},
         black = {args<- list(  red=0  , green=0  , blue=0  )},
         gold = {args<- list(   red=254, green=187, blue=54 )},
         custom = {args<- list( ... )},
         {args<- list(          red=50 , green=132, blue=191)}#ucla blue
  )
  args<- c(
    lapply(args,
           FUN = function(x){
             if(is.na(x*brightness)){
               x
             } else if(x*brightness>255){
               255
             } else if(x*brightness<0){
               0
             } else {
               x*brightness
             }
           }
    ),
    list(maxColorValue = 255, alpha=255*opacity)
  )
  return(do.call(what = 'rgb', args = args))
}

###
# linear rescaling fx.
.linRescale <- function(dat, newRange){
  if (!is.null(dim(dat))){ # if data is not a single vector, run along cols
    return(apply(dat, 2, FUN = .linRescale, newRange = newRange))
  }
  oldRange <- range(dat,na.rm=TRUE)
  return(
    diff(newRange)*(dat-oldRange[1])/diff(oldRange) + newRange[1]
  )
}

###
# 

###
###
# A function to reshape data for moving average, peak analysis, etc.
.datshaper = function(dat,
                      L=NULL, overlap=NULL, FUN = NULL, 
                      .fill = NA, ...){
  #Takes single vector input and reshapes into L rows per col w ovlp.
  #K = (M-overlap)/(L-overlap)
  #     K           -  # of segments
  #     M           -  Length of input data (dat)
  #     overlap     -  #of samples to overlap
  #     L           -  Length of window segments
  ###-------------------------------------------------------------------------###
  ### Developed by Khris Griffis, 2014.                                       ###
  ###-------------------------------------------------------------------------###
  #Handle Inputs
  if(!is.null(L)) {L = as.integer(L);} else {
    message("Window length 'L' undefined, setting to 2 samples."); 
    L = 2;
  }
  if(!is.null(overlap) && L != 2) {
    overlap = overlap
  } else if(!is.null(overlap) && overlap > (L-1) ) {
    message(
      paste("Overlap is too long, setting to window length minus 1 (",
            L-1, ").", sep = "" ));
    overlap = L-1;
  } else if(!is.null(overlap) && overlap == 0){ 
    overlap = overlap;
  } else {
    message("Overlap is NULL, setting to 0"); overlap = 0;
  }
  if(!is.null(FUN)){
    FUN<- match.fun(FUN);
  } else {
    FUN<- FALSE
  }
  if (length(.fill) > 1){
    .fill <- .fill[1]
    message('Argument, ".fill", must be of length 1.')
  }
  #organize
  if(!is.vector(dat)){
    din<- which(dim(dat) == min(dim(dat)))
    if(din == 1) datin<- as.vector(dat[1,], mode = "numeric")
    if(din == 2) datin<- as.vector(dat[,1], mode  = "numeric")
  } else {
    datin = dat
  }
  #Pad with filler if shorter than window
  dl <- length(datin)-L;
  if(dl<0){
    datin = c(datin, rep(.fill, -dl))
  }
  
  #Get info on number of output cols
  M <- length(datin);
  K <- (M-overlap)/(L-overlap);
  Kl = trunc(K);
  #Pad if uneven number of segments
  if(Kl != K){
    Kl <- Kl+1;
    Mnew <- trunc(Kl*(L-overlap)+overlap)
    dfs<- abs(Mnew - M);
    datin <- c(datin, rep(NA,dfs));
  }
  #put data into windowed columns with overlap
  coli <- matrix(1 + c(0:(Kl-1))*(L-overlap), 1);#col indices
  rowi <- matrix(1:L);#row indices
  datout<- matrix(datin[as.vector(coli[,rep(seq_len(Kl), each = L)]+(matrix(rowi, L, Kl)-1))],L)
  if(!is.logical(FUN)){
    datout<- apply(na.omit(datout), 2, FUN, ...)
  }
  return(datout)
}

#___________________________________________________________________________________#
#################################### Fourier Analysis
# Fourier transform
.quickFFT <- function(d,fs,doHilbert=TRUE,centralize=FALSE,forcePad=TRUE,centerFun='mean'){
  # .quickFFT - Pad data and apply fourier transform
  # Description
  # 
  # Arguments:
  #        d:  Data array with dim <= 3
  #       fs:  Sampling Frequency of the data
  #
  # 
  # Returns:
  #   TODO(khris): add descriptions for Description and Returns
  #___________________________________________________________________________________#
  # Error handling
  if (doHilbert){
    stopifnot(
      require(hht,quietly = T, warn.conflicts = F)
    )
  }
  kDims <- dim(d)
  if (length(kDims) > 2) {
    return(
      apply(d, 3, FUN = .quickFFT, 
            fs = fs, 
            doHilbert = doHilbert,
            centralize = centralize,
            forcePad = forcePad)
    )
  }
  if (is.null(kDims)) {
    kDims <- c(length(d), 1)
    d <- as.matrix(d)
  }
  if (centralize) {
    theCenters <- apply(d,2,FUN=match.fun(centerFun))
    # subtract each colMean from each col
    d <- sweep(d,2,theCenters,"-")
  }
  nyquist <- fs/2
  nFFT <- 2^(ceiling(log2(kDims[1]))+forcePad)
  nPad <- nFFT - kDims[1]
  prePad <- array(0, dim = c(trunc(nPad/2),kDims[2]))
  postPad <- array(0, dim = c(ceiling(nPad/2), kDims[2]))
  pData <- rbind(
    prePad,
    as.matrix(d),
    postPad
  )
  if (doHilbert){
    pData <- apply(pData, 2, hht::HilbertTransform)
  }
  fData <- mvfft(pData)
  return(
    list(
      Padding = c(Pre = nrow(prePad), Post = nrow(postPad)),
      Data = fData,
      Magnitudes = apply(fData / kDims[1], 2, 
                         FUN = function(x)abs(Mod(x)))[1:(nFFT/2+1), ],
      Frequencies = seq(0,nyquist,length.out = nFFT/2+1)
    )
  )
}
#___________________________________________________________________________________#
#################################### Fourier Bootstrap
.fftBooter<- function(
  dataForFFT,
  Fs,
  nBoots=1000,
  CI.level=0.95,
  boots.return=FALSE,
  CI.only=FALSE,
  ...
){
  stopifnot(CI.level > 0, CI.level < 1)
  CI.interval <- sort(c(0,1)+c(1,-1)*CI.level)
  fftActual <- .quickFFT(d = dataForFFT, fs = Fs)
  
  #Create boots matrix
  fftBoots <- array(
    as.matrix(dataForFFT),
    dim = c(dim(as.matrix(dataForFFT)), nBoots)
  )
  fftBoots <- array(
    apply(
      X = fftBoots,
      MARGIN = length(dim(fftBoots)),
      FUN = function(m) apply(m,2,sample,replace = F)
    ),
    dim = dim(fftBoots)
  )
  # Apply Fourier
  fftBoots <- .quickFFT(d = fftBoots, fs = Fs,...)
  fftBoots <- sapply(
    fftBoots,
    FUN = function(x)x$Magnitudes,
    simplify = 'array'
  )
  bootCIs <- aperm(
    apply(
      X = fftBoots, 
      MARGIN = c(1,2), 
      FUN = quantile, probs = CI.interval
    ),
    perm = c(2,3,1)
  )
  if(CI.only) return(bootCIs)
  return(
    list(
      Actual = fftActual,
      CI = bootCIs,
      Boots = if(boots.return) fftBoots else NA
    )
  )
}
#___________________________________________________________________________________#
#################################### Fourier Analysis
.doLowPass <- function(d,fs,cutOff=NULL,nPoles=4,orHigh=FALSE,...){
  # .doLowPass - Pad data and apply butterworth lowpass filter
  # Description
  # 
  # Arguments:
  # 
  # Returns:
  #   TODO(khris): add descriptions
  #_________________________________________________________________________________#
  # Error handling
  stopifnot(
    require('signal', quietly = TRUE, warn.conflicts = FALSE)
  )
  if(orHigh){
    fType = 'high'
  } else {
    fType = 'low'
  }
  # check filterParams
  lDots <- list(...)
  if ('filterParams' %in% names(lDots)) {
    filterParams <- lDots$filterParams
  } else if('filterType' %in% names(lDots)) {
    stopifnot(!is.null(cutOff))
    filterParams <- butter(
      n = nPoles,
      W = cutOff*2/fs,
      type = lDots$filterType,
      plane = 'z'
    )
  } else {
    stopifnot(!all(is.null(cutOff)))
    filterParams <- butter(
      n = nPoles,
      W = cutOff[1]*2/fs,
      type = fType,
      plane = 'z'
    )
  }
  kDims <- dim(d)
  if (length(kDims) > 2) {
    return(
      apply(d, 3, FUN = .doLowPass, fs = fs, filterParams = filterParams)
    )
  }
  if (is.null(kDims)) {
    kDims <- c(length(d), 1)
  }
  nFilt <- 2^(ceiling(log2(kDims[1]))+1)
  nPad <- nFilt - kDims[1]
  prePad <- array(0, dim = c(trunc(nPad/2),kDims[2]))
  postPad <- array(0, dim = c(ceiling(nPad/2), kDims[2]))
  pData <- rbind(
    prePad,
    as.matrix(d),
    postPad
  )
  # apply filter with filtfilt (forward and backward at once)
  fData <- apply(
    X = pData,
    MARGIN = 2,
    FUN = function(x){
      filtfilt(filt = filterParams, x = x)
    }
  )
  # unpad Data
  fData <- fData[trunc(nPad/2)+(1:kDims[1]), ]
  return(fData)
}

#___________________________________________________________________________________#
#################################### Plotting fxns
# Uses grid and gridExtras to make viewports for plotting
# Make the layout area x = nrows, y = ncols
.mkV<- function(x,y,...){
  pushViewport(viewport(layout=grid.layout(nrow = x, ncol = y,...)))
}
# Place current plot into row x and col y, can be  span vectors
.ly1<- function(x,y){
  pushViewport(viewport(layout.pos.row=x, layout.pos.col=y))
}
# Use this function when printing a plot into a viewport, print(..., vp=f(x,y))
.lyprint<- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
#---

#___________________________________________________________________________________#
#################################### Inspect Files
.inspector <- function(cur.file, col.drop = NULL, 
                      do.View = TRUE, all.sheets = T, 
                      main.sheet = 1, ROI.scale = 1, 
                      verbose = FALSE, ...){
  stopifnot(
    require('readxl', quietly = T, warn.conflicts = F),
    require('dplyr', quietly = T, warn.conflicts = F)
  )
  # Some functions
  .processAreas <- function(tab){
    nm <- colnames(tab)
    dataColumns <- grepl(
      pattern = 'Cell|Area',
      x = nm,
      ignore.case = T
    )
    areas <- as.data.frame(tab)[,dataColumns]
    colnames(areas) <- make.names(colnames(areas)) %>% 
      gsub(x = ., pattern = '\\.(.*)', replacement = '') %>% 
      make.names(unique = T)
    return(areas)
  }
  
  .processSheets <- function(cur.File, sheets, ROI.scale = 1, verbose=FALSE, ...){
    dat <- list()
    dat$BackgroundID <- NULL
    dat$Areas <- NULL
    dat$ROIs <- NULL
    if (!length(sheets)){
      return(dat)
    }
    #read the rest
    sheetList <- lapply(
      X = sheets,
      FUN = function(s)readxl::read_excel(cur.File,sheet = s, ...)
    )
    if(verbose){
      writeLines('      <Sheets loaded.>\n')
    }
    names(sheetList) <- sheets
    isXY <- grepl('XY', sheets, ignore.case = T)
    XY <- sheetList[isXY]
    sheetList <- sheetList[!isXY]# drop the XY the remainder should be Area
    isArea <- grepl('Area', names(sheetList),ignore.case = T)
    if(any(isArea)){
      Areas <- sheetList[[isArea[1]]]
      if (length(sheetList)>1){dat$ExtraSheets = sheetList[!isArea]}
      dat$Areas <- .processAreas(Areas)
      #find background number
      # isBackground <- which(colnames(Areas[[1]]) == 'background')+1
      isBackground <- grep(
        pattern = 'background',
        x = colnames(Areas),
        ignore.case = TRUE
      ) + 1
      if(!is.na(isBackground) && (length(isBackground)!=0)){
        dat$BackgroundID <- colnames(Areas)[isBackground]
        dat$BackgroundID <- tryCatch(as.numeric(as.character(dat$BackgroundID)),
                 warning = {function(w){NA}},
                 error = {function(e){NA}})
      }
      if(verbose){
        writeLines('      <Areas processed.>\n')
      }
    }
    #manage ROIs
    dat$ROIs <- lapply(XY, FUN = function(x){
      cn<- colnames(x)
      if (length(cn) > 2) {x <-  x[,-c(3:ncol(x))]}
      colnames(x)[1:2] <- c('X','Y')
      mutate(
        .data = x, 
        X.Pixels = round(X*ROI.scale), 
        Y.Pixels = round(Y*ROI.scale)
      ) %>% as.data.frame()
    })
    names(dat$ROIs) <- names(dat$ROIs) %>% 
      strsplit(., '(?<=[[:alpha:]]{2})', perl=T) %>% 
      sapply(FUN = function(x)x[2]) %>% as.numeric() %>% 
      sprintf(fmt = 'Cell.%04d',.)
    return(dat)
  }
  # get the main data sheet from the excel file
  # ... will only be passed to read_excel() for all.sheets=T
  if(verbose){
    writeLines('    <reading excel main sheet...>\n')
  }
  raw <- readxl::read_excel(
    cur.file, 
    col_names = F,
    sheet = main.sheet
  )
  if (do.View){
    View(raw)
    readline(prompt = 'Press Enter to continue')
  }
  # Initialize dat for output
  dat <- list()
  # Load in the rest of the sheets, each as a list
  if(all.sheets){
    if(verbose){
      writeLines('    <reading excel remaining sheets...>\n')
    }
    sheets <- readxl::excel_sheets(cur.file)
    #drop main sheet
    sheets <- sheets[-main.sheet]
    # Manage sheet data
    dat <- c(dat, do.call('.processSheets', 
                          c(list(cur.file, sheets, ROI.scale, verbose), list(...))))
    if(verbose){
      writeLines('    <Sheets processed!>\n')
    }
  }
  # Find start of data
  if(verbose){
    writeLines('    <Detecting raw data...>\n')
  }
  data.start <- which(raw == "Time (s)", arr.ind = TRUE)
  data.end <- c(
    which(
      is.na(
        raw[
          data.start[1,'row']:nrow(raw),
          data.start[1,'col']
        ]
      )
    )[1] + 
      data.start[1,'row'] - 2,
    which(
      is.na(
        raw[
          data.start[1,'row'],
          seq(data.start[1,'col'],ncol(raw))
        ]
      )
    )[1] + 
      data.start[1,'col'] - 2
  )
  if (is.na(data.end[2])) data.end[2] <- ncol(raw)
  if (is.na(data.end[1])) data.end[1] <- nrow(raw)
  
  # Parse
  recordInfo <- as.matrix(raw[1:(which(is.na(raw[,1]))[1]-1), 1:2])
  if(verbose){
    writeLines('    <Parsing File Header info...>\n')
  }
  samplingRate <- 1 / 
    as.numeric(
      recordInfo[grepl(pattern = 'Intervals',x=recordInfo[,1],ignore.case = T),2]
    ) #Hz -> assuming the interval is in seconds
  treatmentStart <- tryCatch(
    as.numeric(
      recordInfo[
        grepl(
          pattern = 'ET[-]?1',
          recordInfo[,1]
        ),
        2
      ]
      ),
    warning = function(w)NA,
    error = function(e)NA
  )
  #filename
  fName <- strsplit(x = cur.file, split = '/') %>% 
    `[[`(1)
  # Collect data
  dat <- c(
    dat,
    list(
      Raw = raw,
      File = list(
        Name = fName[length(fName)],
        Properties = recordInfo
      ),
      Info = list(
        SampleRate = samplingRate,
        TreatmentStart = treatmentStart
      ),
      Original = data.frame(
        raw[seq(data.start[1,'row']+1, data.end[1]),
            seq(data.start[1,'col'], data.end[2])] 
      )
    )
  )
  vNames<- as.character(
    raw[
      data.start[1,'row'],
      seq(data.start[1,'col'], data.end[2])
    ]
  )
  vnC<- grepl(pattern = '[[:digit:]]', x = vNames)
  vNames[!vnC] <- make.names(vNames[!vnC], unique=T) %>%
    gsub(x = ., pattern = '\\.(.*)', replacement = '')
  vNames <- vNames[vnC] %>% 
    as.numeric() %>% 
    sprintf(fmt = 'Cell.%04d') %>%
    c(vNames[!vnC], .) %>% 
    make.names(unique=T)
  colnames(dat$Original) <- vNames
  #Fix some stuff
  if(verbose){
    writeLines('    <Finalizing and cleaning up...>\n')
  }
  dat$Original <- as.data.frame(apply(dat$Original, 2, FUN = as.numeric))
  dat$Time <- dat$Original$Time
  dat$Original[,'Time'] <- NULL
  
  if (!is.null(col.drop)) {
    dat$Original[,col.drop] <- NULL
  }
  row.names(dat$Original) <- NULL
  # pop up a view
  if (do.View) View(dat$Data)
  
  return(dat)
}#eof

###
# Short Time Fourier Function
.doSTFT <- function(dataForFFT,dataForTime,Fs,CellID,FileID,doPlot=TRUE,subsetPlot=1:50){
  # reshape data for stft
  reshapedData <- .datshaper(
    dat = dataForFFT,
    L = floor(2^8),
    overlap = floor(2^8)-1,
    .fill = 0
  )
  # demean each column
  theCenters <- apply(reshapedData,2,FUN=mean)
  # subtract each colMean from each col
  reshapedData <- sweep(reshapedData,2,theCenters,"-")
  # configure new time vector
  reshapedTime <- do.call(
    'seq',
    as.list(c(range(dataForTime), length.out = ncol(reshapedData)))
  )
  # Windowing function to prevent edge artifacts
  welchWin <- function(N){
    return(
      1-(((0:(N-1))-((N-1)/2))/((N-1)/2))^2
    )
  }
  theWindow <- welchWin(nrow(reshapedData))
  # multply each column by the windowvector
  reshapedData <- apply(reshapedData,2,'*',theWindow)
  # Get FFT of each col
  fftActual <- .quickFFT(
    d = reshapedData, 
    fs = Fs,
    centralize = FALSE,
    forcePad = TRUE)
  filtMags <- fftActual$Magnitudes
  filtMags[is.na(filtMags)] <- 0
  filtMagsY <- imager::deriche(
    im = imager::as.cimg(filtMags),
    sigma = 4,
    order = 1,
    axis = 'y'
  )
  filtMags <- t(as.matrix(filtMags)+as.matrix(filtMagsY))
  if (doPlot){
    .doHeatPlot(
      matrixData = filtMags[,subsetPlot],
      timeVector = reshapedTime,
      freqVector = fftActual$Frequencies[subsetPlot],
      dataFrame = cbind(dataForTime,dataForFFT),
      xaxs='i',yaxs='i',doPop=FALSE
    )
    title(main = sprintf('Cell #%d File "%s"', CellID,FileID))
    popViewport()
  }
  # return
  output <- list(list(
    matrixData = filtMags,
    timeVector = reshapedTime,
    freqVector = fftActual$Frequencies,
    dataFrame = cbind(dataForTime,dataForFFT)
  ))
  #Give the cellID to the output for outside usage.
  names(output)<- sprintf('Cell%02dF%s',CellID,FileID)
  invisible(output)
}
###
# Plotting for STFT
.doHeatPlot <- function(matrixData,timeVector,freqVector,dataFrame=NULL,doPop=TRUE,...){
  if (!is.null(dataFrame)) {
    .mkV(2,1,heights = unit(c(0.25,0.6), c('npc','npc')))
    
    .ly1(1,1)
    omar <- par(no.readonly = T)
    par(new=T, fig=gridFIG(), mar = par('mar')*c(0.5,1,0,1))
    plot(dataFrame[,1],dataFrame[,2], type = 'l', lwd = 2, bty='n',ann=F,
         xaxs = 'i')
    box()
    grid()
    theTicks <- par('xaxp')
    theTicks <- do.call(seq,c(as.list(theTicks[1:2]),list(length.out=theTicks[3]+1)))
    tickLabs <- formatC(theTicks)
    theTicks <- theTicks / max(dataFrame[,1])
    par(omar)
    popViewport()
    # Start vp for main plot
    .ly1(2,1)
    par(new=TRUE, fig=gridFIG())
  } else {
    layout(matrix(1), widths = 0.95)
    theTicks <- do.call(seq,list(0,1,length.out=4))
    tickLabs <- formatC(theTicks)
  }
  
  image(
    x = timeVector + 0.5*mean(diff(timeVector[1:10])),
    y = freqVector,
    z = matrixData,
    col = viridis::viridis(2^16),
    ann = F,
    xpd=NA,
    ...
  )
  
  grid(col = rgb(1,0.1,0.1,0.4),nx = NULL)
  title(xlab = 'Time [s]', ylab = 'Frequency [Hz]')
  if (!is.null(dataFrame) && doPop) {
    popViewport()
  }
}


###
# Do fourierComparison
.doFourierComp <- function(
  inputData,
  Fs,
  cutStart = 0.5,
  ...
){
  # First manage inputs
  if (cutStart > 1){
    cutStart <- floor(length(inputData)*abs(cutStart/100))
  } else {
    cutStart <- floor(length(inputData)*cutStart)
  }
  #Subset data and perform fftboot
  fourierData <- .fftBooter(
    dataForFFT = inputData[-(1:cutStart)],
    Fs = Fs,
    nBoots = nBoots,
    CI.level = 0.99,
    doHilbert = TRUE,
    centralize = TRUE,
    forcePad = TRUE #...
  )
  
}
###



