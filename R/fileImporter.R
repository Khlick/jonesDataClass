#' fileImporter: Load Excel Data From Jones Group
#' 
#' @param filePath character: full path to excel file (required)
#' @param dataPath character: full path to data file for appending new data (optional)
#' @param verbose bool: Report progress of file loading, default FALSE (recommended TRUE)
#' @param dataPathAppend chracter: string to append onto the saved datafile, used only if \code{dataPath} is supplied
#' @param ... Arguments passed onto internal functions.
#' 
#' @return List with the following Structure. \cr
#'   \describe{
#'    \item{Background}{List that contains, if applicable, the background lum (0-255), the ROI area and ROI coordinates.
#'    \describe{
#'      \item{ID}{Which ROI ID was used for extraction of the background data.}
#'      \item{Data}{The Time series calcium lum (0-255) of the background roi.}
#'      \item{ROI }{Coordinates of the ROI in X, Y, X.Pixels and Y.Pixels.}
#'      \item{Area}{The Area in microns squared.}
#'    }}
#'    \item{Areas}{data.frame with cell id and area of ROI in microns squared.}
#'    \item{ROIs}{list of ROI coordinates (named) for each cellID}
#'    \describe{
#'      \item{CellID}{Named list of data.frame\cr
#'      \describe{
#'        \item{X}{}
#'        \item{Y}{ }
#'        \item{X.Pixels}{ }
#'        \item{Y.Pixels}{ }
#'      }}
#'    }
#'    \item{Raw}{ }
#'    \item{File}{
#'    \describe{
#'      \item{Name}{ }
#'      \item{Properties}{matrix of values from excel header.}
#'    }}
#'    \item{Info}{
#'    \describe{
#'      \item{SamplingRate}{ }
#'      \item{TreatmentStart}{ }
#'      \item{nCells}{ }
#'      \item{HasBackground}{ }
#'    }}
#'    \item{Original}{data.frame of CellID without Time, but includes bg if applicable.}
#'    \item{Time}{ }
#'    \item{Data}{df/f0}
#'    \item{Corrected}{df/f0 - background}
#'    \item{Standardized}{((data-median)/mad)-mean}
#'    \item{Image}{
#'    \describe{
#'      \item{HasImage}{ }
#'      \item{File}{
#'      \describe{
#'        \item{Name}{ }
#'        \item{Type}{ }
#'        \item{Path}{ }
#'      }}
#'      \item{Data}{rtiff::readTiff}
#'    }}
#'   }
#' 
#' @export fileImporter
#' @import stats
#' @import graphics
#' @import utils
#' @import methods
#' @importFrom dplyr %>% mutate
#' @importFrom rtiff readTiff
#' @import readxl
fileImporter <- function(
  filePath, # which file to load (full or relative)
  dataPath = NULL, # append 
  verbose = FALSE,
  dataPathAppend = '',
    ...
  ){
    
    ## Dependencies
    # stopifnot(
    #   require('dplyr', quietly = TRUE, warn.conflicts = FALSE),
    #   require('readxl', quietly = TRUE, warn.conflicts = FALSE),
    #   require('rtiff', quietly = TRUE, warn.conflicts = FALSE)
    # )
    
    ## Parse inputs
    verbose <- is.logical(verbose) && verbose #make sure its a bool
    fileName<- basename(filePath)
    extLoc <- regexpr('\\.[^\\.]*$',fileName)[1]
    fileBase<- substr(x = fileName, start = 1, stop = extLoc-1)
    fileExtension <- substr(x = fileName, start = extLoc+1, stop = nchar(fileName))
    root<- dirname(filePath)
    
    # Locate Image
    imageFile<- locateImage(
      baseName = fileBase,
      parent = dirname(root), #look in the parent dir of excelfile
      ...
    )
    
    ## Extract Data
    if(verbose){
      writeLines(
        sprintf('Starting file:\n  %s\n', fileBase)
      )
    }
    
    #Collect Excel data
    cFile <- inspector(
      cur.file = filePath,
      col.drop = 'Frame', # frame is a index column
      do.View = FALSE,
      all.sheets = TRUE,
      main.sheet = 1,
      ROI.scale = 1.204434, # this was provided by Elizabeth
      col_names = TRUE, #for reading all sheets
      verbose = verbose
    )
    
    if(verbose){
      writeLines(
        sprintf(
          'Data collected, starting post-processing......\n'
        )
      )
    }
    # normalize by deltaF/F of log2 scaled values
    # use the pre treatment time as a minimum
    if (!is.na(cFile$Info$TreatmentStart)){ # treatment start exists
      tStart <- which(cFile$Time >= cFile$Info$TreatmentStart)[1]
    } else { #treatment start does not exist, use the first 12%
      tStart <- floor(nrow(cFile$Original)*0.12)
      if (tStart < 10) tStart <- 10
    }
    # exrtract background
    
    if (!is.na(cFile$Background$ID)){
      cFile$Background$Data <- cFile$Original[,cFile$Background$ID]
      cFile$Corrected <- as.data.frame(
        apply(
          X = cFile$Original[,-cFile$Background$ID],
          MARGIN = 2,
          FUN = function(X,y) {
            tmp = 2^(log2(X)-log2(y))
          },
          y = cFile$Original[,cFile$Background$ID]
        )
      )
      cFile$Corrected <- apply(
        X = cFile$Corrected,
        MARGIN = 2, 
        FUN = function(x){
          (x - min(x[floor(1:tStart)]))/min(x[floor(1:tStart)])
        }
      )
      cFile$Background$Area = cFile$Areas[cFile$Background$ID,2]
      cFile$Background$ROI = cFile$ROIs[[cFile$Background$ID]]
      ## drop the background traces from the original data
      cFile$Data <- cFile$Original[,-cFile$Background$ID]
      cFile$Areas <- cFile$Areas[-cFile$Background$ID, ]
      cFile$ROIs <- cFile$ROIs[-cFile$Background$ID]
      cFile$Original <- cFile$Original[,-cFile$Background$ID]
      
      
    } else {
      cFile$Data <- cFile$Original
      cFile$Corrected <- NULL
      cFile$Background$Area <- NULL
      cFile$Background$ROI <- NULL
      cFile$Background$Data <- NULL
    }
    
    if(verbose){
      writeLines('  Background parsed...')
    }
    
    # detaF/f0 for 
    cFile$Data <- apply(
      X = cFile$Data,
      MARGIN = 2, 
      FUN = function(x){
        (x - min(x[floor(1:tStart)]))/min(x[floor(1:tStart)])
      }
    )
    if(verbose){
      writeLines('  dF/F0 computed...')
    }
    # Create scaled (standardized by)
    cFile$Standardized <- apply(
      X = cFile$Data, 
      MARGIN = 2, 
      FUN = function(x){
        scaled <- (x-median(x))/mad(x,constant = 1)
        return(scaled - mean(scaled)) #center
      }
    )
    if(verbose){
      writeLines('  Scaled data computed...')
    }
    #if(any(is.nan(cFile$Scaled))){print(fileName)}
    cFile$Info$nCells <- ncol(cFile$Original)
    cFile$Info$HasBackground <- !is.na(cFile$Background$ID)
    # Parse Image
    cFile$Image = list()
    if(identical(imageFile,list())){
      cFile$Image$HasImage = FALSE
      if(verbose) writeLines('  No Image Located.')
    } else {
      cFile$Image$HasImage = TRUE
      cFile$Image$File = list(
        Name = imageFile$Base,
        Type = imageFile$Ext,
        Path = imageFile$Path
      )
      cFile$Image$Data = readTiff(imageFile$Path)
      
      if(verbose) writeLines('  Image loaded...')
    }
    
    # Locate datafile and append new data
    if(!is.null(dataPath)){
      if(verbose) writeLines('  Appending to datafile...')
      dExt <- substr(
        x = dataPath, 
        start = regexpr('\\.[^\\.]*$',dataPath)[1]+1, 
        stop = nchar(dataPath)
      )
      dPath <- locateData(dataPath,dataPathAppend,dExt)
      if(identical(dPath,list())){
        if(verbose) writeLines('    No data file located, creating new...')
        oldData <- list()
        oldDataName <- 'CellInfo'
        dPath$Base <- basename(dataPath) %>% 
          sub(x = ., pattern='\\.[^\\.]*$', replacement = '')
        dPath$Ext <- dExt
      } else {
        if(verbose) writeLines('    Data file located, updating...')
        tempEnv <- new.env()
        load(file = dPath$Path, envir = tempEnv, verbose = FALSE)
        oldData <- get(ls(tempEnv), envir = tempEnv)
        oldDataName <- ls(tempEnv)[1]
        rm(tempEnv)
      }
      
      # check if the field already exists and then override it
      if (fileBase %in% names(oldData)){
        oldData[fileBase] <- list(cFile)
        if (verbose) writeLines('      Overwriting existing slot...')
      } else {
        if (verbose) writeLines('      Creating new slot...')
        oldData <- c(oldData, list(cFile))
        names(oldData)[length(oldData)] <- fileBase
      }
      parseText <- c(
        paste0(oldDataName,'<- oldData'),
        paste0(
          'save(',oldDataName,
          ',file="',
          sprintf(
            '%s/%s.rda',
            dirname(dataPath),
            paste0(
              dPath$Base,
              dataPathAppend
            )
          ),
          '",compress=TRUE)'
        ),
        paste0('rm(',oldDataName,')')
      )
      for(i in 1:3){
        if (i==3) writeLines('    saving...')
        eval(parse(text=paste(parseText[i])))
      }
      
      if(verbose) writeLines('    Done!\n')
    }
    writeLines(
      sprintf('Successfully parsed:\n    "%s"\n--------------------#\n', fileBase)
    )
    invisible(list(cFile))
} #eof



##############################################################################
## Internal Functions

#' inspector
#' 
#' @param cur.file File name for file being loaded
#' @param col.drop Name of a column to drop, typically 'Frame'
#' @param do.View Causes a halt in execution to view the data in a window
#' @param all.sheets Load all the sheets or just the main?
#' @param main.sheet Index of main sheet, 1 for first.
#' @param ROI.scale A scale factor for converting the xy positions to pixels on image
#' @param verbose Chatty, much?
#' @param ... Passed on to other methods
inspector <- function(
  cur.file, col.drop = NULL,
  do.View = TRUE, all.sheets = TRUE,
  main.sheet = 1, ROI.scale = 1, 
  verbose = FALSE, ... 
){
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
    dat <- c(dat, do.call('processSheets', 
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
  # what is the file name
  fileName<- basename(cur.file)
  extLoc <- regexpr('\\.[^\\.]*$',fileName)[1]
  fileBase<- substr(x = fileName, start = 1, stop = extLoc-1)
  # Collect data
  dat <- c(
    dat,
    list(
      Raw = raw,
      File = list(
        Name = fileName,
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
  
  # make columns names
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
    paste(fileBase,.) %>%
    c(vNames[!vnC], .) %>% 
    make.names(unique=T)
  #set names
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
}

#' locateImage
#' 
#' Locate the image file associated with an excel data file.
#' 
#' @param baseName File's base name
#' @param parent Parent directory to search in for baseName
#' @param extension Image file extension
locateImage <- function(
  baseName,
  parent,
  extension = 'tif'
){
  # Parse baseName so that there are no version indicators
  newBase <- gsub(pattern = '[[:punct:]].*$', replacement = '', x = baseName)
  # locate files with given extension
  availableMatches <- list.files(
    path = parent, 
    recursive = TRUE, 
    full.names = TRUE, 
    pattern = paste0(extension,'$')
  )
  imageFile <- availableMatches[grepl(pattern = newBase, x = availableMatches)][1]
  if(is.na(imageFile)) return(list())
  # Data found, let's create a list.
  return(
    list(
      Base = newBase,
      Ext = extension,
      Path = imageFile
    )
  )
}

#' locateData
#' 
#' Recursively look through directories for a data file
#' 
#' @param fileName data file path/name
#' @param altName possible optional name to search for if fileName isn't found
#' @param extension Data file extension
locateData <- function(
  fileName,
  altName = NULL,
  extension = 'rda'
){
  parent <- dirname(fileName)
  newBase <- basename(fileName) %>%
    gsub(
      x = ., 
      pattern = '\\.[^\\.]*$',
      replacement = ''
    )
  # locate files with given extension
  availableMatches <- list.files(
    path = parent, 
    recursive = TRUE, 
    full.names = TRUE, 
    pattern = paste0(extension,'$')
  )
  dataFile <- availableMatches[
    grepl(
      pattern = paste0(newBase,'\\.'), 
      x = availableMatches
    )][1]
  if(is.na(dataFile)) {
    if(is.null(altName)) return(list())
    newName <- file.path(parent,paste0(newBase,altName,'.',extension))
    output <- locateData(newName,NULL,'rda')
  } else {
    # Data found, let's create a list.
    output <- list(
      Base = newBase,
      Ext = extension,
      Path = dataFile
    )
  }
  
  return(
    output
  )
}

#' processAreas
#' 
#' Parse the areas tab of a data file.
#' 
#' @param tab The table (tibble?) of areas data
processAreas <- function(tab){
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


#' processSheets
#' 
#' Collect the data from the excel sheets
#' 
#' @param cur.File File name for file being loaded
#' @param sheets names of sheets to process
#' @param ROI.scale A scale factor for converting the xy positions to pixels on image
#' @param verbose Chatty, much?
#' @param ... Passed on to other methods
processSheets <- function(cur.File, sheets, ROI.scale = 1, verbose=FALSE, ...){
  dat <- list()
  dat$Background <- list()
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
    dat$Areas <- processAreas(Areas)
    #find background number
    # isBackground <- which(colnames(Areas[[1]]) == 'background')+1
    isBackground <- grep(
      pattern = 'background',
      x = colnames(Areas),
      ignore.case = TRUE
    ) + 1
    if(!is.na(isBackground) && (length(isBackground)!=0)){
      dat$Background$ID <- colnames(Areas)[isBackground]
      dat$Background$ID <- tryCatch(as.numeric(as.character(dat$Background$ID)),
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

## end of internal functions
##############################################################################