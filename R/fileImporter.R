###-------------------------------------------------------------------------------###
### Read and Compile new files                                                    ###
###-------------------------------------------------------------------------------###

fileImporter <- function(
  filePath, # which file to load (full or relative)
  dataPath = NULL, # append 
  verbose = FALSE,
  dataPathAppend = '',
  ...
){
  
  ## Dependencies
  require('dplyr', quietly = TRUE, warn.conflicts = FALSE)
  require('readxl', quietly = TRUE, warn.conflicts = FALSE)
  require('rtiff', quietly = TRUE, warn.conflicts = FALSE)
  
  source(
    dir(pattern = 'helperFns.R$', 
        path = getwd(),
        full.names = TRUE,
        recursive = TRUE)
  )
  
  ## Parse names and locatiosn
  verbose <- is.logical(verbose) && verbose #make sure its a bool
  if(verbose){
    writeLines('Locating files...\n')
  }
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
  cFile <- .inspector(
    cur.file = filePath,
    col.drop = 'Frame',
    do.View = FALSE,
    all.sheets = TRUE,
    main.sheet = 1,
    ROI.scale = 1.204434,
    col_names = TRUE, #for reading all sheets
    verbose = verbose
  )
  if(verbose){
    writeLines('  ......')
  }
  if (!is.na(cFile$BackgroundID)){
    ## The background traces are causing strange artifacts
    # cFile$Data <- as.data.frame(
    #   apply(
    #     X = cFile$Original[,-cFile$BackgroundID],
    #     MARGIN = 2,
    #     FUN = function(X,y) {
    #       tmp = log2(X) - log2(y)
    #     },
    #     y = cFile$Original[,cFile$BackgroundID]
    #   )
    # )
    cFile$Data <- cFile$Original[,-cFile$BackgroundID]
  } else {
    cFile$Data <- cFile$Original
  }
  
  if(verbose){
    writeLines('  Background parsed...')
  }
  # normalize by deltaF/F of log2 scaled values
  # use the pre treatment time as a minimum
  if (!is.na(cFile$Info$TreatmentStart)){ # treatment start exists
    tStart <- which(cFile$Time == cFile$Info$TreatmentStart)
  } else { #treatment start does not exist, use the first 10%
    tStart <- nrow(cFile$Data)
  }
  # detaF/f0
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
  # Create scaled and centered data for clusters
  cFile$Scaled <- apply(
    X = cFile$Data, 
    MARGIN = 2, 
    FUN = function(x){
      scaled <- (x-median(x))/mad(x,constant = 1)
      return(scaled - mean(scaled))
    }
  )
  if(verbose){
    writeLines('  Scaled data computed...')
  }
  #if(any(is.nan(cFile$Scaled))){print(fileName)}
  cFile$Info$nCells <- ncol(cFile$Original)
  cFile$Info$HasBackground <- !is.na(cFile$BackgroundID)
  # Parse Image
  cFile$Image = list()
  if(identical(imageFile,list())){
    cFile$Image$hasImage = FALSE
    if(verbose) writeLines('  No Image Located.')
  } else {
    cFile$Image$hasImage = TRUE
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
  writeLines('...Parsing complete!\n  **¯\\_(\`\`/)_/¯**\n')
  invisible(list(cFile))
}


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
    )
  ][1]
  if(is.na(dataFile)) {
    if(is.null(altName)) return(list())
    newName <- file.path(parent,paste0(newBase,altName,'.',extension))
    output <- locateData(newName,NULL,'rda')
  } else {
    output <- list(
      Base = newBase,
      Ext = extension,
      Path = dataFile
    )
  }
  # Data found, let's create a list.
  return(
    output
  )
}
