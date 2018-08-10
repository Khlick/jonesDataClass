#' Class Definition for Storing jonesDatum objects with methods to access data, 
#' subest traces, append/remove files, 
#'
#' @field files Char vector containing full paths to data files (.xlsx)
#' @field names no Input. Contains names of files loaded
#' @field data no Input. Contains named list of jonesDatum objects
#' @field nFiles noInput. Contains the count of currently loaded
dataCollective<- setRefClass('dataCollective',
  fields=list(
    files = 'character',
    names = 'character',
    data = 'list',
    nFiles = 'numeric'
  ),
  methods=list(
    initialize = function(files='',...){
      # need to handle making direct copies
      if (files[1] == ''){
        return(.self)
      }
      .self$files <- files
      .self$names <- sapply(
        X = files,
        FUN = function(f){
          basename(f)%>%sub(x = .,pattern = '\\.[^\\.]*$',replacement = '')
        },
        USE.NAMES = FALSE
      
      )
      .self$nFiles <- length(files)
      .self$data <- sapply(
        files,
        FUN = function(fn,...) {
          jonesDatum(fn,...)
        },
        ...
      )
      
      names(.self$data) <- .self$names
      
      #Return
      invisible(.self)
    },
    size = function() {
      "Get the size of the object in memory."
      return(format(object.size(get('.self')), units = "auto"))
    },
    show = function(){
      "Similar to \\code{str()}, display the fields and thier content types."
      str(.self,max.level = 2)
    },
    copy = function(){
      "Returns a copy of the object."
      theCopy <- dataCollective(files='')
      theCopy$files <- .self$files
      theCopy$names <- .self$names
      theCopy$data <- .self$data
      theCopy$nFiles <- .self$nFiles
      return(theCopy)
    },
    save = function(pathName,objectName='',...){
      "Save the current object on the file in R external object format."
      if (missing(pathName)){
        pathName = file.path(
          getwd(),
          paste0(
            'collective_',
            gsub(x = Sys.Date(), pattern = '[[:punct:]]',replacement = ''),
            '.rda'
          )
        )
      }
      if (objectName == ''){
        objectName <- paste(
          'jonesData',
          gsub(x = Sys.Date(), pattern = '[[:punct:]]',replacement = ''),
          sep = '_'
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
    push = function(newFiles,...){
      "Appends newfiles that do not already exist"
      newNames <- sapply(
        X = newFiles,
        FUN = function(f){
          basename(f)%>%sub(x = .,pattern = '\\.[^\\.]*$',replacement = '')
        },
        USE.NAMES = FALSE
      )
      newFiles = newFiles[!(newNames %in% .self$names)]
      newNames = newNames[!(newNames %in% .self$names)]
      newLen <- length(newFiles)
      
      #add to data model
      .self$data <- c(
        .self$data,
        sapply(
          newFiles,
          FUN = function(fn,...) {
            jonesDatum(fn,...)
          },
          ...
        )
      )
      names(.self$data) <- c(.self$names,newNames)
      .self$nFiles <- .self$nFiles + newLen
      .self$names <- c(.self$names,newNames)
      .self$files <- c(.self$files, newFiles)
      invisible(.self)
    },
    pop = function(first=FALSE){
      "Removes and returns last datum unless arg \\code{first = TRUE}."
      #returns and removes last element
      if (first) {
        index <- 1
      } else {
         index <- .self$nFiles
      }
      output <- .self$data[[index]]$copy()
      #destroy
      .self$nFiles <- .self$nFiles -1
      .self$data <- .self$data[-index]
      .self$names <- .self$names[-index]
      .self$files <- .self$files[-index]
      return(output)
    },
    getMatches = function(confluence=NULL, endothelin=NULL, interleukin=NULL){
      "\\code{getMatches} returns a vector of data file names matching provided criteria. 
      Values left at default will not be checked, thus an empty call will get all
      names. Acceptable values for input arguments are: \\cr
      \\itemize{
        \\item \\code{confluence} Must be NULL or NUMERIC on [0,100]. Checking for percent confluence.
        \\item \\code{endothelin} Must be NULL or BOOLEAN. Checks for ET1 treatment only.
        \\item \\code{interleukin} Must be NULL or BOOLEAN. Checks for IL-1b treatment only.
      }
      "
      collection <- rep(TRUE,.self$nFiles)
      if(!is.null(confluence)){
        if(is.numeric(confluence)){
          collection <- collection & .self$getDetails('checkConfluence', prcnt=confluence)
        }
      }
      if(!is.null(endothelin)){
        ec <- .self$getDetails('endothelin')
        if(!endothelin) ec <- !ec
        collection <- collection & ec
      }
      if(!is.null(interleukin)){
        il <- .self$getDetails('interleukin')
        if(!interleukin) il <- !il
        collection <- collection & il
      }
      return(.self$names[collection])
    },
    subset = function(dataNames, inplace=FALSE){
      "Returns a copy, unless \\code{inplace=TRUE} then data are dropped."
      dN <- dataNames %in% .self$names
      if (!any(dN)){
        stop('No matching names were found in data.\n')
      }
      dataNames <- dataNames[dN]
      
      if(inplace){
        keepInd<- which(.self$names %in% dataNames)
        .self$nFiles <- length(keepInd)
        .self$data <- .self$data[keepInd]
        .self$names <- .self$names[keepInd]
        .self$files <- .self$files[keepInd]
        return(.self)
      }
      # otherwise call subset on a copy
      return(.self$copy()$subset(dataNames=dataNames,inplace=TRUE))
      
    },
    extractDatum = function(dataName,copy=FALSE){
      "Gets the jonesDatum object for a single dataName. \\cr
      Use \\code{copy = FALSE} for chaining (default). Set to \\code{TRUE} 
      if a copy is desired, otherwise changes to the output object will 
      affect the original. In other words, setting to TRUE will mimic
      R's copy-on-modify behavior.
      "
      stopifnot(length(dataName) == 1, dataName %in% .self$names)
      if (copy){
        return(.self$data[[dataName]]$copy())
      }
      return(.self$data[[dataName]])
    },
    extractData = function(
      dataNames,
      type=c('adjusted','standardized','raw','background','corrected'),
      asList = FALSE
      ){
      "Extracts matrix of data from supplied \\code{dataNames}. 
      Always returns named list. \\cr
      Argument, \\code{asList}, set to \\code{TRUE} will extract each dataFile's 
      data as a list of lists rather than as a dataList object (default).
      "
      if (missing(dataNames)){
        #get all data
        dataNames <- .self$names
      }
      dN <- dataNames %in% .self$names
      if (!any(dN)){
        stop('No matching names were found in data.\n')
      }
      type <- match.arg(type)
      dataNames <- dataNames[dN]
      if(!asList){
        output<- lapply(
          dataNames,
          function(n).self$extractDatum(n)$getData(type)
        )
      } else {
        listOfLists<- lapply(
          dataNames,
          function(n).self$extractDatum(n)$getDataAsList(type)
        )
        output <- list()
        for (ll in 1:length(dataNames)){
          # data fields get the filename appended on the front
          # so this output list will have unique ids
          output <- c(output, listOfLists[[ll]])
        }
      }
      
      names(output) <- dataNames
      return(output)
    },
    extractROI = function(dataNames){
      "Extracts data.frames of ROIs from supplied \\code{dataNames}. 
      Always returns named list."
      if (missing(dataNames)){
        #get all data
        dataNames <- .self$names
      }
      dN <- dataNames %in% .self$names
      if (!any(dN)){
        writeLines('No matching names were found in names\n')
        return()
      }
      
      dataNames <- dataNames[dN]
      output<- lapply(
        dataNames,
        function(n).self$extractDatum(n)$rois
      )
      names(output) <- dataNames
      return(output)
    },
    extractArea = function(dataNames){
      "Extracts data.frames of Area from supplied \\code{dataNames}. 
      Always returns named list."
      if (missing(dataNames)){
        #get all data
        dataNames <- .self$names
      }
      dN <- dataNames %in% .self$names
      if (!any(dN)){
        writeLines('No matching names were found in names\n')
        return()
      }
      
      dataNames <- dataNames[dN]
      output<- lapply(
        dataNames,
        function(n).self$extractDatum(n)$areas
      )
      names(output) <- dataNames
      return(output)
    },
    extractIDs = function(dataNames){
      "Extracts data.frames of Area from supplied \\code{dataNames}. 
      Always returns named list."
      if (missing(dataNames)){
        #get all data
        dataNames <- .self$names
      }
      dN <- dataNames %in% .self$names
      if (!any(dN)){
        writeLines('No matching names were found in names\n')
        return()
      }
      
      dataNames <- dataNames[dN]
      output<- lapply(
        dataNames,
        function(n).self$extractDatum(n)$getIDs()
      )
      names(output) <- dataNames
      return(output)
    },
    getDetails = function(
      type = 'confluence', 
      asUnique=FALSE,
      ...
      ){
      "Returns possible inputs for \\code{type}. If \\code{asUnique=TRUE} or data values as vector. \\cr
      Possible inputs for \\code{type} are: \\cr
      \\itemize{
        \\item \\code{SampleRate}: Recording rate in frames per second.
        \\item \\code{TreatmentStart}: Time of Treatment, if applicable.
        \\item \\code{nCells}: Number of cells in the dataset.
        \\item \\code{HasBackground}: Whether or not a background correction performed.
        \\item \\code{ExperimentDate}: Date of the original collection.
        \\item \\code{CellType}: What kind of cells are we dealing with.
        \\item \\code{PassageNumber}: The passage number given as P#.
        \\item \\code{MediaSupl}: What other factors were introduced into the media.
        \\item \\code{StarvedLength}: How long were the cells serum starved prior to collection.
        \\item \\code{confluence}: How conlfuent, 0-100 percent, were the cells at collection time.
        \\item \\code{checkConfluence}: Provide \\code{prcnt} [0,100] in the \\code{...} arg. Returns boolean.
        \\item \\code{primaryTreatment}: Get the primary treatment, either NA or ET1
        \\item \\code{secondaryTreatment}: Get teh secondary treatmetn, either NA or IL1b
        \\item \\code{endothelin}: Returns \\code{TRUE} if ET1 was applied.
        \\item \\code{interleukin}: Returns \\code{TRUE} if IL1b was applied.
      }
      "
      type <- agrep(
        pattern = type,
        x = c(
          'SampleRate',      'TreatmentStart', 'nCells',        'HasBackground', 
          'ExperimentDate',  'CellType',       'PassageNumber', 'MediaSupl', 
          'StarvedLength',   'confluence',     'endothelin',    'interleukin',
          'primaryTreatment','secondaryTreatment','checkConfluence'
        ),
        ignore.case   = TRUE,
        value         = TRUE,
        max.distance = 0
      )[1]
      
      stopifnot(!identical(type,character(0)))
      
      switch(type, 
        confluence = {
          output <- sapply(
            .self$data,
            function(d) { d$getConfluence() }
          )
        },
        checkConfluence = {
          output <- sapply(
            .self$data,
            function(d) { d$isConfluent(...) }
          )
        },
        primaryTreatment = {
          output <- sapply(
            .self$data,
            function(d) { d$getTreatment() }
          )
        },
        secondaryTreatment = {
          output <- sapply(
            .self$data,
            function(d) { d$getOtherTreatment() }
          )
        },
        endothelin = {
          output <- sapply(
            .self$data,
            function(d) { d$isET1() }
          )
        },
        interleukin = {
          output <- sapply(
            .self$data,
            function(d) { d$isIL1b() }
          )
        },
        cellCount = {
          output <- sapply(
            .self$data,
            function(d) { d$nCells() }
          )
        },
        {
          output <- sapply(
            .self$data,
            function(d) { d$getMeta(type) }
          )
        }
      )
      #give an ID
      attr(output,'type')<- type
      
      if (asUnique){
        return(unique(output))
      }
      return(output)
    }
  )
)
