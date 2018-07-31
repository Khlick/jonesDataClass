  #' Class Definition for Storing jonesDatum objects with methods to access data, 
  #' subest traces, append/remove files, 
  #'
  #' @import methods
  #' @export dataCollective
  #' @exportClass dataCollective
  dataCollective<- setRefClass('dataCollective',
  fields=list(
    files = 'character',
    names = 'character',
    data = 'list',
    nFiles = 'numeric'
  ),
  methods=list(
    initialize = function(.self, files='',...){
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
      .self$data <- vector('list', length(files));
      names(.self$data) <- .self$names
      # parse each file 
      for (fn in files){
        fl <- fn %>% basename() %>% gsub(x = .,pattern='\\.[^\\.]*$',replacement = '')
        .self$data[fl] <- JonesDatum(filePath = fn, verbose = TRUE)
      }
      #Return
      invisible(.self)
    },
    size = function() {
      return(format(object.size(get('.self')), units = "auto"))
    },
    show = function(){
      str(.self,max.level = 2)
    },
    copy = function(){
      theCopy <- dataCollective(files='')
      theCopy$files <- .self$files
      theCopy$names <- .self$names
      theCopy$data <- .self$data
      theCopy$nFiles <- .self$nFiles
      return(theCopy)
    },
    save = function(pathName,objectName=''){
      #Save the current object on the file
      #in R external object format.
      if (objectName == ''){
        objectName <- paste0(
          'jonesData',
          gsub(x = Sys.Date(), pattern = '[[:punct:]]',replacement = '')
        )
      }
      #give obj a name
      objCall <- sprintf('%s <- .self$copy()', objectName)
      eval(parse(text=objCall))
      saveCall <- sprintf('base::save(%s, file = %s)', objectName,pathName)
      eval(parse(text=saveCall))
      cat('\n', sprintf('Saving object %s', objectName), '\n')
      cat(sprintf('located in %s', pathName),'\n\n')
    },
    push = function(newFiles){
      #Appends newfiles that do not already exist
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
      newData <- vector('list', newLen);
      names(newData) <- newNames
      #add to data model
      .self$data <- c(.self$data, newData)
      # parse each file 
      for (fn in newFiles){
        fi <- fn %>% basename() %>% gsub(x = .,pattern='\\.[^\\.]*$',replacement = '')
        .self$data[fi] <- JonesDatum(filePath = fn, verbose = TRUE)
      }
      .self$nFiles <- .self$nFiles + newLen
      .self$names <- c(.self$names,newNames)
      .self$files <- c(.self$files, newFiles)
      invisible(.self)
    },
    pop = function(first=FALSE){
      #returns and removes last element
      if (first) {
        index <- 1
      } else {
         index <- .self$nFiles
      }
      output <- .self$data[index]$copy();
    },
    merge = function(newFiles){
      #use newfiles to replace existing matching filenames
      #if newFiles are not within, push method is called 
    },
    getMatches = function(confluence=NULL, endothelin=NULL, interleukin=NULL){
      # Search through data and find provided conditions. 
      # Inputs left as NULL are ignored.
      # Returns vector of file names which match conditions
      
    },
    getDetails = function(type=c('confluence', 'endothelin', 'interleukin'), asUnique=FALSE){
      type = match.arg(type)
      output <- rep(NA, .self$nFiles)
      switch(type, 
        confluence = {
          for (d in 1:.self$nFiles){
            output[d] <- .self$data[.self$names[d]]$getConfluence()
          }
        },
        endothelin = {
          for (d in 1:.self$nFiles){
            output[d] <- .self$data[.self$names[d]]$getTreatment()
          }
        },
        interleukin = {
          for (d in 1:.self$nFiles){
            output[d] <<- .self$data[.self$names[d]]$getOtherTreatment()
          }
        }
      )
      
      if (asUnique){
        return(unique(output))
      }
      return(output)
    }
  )
  )
