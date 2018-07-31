#' A class description
#'
#' @import methods
#' @export jonesDatum
#' @exportClass jonesDatum
#' 
jonesDatum<- setRefClass('jonesDatum',
  fields=list(
    fileName='character',
    meta='list',
    data='list',
    areas='data.frame',
    rois='list',
    image='list'
  ),
  methods=list(
    initialize = function(.self, fileName='',...) {################################################################### init
      #handle copy case
      if (fileName == '') {
        return(.self)
      }
      
      .self$fileName = basename(fileName) %>%
        sub(x = .,pattern = '\\.[^\\.]*$',replacement = '')
      
      dat<- fileImporter(filePath = fileName,...)
      
      .self$areas <- dat[[1]]$Areas
      .self$rois <- dat[[1]]$ROIs
      .self$image = dat[[1]]$Image
      
      ## parse metadata
      meta <- dat[[1]]$Info
      meta$nROI <- meta$nCells
      meta$nCells <- meta$nROI - meta$HasBackground
      #exp date
      meta$ExperimentDate <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'date of experiment',2
        ] %>% unname()
      # celltype
      meta$CellType <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'cells:',2
        ] %>% unname()
      meta$PassageNumber <- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'Passage:',2
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
      meta$BackgroundID <- dat[[1]]$BackgroundID
      meta$Treatment <- if (is.numeric(meta$TreatmentStart)) 'ET1' else 'none'
      meta$SecondaryTreatment<- dat[[1]]$File$Properties[
        dat[[1]]$File$Properties[,1] == 'other treatments',2
        ] %>% unname() %>% as.character()
      meta$CellIDs <- colnames(dat[[1]]$Data)
      # assign into .self
      .self$meta <- meta
      
      ## parse data
      .self$data <- list(
        Time= list(data=dat[[1]]$Time,units='sec'),
        Raw=  list(data=dat[[1]]$Original,units='Lum'),
        Adjusted= list(data=dat[[1]]$Data,units='dF/F0'),
        Rescaled= list(data=dat[[1]]$Scaled,units='AU')
      )
      
      #return
      return(.self)
    },
    show= function ( ){################################################################### Show / Plot / copy
      cat (" Jones Data File \ n ")
      cat (" Exp. Date :" , .self$meta$ExperimentDate , "\n")
      cat (" Cell Type :" , .self$meta$CellType , "\n")
      cat (" Treatment :" , .self$meta$Treatment , "\n")
      cat (" Other Treatments :" , .self$meta$SecondaryTreatment , "\n")
      cat (" Confluency :", paste0(.self$meta$Confluency*100, '% ') , "\n")
      cat (" Passage Number :" , .self$meta$PassageNumber , "\n")
      cat (" Data Containes:\n     ", 
           paste(
             paste0(.self$meta$nROI, ' ROIs,'),
             sprintf('%sbackground,', ifelse(.self$meta$HasBackground,'including ', 'without ')),
             sprintf('%d samples long', length(.self$data$Time$data)) 
           ),
           "\n")
      cat ("\n")
    },
    plot = function (x , y=c('','raw', 'adjusted', 'scaled') , xlab ="" , ylab ="" , axes = FALSE , asp =1 , ...){
      y<- match.arg(y)
      switch(y,
             raw={
               plotY<- x$data$Raw$data;
               yUnits <- x$data$Raw$units;
             },
             scaled={
               plotY<- x$data$Rescaled$data;
               yUnits <- x$data$Rescaled$units;
             },
             adjusted={
               plotY<- x$data$Adjusted$data;
               yUnits <- x$data$Adjusted$units;
             },
             {
               plotY<- x$data$Adjusted$data;
               yUnits <- x$data$Adjusted$units;
             }
      )
      # Plot the data
      graphics::plot( x= x$data$Time$data, y= plotY ,
             xlab = paste(xlab,paste0('(',x$data$Time$units,')')) , 
             ylab = paste(ylab,paste0('(',yUnits,')')) , 
             axes = axes ,
             asp = asp , ...)
    },
    copy = function() {
      theCopy <- jonesDatum('')
      theCopy$fileName=.self$fileName
      theCopy$meta= .self$meta
      theCopy$data= .self$data
      theCopy$areas= .self$areas
      theCopy$rois= .self$rois
      theCopy$image= .self$image
      return(theCopy)
    },################################################################### Getters
    getConfluence = function(){
      return(.self$meta$Confluency*100)
    },
    getTreatment = function(){
      return(
        .self$meta$Treatment
      )
    },
    getOtherTreatment = function(){
      return(
        .self$meta$SecondaryTreatment
      )
    },
    getData = function(type=c('adjusted', 'scaled','raw')){
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
             scaled = {
               output$y = .self$data$Rescaled$data
               output$units <- append(output$units,c(y=.self$data$Rescaled$units))
             },
             raw = {
               output$y = .self$data$Raw$data
               output$units <- append(output$units,c(y=.self$data$Raw$units))
             }
      )
      return(output)
    },
    getDataAsList = function(type=c('adjusted', 'scaled','raw')){
      type = match.arg(type)
      ncells <- .self$meta$nCells
      cellNames <- .self$meta$CellIDs
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
             scaled = {
               for (cell in 1:ncells){
                 output[[cellNames[cell]]] <- list(
                   data = data.frame(
                     x = .self$data$Time$data,
                     y = .self$data$Rescaled$data[,cell]
                   ),
                   units = c(x=.self$data$Time$units, y=.self$data$Rescaled$units)
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
             {
               output <- list()
             }
      )
      return(output)
    },################################################################### Logicals
    isConfluent = function(prcnt=100){
      return(
        getConfluence() == prcnt
      )
    },
    isET1 = function(){
      return(
        grepl('et[-]?1',tolower(getTreatment()))
      )
    },
    isIL1b = function(){
      return(
        grepl('il[-]?1b', tolower(getOtherTreatment()))
      )
    }
  )
)
