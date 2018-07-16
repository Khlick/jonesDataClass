#####
## Data class for Jones Group Calcium oscillation files
# The following S4 class imports data from an excel file with a well-defined 
# structure. Metadata provided in the excel file header will be allocated to the
# metadata slot. The defined, `get`, methods will access sealed data.


#dependent static functions
# library(dplyr)
# source('fileImporter.R')

#####
## Define datum class

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
    ################################################################### init
    initialize = function(.self, fileName='',...) {
      
      stopifnot(fileName != '')
      
      .self$fileName = basename(fileName) %>%
        sub(x = .,pattern = '\\.[^\\.]*$',replacement = '')
      
      dat<- fileImporter(filePath = fileName,...)
      
      .self$areas <- dat[[1]]$Areas
      .self$rois <- dat[[1]]$ROIs
      .self$image = dat[[1]]$Image
      
      ## parse metadata
      meta <- dat[[1]]$Info
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
    ################################################################### Show / Plot
    show= function ( ){
      cat (" Jones Data File \ n ")
      cat (" Exp. Date :" , .self$meta$ExperimentDate , "\n")
      cat (" Cell Type :" , .self$meta$CellType , "\n")
      cat (" Treatment :" , .self$meta$Treatment , "\n")
      cat (" Other Treatments :" , .self$meta$SecondaryTreatment , "\n")
      cat (" Confluency :", paste0(.self$meta$Confluency*100, '% ') , "\n")
      cat (" Passage Number :" , .self$meta$PassageNumber , "\n")
      cat (" Data Containes:\n     ", 
           paste(
             paste0(.self$meta$nCells, ' ROIs,'),
             sprintf('%sbackground,', ifelse(.self$meta$HasBackground,'including ', 'without ')),
             sprintf('%d samples long', length(.self$data$Time$data)) 
           ),
           "\n")
      cat ("\n")
    }
  ),
  plot = function (x , y=c('','raw', 'adjusted', 'scaled') , xlab ="" , ylab ="" , axes = FALSE , asp =1 , ...){
    y<- match.arg(y)
    switch(y,
           raw={
             plotY<- x@data$Raw$data;
             yUnits <- x@data$Raw$units;
           },
           scaled={
             plotY<- x@data$Rescaled$data;
             yUnits <- x@data$Rescaled$units;
           },
           adjusted={
             plotY<- x@data$Adjusted$data;
             yUnits <- x@data$Adjusted$units;
           },
           {
             plotY<- x@data$Adjusted$data;
             yUnits <- x@data$Adjusted$units;
           }
    )
    plot ( x= x@data$Time$data, y= x@col = x@col ,
           xlab = paste(xlab,paste0('(',x@data$Time$units,')')) , 
           ylab = paste(ylab,paste0('(',yUnits,')')) , 
           axes = axes ,
           asp = asp , ...)
  },
  ################################################################### Getters
  getConfluence = function(){
    return(.self$meta$Confluency*100)
  },
  ################################################################### Logicals
  isConfluent = function(prcnt=100){
    return(
      getConfluence() == prcnt
    )
  }
  
)
