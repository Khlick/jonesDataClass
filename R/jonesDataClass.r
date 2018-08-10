#' jonesDataClass: A package for Jones Calium Imaging data
#'
#'
#' @section Included Functions:
#'   \itemize{
#'     \item[jonesDatum] Data Class for single file
#'     \item[jonesDataClass] Data Class for collection of \code{datum}s
#'     \item[fileImporter] Function that parses excel and image files. 
#'   }
#' @param files input file names
#' @param ... passed to other methods
#' @docType package
#' @name jonesDataClass
#' @examples
#' 
#' \donttest{
#' ## Load test files from package
#' dataFiles <- list.files(
#'   path = system.file('extdata',package='jonesDataClass'),
#'   pattern = '.xlsx$',
#'   full.names = TRUE
#' )
#' 
#' ## Import contents
#' dataObj <- jonesDataClass(dataFiles, verbose= TRUE)
#' }
#'
#' @export
jonesDataClass = function(files='', ...){
  return(dataCollective(files, ...))
}
