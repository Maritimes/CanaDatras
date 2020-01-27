#' @title DATRAS_Pac
#' @description This is a placeholder for an imaginary function which would generate
#' ICES DATRAS-compatible files for Pacific region.
#' @param yr default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param season default is \code{NULL}.
#' @param csv default is \code{TRUE}.  If \code{TRUE}, csv files are generated for each HH code.  If
#' \code{FALSE}, the output exists only in the resultant list.
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
DATRAS_Pac <- function(yr=NULL, season=NULL, csv = NULL){
  cat("\n","Pac not implemented yet")
  return(NULL)
}
