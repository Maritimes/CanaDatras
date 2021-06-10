#' @title makeDATRAS
#' @description This function generates ICES DATRAS-compatible HH for a particular DFO region.
#' @param region default is \code{NULL}. This is the Canadian DFO region for which you want to
#' make the DATRAS files.  Valid regions are listed below, but initially, only "Mar" works (Jan, 2020)
#' \itemize{
#' \item \code{Mar}
#' \item \code{NFLD}
#' \item \code{Gulf}
#' \item \code{Que}
#' \item \code{Cen}
#' \item \code{Pac}
#' }
#' @param yr default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param survey default is \code{NULL}. This specifies the survey for which you'd like to generate
#' HH files.  This will be specific to the different regions.  Valid values for Maritimes are
#' \itemize{
#' \item \code{"4VSW"}    Types 1 & 3; Jan - April (inclusive); Strata 396:411
#' \item \code{"SUMMER"}  Types 1 & 3; May - August (inclusive)
#' \item \code{"GEORGES"} Types 1 & 3; Jan - April (inclusive); != Strata 396:411
#' }
#' @param csv default is \code{TRUE}.  If \code{TRUE}, csv files are generated for each HH code.
#' If \code{FALSE}, the output exists only in the resultant list.
#' @param fn.oracle.username default is \code{'_none_'} Required for Maritimes HH files. This is
#' your username for accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} Required for Maritimes HH files. This is
#' your password for accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} Required for Maritimes HH files. This is your
#' dsn/ODBC identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. Required for Maritimes HH files. This indicates whether
#' the connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is
#' slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param data.dir  The default is \code{NULL}. Required for Maritimes HH files. This is the path
#' to your Mar.datawrangling rdata files
#' @param debug  The default is \code{F}. Setting this to TRUE will limit the 
#' results to a single set for a single species. 
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
makeDATRAS<-function(region = NULL, yr=NULL, survey = NULL, csv=T,
                 fn.oracle.username = "_none_",
                 fn.oracle.password = "_none_",
                 fn.oracle.dsn = "_none_",
                 usepkg = "rodbc",
                 data.dir = NULL,
                 debug =F){
region <- toupper(region)
survey <- toupper(survey)
good <- c("NFLD", "MAR", "GULF","QUE", "CEN", "PAC")
if (!region %in% good)stop("Please provide a valid region")

switch(region,
       "NFLD" = NFLD_DATRAS(yr=yr, survey=survey,  csv=csv, debug = debug),
       "MAR" = Mar_DATRAS(yr=yr, survey=survey,  csv=csv,
                            fn.oracle.username = fn.oracle.username,
                            fn.oracle.password = fn.oracle.password,
                            fn.oracle.dsn = fn.oracle.dsn,
                            data.dir = data.dir,
                            usepkg = usepkg,
                            debug = debug),
       "GULF" = Gulf_DATRAS(yr=yr, survey=survey,  csv=csv, debug = debug),
       "QUE" = Que_DATRAS(yr=yr, survey=survey,  csv=csv, debug = debug),
       "CEN" = Cen_DATRAS(yr=yr, survey=survey,  csv=csv, debug = debug),
       "PAC" = Pac_DATRAS(yr=yr, survey=survey,  csv=csv, debug = debug)
       )
}
