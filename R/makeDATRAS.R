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
#' @param season default is \code{NULL}. This specifies the season(s) for which you'd like to generate
#' HH files.  Valid values are
#' \itemize{
#' \item \code{"SPRING"} i.e. Jan, Feb, Mar, Apr
#' \item \code{"SUMMER"} i.e. May, Jun, Jul, Aug
#' \item \code{"FALL"} i.e. Sep, Oct, Nov Dec
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
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
makeDATRAS<-function(region = NULL, yr=NULL, season = NULL, csv=T,
                 fn.oracle.username = "_none_",
                 fn.oracle.password = "_none_",
                 fn.oracle.dsn = "_none_",
                 usepkg = "rodbc",
                 data.dir = NULL){
region <- toupper(region)
good <- c("NFLD", "MAR", "GULF","QUE", "CEN", "PAC")
if (!region %in% good)stop("Please provide a valid region")

switch(region,
       "NFLD" = DATRAS_NFLD(yr=yr, season=season,  csv=csv),
       "MAR" = DATRAS_Mar(yr=yr, season=season,  csv=csv,
                            fn.oracle.username = fn.oracle.username,
                            fn.oracle.password = fn.oracle.password,
                            fn.oracle.dsn = fn.oracle.dsn,
                            data.dir = data.dir,
                            usepkg = usepkg),
       "GULF" = DATRAS_Gulf(yr=yr, season=season,  csv=csv),
       "QUE" = DATRAS_Que(yr=yr, season=season,  csv=csv),
       "CEN" = DATRAS_Cen(yr=yr, season=season,  csv=csv),
       "PAC" = DATRAS_Pac(yr=yr, season=season,  csv=csv)
       )
}
