#' @title Gulf_DATRAS
#' @description This function generates ICES DATRAS-compatible files directly from the Gulf
#' groundfish database.
#' @param yr default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param survey default is \code{RV}. This specifies the survey(s) for which you'd like to generate
#' HH files.  Valid values are
#' \itemize{
#' \item \code{"RV"} for the September survey
#' \item \code{"NS"} for the Northumberland Strait survey
#' \item \code{"SC"} for the snow crab survey
#' }
#' @param csv default is \code{TRUE}.  If \code{TRUE}, csv files are generated for each HH code.  If
#' \code{FALSE}, the output exists only in the resultant list.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param data.dir  The default is \code{NULL}. This is the path to your gulf
#' package Rdata files
#' @param debug  The default is \code{F}. Setting this to TRUE will limit the 
#' results to a single set for a single species. 
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Daniel Ricard, \email{Daniel.Ricard@dfo-mpo.gc.ca}
#' @export
#'
#'
Gulf_DATRAS <- function(yr=NULL, survey="RV", csv =T,
                fn.oracle.username = "_none_",
                fn.oracle.password = "_none_",
                fn.oracle.dsn = "_none_",
                usepkg = "rodbc",
                data.dir = NULL,
                debug = debug){
  timestamp<-format(Sys.time(), "%Y%m%d_%H%M")
  Sys.setenv(TZ = "America/Halifax")
  scratch_env = new.env()
  results<-list()
  ord_HH <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR", "MONTH","DAY",
              "TIMESHOT","DEPTHSTRATUM","HAULDUR","DAYNIGHT", "SHOOTLAT","SHOOTLONG",
              "HAULLAT","HAULLONG","STATREC","DEPTH", "HAULVAL","HYDROSTNO",
              "STDSPECRECCODE","BYSPECRECCODE","DATATYPE","NETOPENING",
              "RIGGING","TICKLER","DISTANCE","WARPLNGT","WARPDIA", "WARPDEN",
              "DOORSURFACE","DOORWGT","DOORSPREAD","WINGSPREAD","BUOYANCY",
              "KITEDIM","WGTGROUNDROPE","TOWDIR","GROUNDSPEED","SPEEDWATER",
              "SURCURDIR","SURCURSPEED","BOTCURDIR","BOTCURSPEED","WINDDIR",
              "WINDSPEED","SWELLDIR","SWELLHEIGHT","SURTEMP","BOTTEMP","SURSAL",
              "BOTSAL","THERMOCLINE","THCLINEDEPTH","CODENDMESH","SECCHIDEPTH",
              "TURBIDITY","TIDEPHASE","TIDESPEED","PELSAMPTYPE", 
              "MINTRAWLDEPTH","MAXTRAWLDEPTH")
  ord_HL <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT", 
              "GEAREXP", "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE", 
              "SPECCODE","SPECVAL","SEX","TOTALNO","CATIDENTIFIER","NOMEAS",
              "SUBFACTOR","SUBWGT","CATCATCHWGT","LNGTCODE","LNGTCLASS",
              "HLNOATLNGT","DEVSTAGE","LENMEASTYPE")
  ord_CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
              "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
              "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
              "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
              "AGEPREPMET","OTGRADING","PARSAMP")
  cat("\n", "Extracting Data")
  
  # Get all of the requested data
  for (y in 1:length(yr)){
    for (s in 1:length(survey)){
      cat(paste0("\n","Working on ", yr[y], " ",survey[s]))
      nm = paste0(survey[s],"_",yr[y])
      fullnm <- paste0(nm,"_",timestamp,".csv")
      tmp_HH <- data.frame(year=yr[y], survey=survey[s])
      tmp_HL <- data.frame(year=yr[y], survey=survey[s])
      tmp_CA <- data.frame(year=yr[y], survey=survey[s])
      
      if(csv){
        theFile <- file.create(fullnm)
        suppressWarnings(utils::write.table(x = tmp_HH, file = fullnm, row.names = F, col.names = F, quote = FALSE, sep = ","))
        suppressWarnings(utils::write.table(x = tmp_HL, file = fullnm, row.names = F, col.names = F, quote = FALSE, sep = ",", append = T))
        suppressWarnings(utils::write.table(x = tmp_CA, file = fullnm, row.names = F, col.names = F, quote = FALSE, sep = ",", append = T))

        cat("\n",paste0("File written to", getwd(),"/", fullnm))
      }
      thisyr <- list(HH = tmp_HH, HL = tmp_HL, CA = tmp_CA)
      results[[nm]]<-thisyr
      tmp_env<-NULL
      
      
    } ## end loop over surveys
  } ## end loop over year
  
  return(results)
}

