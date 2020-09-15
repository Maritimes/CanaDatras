#' @title DATRAS_Mar
#' @description This function generates ICES DATRAS-compatible files directly from the Maritimes
#' groundfish database.
#' @param yr default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param season default is \code{NULL}. This specifies the season(s) for which you'd like to generate
#' HH files.  Valid values are
#' \itemize{
#' \item \code{"SPRING"} i.e. Jan, Feb, Mar, Apr
#' \item \code{"SUMMER"} i.e. May, Jun, Jul, Aug
#' \item \code{"FALL"} i.e. Sep, Oct, Nov Dec
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
#' @param data.dir  The default is \code{NULL}. This is the path to your Mar.datawrangling
#' rdata files
#' @import Mar.datawrangling
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
DATRAS_Mar <- function(yr=NULL, season=NULL, csv =T,
                       fn.oracle.username = "_none_",
                       fn.oracle.password = "_none_",
                       fn.oracle.dsn = "_none_",
                       usepkg = "rodbc",
                       data.dir = NULL){
  season <- toupper(season)
  timestamp<-format(Sys.time(), "%Y%m%d_%H%M")
  Sys.setenv(TZ = "America/Halifax")
  scratch_env = new.env()
  results<-list()
  ord_HH <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR", "MONTH","DAY",
              "TIMESHOT","STRATUM","HAULDUR","DAYNIGHT", "SHOOTLAT","SHOOTLONG",
              "HAULLAT","HAULLONG","STATREC","DEPTH", "HAULVAL","HYDROSTNO",
              "STDSPECRECCODE","BYCSPECRECCODE","DATATYPE","NETOPENING",
              "RIGGING","TICKLER","DISTANCE","WARPLNGT","WARPDIA", "WARPDEN",
              "DOORSURFACE","DOORWGT","DOORSPREAD","WINGSPREAD","BUOYANCY",
              "KITEDIM","WGTGROUNDROPE","TOWDIR","GROUNDSPEED","SPEEDWATER",
              "SURCURDIR","SURCURSPEED","BOTCURDIR","BOTCURSPEED","WINDDIR",
              "WINDSPEED","SWELLDIR","SWELLHEIGHT","SURTEMP","BOTTEMP","SURSAL",
              "BOTSAL","THERMOCLINE","THCLINEDEPTH","CODENDMESH","SECCHIDEPTH",
              "TURBIDITY","TIDEPHASE","TIDESPEED","PELSAMPTYPE", 
              "MINTRAWLDEPTH","MAXTRAWLDEPTH")
  ord_HL <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT", 
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE", 
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
  if (!exists("ds_all")) ds_all <<- Mar.datawrangling::load_datasources()
  Mar.datawrangling::get_data_custom(schema="GROUNDFISH",
                                     fn.oracle.username = fn.oracle.username,
                                     fn.oracle.password = fn.oracle.password,
                                     fn.oracle.dsn = fn.oracle.dsn,
                                     data.dir = data.dir,
                                     usepkg = usepkg,
                                     tables = c("GSWARPOUT","GSSPEC","GSSPECIES_CODES","GS_LV1_OBSERVATIONS"),
                                     env = scratch_env, quiet = T)
  getRaw<-function(yr=NULL, season=NULL,
                   fn.oracle.username = fn.oracle.username,
                   fn.oracle.password = fn.oracle.password,
                   fn.oracle.dsn = fn.oracle.dsn,
                   data.dir = data.dir,
                   usepkg = usepkg){
    Mar.datawrangling::get_data('rv',
                                fn.oracle.username = fn.oracle.username,
                                fn.oracle.password = fn.oracle.password,
                                fn.oracle.dsn = fn.oracle.dsn,
                                data.dir = data.dir,
                                usepkg = usepkg,
                                env = scratch_env, quiet = T)
    scratch_env$GSMISSIONS = scratch_env$GSMISSIONS[scratch_env$GSMISSIONS$YEAR == yr
                                                    & scratch_env$GSMISSIONS$SEASON==season,]
    scratch_env$GSINF = scratch_env$GSINF[scratch_env$GSINF$GEAR %in% c(3,9,15)
                                          & scratch_env$GSINF$TYPE %in% c(1,3),]
    #get rid of 4VSW cod surey data
    # scratch_env$GSINF <- scratch_env$GSINF[-which(scratch_env$GSINF$STRAT %in% c(396:411) & lubridate::month(scratch_env$GSINF$SDATE) %in% c(1,2,3,4)),]
    Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = T)
    return(scratch_env)
  }

  # Get all of the requested data
  for (y in 1:length(yr)){
    for (s in 1:length(season)){
      cat(paste0("\n","Working on ", yr[y], " ",season[s]))
      nm = paste0(season[s],"_",yr[y])
      fullnm <- paste0(nm,"_",timestamp,".csv")
      tmp_env <- getRaw(yr=yr[y], season = season[s],
                        fn.oracle.username = fn.oracle.username,
                        fn.oracle.password = fn.oracle.password,
                        fn.oracle.dsn = fn.oracle.dsn,
                        data.dir = data.dir,
                        usepkg = usepkg)
      tmp_HH <- HH_Mar(scratch_env = tmp_env)
      tmp_HL <- HL_Mar(scratch_env = tmp_env)
      tmp_HL<-merge(tmp_HH[,c("mission","RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR")],
                    tmp_HL, all.y = T, by.x=c("mission", "STNO"), by.y=c("MISSION","SETNO"))
      tmp_HL$RECORDTYPE <- "HL"
      
      tmp_CA <- CA_Mar(scratch_env = tmp_env, HL = tmp_HL)
      tmp_CA <- merge(unique(tmp_CA), unique(tmp_HH[,c("mission", "STNO","DEPTHSTRATUM")]), all.x=T)
      colnames(tmp_CA)[colnames(tmp_CA)=="DEPTHSTRATUM"] <- "AREACODE"
      tmp_CA[is.na(tmp_CA)]<- -9

       SPP <- sort(unique(c(unique(scratch_env$GSCAT$SPEC), unique(scratch_env$GSDET$SPEC))))
       SPP <- data.frame(SPEC = SPP)
       GSSPECIES_CODES <- scratch_env$GSSPECIES_CODES[scratch_env$GSSPECIES_CODES$CODE %in% SPP$SPEC,c("CODE","APHIAID")]
       colnames(GSSPECIES_CODES)[colnames(GSSPECIES_CODES)=="APHIAID"] <- "SPECCODE"
       GSSPECIES_CODES$SPECCODETYPE <- -9
       GSSPECIES_CODES[!is.na(GSSPECIES_CODES$SPECCODE),"SPECCODETYPE"]<- "W"
       
       tmp_HL <- merge(tmp_HL, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
       tmp_CA <- merge(tmp_CA, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
       
      ord_HH<-ord_HH[ord_HH %in% names(tmp_HH)]
      ord_HL<-ord_HL[ord_HL %in% names(tmp_HL)]
      ord_CA<-ord_CA[ord_CA %in% names(tmp_CA)]
      tmp_HH<-tmp_HH[,ord_HH]
      tmp_HL<-tmp_HL[,ord_HL]
      tmp_CA<-tmp_CA[,ord_CA]
      
      if(csv){
        theFile <- file.create(fullnm)
        utils::write.table(x = tmp_HH, file = fullnm, row.names = F, col.names = FALSE, quote = FALSE, sep = ",")
        utils::write.table(x = tmp_HL, file = fullnm, row.names = F, col.names = FALSE, quote = FALSE, sep = ",", append = T) 
        utils::write.table(x = tmp_CA, file = fullnm, row.names = F, col.names = FALSE, quote = FALSE, sep = ",", append = T)

      }
      thisyr <- list(HH = tmp_HH, HL = tmp_HL, CA = tmp_CA)
      results[[nm]]<-thisyr
      tmp_env<-NULL
    }
  }
  return(results)
}
