#' @title Mar_DATRAS
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
#' @param debug  The default is \code{F}. Setting this to TRUE will limit the 
#' results to a single set for a single species. 
#' @import Mar.datawrangling
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
Mar_DATRAS <- function(yr=NULL, season=NULL, csv =T,
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
              "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE", 
              "SPECCODE","SPECVAL","SEX","TOTALNO","CATIDENTIFIER","NOMEAS",
              "SUBFACTOR","SUBWGT","CATCATCHWGT","LNGTCODE","LNGTCLASS",
              "HLNOATLNGT","DEVSTAGE","LENMEASTYPE") #"GEAREXP",
  ord_CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
              "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
              "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
              "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
              "AGEPREPMET","OTGRADING","PARSAMP") #"GEAREXP",
  cat("\n", "Extracting Data")
  if (!exists("ds_all")) ds_all <<- Mar.datawrangling::load_datasources()
  Mar.datawrangling::get_data_custom(schema="GROUNDFISH",
                                     fn.oracle.username = fn.oracle.username,
                                     fn.oracle.password = fn.oracle.password,
                                     fn.oracle.dsn = fn.oracle.dsn,
                                     data.dir = data.dir,
                                     usepkg = usepkg,
                                     tables = c("GSWARPOUT","GSSPECIES_CODES","GS_LV1_OBSERVATIONS"),
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
    #temporary!
    Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = F)
    
    #identify those species that were caught and recorded, but for which we have no APHIA_ID
    #These cannot go to DATRAS
    #notify user of their presence, then remove from data
    unkSpp <-  scratch_env$GSSPECIES[!(scratch_env$GSSPECIES$CODE %in% scratch_env$GSSPECIES_CODES[!is.na(scratch_env$GSSPECIES_CODES$APHIAID),"CODE"]), c("CODE", "COMM", "SPEC")]
    badSpp1 <- unique(scratch_env$GSCAT[scratch_env$GSCAT$SPEC %in% unkSpp$CODE,"SPEC"])
    badSpp2 <- unique(scratch_env$GSDET[scratch_env$GSDET$SPEC %in% unkSpp$CODE,"SPEC"])
    allBad <- unique(c(badSpp1, badSpp2))

     if (length(allBad)>0){
       cat("\n!!!\n","The following 'species' were present, but don't have AphiaIDs in the GROUNDFISH.GSSPECIES_CODES table.","\n"," Until they do, they can't be included in DATRAS submissions and will be dropped:","\n")
       print(scratch_env$GSSPECIES[scratch_env$GSSPECIES$CODE %in% allBad, c("CODE", "COMM", "SPEC")])
       cat("\n!!!")
       scratch_env$GSSPECIES<-scratch_env$GSSPECIES[!(scratch_env$GSSPECIES$CODE %in% allBad),]
       scratch_env$GSCAT<-scratch_env$GSCAT[!(scratch_env$GSCAT$SPEC %in% allBad),]
       scratch_env$GSDET<-scratch_env$GSDET[!(scratch_env$GSDET$SPEC %in% allBad),]
       Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = T)
     }
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
      
      #convert gscat values to grams (gsdet already in g)
      tmp_env$GSCAT$SAMPWGT <- tmp_env$GSCAT$SAMPWGT*1000
      tmp_env$GSCAT$TOTWGT <- tmp_env$GSCAT$TOTWGT*1000
      
      tmp_HH <- Mar_HH(scratch_env = tmp_env)
      tmp_HL <- Mar_HL(scratch_env = tmp_env)
      tmp_HL<-merge(tmp_HH[,c("mission","RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR")],
                    tmp_HL, all.y = T, by.x=c("mission", "STNO"), by.y=c("MISSION","SETNO"))
      tmp_HL$RECORDTYPE <- "HL"
      
      tmp_CA <- Mar_CA(scratch_env = tmp_env)
      tmp_CA<-merge(tmp_HH[,c("mission","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","DEPTHSTRATUM")], tmp_CA, all.y = T)
      
      tmp_HL$LNGTCODE <- as.character(tmp_HL$LNGTCODE) 
      tmp_HL$LNGTCODE <- gsub('0.1', '.', tmp_HL$LNGTCODE)
      tmp_CA$LNGTCODE <- as.character(tmp_CA$LNGTCODE) 
      tmp_CA$LNGTCODE <- gsub('0.1', '.', tmp_CA$LNGTCODE)
      
      colnames(tmp_CA)[colnames(tmp_CA)=="DEPTHSTRATUM"] <- "AREACODE"
      tmp_CA[is.na(tmp_CA)]<- -9
      SPP <- sort(unique(c(unique(scratch_env$GSCAT$SPEC), unique(scratch_env$GSDET$SPEC))))
      SPP <- data.frame(SPEC = SPP)
      GSSPECIES_CODES <- scratch_env$GSSPECIES_CODES[scratch_env$GSSPECIES_CODES$CODE %in% SPP$SPEC,c("CODE","APHIAID")]
      colnames(GSSPECIES_CODES)[colnames(GSSPECIES_CODES)=="APHIAID"] <- "SPECCODE"
      GSSPECIES_CODES$SPECCODETYPE <- -9
      GSSPECIES_CODES[!is.na(GSSPECIES_CODES$SPECCODE),"SPECCODETYPE"]<- "W"
      GSSPECIES_CODES[is.na(GSSPECIES_CODES$SPECCODE),"SPECCODE"]<- -9
      
      tmp_HL <- merge(tmp_HL, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
      tmp_CA <- merge(tmp_CA, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
      HHMissing <- setdiff(ord_HH, names(tmp_HH))
      tmp_HH[HHMissing]<- -9
      
      HLMissing <- setdiff(ord_HL, names(tmp_HL))
      tmp_HL[HLMissing]<- -9
      
      CAMissing <- setdiff(ord_CA, names(tmp_CA))
      tmp_CA[CAMissing]<- -9
      
      ord_HH<-ord_HH[ord_HH %in% names(tmp_HH)]
      ord_HL<-ord_HL[ord_HL %in% names(tmp_HL)]
      ord_CA<-ord_CA[ord_CA %in% names(tmp_CA)]
      tmp_HH<-tmp_HH[,ord_HH]
      tmp_HL<-tmp_HL[,ord_HL]
      tmp_CA<-tmp_CA[,ord_CA]
      if (debug){
        cat("Just getting 1 set and 1 species","\n")
        CAsp =aggregate(tmp_CA$SPECCODE,
                        by = list(
                          STNO = tmp_CA$STNO,
                          SPECCODE = tmp_CA$SPECCODE
                        ),
                        length
        )
        CAspMAX <- CAsp[which.max(CAsp$x),]
        # tmp_CA <- 
        tmp_CA <- tmp_CA[tmp_CA$STNO == CAspMAX$STNO & tmp_CA$SPECCODE == CAspMAX$SPECCODE, ]
        tmp_HL <- tmp_HL[tmp_HL$STNO == CAspMAX$STNO & tmp_HL$SPECCODE == CAspMAX$SPECCODE, ]
        tmp_HH <- tmp_HH[tmp_HH$STNO == CAspMAX$STNO, ]
      }
      
      colnames(tmp_HH)[colnames(tmp_HH)=="GEAREXP"] <- "GEAREX"
      if(csv){
        theFile <- file.create(fullnm)
        suppressWarnings(utils::write.table(x = tmp_HH, file = fullnm, row.names = F, col.names = TRUE, quote = FALSE, sep = ","))
        suppressWarnings(utils::write.table(x = tmp_HL, file = fullnm, row.names = F, col.names = TRUE, quote = FALSE, sep = ",", append = T))
        suppressWarnings(utils::write.table(x = tmp_CA, file = fullnm, row.names = F, col.names = TRUE, quote = FALSE, sep = ",", append = T))
        if (debug){
          utils::write.csv(x = tmp_HH, file = paste0(fullnm,"_HH_debug.csv"), row.names = F)
          utils::write.csv(x = tmp_HL, file = paste0(fullnm,"_HL_debug.csv"), row.names = F) 
          utils::write.csv(x = tmp_CA, file = paste0(fullnm,"_CA_debug.csv"), row.names = F)
        }
        cat("\n","File written to", getwd(),"/", fullnm)
      }
      thisyr <- list(HH = tmp_HH, HL = tmp_HL, CA = tmp_CA)
      results[[nm]]<-thisyr
      tmp_env<-NULL
    }
  }
  return(results)
}
