#' @title Mar_DATRAS
#' @description This function generates ICES DATRAS-compatible files directly from the Maritimes
#' groundfish database.
#' @param yr default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param survey default is \code{NULL}. This specifies the survey(s) for which you'd like to generate
#' HH files.  Valid values are
#' \itemize{
#' \item \code{4X} - Type 1; Spring (i.e. months 1:4); 2008+; not strata 5Z* 
#' \item \code{GEORGES} - Type 1; Spring (i.e. months 1:4);  2008+; strata 5Z*
#' \item \code{SPRING} - Type 1; Spring (i.e. months 1:4); pre-2008; specific strata 
#' \item \code{4VSW}  - Type 1; Spring (i.e. months 1:4); 4VSW strata;  
#' \item \code{SUMMER} - Type 1; Summer (i.e. months 5:8); specific strata
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:12)
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
Mar_DATRAS <- function(yr=NULL, survey=NULL, csv =T,
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
              "HLNOATLNGT","DEVSTAGE","LENMEASTYPE") #"GEAREXP",
  ord_CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
              "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
              "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
              "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
              "AGEPREPMET","OTGRADING","PARSAMP") #"GEAREXP",
  cat("\n", "Extracting Data")
  if (!exists("ds_all")) ds_all <<- Mar.datawrangling::load_datasources()
  Mar.datawrangling::get_data('rv', 
                      fn.oracle.username = fn.oracle.username,
                      fn.oracle.password = fn.oracle.password,
                      fn.oracle.dsn = fn.oracle.dsn,
                      data.dir = data.dir,
                      usepkg = usepkg,
                      env = scratch_env, quiet = T)
  Mar.datawrangling::get_data_custom(schema="GROUNDFISH",
                                     fn.oracle.username = fn.oracle.username,
                                     fn.oracle.password = fn.oracle.password,
                                     fn.oracle.dsn = fn.oracle.dsn,
                                     data.dir = data.dir,
                                     usepkg = usepkg,
                                     tables = c("GSWARPOUT","GSSPECIES_CODES","GS_LV1_OBSERVATIONS"),
                                     env = scratch_env, quiet = T)
  
  getRaw<-function(yr=NULL, survey=NULL,
                   fn.oracle.username = fn.oracle.username,
                   fn.oracle.password = fn.oracle.password,
                   fn.oracle.dsn = fn.oracle.dsn,
                   data.dir = data.dir,
                   usepkg = usepkg){

    Mar.datawrangling::get_survey('rv', data.dir = data.dir, env = scratch_env, quiet = T, survey = survey, keepBadSets = T)
    scratch_env$GSMISSIONS = scratch_env$GSMISSIONS[scratch_env$GSMISSIONS$YEAR == yr,]
    Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = F)
    
    #identify those species that were caught and recorded, but for which we have no APHIA_ID
    #These cannot go to DATRAS
    #notify user of their presence, then remove from data
    unkSpp <-  scratch_env$GSSPECIES[!(scratch_env$GSSPECIES$CODE %in% scratch_env$GSSPECIES_CODES[!is.na(scratch_env$GSSPECIES_CODES$APHIAID),"CODE"]), c("CODE", "COMM", "SPEC")]
    badSpp1 <- unique(scratch_env$GSCAT[scratch_env$GSCAT$SPEC %in% unkSpp$CODE,"SPEC"])
    badSpp2 <- unique(scratch_env$GSDET[scratch_env$GSDET$SPEC %in% unkSpp$CODE,"SPEC"])
    allBad <- unique(c(badSpp1, badSpp2))
    if (length(allBad)>0){
      fullnmSpp <- gsub(".csv", "_sppMissing.csv", fullnm)
      theSppFile <- file.create(fullnmSpp)
      suppressWarnings(utils::write.table(x = scratch_env$GSSPECIES[scratch_env$GSSPECIES$CODE %in% allBad, c("CODE", "COMM", "SPEC")], file = fullnmSpp, row.names = F, col.names = T, quote = FALSE, sep = ","))
      message("\nA file was generated containing species names reported in the catch that don't have aphiaids (", fullnmSpp,")")
      scratch_env$GSSPECIES<-scratch_env$GSSPECIES[!(scratch_env$GSSPECIES$CODE %in% allBad),]
      scratch_env$GSCAT<-scratch_env$GSCAT[!(scratch_env$GSCAT$SPEC %in% allBad),]
      scratch_env$GSDET<-scratch_env$GSDET[!(scratch_env$GSDET$SPEC %in% allBad),]
      Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = T)
    }
    return(scratch_env)
  }
  
  # Get all of the requested data
  for (y in 1:length(yr)){
    for (s in 1:length(survey)){
      cat(paste0("\n","Working on ", yr[y], " ",survey[s]))
      nm = paste0(survey[s],"_",yr[y])
      fullnm <- paste0(nm,"_",timestamp,".csv")
      tmp_env <- getRaw(yr=yr[y], survey = survey[s],
                        fn.oracle.username = fn.oracle.username,
                        fn.oracle.password = fn.oracle.password,
                        fn.oracle.dsn = fn.oracle.dsn,
                        data.dir = data.dir,
                        usepkg = usepkg)
      
      #convert gscat values to grams (gsdet already in g)
      if (nrow(tmp_env$GSINF)==0){
        message("\nNo data found matching parameters")
        theFile <- file.create(fullnm)
        results[[nm]]<-NA
        utils::write.csv(x = NA, file = paste0(fullnm,"_noResults.csv"), row.names = F)
        next
      }
      tmp_env$GSCAT$SAMPWGT <- tmp_env$GSCAT$SAMPWGT*1000
      tmp_env$GSCAT$TOTWGT <- tmp_env$GSCAT$TOTWGT*1000
      
      tmp_HH <- Mar_HH(scratch_env = tmp_env, survey = survey)
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
      badTows <- tmp_HH[tmp_HH$HAULVAL == "I","HAULNO"]
      if (length(badTows)>0){
        tmp_HL[tmp_HL$HAULNO %in% badTows,"SPECVAL"] <-0
      }
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
        CAsp =stats::aggregate(tmp_CA$SPECCODE,
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
      colnames(tmp_HL)[colnames(tmp_HL)=="GEAREXP"] <- "GEAREX"
      
      
      SHIPS <- unique(tmp_HH$SHIP)
      for (s in 1:length(SHIPS)){
        nmShip = paste0(nm,"_",SHIPS[s])
        fullnmShip <- gsub(".csv", paste0("_",SHIPS[s],".csv"), fullnm)
        this_tmp_HH <- tmp_HH[tmp_HH$SHIP == SHIPS[s],]
        this_tmp_HL <- tmp_HL[tmp_HL$SHIP == SHIPS[s],]
        this_tmp_CA <- tmp_CA[tmp_CA$SHIP == SHIPS[s],]
        
        if(csv){
          theFile <- file.create(fullnmShip)
          suppressWarnings(utils::write.table(x = this_tmp_HH, file = fullnmShip, row.names = F, col.names = F, quote = FALSE, sep = ","))
          suppressWarnings(utils::write.table(x = this_tmp_HL, file = fullnmShip, row.names = F, col.names = F, quote = FALSE, sep = ",", append = T))
          suppressWarnings(utils::write.table(x = this_tmp_CA, file = fullnmShip, row.names = F, col.names = F, quote = FALSE, sep = ",", append = T))
          if (debug){
            utils::write.csv(x = this_tmp_HH, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_HH_debug.csv"), row.names = F)
            utils::write.csv(x = this_tmp_HL, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_HL_debug.csv"), row.names = F) 
            utils::write.csv(x = this_tmp_CA, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_CA_debug.csv"), row.names = F)
          }
          cat("\n",paste0("File written to", getwd(),"/", fullnmShip))
        }
        thisyrShp <- list(HH = this_tmp_HH, HL = this_tmp_HL, CA = this_tmp_CA)
        results[[nmShip]]<-thisyrShp
      }
      

      tmp_env<-NULL
    }
  }
  return(results)
}
