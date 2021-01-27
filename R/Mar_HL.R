#' @title Mar_HL
#' @description This function generates ICES DATRAS-compatible "HL" files 
#' directly from the Maritimes groundfish database. "HL" files contain Species 
#' length-based information
#' @param scratch_env default is \code{NULL} This is an environment containing the results of a
#' Mar.datawrangling extraction - i.e. it contains all of the data necessary for HH, HL and CA
#' @return a df generated HL file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
Mar_HL <- function(scratch_env = NULL){
  # Our "std spp" are:
  # c(10, 11, 12, 14, 16, 23, 30, 40, 41, 42, 43, 60, 70)
  # Our known 67 spp since 1970  are:
  # c(10,11,12,13,14,15,16,17,23,30,31,40,41,42,43,44,50,51,52,60,61,62,64,70,94,112,114,118,122,
  #   123,141,142,143,160,200,201,202,203,204,216,220,221,240,241,300,304,320,350,400,409,410,411,
  #   412,413,414,501,502,610,622,623,630,637,640,647,701,704,2550
  cat("\n","Generating HL... ")
  
  handleGSINF<-function(){
    df<-scratch_env$GSINF
    df <- df[,c("MISSION","SETNO", "STRAT","TYPE")]
    colnames(df)[colnames(df)=="STRAT"] <- "AREACODE"
    return(df)
  }
  handleGSCAT<-function(){
    df<-scratch_env$GSCAT
    doSubFact <- function(df = NULL){
      # SubFactor – sub-sampling factor by haul, species, sex, length. Value = or > 1. 
      # Make sure that TotalNo =  NoMeas x SubFactor
      df$SUBFACTOR <- -9
      df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SUBFACTOR"] <- df[df$TOTWGT > 0 & df$SAMPWGT > 0,"TOTWGT"]/df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SAMPWGT"]
 
      return(df)
    }
    #rename some fields to match ICES
    df = doSubFact(df)
    colnames(df)[colnames(df)=="TOTNO"] <- "TOTALNO"
    colnames(df)[colnames(df)=="SAMPWGT"] <- "SUBWGT"
    #   # make subwgt -9 for all cases where no subsampling occurred
    if (nrow(df[df$TOTWGT == df$SUBWGT,])>0) df[df$TOTWGT == df$SUBWGT,"SUBWGT"]<--9
    if (length(df[which(df$SUBWGT == 0),"SUBWGT"])>0) df[which(df$SUBWGT == 0),"SUBWGT"] <--9
    df$SUBWGT <- round2(df$SUBWGT,0)
    return(df)
  }
  handleGSDET<-function(){
    addSexCodes <- function(df = NULL){
      # change to ICES sex codes
      df$SEX<-NA
      df[which(is.na(df$FSEX)),"SEX"]<--9                 #undetermined
      df[which(df$FSEX==0),"SEX"]<-"U"                    #unknown (0)
      df[which(df$FSEX==1),"SEX"]<-"M"                    #male (1)
      df[which(df$FSEX==2),"SEX"]<-"F"                    #female (2)
      df[which(df$FSEX==3),"SEX"]<-"B"                    #berried female (3)
      df$FSEX<-NULL
      return(df)
    }
    addDevStage <- function(df = NULL){
      # change to ICES devstage codes
      df$DEVSTAGE<--9
      return(df)
    }
    handleDetSpecies<-function(df = NULL){
      #herring were recorded in mm starting SUMMER 2016 
      #want to convert all measurements to mm (Summer 2016 ie.NED2016016 was in mm, Spring was in cm)
      
      # - this converts to cm (same units as LGRP) MISSION reqd to avoid Spring surveys, where they were recorded in cm
      if (nrow(df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <=2016 & (!df$MISSION %in% c("NED2016016")))),])>0)
        df[df$SPEC == 60 & (substr(df$MISSION, 4,7)<=2016 & (!df$MISSION %in% c("NED2016016"))),"FLEN"] <- df[df$SPEC == 60 & (substr(df$MISSION, 4,7)<=2016 & (!df$MISSION %in% c("NED2016016"))),"FLEN"]*10
      return(df)
    }
    df<-scratch_env$GSDET[,names(scratch_env$GSDET) %in% c("MISSION", "SETNO", "SPEC", "FMAT", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS", "SPECIMEN_ID")]
    colnames(df)[colnames(df)=="SIZE_CLASS"] <- "CATIDENTIFIER"
    df <- addSexCodes(df)
    df <- addDevStage(df)
    df <- handleDetSpecies(df)
    # if (nrow(df[which(is.na(df$FWT)),])>0) df[which(is.na(df$FWT)),"FWT"]<-0
    # # df$FWT[is.na(df$FWT)] <- 1
    df$FMAT <- NULL
    return(df)
  }
  handleSpecies<-function(){
    
    #get all of the species - the CODE, APHIAID, LGRP
    SPP <- sort(unique(c(unique(scratch_env$GSCAT$SPEC), unique(scratch_env$GSDET$SPEC))))
    SPP <- data.frame(SPEC = SPP)
    SPP <- addLenMeasInfo(SPP)
  }
  GSDET <- handleGSDET()
  GSCAT <- handleGSCAT()
  GSINF <- handleGSINF()
  SPP <- handleSpecies()
  
  #subfactor - maybe this needs to be determined first?
  forAgg <- merge(SPP, GSDET[,c("SPEC", "MISSION","SETNO", "FLEN", "FWT", "SEX","CATIDENTIFIER")], by = "SPEC")
  # browser()
  forAgg <- addLNGTCLASS(forAgg)
  # forAgg$LNGTCLASS<- ceiling(forAgg$FLEN/forAgg$LNGTCODE) * forAgg$LNGTCODE
  forAgg$FLEN<-NULL
  
  tmp_CATCATCHWGT <- stats::aggregate(
    x = list(CATCATCHWGT = forAgg$FWT),
    by = list(SPEC = forAgg$SPEC,
              MISSION = forAgg$MISSION,
              SETNO = forAgg$SETNO,
              SEX = forAgg$SEX,
              CATIDENTIFIER = forAgg$CATIDENTIFIER
    ),
    sum
  )
  tmp_CATCATCHWGT$CATCATCHWGT <- round2(tmp_CATCATCHWGT$CATCATCHWGT,0)
  tmp_HLNOATLNGT <- stats::aggregate(
    x = list(HLNOATLNGT = forAgg$LNGTCLASS),
    by = list(SPEC = forAgg$SPEC,
              MISSION = forAgg$MISSION,
              SETNO = forAgg$SETNO,
              SEX = forAgg$SEX,
              CATIDENTIFIER = forAgg$CATIDENTIFIER,
              LNGTCLASS = forAgg$LNGTCLASS
    ),
    length
  )
  
  # tmp_HL1 <- getHLNOATLNGT(forAgg)
  tmp_HL2 <- merge(tmp_CATCATCHWGT, tmp_HLNOATLNGT, all.y=T)
  tmp_HL2 <- tmp_HL2[,c("SPEC", "MISSION","SETNO","SEX","LNGTCLASS","CATIDENTIFIER","HLNOATLNGT", "CATCATCHWGT")]
  
  # tmp_HL$DEVSTAGE <- -9 #devstage (i.e. maturity) not used at a haul-level (i.e. GSCAT)
  tmp_HL2 = merge(tmp_HL2, GSCAT, all.x = T)
  
  tmp_NoMeas= unique(as.data.frame(as.list(stats::aggregate(
    x = list(NOMEAS = tmp_HL2$HLNOATLNGT),
    by = list(SPEC = tmp_HL2$SPEC,
              MISSION = tmp_HL2$MISSION,
              SETNO = tmp_HL2$SETNO,
              CATIDENTIFIER = tmp_HL2$CATIDENTIFIER,
              SEX = tmp_HL2$SEX
    ),
    sum
  ))))

  
  tmp_HL2 = merge(tmp_HL2, tmp_NoMeas)
  
  tmp_HL2 <- merge(tmp_HL2, SPP)
  tmp_HL3 <- merge(tmp_HL2, GSINF)
  
  tmp_HL3$SPECVAL <- NA
  tmp_HL3$SPECVAL <- ifelse(tmp_HL3$TYPE == 3, 0,
                          ifelse(tmp_HL3$LNGTCLASS == -9, 7,
                              ifelse(tmp_HL3$TOTALNO <= 0 & tmp_HL3$TOTWGT >0 , 6, 1)))
  
  tmp_HL3$TOTWGT<-tmp_HL3$TYPE <- NULL
  tmp_HL3[which(tmp_HL3$CATCATCHWGT == 0),"CATCATCHWGT"] <- -9
  tmp_HL3[is.na(tmp_HL3)]<- -9 #catch all to turn all NAs to -9
  tmp_HL3[which(tmp_HL3$CATCATCHWGT==-9 & tmp_HL3$TOTALNO ==9),"SPECVAL"]<- 5

  cat("Done")
  return(tmp_HL3)
}
