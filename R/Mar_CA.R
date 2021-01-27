#' @title Mar_CA
#' @description This function generates ICES DATRAS-compatible "CA" files 
#' directly from the Maritimes. "CA" files contain Species age-based information
#' @param scratch_env default is \code{NULL} This is an environment containing the results of a
#' Mar.datawrangling extraction - i.e. it contains all of the data necessary for HH, HL and CA
#' @return a df generated CA file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
Mar_CA <- function(scratch_env = NULL){
  cat("\n","Generating CA... ")
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
    addMaturity<-function(df = NULL){
      df$MATURITYSCALE<- "M6"
      df$MATURITY<- -9
      if (nrow(df[which(is.na(df$FMAT)),])>0)         df[which(is.na(df$FMAT)),"MATURITY"]<--9
      if (nrow(df[which(df$FMAT==0),])>0)             df[which(df$FMAT==0),"MATURITY"]<--9 #DClark indicates that 0 should be NULL
      if (nrow(df[which(df$FMAT==9),])>0)             df[which(df$FMAT==9),"MATURITY"]<--9 #DClark indicates that 9 is probably an error
      if (nrow(df[which(df$FMAT %in% c(1)),])>0)      df[which(df$FMAT %in% c(1)),"MATURITY"]<-61
      if (nrow(df[which(df$FMAT %in% c(2,3,4)),])>0)  df[which(df$FMAT %in% c(2,3,4)),"MATURITY"]<-62
      if (nrow(df[which(df$FMAT %in% c(5)),])>0)      df[which(df$FMAT %in% c(5)),"MATURITY"]<-63
      if (nrow(df[which(df$FMAT %in% c(6)),])>0)      df[which(df$FMAT %in% c(6)),"MATURITY"]<-64
      if (nrow(df[which(df$FMAT %in% c(7,8)),])>0)   df[which(df$FMAT %in% c(7,8)),"DEVSTAGE"]<-65
      df$FMAT <- NULL
      return(df)
    }
    addAges <- function(df = NULL){
      df$PLUSGR <- -9
      df$AGERINGS <- df$AGE
      df[which(is.na(df$AGERINGS)),"AGERINGS"]<--9 
      
      df$AGESOURCE <- -9
      if (nrow(df[which(df$AGMAT==1),])>0)             df[which(df$AGMAT==1),"AGESOURCE"]<-"OT"
      if (nrow(df[which(df$AGMAT==2),])>0)             df[which(df$AGMAT==2),"AGESOURCE"]<-"SC"
      if (nrow(df[which(df$AGMAT==4),])>0)             df[which(df$AGMAT==4),"AGESOURCE"]<-"VR"
      #no ICES codes for "rays", or "otolith AND scale"
      df$AGMAT <- NULL
      df$OTGRADING <- -9  #Grading of the otolith reading
      df$AGEPREPMET <- -9 #Age reading preparation method
      return(df)
    }
    # addPreStomSamp <-function(df=NULL){
    #   return(df)
    #   
    # }
    handleDetSpecies<-function(df = NULL){
      #herring were recorded in mm starting SUMMER 2016 - this converts cm to mm
      if (nrow(df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),])>0){
        df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),"FLEN"]<-
          df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),"FLEN"]*10
      }
      return(df)
    }
    
    df<-scratch_env$GSDET[,names(scratch_env$GSDET) %in% c("MISSION", "SETNO", "SPEC", "FSHNO", "FMAT", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS","AGE","SPECIMEN_ID")]
    # colnames(df)[colnames(df)=="SIZE_CLASS"] <- "CATIDENTIFIER"
    df <- addSexCodes(df)
    df <- addMaturity(df)
    df <- addAges(df)
    df <- handleDetSpecies(df)
    if (nrow(df[which(is.na(df$FWT)),])>0) df[which(is.na(df$FWT)),"FWT"]<-0
    df$FMAT <- NULL
    return(df)
  }
  handleSpecimens<- function(){    
    
    df<-scratch_env$GS_LV1_OBSERVATIONS[paste0(scratch_env$GS_LV1_OBSERVATIONS$MISSION,"_",
                                               scratch_env$GS_LV1_OBSERVATIONS$SETNO,"_",
                                               scratch_env$GS_LV1_OBSERVATIONS$SPEC,"_",
                                               scratch_env$GS_LV1_OBSERVATIONS$SIZE_CLASS) %in%
                                          paste0(GSDET$MISSION,"_",
                                                 GSDET$SETNO,"_",
                                                 GSDET$SPEC,"_",
                                                 GSDET$SIZE_CLASS),  c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "LV1_OBSERVATION")]
    if (nrow(df)==0)return(NULL)
    df$GENSAMP <- -9 #Flag whether genetic sample was taken
    
    if (nrow(df[grep(pattern = "Genetic",x = df$LV1_OBSERVATION,ignore.case = T),])>0) df[grep(pattern = "Genetic",x = df$LV1_OBSERVATION,ignore.case = T),"GENSAMP"]<-"Y"
    df$STOMSAMP <- -9 #Flag whether stomach sampling was performed
    
    if (nrow(df[grep(pattern = "Stomach Number",x = df$LV1_OBSERVATION,ignore.case = T),])>0) df[grep(pattern = "Stomach Number",x = df$LV1_OBSERVATION,ignore.case = T),"STOMSAMP"]<-"Y"
    df$PARSAMP <- -9 #Flag whether parasites sampling was performed    
    #colnames(df)[colnames(df)=="FSHNO"] <- "FISHID" #these are numbered per mission/set/species
    # colnames(df)[colnames(df)=="FWT"] <- "INDWGT"
    df$LV1_OBSERVATION<-NULL
    df = unique(df)
    return(df)
  }
  #make a df
  GSDET <- handleGSDET()
  
  GSspecimens <- handleSpecimens() 
  if (is.null(GSspecimens)){
    GSDET$GENSAMP <- NA
    GSDET$STOMSAMP <- NA
    GSDET$PARSAMP <- NA
  }else{
    GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$GENSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "GENSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
    GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$STOMSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "STOMSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
    GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$PARSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "PARSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
  }
  GSDET$AREATYPE <- 27
  
  # if GSDET$INDWGT[GSDET$INDWGT == 0] <- -9 #datras doesn't like values of 0 for wgt
  colnames(GSDET)[colnames(GSDET)=="SIZE_CLASS"] <- "CATIDENTIFIER"
  
  colnames(GSDET)[colnames(GSDET)=="MISSION"] <- "mission"
  colnames(GSDET)[colnames(GSDET)=="SETNO"] <- "STNO"
  
  forAgg <- unique(GSDET[,c("mission","STNO","SPEC","SEX","MATURITY", "AGERINGS", "FWT", "FLEN")])
  
  forAgg <- addLenMeasInfo(forAgg)
  
  forAgg <- addLNGTCLASS(forAgg)
  forAgg$FLEN <- NULL
  #if any have na for FWT, they should be zero or aggregate will drop them
  if (length(forAgg[is.na(forAgg$FWT),"FWT"])>0) forAgg[is.na(forAgg$FWT),"FWT"]<-0
  #Amount of fish (CANOATLNGT)and mean wt (INDWGT)at the given category (per haul, species, length class, sex, maturity, age)
  forAgg_cnt= unique(as.data.frame(as.list(stats::aggregate(
    x = list(CANOATLNGT = forAgg$mission),
    by = list(SPEC = forAgg$SPEC,
              mission = forAgg$mission,
              STNO = forAgg$STNO,
              LNGTCLASS = forAgg$LNGTCLASS, 
              LNGTCODE = forAgg$LNGTCODE,
              SEX = forAgg$SEX,
              MATURITY = forAgg$MATURITY,
              AGERINGS = forAgg$AGERINGS
    ),
    length
  ))))
  forAgg_indwgt= unique(as.data.frame(as.list(stats::aggregate(
    x = list(INDWGT = forAgg$FWT),
    by = list(SPEC = forAgg$SPEC,
              mission = forAgg$mission,
              STNO = forAgg$STNO,
              LNGTCLASS = forAgg$LNGTCLASS,
              LNGTCODE = forAgg$LNGTCODE,
              SEX = forAgg$SEX,
              MATURITY = forAgg$MATURITY,
              AGERINGS = forAgg$AGERINGS
    ),
    mean
  ))))
  #turn the zeroes back to -9
  if (length(forAgg_indwgt[forAgg_indwgt$INDWGT==0,"INDWGT"])>0) forAgg_indwgt[forAgg_indwgt$INDWGT==0,"INDWGT"]<- -9
  
  aggRes <- merge(forAgg_indwgt, forAgg_cnt)
  #all
  # CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
  #         "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
  #         "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
  #         "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
  #         "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
  #         "AGEPREPMET","OTGRADING","PARSAMP")
  #from HH/HL"mission","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","DEPTHSTRATUM"
  # CA <- c("SPECCODETYPE", "SPECCODE","AREATYPE","AREACODE","PLUSGR",
  #         "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
  #         "AGEPREPMET","OTGRADING","PARSAMP")
  
  #aggRes has "LNGTCODE","LNGTCLASS","SEX", "MATURITY","AGERINGS","CANOATLNGT","INDWGT",
  #GSspecimens has 
  
  # GSDET$SPECIMEN_ID<- GSDET$FSHNO<- GSDET$CLEN<- GSDET$DEVSTAGE<-GSDET$FLEN<-GSDET$AGE <- NULL
  aggRes$FISHID <- -9
  aggRes$RECORDTYPE<-"CA"
  cat("Done")
  return(aggRes)
}
