#' @title HL_Mar
#' @description This function generates ICES DATRAS-compatible HL files directly from the Maritimes
#' groundfish database.
#' @param scratch_env default is \code{NULL} This is an environment containing the results of a
#' Mar.datawrangling extraction - i.e. it contains all of the data necessary for HH, HL and CA
#' @return a df generated HL file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
HL_Mar <- function(scratch_env = NULL){
  # Our "std spp" are:
  # c(10, 11, 12, 14, 16, 23, 30, 40, 41, 42, 43, 60, 70)
  # Our known 67 spp since 1970  are:
  # c(10,11,12,13,14,15,16,17,23,30,31,40,41,42,43,44,50,51,52,60,61,62,64,70,94,112,114,118,122,
  #   123,141,142,143,160,200,201,202,203,204,216,220,221,240,241,300,304,320,350,400,409,410,411,
  #   412,413,414,501,502,610,622,623,630,637,640,647,701,704,2550
  cat("\n","Generating HL... ")

  aggByLenSexGroups <- function(df = NULL){
    # generate appropriate length group
    df$LNGTCLASS<- ceiling(df$FLEN/df$LNGTCODE) * df$LNGTCODE
    df$FLEN<-NULL
    df[is.na(df$FWT),"FWT"]<-0
    # for each sex/size class/species in a set, calculate the weight and count
    df<-unique(as.data.frame(as.list(stats::aggregate(
      x = list(FWT = df$FWT/1000),
      by = list(SPEC = df$SPEC,
                LNGTCODE = df$LNGTCODE,
                MISSION = df$MISSION,
                SETNO = df$SETNO,
                SEX = df$SEX,
                LNGTCLASS = df$LNGTCLASS,
                CATIDENTIFIER = df$CATIDENTIFIER
      ),
      FUN = function(x) c(CNT = round(length(x), 4),
                          SUM = round(sum(x), 4))
    ))))
    # rename the aggregation results
    colnames(df)[colnames(df)=="FWT.CNT"] <- "HLNOATLNGT"
    colnames(df)[colnames(df)=="FWT.SUM"] <- "CATCATCHWGT"
    return(df)
  }
  # add set-level catch information
  #   tmp_HL=merge(tmp_HL, GSCAT, by = c("SPEC", "MISSION", "SETNO"), all.y=T)
  #   tmp_HL$LENMEASTYPE <- NA
  #   tmp_HL$SPEC <-NULL
  #   colnames(tmp_HL)[colnames(tmp_HL)=="TOTNO"] <- "TOTALNO"
  #   # LNGTCODE is how ICES describes length classes 1 = 1cm length class
  #   colnames(tmp_HL)[colnames(tmp_HL)=="LGRP"] <- "LNGTCODE"
  #
  #   # Make CATCATCHWGT -9 in cases where the no weights exist
  #   tmp_HL[is.na(tmp_HL$CATCATCHWGT),"CATCATCHWGT"]<--9
  #   # joined GSCAT - may have a bunch of records with NA values for reqd fields
  #   tmp_HL[is.na(tmp_HL$SEX),"SEX"]<--9
  #   tmp_HL[is.na(tmp_HL$LNGTCLASS),"LNGTCLASS"]<--9
  #   tmp_HL[is.na(tmp_HL$LNGTCODE),"LNGTCODE"]<--9
  #   tmp_HL[is.na(tmp_HL$HLNOATLNGT),"HLNOATLNGT"]<--9
  #   tmp_HL[tmp_HL$SUBWGT == 0,"SUBWGT"]<--9
  #   tmp_HL$NOMEAS <- tmp_HL$HLNOATLNGT
  #   tmp_HL$SUBFACTOR <- NA
  #   tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"SUBFACTOR"]<-tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"TOTALNO"]/
  #     tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"NOMEAS"]
  #   tmp_HL[tmp_HL$TOTALNO == 0,"TOTALNO"]<--9
  #   tmp_HL[tmp_HL$NOMEAS == -9 | tmp_HL$TOTALNO == -9,"SUBFACTOR"]<--9
  #   tmp_HL[is.na(tmp_HL$CATIDENTIFIER) ,"CATIDENTIFIER"]<-1
  #
  #   tmp_HL$SPECCODETYPE <- "W" #worrms/aphiaid
  #   tmp_HL[is.na(tmp_HL$SPECCODE),"SPECCODE"]<--9

  #tmp_HL <- addSpCodes()
  handleGSCAT<-function(){
    df<-scratch_env$GSCAT
    doSubFact <- function(df = NULL){
      df$SUBFACTOR <- -9
      df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SUBFACTOR"] <- df[df$TOTWGT > 0 & df$SAMPWGT > 0,"TOTWGT"]/df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SAMPWGT"]
      return(df)
    }
    #rename some fields to matche ICES
    df = doSubFact(df)
    colnames(df)[colnames(df)=="TOTNO"] <- "TOTALNO"
    colnames(df)[colnames(df)=="SAMPWGT"] <- "SUBWGT"
    #   # make subwgt -9 for all cases where no subsampling occurred
    if (nrow(df[df$TOTWGT == df$SUBWGT,])>0) df[df$TOTWGT == df$SUBWGT,"SUBWGT"]<--9
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
    # addDevStage<-function(df = NULL){
    #   df$DEVSTAGE<- -9
    #   # if (nrow(df[which(is.na(df$FMAT)),])>0)         df[which(is.na(df$FMAT)),"DEVSTAGE"]<--9
    #   # if (nrow(df[which(df$FMAT==0),])>0)             df[which(df$FMAT==0),"DEVSTAGE"]<--9 #DClark indicates that 0 should be NULL
    #   # if (nrow(df[which(df$FMAT==9),])>0)             df[which(df$FMAT==9),"DEVSTAGE"]<--9 #DClark indicates that 9 is probably an error
    #   # if (nrow(df[which(df$FMAT %in% c(1)),])>0)      df[which(df$FMAT %in% c(1)),"DEVSTAGE"]<-61
    #   # if (nrow(df[which(df$FMAT %in% c(2,3,4)),])>0)  df[which(df$FMAT %in% c(2,3,4)),"DEVSTAGE"]<-62
    #   # if (nrow(df[which(df$FMAT %in% c(5)),])>0)      df[which(df$FMAT %in% c(5)),"DEVSTAGE"]<-63
    #   # if (nrow(df[which(df$FMAT %in% c(6)),])>0)      df[which(df$FMAT %in% c(6)),"DEVSTAGE"]<-64
    #   # if (nrow(df[which(df$FMAT %in% c(7,8)),])>0)   df[which(df$FMAT %in% c(7,8)),"DEVSTAGE"]<-65
    #   df$FMAT <- NULL
    #   return(df)
    # }
    handleDetSpecies<-function(df = NULL){
      #herring were recorded in mm starting SUMMER 2016 - this converts to cm (same units as LGRP)
      #MISSION reqd to avoid Spring surveys, where they were recorded in cm
      if (nrow(df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) >2015 & (!df$MISSION %in% c("TEL2016002","TEL2016003")))),])>0)
        df[df$SPEC == 60 & (substr(df$MISSION, 4,7) >2015 & (!df$MISSION %in% c("TEL2016002","TEL2016003"))),"FLEN"] <-
          df[df$SPEC == 60 & (substr(df$MISSION, 4,7) >2015 & (!df$MISSION %in% c("TEL2016002","TEL2016003"))),"FLEN"]/10
      return(df)
    }

    df<-scratch_env$GSDET[,names(scratch_env$GSDET) %in% c("MISSION", "SETNO", "SPEC", "FMAT", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS")]
    colnames(df)[colnames(df)=="SIZE_CLASS"] <- "CATIDENTIFIER"
    df <- addSexCodes(df)
    # df <- addDevStage(df)
    df <- handleDetSpecies(df)
    if (nrow(df[which(is.na(df$FWT)),])>0) df[which(is.na(df$FWT)),"FWT"]<-0
    df$FMAT <- NULL
    return(df)
  }
  handleSpecies<-function(){
    addLenMeasType <- function(df = NULL){
      #Make everything standard length (2) then overwrite
      df$LENMEASTYPE <- 2
      #crabs -     7 "Carapace Width"
      df[df$CODE >= 2506 & df$CODE <= 2547,"LENMEASTYPE"]<-7
      df[df$CODE == 6006,"LENMEASTYPE"]<-7
      #lobsters -  6 "Carapace Length"
      df[df$CODE %in% c(2550,2551,2552,2553,8145),"LENMEASTYPE"]<-6
      #squid -     5 "Mantle Length"
      df[df$CODE %in% c(4511,4512,4514,4664),"LENMEASTYPE"]<-5
      #scallops -  9 "Shell Height"
      df[df$CODE %in% c(4320,4321,4322,4324,4325),"LENMEASTYPE"]<-9
      return(df)
    }
    #get all of the species - the CODE, APHIAID, LGRP
    SPP <- sort(unique(c(unique(scratch_env$GSCAT$SPEC), unique(scratch_env$GSDET$SPEC))))
    SPP <- data.frame(SPEC = SPP)
    GSSPECIES_CODES <- scratch_env$GSSPECIES_CODES[scratch_env$GSSPECIES_CODES$CODE %in% SPP$SPEC,c("CODE","APHIAID")]
    colnames(GSSPECIES_CODES)[colnames(GSSPECIES_CODES)=="APHIAID"] <- "SPECCODE"
    GSSPECIES_CODES$SPECCODETYPE <- -9
    GSSPECIES_CODES[!is.na(GSSPECIES_CODES$SPECCODE),"SPECCODETYPE"]<- "W"
    GSSPEC <- scratch_env$GSSPEC[scratch_env$GSSPEC$SPEC %in% SPP$SPEC,c("SPEC", "LGRP")]
    colnames(GSSPEC)[colnames(GSSPEC)=="LGRP"] <- "LNGTCODE"
    SPP <- merge(SPP, GSSPECIES_CODES, all.x = T, by.x= "SPEC", by.y="CODE")
    SPP <- merge(SPP, GSSPEC, all.x = T)
    SPP <- addLenMeasType(SPP)
  }
  getNoMeas <- function(df=NULL){
    df<-unique(as.data.frame(as.list(stats::aggregate(
      x = list(NOMEAS = df$HLNOATLNGT),
      by = list(SPEC = df$SPEC,
                MISSION = df$MISSION,
                SETNO = df$SETNO,
                CATIDENTIFIER = df$CATIDENTIFIER,
                SEX = df$SEX
      ),
      sum
    ))))
    return(df)
  }
  GSDET <- handleGSDET()
  GSCAT <- handleGSCAT()
  SPP <- handleSpecies()
  #subfactor - maybe this needs to be determined first?
  forAgg <- merge(SPP[,c("SPEC", "LNGTCODE")], GSDET[,c("SPEC", "MISSION","SETNO", "FLEN", "FWT", "SEX","CATIDENTIFIER")], by = "SPEC", all.y = T )
  forAgg <- aggByLenSexGroups(forAgg)
  forAgg <- forAgg[,c("SPEC", "MISSION","SETNO","SEX","LNGTCLASS","CATIDENTIFIER","HLNOATLNGT", "CATCATCHWGT")]
  forAgg$DEVSTAGE <- -9 #devstage (i.e. maturity) not used at a haul-level (i.e. GSCAT)

  forAgg = merge(forAgg, GSCAT, all.x = T)
  noMeas <- getNoMeas(forAgg[,c("SPEC", "MISSION", "SETNO", "SEX", "HLNOATLNGT", "CATIDENTIFIER")])
  forAgg = merge(forAgg, noMeas)
  forAgg$SPECVAL <- NA
  forAgg$SPECVAL <- ifelse(forAgg$TOTALNO <= 0 & forAgg$TOTWGT >0 , 6, ifelse(forAgg$LNGTCLASS == -9, 7,  1))
  forAgg <- merge(forAgg, SPP)
  forAgg$TOTWGT<-forAgg$SPEC<-NULL

  forAgg[is.na(forAgg)]<- -9 #catch all to turn all NAs to -9
  cat("Done")
  return(forAgg)
}
