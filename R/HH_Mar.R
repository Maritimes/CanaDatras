#' @title HH_Mar
#' @description This function generates ICES DATRAS-compatible "HH" files 
#' directly from the Maritimes groundfish database. "HH" files contain Haul 
#' metadata 
#' @param scratch_env default is \code{NULL} This is an environment containing the results of a
#' Mar.datawrangling extraction - i.e. it contains all of the data necessary for HH, HL and CA
#' @return a df generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'  @export
HH_Mar <- function(scratch_env = NULL){
  cat("\n","Generating HH...")
  df= merge(scratch_env$GSINF, scratch_env$GSMISSIONS, all.x = T)
  df = merge(df, scratch_env$GSWARPOUT, all.x=T)
  #' drop unneeded fields, and make original cols lowercase - finals will be uppercase
  df = df[,c("MISSION","BOTTOM_SALINITY","BOTTOM_TEMPERATURE","SDATE",
             "DEPTH","DIST","GEAR","DUR","SETNO","TYPE","LATITUDE",
             "LONGITUDE", "ELATITUDE", "ELONGITUDE", "STRAT", "CURNT",
             "SURFACE_TEMPERATURE","TIME","WIND","FORCE","SPEED", "WARPOUT","DMIN","DMAX")]
  names(df) <- tolower(names(df))
  calcValues<-function(df=NULL){
    # Define some functions
    processTimes<-function(df=NULL){
      # Generate some additional values from our datetime -----------------------
      df$YEAR<-lubridate::year(df$sdate)
      df$MONTH<-lubridate::month(df$sdate)
      df$DAY<-lubridate::day(df$sdate)
      df$HOUR <- as.integer(substr(sprintf('%04d',df$time),1,2))
      df$MIN <- as.integer(substr(sprintf('%04d',df$time),3,4))
      df$DATETIME = lubridate::make_datetime(year = df$YEAR, month = df$MONTH, day = df$DAY, hour =df$HOUR, min = df$MIN, sec=0, tz = "Canada/Atlantic")
      df$QUARTER<-lubridate::quarter(df$DATETIME)
      df$TIMESHOT<-paste0(df$HOUR,df$MIN)
      df$sdate <- NULL
      df$time <- NULL
      df$HOUR <- NULL
      df$MIN <- NULL
      df_sp <- Mar.utils::df_to_sp(df = df,lat.field = "latitude", lon.field = "longitude")
      df_sp@data$SUNRISE <- maptools::sunriset(crds = df_sp,dateTime = df_sp@data$DATETIME,direction = "sunrise", POSIXct.out=TRUE)[,2]
      df_sp@data$SUNSET <- maptools::sunriset(crds = df_sp,dateTime = df_sp@data$DATETIME,direction = "sunset", POSIXct.out=TRUE)[,2]
      df<-df_sp@data
      rm(df_sp)
      df$DAYNIGHT<-NA
      df$DAYNIGHT<- ifelse(df$DATETIME>df$SUNRISE & df$DATETIME<df$SUNSET,"D","N")
      df$SUNSET <- df$SUNRISE <- df$DATETIME<- NULL
      return(df)
    }
    processTowDets<-function(df=NULL){
      getTowDir <- function(df=NULL){
        df[,"latitude"] <- round(df[,"latitude"],4)
        df[,"longitude"] <- round(df[,"longitude"],4)
        df[!is.na(df$elatitude),"elatitude"] <- round(df[!is.na(df$elatitude),"elatitude"],4)
        df[!is.na(df$elongitude),"elongitude"] <- round(df[!is.na(df$elongitude),"elongitude"],4)
        df$TOWDIR<-NA
        df[which(!is.na(df$latitude) & !is.na(df$longitude) &
                   !is.na(df$elatitude) &!is.na(df$elongitude)) ,"TOWDIR"] <- round(geosphere::bearingRhumb(p1 = df[which(!is.na(df$latitude) & !is.na(df$longitude) &
                                                                                                                            !is.na(df$elatitude) &!is.na(df$elongitude)) ,c("longitude","latitude")],
                                                                                                            p2 = df[which(!is.na(df$latitude) & !is.na(df$longitude) &
                                                                                                                            !is.na(df$elatitude) &!is.na(df$elongitude)) ,c("elongitude","elatitude")]),0)
        colnames(df)[colnames(df)=="latitude"] <- "SHOOTLAT"
        colnames(df)[colnames(df)=="longitude"] <- "SHOOTLONG"
        colnames(df)[colnames(df)=="elatitude"] <- "HAULLAT"
        colnames(df)[colnames(df)=="elongitude"] <- "HAULLONG"
        return(df)
      }
      getHaulDets<-function(df= NULL){
        # Decode TYPE to haulval  https://vocab.ices.dk/?ref=1
        df$HAULVAL<- NA
        df$HAULVAL <- ifelse(df$type == 1, "V", ifelse(df$type == 3, "I", NA))
        df$type <-NULL
        return(df)
      }
      df <- getTowDir(df)
      df <- getHaulDets(df)
      df$STATREC <- -9
      return(df)
    }
    processCurrents<-function(df=NULL){
      # CURNT/SurCurDir - transformation ----------------------------------------
      #' we record the direction of the current relative to the ship (eg to bow,
      #' to starboard, etc), but ICES wants it in a compass direction.
      #' I use the tow direction (determined above) combined with the
      #' value stored in GSINF.CURNT  to derive the value.
      #' For example, if the tow was found to be West to East (90deg), and the CURNT value was
      #' "to starboard", the combined angle would be 90+90 = 180 (or South)
      df$SURCURDIR<-NA
      df$SURCURDIR <- ifelse(df$curnt == 1, 0,
                             ifelse(df$curnt == 2, 90,
                                    ifelse(df$curnt == 3, -90,
                                           ifelse(df$curnt == 4, 180,
                                                  ifelse(df$curnt == 5, 999,
                                                         ifelse(df$curnt == 6, NA,
                                                                NA))))))
      df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"] <-
        df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"]+
        df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"TOWDIR"]
      #round determined SURCURDIR to nearest 90-deg
      df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"] <- 45*round(df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"]/45)
      df[!is.na(df$SURCURDIR) & df$SURCURDIR == 999,"SURCURDIR"] <- 0
      df[!is.na(df$SURCURDIR) & df$SURCURDIR<0,"SURCURDIR"]<-df[!is.na(df$SURCURDIR) & df$SURCURDIR<0,"SURCURDIR"]+360
      df[!is.na(df$SURCURDIR) & df$SURCURDIR>360 & df$SURCURDIR < 999,"SURCURDIR"]<-df[!is.na(df$SURCURDIR) & df$SURCURDIR>360 & df$SURCURDIR < 999,"SURCURDIR"]-360
      df$curnt <- NULL
      return(df)
    }
    processWind<-function(df=NULL){
      # Turn force column into WINDSPEED ----------------------------------------
      df$WINDSPEED<-NA
      # force holds a value of 1-8, each of which is decoded into to the lower bound of the beaufort scale range
      df$WINDSPEED <- ifelse(df$force == 0, 0,
                             ifelse(df$force == 1, 1,
                                    ifelse(df$force == 2, 4,
                                           ifelse(df$force == 3, 7,
                                                  ifelse(df$force == 4, 11,
                                                         ifelse(df$force == 5, 17,
                                                                ifelse(df$force == 6, 22,
                                                                       ifelse(df$force == 7, 28,
                                                                              ifelse(df$force == 8, 34,
                                                                                     NA)))))))))
      df$WINDSPEED = round(df$WINDSPEED*0.514444,0) #knots to  m/s
      df$force<-NULL
      colnames(df)[colnames(df)=="wind"] <- "WINDDIR"                         #dir in 360deg
      return(df)
    }
    df <- processTimes(df)
    df <- processTowDets(df)
    df <- processCurrents(df)
    df <- processWind(df)
    return(df)
  }
  addPlatformDets <- function(df=NULL){
    addGearDets<-function(df=NULL){
      # Add Gear details --------------------------------------------------------
      df$GEAR <- ifelse(df$gear == 3, "Y36", #Yankee 36
                        ifelse(df$gear == 9, "W2A", #Western 2A
                               ifelse(df$gear == 15, "US4S3B", #Yankee 36
                                      NA)))
      df$WINGSPREAD<- round(ifelse(df$gear == 3, 10.9728,                           #36ft in m
                                   ifelse(df$gear == 9, 12.4968,                           #41 ft in
                                          ifelse(df$gear==15, 13,                                 #provided in m - will be tow by tow in the future
                                                 NA))),2)
      #' I am awaiting direction on the values below from folks who are familiar with the gear
      #' In the meantime all of these become "-9"
      df$BUOYANCY<-df$DOORSPREAD <- df$DOORSURFACE<-df$DOORTYPE<-
        df$DOORWGT<-df$GEAREXP<-df$KITEDIM<-df$NETOPENING<-
        df$RIGGING<-df$TICKLER<-df$WARPDEN<-df$WARPDIA<-
        df$WGTGROUNDROPE<-df$SWEEPLNGT<--9
      # df$BUOYANCY<- ifelse(df$gear == 3, "<val for 3>",
      #                   ifelse(df$gear == 9, "<val for 9>",
      #                   ifelse(df$gear==15,"<val for 15>",
      #                   NA)))
      # df$DOORSPREAD <- ifelse(df$gear == 3, "<val for 3>",
      #                     ifelse(df$gear == 9, "<val for 9>",
      #                     ifelse(df$gear==15,"<val for 15>",
      #                     NA)))
      # df$DOORSURFACE<- ifelse(df$gear == 3, "<val for 3>",
      #                      ifelse(df$gear == 9, "<val for 9>",
      #                      ifelse(df$gear==15,"<val for 15>",
      #                      NA)))
      # df$DOORTYPE<-df$DOORTYPE<- ifelse(df$gear == 3, "<val for 3>",
      #                   ifelse(df$gear == 9, "<val for 9>",
      #                   ifelse(df$gear==15,"<val for 15>",
      #                   NA)))
      # df$DOORWGT<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$GEAREXP<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$KITEDIM<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$NETOPENING<- ifelse(df$gear == 3, "<val for 3>",
      #                     ifelse(df$gear == 9, "<val for 9>",
      #                     ifelse(df$gear==15,"<val for 15>",
      #                     NA)))
      # df$RIGGING<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$TICKLER<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$WARPDEN<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$WARPDIA<- ifelse(df$gear == 3, "<val for 3>",
      #                  ifelse(df$gear == 9, "<val for 9>",
      #                  ifelse(df$gear==15,"<val for 15>",
      #                  NA)))
      # df$WGTGROUNDROPE<- ifelse(df$gear == 3, "<val for 3>",
      #                        ifelse(df$gear == 9, "<val for 9>",
      #                        ifelse(df$gear==15,"<val for 15>",
      #                        NA)))
      # df$SWEEPLNGT<- ifelse(df$gear == 3, "<val for 3>",
      #                    ifelse(df$gear == 9, "<val for 9>",
      #                    ifelse(df$gear==15,"<val for 15>",
      #                    NA)))
      df$gear <- NULL #by the time we get here, we can delete this field
      return(df)
    }
    addShipDets<-function(df=NULL){
      df$SHIP <- substr(df$mission,1,3)                                       #grab ship from Mission
      df[df$SHIP == "NED","SHIP"]<-"18NE"
      df[df$SHIP == "TEM","SHIP"]<-"181C"
      df[df$SHIP == "ATC","SHIP"]<-"18AT"
      df[df$SHIP == "HAM","SHIP"]<-"18LH"
      df[df$SHIP == "TEL","SHIP"]<-"18TL"
      df[df$SHIP == "VEN","SHIP"]<-"188O"
      df[df$SHIP == "CARTIER","SHIP"]<- "NEWID1"
      df[df$SHIP == "PRINCE","SHIP"]<- "NEWID2"
      return(df)
    }
    df <- addGearDets(df)
    df <- addShipDets(df)
    return(df)
  }
  addICESStrata<-function(df=NULL){
    # decided we could use our own strata rather than the CAN1-4 DEPTHSTRATA
    colnames(df)[colnames(df)=="STRATUM"] <- "DEPTHSTRATUM"
    return(df)
  }
  addICESFields<-function(df=NULL){
    # Adopt ICES field names where possible -----------------------------------
    colnames(df)[colnames(df)=="bottom_salinity"] <- "BOTSAL"
    colnames(df)[colnames(df)=="bottom_temperature"] <- "BOTTEMP"           #temp already in Cel
    colnames(df)[colnames(df)=="dur"] <- "HAULDUR"                          #already in minutes
    colnames(df)[colnames(df)=="setno"] <- "STNO"
    df$HAULNO <- df$STNO                                                    #ICES has STNO and HAULNO - examples show them as same
    colnames(df)[colnames(df)=="strat"] <- "STRATUM"
    colnames(df)[colnames(df)=="surface_temperature"] <- "SURTEMP"
    # Populate other required fields ------------------------------------------
    df$HYDROSTNO <- -9
    df$SPEEDWATER <- -9
    df$SURCURSPEED <- -9
    df$BOTCURDIR <- -9
    df$BOTCURSPEED <- -9
    df$SWELLDIR <- -9
    df$SWELLHEIGHT <- -9
    df$SURSAL <- -9
    df$THERMOCLINE <- -9
    df$THCLINEDEPTH <- -9
    df$DATATYPE <- 'R'
    df$RECORDTYPE <- "HH"
    #df$SURVEY <- "CAN-MAR"    #make up a ficticious survey name
    df$COUNTRY <- "CAN"
    return(df)
  }
  convertUnits <- function(df=NULL){
    # Convert units as necessary ----------------------------------------------
    df[!is.na(df$warpout),"warpout"]<- round(df[!is.na(df$warpout),"warpout"] * 1.8288,0)          #depth from fathoms to meters (non NA)
    df[!is.na(df$depth),"depth"]<- round(df[!is.na(df$depth),"depth"] * 1.8288,0)          #depth from fathoms to meters (non NA)
    df[!is.na(df$dmin),"dmin"]<- round(df[!is.na(df$dmin),"dmin"] * 1.8288,0)          #dmin from fathoms to meters (non NA)
    df[!is.na(df$dmax),"dmax"]<- round(df[!is.na(df$dmax),"dmax"] * 1.8288,0)          #dmax from fathoms to meters (non NA)
    df[!is.na(df$dist),"dist"]<- round(df[!is.na(df$dist),"dist"] * 1852,0)        #distance from NM to meters (non NA)
    df[!is.na(df$speed),"speed"]<- round(df[!is.na(df$speed),"speed"],1)
    colnames(df)[colnames(df)=="warpout"] <- "WARPLNGT"                     #pretty sure it's in meters
    colnames(df)[colnames(df)=="depth"] <- "DEPTH"    
    colnames(df)[colnames(df)=="dmin"] <- "MINTRAWLDEPTH"
    colnames(df)[colnames(df)=="dmax"] <- "MAXTRAWLDEPTH"
    colnames(df)[colnames(df)=="dist"] <- "DISTANCE"
    colnames(df)[colnames(df)=="speed"] <- "GROUNDSPEED"
    return(df)
  }
  addSp <- function(df=NULL){
    df$BYCSPECRECCODE <- 1
    df[df$YEAR<2005,"BYCSPECRECCODE"]<-6
    df$STDSPECRECCODE <- 1                                                      #(https://vocab.ices.dk/?ref=89)                                                    
    #Maybe we can use this to store information related to the changes in species ID over time?
    return(df)
  }
  finalClean <- function(df=NULL){
    df = df[,c('RECORDTYPE','QUARTER','COUNTRY','SHIP','GEAR','SWEEPLNGT',
               'GEAREXP','DOORTYPE','STNO','HAULNO','YEAR','MONTH','DAY',
               'TIMESHOT','DEPTHSTRATUM','HAULDUR','DAYNIGHT','SHOOTLAT',
               'SHOOTLONG','HAULLAT','HAULLONG','STATREC','DEPTH','HAULVAL',
               'HYDROSTNO','STDSPECRECCODE','BYCSPECRECCODE','DATATYPE',
               'NETOPENING','RIGGING','TICKLER','DISTANCE','WARPLNGT',
               'WARPDIA','WARPDEN','DOORSURFACE','DOORWGT','DOORSPREAD',
               'WINGSPREAD','BUOYANCY','KITEDIM','WGTGROUNDROPE','TOWDIR',
               'GROUNDSPEED','SPEEDWATER','SURCURDIR','SURCURSPEED',
               'BOTCURDIR','BOTCURSPEED','WINDDIR','WINDSPEED','SWELLDIR',
               'SWELLHEIGHT','SURTEMP','BOTTEMP','SURSAL','BOTSAL',
               'THERMOCLINE','THCLINEDEPTH','MINTRAWLDEPTH','MAXTRAWLDEPTH','mission')]
    df[is.na(df)]<- -9
    return(df)
  }
  # Get all of the requested data
  tmp_HH <- calcValues(df)
  tmp_HH <- addPlatformDets(tmp_HH)
  tmp_HH <- addICESFields(tmp_HH)
  tmp_HH <- addICESStrata(tmp_HH)
  tmp_HH <- convertUnits(tmp_HH)
  tmp_HH <- addSp(tmp_HH)
  tmp_HH <- finalClean(tmp_HH)
  
  cat("Done")
  return(tmp_HH)
}
