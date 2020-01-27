#given an HH record (i.e. tmp_HH) , get the catches
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=Sex&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=LngtClass&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=HLNOATLNGT&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=SUBWGT&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=SpecCodeType&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=SpecCode&SurveyID=3024
#http://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=SpecVal&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=SubFactor&SurveyID=3024
#https://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=CatIdentifier&SurveyID=3024

tmp_HH <- read.csv(file.path("c:","git","DATRAS-Canada-2019","R","exampleOutput","HH_Maritimes_20200114_1312.csv"))
# extract the data matching what's in the HH file
library(Mar.datawrangling)
library(lubridate)
get_data('rv', data.dir = data.dir)
GSINF <- GSINF[lubridate::year(GSINF$SDATE) %in% tmp_HH$YEAR 
               & lubridate::month(GSINF$SDATE) %in% tmp_HH$MONTH 
               & lubridate::day(GSINF$SDATE) %in% tmp_HH$DAY
               & GSINF$SETNO %in% tmp_HH$STNO,]
self_filter()
# also will need aphiaidsstored in the GSSPECIES_CODES table - merge to GSCAT
get_data_custom(schema="GROUNDFISH",data.dir = data.dir,tables = "GSSPECIES_CODES")
GSSPECIES_CODES = GSSPECIES_CODES[GSSPECIES_CODES$CODE %in% c(GSCAT$SPEC, GSDET$SPEC),]
# HL data requires stuff that is stored in 3 different tables locally - merge them
GSCAT<-GSCAT[,names(GSCAT) %in% c("MISSION", "SETNO", "SPEC", "SAMPWGT", "TOTWGT", "TOTNO")]
GSCAT = merge(GSCAT, GSSPECIES_CODES[,c("CODE","APHIAID")], by.x = "SPEC", by.y="CODE", all.x=T)
rm(GSSPECIES_CODES)
colnames(GSCAT)[colnames(GSCAT)=="APHIAID"] <- "SPECCODE"
GSDET<-GSDET[,names(GSDET) %in% c("MISSION", "SETNO", "SPEC", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS")]

# also will need data that's stored in the non-standard GSSPEC table
get_data_custom(schema="GROUNDFISH",data.dir = data.dir,tables = "GSSPEC")
GSSPEC<-GSSPEC[,names(GSSPEC) %in% c("SPEC", "LGRP", "LMIN", "LMAX")]
tmp_HL<- merge(GSDET, GSSPEC)
rm(GSSPEC)
# change to ICES sex codes
tmp_HL$SEX<-NA
tmp_HL[which(is.na(tmp_HL$FSEX)),"SEX"]<--9                 #undetermined
tmp_HL[which(tmp_HL$FSEX==0),"SEX"]<-"U"                    #unknown (0)
tmp_HL[which(tmp_HL$FSEX==1),"SEX"]<-"M"                    #male (1)
tmp_HL[which(tmp_HL$FSEX>1),"SEX"]<-"F"                     #female - both normal (2) and 'berried' (3)
# generate appropriate length group
tmp_HL$LNGTCLASS<- ceiling(tmp_HL$FLEN/tmp_HL$LGRP) * tmp_HL$LGRP
tmp_HL$LMIN<-tmp_HL$LMAX<-tmp_HL$FSEX<-tmp_HL$FLEN<-NULL
# for each sex/size class/species in a set, calculate the weight and count
tmp_HL<-unique(as.data.frame(as.list(aggregate(
  x = list(FWT = tmp_HL$FWT/1000),
  by = list(SPEC = tmp_HL$SPEC,
            MISSION = tmp_HL$MISSION,
            SETNO = tmp_HL$SETNO,
            SEX = tmp_HL$SEX,
            LNGTCLASS = tmp_HL$LNGTCLASS,
            LGRP = tmp_HL$LGRP,
            CATIDENTIFIER = tmp_HL$SIZE_CLASS
  ),
  FUN = function(x) c(CNT = round(length(x), 4), 
                      SUM = round(sum(x), 4))
))))
# rename the aggregation results
colnames(tmp_HL)[colnames(tmp_HL)=="FWT.CNT"] <- "HLNOATLNGT"
colnames(tmp_HL)[colnames(tmp_HL)=="FWT.SUM"] <- "CATCATCHWGT"
# add set-level catch information
tmp_HL=merge(tmp_HL, GSCAT, by = c("SPEC", "MISSION", "SETNO"), all.y=T)
tmp_HL$SPEC <-NULL
cleanup('rv')
colnames(tmp_HL)[colnames(tmp_HL)=="TOTNO"] <- "TOTALNO"
# LNGTCODE is how ICES describes length classes 1 = 1cm length class 
colnames(tmp_HL)[colnames(tmp_HL)=="LGRP"] <- "LNGTCODE"
# SUBWGT appears to match sampwgt
colnames(tmp_HL)[colnames(tmp_HL)=="SAMPWGT"] <- "SUBWGT"
# make subwgt -9 for all cases where no subsampling occurred
tmp_HL[tmp_HL$TOTWGT == tmp_HL$SUBWGT,"SUBWGT"]<--9
# Make CATCATCHWGT -9 in cases where the no weights exist
tmp_HL[is.na(tmp_HL$CATCATCHWGT),"CATCATCHWGT"]<--9
# joined GSCAT - may have a bunch of records with NA values for reqd fields
tmp_HL[is.na(tmp_HL$SEX),"SEX"]<--9
tmp_HL[is.na(tmp_HL$LNGTCLASS),"LNGTCLASS"]<--9
tmp_HL[is.na(tmp_HL$LNGTCODE),"LNGTCODE"]<--9
tmp_HL[is.na(tmp_HL$HLNOATLNGT),"HLNOATLNGT"]<--9
tmp_HL[tmp_HL$SUBWGT == 0,"SUBWGT"]<--9
tmp_HL$NOMEAS <- tmp_HL$HLNOATLNGT
tmp_HL$SUBFACTOR <- NA
tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"SUBFACTOR"]<-tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"TOTALNO"]/
                          tmp_HL[tmp_HL$TOTALNO > 0 & tmp_HL$NOMEAS > 0,"NOMEAS"]
tmp_HL[tmp_HL$TOTALNO == 0,"TOTALNO"]<--9
tmp_HL[tmp_HL$NOMEAS == -9 | tmp_HL$TOTALNO == -9,"SUBFACTOR"]<--9
tmp_HL[is.na(tmp_HL$CATIDENTIFIER) ,"CATIDENTIFIER"]<-1

tmp_HL$SPECCODETYPE <- "W" #worrms/aphiaid
tmp_HL[is.na(tmp_HL$SPECCODE),"SPECCODE"]<--9
tmp_HL$SPECVAL <- NA
tmp_HL$SPECVAL <- ifelse(tmp_HL$TOTALNO <= 0 & tmp_HL$TOTWGT >0 , 6, ifelse(tmp_HL$LNGTCLASS == -9, 7,  1))

mrg_HH <- tmp_HH[,c("mission","RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR")]
tmp_HL<-merge(mrg_HH, tmp_HL, all.y = T, by.x=c("mission", "STNO"), by.y=c("MISSION","SETNO"))
rm(mrg_HH)
timestamp<-format(Sys.time(), "%Y%m%d_%H%M")
write.csv(tmp_HL[,!names(tmp_HL) %in% "mission"] ,file = paste0("R/exampleOutput/HL_Maritimes_",timestamp,".csv"), row.names = F)