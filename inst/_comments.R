
# I relied heavily on http://www.ices.dk/marine-data/Documents/DATRAS/DATRAS_dataproducts_units.pdf
# for details about the various fields (specifically, the links on pgs 2-4)
# These are all the required HH fields ------------------------------------
# allHH = c('RECORDTYPE','QUARTER','COUNTRY','SHIP','GEAR','SWEEPLNGT',
#           'GEAREXP','DOORTYPE','STNO','HAULNO','YEAR','MONTH','DAY',
#           'TIMESHOT','STRATUM','HAULDUR','DAYNIGHT','SHOOTLAT',
#           'SHOOTLONG','HAULLAT','HAULLONG','STATREC','DEPTH','HAULVAL',
#           'HYDROSTNO','STDSPECRECCODE','BYSPECRECCODE','DATATYPE',
#           'NETOPENING','RIGGING','TICKLER','DISTANCE','WARPLNGT',
#           'WARPDIA','WARPDEN','DOORSURFACE','DOORWGT','DOORSPREAD',
#           'WINGSPREAD','BUOYANCY','KITEDIM','WGTGROUNDROPE','TOWDIR',
#           'GROUNDSPEED','SPEEDWATER','SURCURDIR','SURCURSPEED',
#           'BOTCURDIR','BOTCURSPEED','WINDDIR','WINDSPEED','SWELLDIR',
#           'SWELLHEIGHT','SURTEMP','BOTTEMP','SURSAL','BOTSAL',
#           'THERMOCLINE','THCLINEDEPTH')

# Get some data to process ------------------------------------------------
#library(devtools)
#install_github(('Maritimes/Mar.datawrangling')
#if you don't have permission to access the Maritimes RV data, contact Mike McMahon,
#and provide him with your Oracle user account

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
