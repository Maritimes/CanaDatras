# I relied heavily on http://www.ices.dk/marine-data/Documents/DATRAS/DATRAS_dataproducts_units.pdf
# for details about the various fields (specifically, the links on pgs 2-4)
# These are all the required HH fields ------------------------------------
# allHH = c('RECORDTYPE','QUARTER','COUNTRY','SHIP','GEAR','SWEEPLNGT',
#           'GEAREXP','DOORTYPE','STNO','HAULNO','YEAR','MONTH','DAY',
#           'TIMESHOT','STRATUM','HAULDUR','DAYNIGHT','SHOOTLAT',
#           'SHOOTLONG','HAULLAT','HAULLONG','STATREC','DEPTH','HAULVAL',
#           'HYDROSTNO','STDSPECRECCODE','BYCSPECRECCODE','DATATYPE',
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
