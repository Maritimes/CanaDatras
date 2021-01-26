library(Mar.datawrangling)
get_data_custom("GROUNDFISH", data.dir ="c:/git/wrangledData/", tables = "GSSPECIES_CODES", usepkg = "roracle", fn.oracle.username = "mcmahonm", fn.oracle.password = "KimoIsG00d", fn.oracle.dsn = "PTRAN")



get_data('rv', data.dir ="c:/git/wrangledData/")
GSMISSIONS = GSMISSIONS[GSMISSIONS$SEASON =="SUMMER" & GSMISSIONS$YEAR ==2020,]
GSINF = GSINF[GSINF$SETNO %in% c(106),]
self_filter(keep_nullsets = F)
GSSPECIES_CODES= GSSPECIES_CODES[GSSPECIES_CODES$CODE %in% GSCAT$SPEC,]

GSSPECIES =   GSSPECIES[!(GSSPECIES$CODE %in% GSSPECIES_CODES$CODE),]
