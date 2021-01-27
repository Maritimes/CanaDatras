round2 = function(x, n) {
  #this function ensures that values ending in 0.5 are round up to the next integer - not down to zero (R's default)
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

addLenMeasInfo <- function(df = NULL){
  #set LENMEASTYPE to standard length (2) for all, then overwrite weirdies
  #set LNGTCODE to 1cm for all, then overwrite weirdies
  df$LENMEASTYPE <- 2
  df$LNGTCODE <- 1
  
  #crabs -     7 "Carapace Width"; in mm
  df[(df$SPEC >= 2506 & df$SPEC <= 2547) | df$SPEC == 6006,c("LENMEASTYPE", "LNGTCODE")]<-data.frame(7,0.1)
  #lobsters -  6 "Carapace Length"; mm
  df[df$SPEC %in% c(2550,2551,2552,2553,8145),c("LENMEASTYPE", "LNGTCODE")]<-data.frame(6,0.1)
  #scallops -  9 "Shell Height"; mm
  df[df$SPEC %in% c(4320,4321,4322,4324,4325),c("LENMEASTYPE", "LNGTCODE")]<-data.frame(9,0.1)
  #squid -     5 "Mantle Length"
  df[df$SPEC %in% c(4511,4512,4514,4664),"LENMEASTYPE"]<-5 #and LNGTCODE is default (cm)
  #herring recorded in mm
  df[df$SPEC ==60 ,"LNGTCODE"]<-0.1
  return(df)
}

addLNGTCLASS <- function(df=NULL){
  df$LNGTCLASS <- NA
  df$LNGTCLASS<- ceiling(df$FLEN/df$LNGTCODE) * df$LNGTCODE
  return(df)
}