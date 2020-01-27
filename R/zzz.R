.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('CanaDatras')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
  # Mar.utils::updateCheck(gitPkg = 'Maritimes/CanaDatras')
}
