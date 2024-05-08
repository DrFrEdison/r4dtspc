# Drop zip ####
drop_zip <- function(path, log_or_spc, LG, filetype, delete_extract){
  setwd( path )
  zipfile = c(dir( pattern = ".zip$"), dir( pattern = ".7z$"))
  lapply(zipfile, function( x )  read.zip.spc_log(cd.dirp = path
                                                  , wd.dirp = wd
                                                  , zipfile = x
                                                  , LG = LG
                                                  , log_or_spc = log_or_spc
                                                  , filetype = filetype
                                                  , delete_extract = delete_extract))
}

# Drop log ####
drop_log <- function(path = dirname(rstudioapi::getSourceEditorContext()$path), recursive = T, type = c("spc", "ref", "drk"), SG = F){
  writelog <- list()

  writelog$spc <- c( dir( path = path, pattern = "M.log", recursive = recursive)
                     , dir( path = path, pattern = "R.log", recursive = recursive)
                     , dir( path = path, pattern = "D.log", recursive = recursive))

  writelog$type <- c(ifelse(length( grep("M.log", writelog$spc) > 0), "spc", NA)
                     , ifelse(length( grep("R.log", writelog$spc) > 0), "ref", NA)
                     , ifelse(length( grep("D.log", writelog$spc) > 0), "drk", NA))

  writelog$type <- writelog$type[ !is.na( writelog$type ) ]

  setwd(path)

  if( any( writelog$type %in% "ref") ) read_log(writelog$spc[ grep("R.log", writelog$spc) ], SG = SG, type = "ref")
  if( any( writelog$type %in% "drk") ) read_log(writelog$spc[ grep("D.log", writelog$spc) ], SG = SG, type = "drk")
  if( any( writelog$type %in% "spc") ) read_log(writelog$spc[ grep("M.log", writelog$spc) ], SG = SG, type = "spc")

}

