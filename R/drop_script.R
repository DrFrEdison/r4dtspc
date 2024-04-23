# Drop txt ####
drop_txt <- function( path, recursive = F, wl = 190:598){

  dt <- list()
  dt$wd <- path
  setwd( dt$wd )

  dt$files <- list.files( pattern = "\\.txt$", recursive = recursive)

  dt$raw <- lapply( dt$files, function( x ) fread( x, header = F, sep = ";"))

  dt$raw <- lapply( dt$raw, function( x ) gsub("\\{", "", x))
  dt$raw <- lapply( dt$raw, function( x ) gsub("\\}", "", x))
  dt$raw <- lapply( dt$raw, function( x ) gsub("\\,", ".", x))
  dt$raw <- lapply( dt$raw, as.numeric)

  dt$trs <- do.call( rbind, dt$raw)
  colnames( dt$trs ) <- wl

  dt$name$date <- substr(dt$files, 3, 8)
  dt$name$time <- substr(dt$files, 10, 15)
  dt$name$type <- substr( dt$files
                          , lapply(gregexpr("_", dt$files), function( x ) x[[ 3 ]] + 1)
                          , lapply(gregexpr("\\.", dt$files), function( x ) x[[ 1 ]] - 1))

  dt$export <- data.frame( ID = dt$files
                           , datetime = paste(dt$name$date, dt$name$time)
                           , date = dt$name$date
                           , time = dt$name$time
                           , dt$trs)

  write.csv2( dt$export, paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_Spektren.csv"), row.names = F)
}

# Drop zip ####
drop_zip <- function(path, log_or_spc, LG, filetype, delete_extract){
  setwd( path )
  zipfile = c(dir( pattern = ".zip$"), dir( pattern = ".7z$"))
  lapply(zipfile, function( x )  read.zip.spc_log( wd = path
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

