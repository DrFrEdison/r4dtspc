read.zip.spc_log <- function( cd.dirp
                              , wd.dirp
                              , zipfile
                              , LG = "LG"
                              , log_or_spc = c("log", "spc")
                              , filetype = c("spc", "ref", "drk")
                              , delete_extract = T){

  zipinfo <- list()

  zipinfo$wd <- cd.dirp
  setwd( zipinfo$wd )

  zipinfo$file  <- zipfile
  LG <- LG

  zipinfo$inside_file <- unzip(zipfile = zipinfo$file, list = TRUE)

  if( log_or_spc == "log"){
    zipinfo$inside_mod <- grep( ".log$", zipinfo$inside_file$Name, value = T)

    if( ! "spc" %in% filetype ) zipinfo$inside_mod <- grep("-M.log", zipinfo$inside_mod, value = T, invert = T)
    if( ! "ref" %in% filetype ) zipinfo$inside_mod <- grep("-R.log", zipinfo$inside_mod, value = T, invert = T)
    if( ! "drk" %in% filetype ) zipinfo$inside_mod <- grep("-D.log", zipinfo$inside_mod, value = T, invert = T)

  }

  if( log_or_spc == "spc"){
    zipinfo$inside_mod <- grep( "spc$", zipinfo$inside_file$Name, value = T)

    if( ! "spc" %in% filetype ) zipinfo$inside_mod <- grep("-M.spc", zipinfo$inside_mod, value = T, invert = T)
    if( ! "ref" %in% filetype ) zipinfo$inside_mod <- grep("-R.spc", zipinfo$inside_mod, value = T, invert = T)
    if( ! "drk" %in% filetype ) zipinfo$inside_mod <- grep("-D.spc", zipinfo$inside_mod, value = T, invert = T)

  }

  # Extraction folder
  dir.create( gsub(paste0(".", tools::file_ext(zipfile), "$"), "", zipfile) )

  setwd(wd.dirp$master)
  if( !dir( pattern = "7z.exe")[1] == "7z.exe" ) stop( "7z.exe is missing")

  if( log_or_spc == "log" & LG != "SG") suppressMessages(system(paste0("7z.exe e "
                  , cd.dirp, "/", zipfile
                  , " -o"
                  , paste(cd.dirp, gsub(paste0(".", tools::file_ext(zipfile), "$"), "", zipfile),sep="/")
                  , " *.log -r -aoa")
           , show.output.on.console = F))

  if( log_or_spc == "spc" & LG != "SG") suppressMessages(system(paste0("7z.exe e "
                                                          , cd.dirp, "/", zipfile
                                                          , " -o"
                                                          , paste(cd.dirp, gsub(paste0(".", tools::file_ext(zipfile), "$"), "", zipfile),sep="/")
                                                          , " *.spc -r -aoa")
                                                   , show.output.on.console = F))

  if( LG == "SG") untar(  tarfile = paste0( getwd(), "/", zipinfo$file)
                          , files = zipinfo$inside_mod)

  zipinfo$zipdir <- gsub("\\.", "", gsub(".7z", "", gsub("zip", "", zipinfo$file), zipinfo$file))

  setwd( zipinfo$wd )
  setwd( zipinfo$zipdir )

  SG <- ifelse(LG == "SG", T, F)
  if( log_or_spc == "spc") drop_spc( getwd(), T)
  if( log_or_spc == "log" & "spc" %in% filetype) drop_log( path = getwd(), recursive = T, type = "spc", SG = SG)
  if( log_or_spc == "log" & "ref" %in% filetype) drop_log( path = getwd(), recursive = T, type = "ref", SG = SG)
  if( log_or_spc == "log" & "drk" %in% filetype) drop_log( path = getwd(), recursive = T, type = "drk", SG = SG)

  file.copy( dir( pattern = ".csv" ), paste0(zipinfo$wd, "/", dir( pattern = ".csv" )))

  setwd( zipinfo$wd )

  if(delete_extract){
    unlink(zipinfo$zipdir, recursive = TRUE)
  }
}
