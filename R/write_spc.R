write_spc_files <- function(spc_file, baseline = F, write = F, filename = "export", tzp = "UTC", path = getwd(), return_R = T){

  op <- options(digits.secs=3)
  setwd( path )
  if(baseline == F) spctoadd <- do.call(plyr::rbind.fill, lapply(lapply(spc_file$spc, function(x) plyr::rbind.fill(x)), function(x) data.frame(t(x))))
  if(baseline == T) spctoadd <- do.call(plyr::rbind.fill, lapply(lapply(spc_file$spc_baseline, function(x) plyr::rbind.fill(x)), function(x) data.frame(t(x))))

  for(i in sort(as.numeric(gsub("X", "", colnames(spctoadd)))))
    spctoadd <- spctoadd[ , moveme(colnames(spctoadd), paste0(colnames(spctoadd)[grep(i, colnames(spctoadd))], " last"))]

  require(lubridate)

  Iterations <- lapply(spc_file$data, function(x) unique(as.numeric(as.character(gsub(",", ".", x$It))) / 1000))

  if(length(grep("Interval", names(spc_file$data[[1]]))) > 0)  Interval <- lapply(spc_file$data, function(x) as.character(x$Interval))
  if(length(grep("Interval", names(spc_file$data[[1]]))) == 0)  Interval <- 0

  if(unique(substr(Interval[[1]], nchar(Interval[[1]]), nchar(Interval[[1]]))) == "s") Interval <- lapply(Interval, function(x) as.numeric(gsub(",", ".", substr(x, 1, nchar(x) - 1))))

  Interval <- mapply(function(x, y) x + y
                     , x = Interval
                     , y = Iterations)

  Interval <- lapply(Interval, function(x) cumsum(x) - x[1])

  datetime <- lapply(spc_file$data, function(x) as.POSIXct(as.character(x$fdate), tz = tzp))

  for(i in 1 : length( spc_file$files)){
  if(spc_file$data[[ i ]]$fztype[ 1 ] != "t/s" | is.na( spc_file$data[[ i ]]$fztype[ 1 ] )) next

  spc_file$recorded <- spc_file$data[[ i ]][ 1, c(grep( "Originally.recorded", names( spc_file$data[[ i ]] ) ) ) ]
  spc_file$spline <- spc_file$data[[ i ]][ 1, c(grep( "Interpolate.Spline", names( spc_file$data[[ i ]] ) ) ) ]

  spc_file$recorded <- substr( spc_file$recorded, 1, gregexpr("\\|", spc_file$recorded )[[ 1 ]][ 1 ] - 1)
  spc_file$spline <- substr( spc_file$spline, 1, gregexpr("\\|", spc_file$spline )[[ 1 ]][ 1 ] - 1)

  spc_file$recorded <- gsub("\\,", "\\.", spc_file$recorded)
  spc_file$spline <- gsub("\\,", "\\.", spc_file$spline)

  spc_file$timediff <- as.numeric( difftime(file.info( basename( spc_file$data[[ i ]]$NAME[ 1 ] ) )$mtime
                                            , as.POSIXct( spc_file$spline )
                                            , units = "s")) / spc_file$data[[ i ]]$fnsub[ 1 ]

  datetime[[ i ]] <-  seq(as.POSIXct(spc_file$spline, format = "%Y-%m-%d %H:%M:%OS")
                          , file.info( basename( spc_file$data[[ i ]]$NAME[ 1 ] ) )$mtime
                          , len = spc_file$data[[ i ]]$fnsub[ 1 ])
  }

  any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s")

  if( exists("Interval")) if( !any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) if(sum(unique(unlist(Interval)), na.rm = T) != 0)  datetime <-   mapply(function(x, y) x + y
                                                           , x = datetime
                                                           , y = Interval)

  if( !any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) datetime <- as.POSIXct(unlist(lapply(datetime, as.character)), tz = tzp, format = "%Y-%m-%d %H:%M:%S")
  if( any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) datetime <- as.POSIXct(unlist(lapply(datetime, as.character)), tz = tzp, format = "%Y-%m-%d %H:%M:%OS")

  date <- as.Date(datetime, tz = tzp)
  if( !any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) time = strftime(with_tz(datetime, tzone = tzp), format = "%H:%M:%S", tz = tzp)
  if( any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) time = strftime(with_tz(datetime, tzone = tzp), format = "%H:%M:%OS", tz = tzp)

  Iterations = unlist(lapply(spc_file$data, function(x) x$It))
  Average = unlist(lapply(spc_file$data, function(x) x$Aver))
  filenamep = unlist(lapply(spc_file$data, function(x) as.character(basename(as.character(x$filename)))))

  if(is.null(datetime)) datetime <- NA
  if(is.null(date)) date <- NA
  if(is.null(time)) time <- NA
  if(is.null(Iterations)) Iterations <- NA
  if(is.null(Average)) Average <- NA
  if(is.null(filename)) filenamep <- NA

  toexport <- data.frame(datetime = as.character(datetime), date, time, Iterations, Average, filenamep, spctoadd)

  toexport$Iterations <- as.numeric(as.character(gsub(",",".",toexport$Iterations)))
  toexport$Average <- as.numeric(as.character(gsub(",",".",toexport$Average)))

  suppressWarnings(
    names(toexport)[which(!is.na(as.numeric(gsub("X", "", names(toexport)))))] <-
      wl <- gsub("X","",names(toexport)[which(!is.na(as.numeric(gsub("X", "", names(toexport)))))])
  )
  wl <- as.numeric(wl)
  toexport <- toexport[order(toexport$datetime) , ]

  difftime <- cumsum(c(0, diff( as.POSIXct(toexport$datetime, tz = tzp, format = "%Y-%m-%d %H:%M:%OS"))))
  difftimeraw <- c(0, diff( as.POSIXct(toexport$datetime, tz = tzp, format = "%Y-%m-%d %H:%M:%OS")))

  if(sum(unique(unlist(Interval)), na.rm = T) != 0)  toexport <- cbind(toexport, difftime_sec = difftimeraw, difftime_cum = difftime)
  if( exists("Interval")) if( !any(unlist(lapply(spc_file$data, function( x ) unique( x$fztype))) == "t/s") ) toexport <- cbind(toexport, difftime_sec = difftimeraw, difftime_cum = difftime)

  toexport <- toexport[ , moveme(names(toexport), "Iterations Average filenamep last")]
  for(i in wl) toexport <- toexport[ , moveme(names(toexport), paste0(i, " last"))]

  if(write == T) fwrite(x = toexport, file = paste0(path,"/", filename,".csv"), sep = ";", dec = ",", row.names = F)

  if(return_R ==T) return(toexport)
}


