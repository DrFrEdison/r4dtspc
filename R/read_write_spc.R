read_spc <- function( working_directory = getwd(), spc.write = T, return.R = T, recursive = F){

  setwd( working_directory )
  read.spc <- list()

  # List all .spc files in dir ####
  read.spc$file$name <- dir(path = working_directory, pattern = "\\.spc$", recursive = recursive)

  # file size ####
  read.spc$file$info <- lapply(read.spc$file$name, file.info)
  read.spc$file$size <- unlist( lapply(read.spc$file$info, function( x ) x$size))

  # read binary data from .spc files ####
  # From binary data the date/time when the file was originally recorded, the Channel Type and Normalization way can be read
  for(i in 1 : length( read.spc$file$name))
    read.spc$file$Bin[[ i ]] <- readBin(read.spc$file$name[ i ]
                                        , "hex"
                                        , round(read.spc$file$size , 0)[ i ])

  # change to character and apply encoding UTF-8 to avoid issues with German Umlaut etc.
  read.spc$file$Bin <- lapply(read.spc$file$Bin, as.character)
  read.spc$file$Bin <- lapply(read.spc$file$Bin, function( x ) iconv(x, to = "UTF-8", sub = "byte"))

  # get date when the .spc file was originally recorded
  read.spc$bin$Originally.recorded <- lapply(read.spc$file$Bin, function( x ) x[ grep("Originally recorded", x)] )
  read.spc$bin$Originally.recorded <- substr(read.spc$bin$Originally.recorded
                                             , unlist( gregexpr("Originally.recorded", read.spc$bin$Originally.recorded)) - 24
                                             , unlist( gregexpr("Originally.recorded", read.spc$bin$Originally.recorded)) - 6)

  # get date when the .spc file was RECORDED (seems to be a very rare event, found e.g. in Linse1002.spc from 2013)
  if( any( unlist( lapply(read.spc$bin$Originally.recorded, nchar)) < 1)){

    read.spc$bin$RECORDED <- lapply(read.spc$file$Bin, function( x ) x[ grep("RECORDED", x)] )

    if(any( unlist(lapply(read.spc$bin$RECORDED, length)) > 0)){

      read.spc$bin$RECORDED <- substr(read.spc$bin$RECORDED
                                      , unlist( gregexpr("RECORDED", read.spc$bin$RECORDED)) + nchar( "RECORDED" )
                                      , unlist( gregexpr("RECORDED", read.spc$bin$RECORDED)) + nchar( "RECORDED" ) + 34)

      read.spc$bin$RECORDED <- as.character( as.POSIXct( paste(as.character( substr(read.spc$bin$RECORDED
                                                                                    , unlist(gregexpr("Date", read.spc$bin$RECORDED)) + 5
                                                                                    , unlist(gregexpr("Date", read.spc$bin$RECORDED)) + 14))
                                                               , as.character( substr(read.spc$bin$RECORDED
                                                                                      , unlist(gregexpr("Time", read.spc$bin$RECORDED)) + 5
                                                                                      , unlist(gregexpr("Time", read.spc$bin$RECORDED)) + 12)))
                                                         , format = "%d.%m.%Y %H:%M:%S", tz = "UTC"))

    }


    read.spc$bin$Originally.recorded[ nchar(read.spc$bin$Originally.recorded) < 19 | is.na(read.spc$bin$Originally.recorded) ] <-
      read.spc$bin$RECORDED[ nchar(read.spc$bin$Originally.recorded) < 19 | is.na(read.spc$bin$Originally.recorded) ]
  }

  # get Channel Type (i.e. Normalization, Reference or Dark)
  read.spc$bin$ChannelType <- lapply(read.spc$file$Bin, function( x ) x[ grep("ChannelType", x)] )
  read.spc$bin$ChannelType <- substr(read.spc$bin$ChannelType
                                     , gregexpr("ChannelType", read.spc$bin$ChannelType)
                                     , nchar( read.spc$bin$ChannelType ))

  read.spc$bin$ChannelType <- substr(read.spc$bin$ChannelType
                                     , nchar("ChannelType") + 2
                                     , unlist(lapply(gregexpr("\r", read.spc$bin$ChannelType), function( x ) x[[ 1 ]])) - 1)

  # get Channel Type (i.e. Normalization, Reference or Dark)
  read.spc$bin$ChannelName <- lapply(read.spc$file$Bin, function( x ) x[ grep("ChannelName", x)] )
  read.spc$bin$ChannelName <- substr(read.spc$bin$ChannelName
                                     , gregexpr("ChannelName", read.spc$bin$ChannelName)
                                     , nchar( read.spc$bin$ChannelName ))

  read.spc$bin$ChannelName <- substr(read.spc$bin$ChannelName
                                     , nchar("ChannelName") + 2
                                     , unlist(lapply(gregexpr("\r", read.spc$bin$ChannelName), function( x ) x[[ 1 ]])) - 1)

  # get Normalization (i.e. Transmission or Absorbance)
  read.spc$bin$Normalization <- lapply(read.spc$file$Bin, function( x ) x[ grep("Normalization", x)] )
  read.spc$bin$Normalization <- substr(read.spc$bin$Normalization
                                       , unlist( gregexpr("Normalization", read.spc$bin$Normalization)) + nchar( "Normalization" ) + 1
                                       , nchar( read.spc$bin$Normalization ))

  # Either | or \r comes after normalization method
  minp <- apply(data.frame(unlist( lapply(gregexpr("\r", read.spc$bin$Normalization), function( x ) x[[ 1 ]])) - 1
                           , unlist( lapply(gregexpr("\\|", read.spc$bin$Normalization), function( x ) x[[ 1 ]])) - 1)
                , 1
                , min)

  read.spc$bin$Normalization <- substr(read.spc$bin$Normalization
                                       , 1
                                       , minp)

  # Unify Channel / Normalization ####
  read.spc$bin$spc.type <- apply(data.frame( read.spc$bin$ChannelType, read.spc$bin$ChannelName, read.spc$bin$Normalization )
                                 , 1
                                 , function( x ) as.character(x[ which( x %in% c("Absorbance", "Transmission", "Refer", "Dark"))]))

  # rename Refer to Reference
  if( length( read.spc$bin$spc.type ) > 0){
    for(i in 1 : length( read.spc$bin$spc.type ))
      read.spc$bin$spc.type[[ i ]][ which(read.spc$bin$spc.type[[ i ]] == "Refer")] <- "Reference"

    for(i in 1 : length( read.spc$bin$spc.type ))
      if( length ( read.spc$bin$spc.type[[ i ]] ) == 0 ) read.spc$bin$spc.type[[ i ]] <- ""
  }
  # unique (as e.g. "Dark" is in ChannelName and ChannelType)
  read.spc$bin$spc.type <- lapply(read.spc$bin$spc.type, unique)

  # Write as vector #
  read.spc$bin$spc.type <- do.call(c, read.spc$bin$spc.type)

  # get Iteration ####
  read.spc$bin$Iteration <- lapply(read.spc$file$Bin, function( x ) x[ grep("It=\\d{1}", x)] )

  for(i in 1 : length( read.spc$bin$Iteration )){
    read.spc$bin$Iteration[[ i ]] <- substr(read.spc$bin$Iteration[[ i ]]
                                            , unlist( gregexpr("It=\\d{1}", read.spc$bin$Iteration[[ i ]]))
                                            , unlist( gregexpr("Aver=\\d{1}", read.spc$bin$Iteration[[ i ]])))

    read.spc$bin$Iteration[[ i ]] <- substr(read.spc$bin$Iteration[[ i ]]
                                            , unlist( gregexpr("It=\\d{1}", read.spc$bin$Iteration[[ i ]]))
                                            , unlist( lapply( gregexpr("\r", read.spc$bin$Iteration[[ i ]]), function( x ) x[[ 1 ]])) - 1)

    read.spc$bin$Iteration[[ i ]] <- as.numeric(gsub("I", "", gsub(",", ".", gsub("t=", "", read.spc$bin$Iteration[[ i ]]))))

    if( length(read.spc$bin$Iteration[[ i ]]) == 0) read.spc$bin$Iteration[[ i ]] <- NA
  }
  read.spc$bin$Iteration <- unlist( lapply(read.spc$bin$Iteration, unique))

  # get Average ####
  read.spc$bin$Average <- lapply(read.spc$file$Bin, function( x ) x[ grep("Aver=\\d{1}", x)] )

  for(i in 1 : length( read.spc$bin$Average)){
    read.spc$bin$Average[[ i ]] <- substr(read.spc$bin$Average[[ i ]]
                                          , unlist( gregexpr("Aver=\\d{1}", read.spc$bin$Average[[ i ]]))
                                          , unlist( gregexpr("Norm=\\d{1}", read.spc$bin$Average[[ i ]])))
    read.spc$bin$Average[[ i ]] <- substr(read.spc$bin$Average[[ i ]]
                                          , unlist( gregexpr("Aver=\\d{1}", read.spc$bin$Average[[ i ]]))
                                          , unlist( lapply( gregexpr("\r", read.spc$bin$Average[[ i ]]), function( x ) x[[ 1 ]])) - 1)
    read.spc$bin$Average[[ i ]] <- as.numeric(gsub(",", ".", gsub("Aver=", "", read.spc$bin$Average[[ i ]])))

    if( length(read.spc$bin$Average[[ i ]]) == 0) read.spc$bin$Average[[ i ]] <- NA
  }
  read.spc$bin$Average <- unlist( lapply(read.spc$bin$Average, unique))

  # read all .spc files ####
  for(i in 1 : length( read.spc$file$name))
    read.spc$file$raw[[ i ]] <- hyperSpec::read.spc(read.spc$file$name[ i ]
                                                    , keys.hdr2data = T
                                                    , keys.log2data = F
                                                    , log.txt = F
                                                    , log.bin = F)

  # get type by label name
  # Ugly way, but log.txt = T is corrupt and does not work anymore ?!
  # Work Around with readBin (above)
  # Hopefully this way is robust enough, especially for drk and ref it is critical!
  # Still, .spc files from LG2 Systems need this ugly way to detect if they are Absorbance, Transmission, Counts (Reference or Dark)
  read.spc$type <- unlist( lapply( read.spc$file$raw, function( x ) x@label$spc))

  if( length( grep("-D.spc$", read.spc$file$name) ) > 0) read.spc$type[ grep("-D.spc$", read.spc$file$name) ] <- "Counts"
  if( length( grep("-R.spc$", read.spc$file$name) ) > 0) read.spc$type[ grep("-R.spc$", read.spc$file$name) ] <- "Counts"

  if( length( read.spc$bin$spc.type ) > 0){
    read.spc$bin$spc.type[ read.spc$type == "A" & nchar( read.spc$bin$spc.type ) == 0 ] <- "Absorbance"
    read.spc$bin$spc.type[ read.spc$type == "T" & nchar( read.spc$bin$spc.type ) == 0] <- "Transmission"
    read.spc$bin$spc.type[ read.spc$type == "Counts" & nchar( read.spc$bin$spc.type ) == 0] <- "Counts"
  }

  if( length( grep("-D.spc$", read.spc$file$name) ) > 0) read.spc$bin$spc.type[ grep("-D.spc$", read.spc$file$name) ] <- "Counts"
  if( length( grep("-R.spc$", read.spc$file$name) ) > 0) read.spc$bin$spc.type[ grep("-R.spc$", read.spc$file$name) ] <- "Counts"

  if( length( read.spc$bin$spc.type ) == 0){
    read.spc$bin$spc.type[ read.spc$type == "A"] <- "Absorbance"
    read.spc$bin$spc.type[ read.spc$type == "T"] <- "Transmission"
    read.spc$bin$spc.type[ read.spc$type == "Counts"] <- "Counts"
  }

  read.spc$file$LG2.file$all <- which( read.spc$bin$spc.type == "Counts")
  read.spc$file$LG2.file$dark <- unique( c(grep("\\-D.spc$", unlist( lapply(read.spc$file$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))
                                           , grep("_d\\d{1}\\d{1}_", unlist( lapply(read.spc$Counts$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))
                                           , grep("_d\\d{1}\\d{1}\\.", unlist( lapply(read.spc$Counts$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))))

  read.spc$file$LG2.file$ref <- unique( c(grep("\\-R.spc$", unlist( lapply(read.spc$file$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))
                                          , grep("_r\\d{1}\\d{1}_", unlist( lapply(read.spc$Counts$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))
                                          , grep("_r\\d{1}\\d{1}\\.", unlist( lapply(read.spc$Counts$raw[ read.spc$file$LG2.file$all ], function( x ) x@data$filename)))))

  read.spc$bin$spc.type[ read.spc$file$LG2.file$all[ read.spc$file$LG2.file$dark ] ] <- "Dark"
  read.spc$bin$spc.type[ read.spc$file$LG2.file$all[ read.spc$file$LG2.file$ref ] ] <- "Reference"

  # Split in Absorbance, Reference, Dark, & Transmission ####
  read.spc$sub$spc$raw <-  read.spc$file$raw[ read.spc$bin$spc.type == "Absorbance" ]
  read.spc$sub$ref$raw <-  read.spc$file$raw[ read.spc$bin$spc.type == "Reference" ]
  read.spc$sub$drk$raw <-  read.spc$file$raw[ read.spc$bin$spc.type == "Dark" ]
  read.spc$sub$trans$raw <-  read.spc$file$raw[ read.spc$bin$spc.type == "Transmission" ]

  # Are all files in spc, drk, ref and trans?
  if(sum( unlist( lapply(read.spc$sub, function( x ) length( x$raw)))) != length( read.spc$file$name))
    warning("not all spc data could be assigned to a category (spc, ref, drk, trans)")

  # drk ####
  if( length( read.spc$sub$drk$raw ) > 0){
    # extract wavelengths
    read.spc$sub$drk$wl <- sort( unique( unlist(lapply(read.spc$sub$drk$raw, function( x ) x@wavelength))))
    # convert spectra to data.frame
    read.spc$sub$drk$df <- lapply(read.spc$sub$drk$raw, function( x ) data.frame( x@data$spc))
    # set new column names as wavelength
    for(i in 1 : length( read.spc$sub$drk$df )){
      colnames( read.spc$sub$drk$df[[ i ]] ) <- paste0("X", read.spc$sub$drk$raw[[ i ]]@wavelength)
    }
    # create new df
    read.spc$sub$drk$df <- rbindlist( read.spc$sub$drk$df, fill = T )
    read.spc$sub$drk$df <- read.spc$sub$drk$df[ , order( as.numeric( gsub("X", "", colnames(read.spc$sub$drk$df)))), with = F]

    for(i in unique( c(which( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ]) == 0),
                       which(unlist(lapply(read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ], function( x ) identical(x, character(0)))))))){

      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ][[ i ]] <- as.character( read.spc$sub$drk$raw[[ i ]]@data$fcmnt)
      if( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ][[ i ]] ) > 12 ) next
      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ][[ i ]] <- as.character( substr(gsub("_", " ", read.spc$sub$drk$raw[[ i ]]@data$filename), 1, 19), format = "%Y-%m-%d %H-%M-%S")
      if( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ][[ i ]] ) > 12 ) next
      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ][[ i ]] <- as.character(read.spc$sub$drk$raw[[ i ]]@data$fdate)

    }

    read.spc$sub$drk$df <- data.table( datetime = read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Dark" ]
                                       , Iteration = read.spc$bin$Iteration[ read.spc$bin$spc.type == "Dark" ]
                                       , Average = read.spc$bin$Average[ read.spc$bin$spc.type == "Dark" ]
                                       , filename = read.spc$file$name[ read.spc$bin$spc.type == "Dark" ]
                                       , read.spc$sub$drk$df )
  }

  # ref ####
  if( length( read.spc$sub$ref$raw ) > 0){
    # extract wavelengths
    read.spc$sub$ref$wl <- sort( unique( unlist(lapply(read.spc$sub$ref$raw, function( x ) x@wavelength))))
    # convert spectra to data.frame
    read.spc$sub$ref$df <- lapply(read.spc$sub$ref$raw, function( x ) data.frame( x@data$spc))
    # set new column names as wavelength
    for(i in 1 : length( read.spc$sub$ref$df )){
      colnames( read.spc$sub$ref$df[[ i ]] ) <- paste0("X", read.spc$sub$ref$raw[[ i ]]@wavelength)
    }
    # create new df
    read.spc$sub$ref$df <- rbindlist( read.spc$sub$ref$df, fill = T )
    read.spc$sub$ref$df <- read.spc$sub$ref$df[ , order( as.numeric( gsub("X", "", colnames(read.spc$sub$ref$df)))), with = F]

    for(i in unique( c(which( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ]) == 0),
                       which(unlist(lapply(read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ], function( x ) identical(x, character(0)))))))){

      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ][[ i ]] <- as.character( read.spc$sub$drk$raw[[ i ]]@data$fcmnt)
      if( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ][[ i ]] ) > 12 ) next
      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ][[ i ]] <- as.character( substr(gsub("_", " ", read.spc$sub$drk$raw[[ i ]]@data$filename), 1, 19), format = "%Y-%m-%d %H-%M-%S")
      if( nchar( read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ][[ i ]] ) > 12 ) next
      read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ][[ i ]] <- as.character(read.spc$sub$drk$raw[[ i ]]@data$fdate)

    }

    read.spc$sub$ref$df <- data.table( datetime = read.spc$bin$Originally.recorded[ read.spc$bin$spc.type == "Reference" ]
                                       , Iteration = read.spc$bin$Iteration[ read.spc$bin$spc.type == "Reference" ]
                                       , Average = read.spc$bin$Average[ read.spc$bin$spc.type == "Reference" ]
                                       , filename = read.spc$file$name[ read.spc$bin$spc.type == "Reference" ]
                                       , read.spc$sub$ref$df )
  }

  # spc ####
  if( length( read.spc$sub$spc$raw ) > 0){
    # extract wavelengths
    read.spc$sub$spc$wl <- sort( unique( unlist(lapply(read.spc$sub$spc$raw, function( x ) x@wavelength))))
    # convert spectra to data.frame
    read.spc$sub$spc$df <- lapply(read.spc$sub$spc$raw, function( x ) data.frame( x@data$spc))
    # set new column names as wavelength
    for(i in 1 : length( read.spc$sub$spc$df )){
      colnames( read.spc$sub$spc$df[[ i ]] ) <- paste0("X", read.spc$sub$spc$raw[[ i ]]@wavelength)
    }

    # Data.Table for .spc files with more than one spectra ####
    for(i in which( unlist( lapply(read.spc$sub$spc$df, nrow)) > 1)){

      xrange <- which(read.spc$bin$spc.type == "Absorbance")[ i ]

      read.spc$sub$spc$Interval[[ i ]] <- c(0
                                            , rep( read.spc$bin$Iteration[ xrange ] * read.spc$bin$Average[ xrange ] / 1000
                                                   , nrow( read.spc$sub$spc$df[[ i ]] ) - 1))
      read.spc$sub$spc$Interval[[ i ]] <- cumsum( read.spc$sub$spc$Interval[[ i ]] )
      read.spc$sub$spc$Interval[[ i ]] <- as.character(as.POSIXct( read.spc$bin$Originally.recorded[ xrange ][[ 1 ]]) + read.spc$sub$spc$Interval[[ i ]])

      read.spc$sub$spc$df[[ i ]] <- data.table( datetime = read.spc$sub$spc$Interval[[ i ]]
                                                , datetimeunique = read.spc$sub$spc$Interval[[ i ]][ 1 ]
                                                , Iteration = read.spc$bin$Iteration[ xrange ]
                                                , Average = read.spc$bin$Average[ xrange ]
                                                , filename = read.spc$file$name[ xrange ]
                                                , read.spc$sub$spc$df[[ i ]])
    }

    # Data.Table for .spc files with one spectra ####
    for(i in which( unlist( lapply(read.spc$sub$spc$df, nrow)) == 1)){

      xrange <- which(read.spc$bin$spc.type == "Absorbance")[ i ]

      # Datetime in data$fcmnt?
      string <- as.character( read.spc$sub$spc$raw[[ i ]]@data$fcmnt )
      if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
        read.spc$bin$Originally.recorded[ xrange ] <- as.character(as.POSIXct( string))

      if( nchar( read.spc$bin$Originally.recorded[ xrange ] ) < 13 | is.na( read.spc$bin$Originally.recorded[ xrange ])){

        string <- as.character( substr(gsub("_", " ", read.spc$sub$spc$raw[[ i ]]@data$filename), 1, 19))
        if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
          read.spc$bin$Originally.recorded[ xrange ] <- as.character( as.POSIXct( string, format = "%Y-%m-%d %H-%M-%S"))
        if( nchar( read.spc$bin$Originally.recorded[ xrange ]) > 13  | is.na( read.spc$bin$Originally.recorded[ xrange ])){

          string <- as.character(read.spc$sub$spc$raw[[ i ]]@data$fdate)
          if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
            read.spc$bin$Originally.recorded[ xrange ] <- string
        }
      }

      read.spc$sub$spc$df[[ i ]] <- data.table( datetime = read.spc$bin$Originally.recorded[[ i ]]
                                                , Iteration = read.spc$bin$Iteration[ xrange ]
                                                , Average = read.spc$bin$Average[ xrange ]
                                                , filename = read.spc$file$name[ xrange ]
                                                , read.spc$sub$spc$df[[ i ]])
    }

    # create new dt and order by columns ####
    read.spc$sub$spc$df <- rbindlist( read.spc$sub$spc$df, fill = T )

    if("datetimeunique" %in% colnames(read.spc$sub$spc$df)){
      read.spc$sub$spc$column.order <- c("datetime", "datetimeunique", "Iteration", "Average", "filename"
                                         , sort( colnames(read.spc$sub$spc$df)[ which( !is.na( as.numeric( gsub("X", "", colnames(read.spc$sub$spc$df))) )) ]))} else{
                                           read.spc$sub$spc$column.order <- c("datetime", "Iteration", "Average", "filename"
                                                                              , sort( colnames(read.spc$sub$spc$df)[ which( !is.na( as.numeric( gsub("X", "", colnames(read.spc$sub$spc$df))) )) ]))
                                         }
    read.spc$sub$spc$df <- read.spc$sub$spc$df[ , read.spc$sub$spc$column.order, with = F]
  }

  # trans ####
  if( length( read.spc$sub$trans$raw ) > 0){
    # extract wavelengths
    read.spc$sub$trans$wl <- sort( unique( unlist(lapply(read.spc$sub$trans$raw, function( x ) x@wavelength))))
    # convert spectra to data.frame
    read.spc$sub$trans$df <- lapply(read.spc$sub$trans$raw, function( x ) data.frame( x@data$spc))
    # set new column names as wavelength
    for(i in 1 : length( read.spc$sub$trans$df )){
      colnames( read.spc$sub$trans$df[[ i ]] ) <- paste0("X", read.spc$sub$trans$raw[[ i ]]@wavelength)
    }

    # Data.Table for .spc files with more than one spectra ####
    for(i in which( unlist( lapply(read.spc$sub$trans$df, nrow)) > 1)){

      xrange <- which(read.spc$bin$spc.type == "Transmission")[ i ]

      read.spc$sub$trans$Interval[[ i ]] <- c(0
                                              , rep( read.spc$bin$Iteration[ xrange ] * read.spc$bin$Average[ xrange ] / 1000
                                                     , nrow( read.spc$sub$trans$df[[ i ]] ) - 1))
      read.spc$sub$trans$Interval[[ i ]] <- cumsum( read.spc$sub$trans$Interval[[ i ]] )
      read.spc$sub$trans$Interval[[ i ]] <- as.character(as.POSIXct( read.spc$bin$Originally.recorded[ xrange ][[ 1]]) + read.spc$sub$trans$Interval[[ i ]])

      read.spc$sub$trans$df[[ i ]] <- data.table( datetime = read.spc$sub$trans$Interval[[ i ]]
                                                  , datetimeunique = read.spc$sub$trans$Interval[[ i ]][ 1 ]
                                                  , Iteration = read.spc$bin$Iteration[ xrange ]
                                                  , Average = read.spc$bin$Average[ xrange ]
                                                  , filename = read.spc$file$name[ xrange ]
                                                  , read.spc$sub$trans$df[[ i ]])
    }

    # Data.Table for .spc files with one spectra ####
    for(i in which( unlist( lapply(read.spc$sub$trans$df, nrow)) == 1)){

      xrange <- which(read.spc$bin$spc.type == "Transmission")[ i ]

      string <- as.character( read.spc$sub$trans$raw[[ i ]]@data$fcmnt )
      if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
        read.spc$bin$Originally.recorded[ xrange ] <- as.character(as.POSIXct( string))
      if( nchar( read.spc$bin$Originally.recorded[ xrange ] ) < 13 | is.na( read.spc$bin$Originally.recorded[ xrange ])){

        string <- as.character( substr(gsub("_", " ", read.spc$sub$trans$raw[[ i ]]@data$filename), 1, 19))
        if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
          read.spc$bin$Originally.recorded[ xrange ] <- as.character( as.POSIXct( string, format = "%Y-%m-%d %H-%M-%S"))
        if( nchar( read.spc$bin$Originally.recorded[ xrange ] < 13)  | is.na( read.spc$bin$Originally.recorded[ xrange ]) ){

          string <- as.character(read.spc$sub$trans$raw[[ i ]]@data$fdate)
          if( tryCatch(as.POSIXct( string ), error = function(e) FALSE) != F)
            read.spc$bin$Originally.recorded[ xrange ] <- string
        }
      }

      read.spc$sub$trans$df[[ i ]] <- data.table( datetime = read.spc$bin$Originally.recorded[[ i ]]
                                                  , Iteration = read.spc$bin$Iteration[ xrange ]
                                                  , Average = read.spc$bin$Average[ xrange ]
                                                  , filename = read.spc$file$name[ xrange ]
                                                  , read.spc$sub$trans$df[[ i ]])
    }

    # create new dt and order by columns ####
    read.spc$sub$trans$df <- rbindlist( read.spc$sub$trans$df, fill = T )

    if("datetimeunique" %in% colnames(read.spc$subtrans$df)){
      read.spc$subtrans$column.order <- c("datetime", "datetimeunique", "Iteration", "Average", "filename"
                                         , sort( colnames(read.spc$subtrans$df)[ which( !is.na( as.numeric( gsub("X", "", colnames(read.spc$subtrans$df))) )) ]))} else{
                                           read.spc$subtrans$column.order <- c("datetime", "Iteration", "Average", "filename"
                                                                              , sort( colnames(read.spc$subtrans$df)[ which( !is.na( as.numeric( gsub("X", "", colnames(read.spc$subtrans$df))) )) ]))
                                         }
    read.spc$sub$trans$column.order <- c("datetime", "Iteration", "Average", "filename"
                                         , sort( colnames(read.spc$sub$trans$df)[ which( !is.na( as.numeric( gsub("X", "", colnames(read.spc$sub$trans$df))) )) ]))
    read.spc$sub$trans$df <- read.spc$sub$trans$df[ , read.spc$sub$trans$column.order, with = F]
  }

  # final data ####
  if(spc.write) setwd( working_directory )
  if(spc.write) if( length(read.spc$sub$drk$df) > 0) fwrite(read.spc$sub$drk$df, paste0(substr(gsub("-", "", Sys.Date()), 3, 8), "_drk.csv"), sep = ";", dec = ",")
  if(spc.write) if( length(read.spc$sub$ref$df) > 0) fwrite(read.spc$sub$ref$df, paste0(substr(gsub("-", "", Sys.Date()), 3, 8), "_ref.csv"), sep = ";", dec = ",")
  if(spc.write) if( length(read.spc$sub$spc$df) > 0) fwrite(read.spc$sub$spc$df, paste0(substr(gsub("-", "", Sys.Date()), 3, 8), "_spc.csv"), sep = ";", dec = ",")
  if(spc.write) if( length(read.spc$sub$trans$df) > 0) fwrite(read.spc$sub$trans$df, paste0(substr(gsub("-", "", Sys.Date()), 3, 8), "_trans.csv"), sep = ";", dec = ",")

  if(return.R) return( lapply(read.spc$sub, function( x ) x$df) )

}
