produkt_per_day_year <- function(customer, location, line, LG, year, date_file = NA, dir_wd){

  filep <- list()
  filep$year <- as.character(year)

  for(k in 1:length(filep$year)){

    setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))

    setwd("./spc")
    filep$files <- dir(pattern = filep$year[k])

    if( customer == "CCEP"){
      filep$DTProductNumber <- DTProductNumber(customer, dir_wd)
    }

    if(!is.na(date_file)){
      filep$files <- filep$files[grep(date_file, filep$files)]
    }

    if(length(filep$files) < 1) next

    for(j in 1:length(filep$files)){

      setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))
      setwd("./spc")

      values = tryCatch(read.csv2(filep$files[j], nrows = 1, fileEncoding="437"),
                        error = function(e) NA, warning = function(w) print('Without semanal file'))

      if(is.na(values)[1]) next
      if( nrow(values) == 0) next

      filep$trans <- data.table::fread(filep$files[j])

      if( length( unique( filep$trans$date)) > 1) message("More than one date in csv file")
      if( length( unique( filep$trans$date)) > 1) break

      if(LG != "3" & LG != "SG3") if(length(unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F])) == 0) next
      if(LG != "3" & LG != "SG3"){
        filep$export <- data.frame(cbind(unique( filep$trans$date), unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F])))
        names(filep$export)[ 2 ] <- "MixerNumber"

      }

      if(LG == "3" |  LG == "SG3"){

        if( length( grep("DTproductNumber", names(filep$trans))) > 0 &
            length( grep("MixerNumber", names(filep$trans))) == 0) filep$export <- data.frame(date = unique( filep$trans$date)
                                                                                              , Produktname = NA
                                                                                              , DTProductNumber = as.numeric( unlist( unique(filep$trans[ , grep("DTproductNumber", names(filep$trans)), with = F])))
                                                                                              , MixerNumber = NA)
        if( length( grep("^date$", names(filep$trans), ignore.case = T) ) > 0)
          if( length( grep("DTproductNumber", names(filep$trans))) == 0 &
              length( grep("MixerNumber", names(filep$trans))) > 0) filep$export <- data.frame(date = unique( filep$trans$date)
                                                                                               , Produktname = NA
                                                                                               , DTProductNumber = NA
                                                                                               , MixerNumber = unique(filep$trans[ , grep("MixerNumber", names(filep$trans)), with = F]))

        if( length( grep("^Day$", names(filep$trans), ignore.case = T) ) > 0)
          if( length( grep("DTproductNumber", names(filep$trans))) == 0 &
              length( grep("MixerNumber", names(filep$trans))) > 0) filep$export <- data.frame(date = unique(filep$trans[ , grep("^Day$", names(filep$trans), ignore.case = T), with = F])
                                                                                               , Produktname = NA
                                                                                               , DTProductNumber = NA
                                                                                               , MixerNumber = unique(filep$trans[ , grep("MixerNumber", names(filep$trans)), with = F]))


        if( length( grep("DTproductNumber", names(filep$trans))) > 0 &
            length( grep("MixerNumber", names(filep$trans))) > 0) filep$export <- data.frame(date = unique( filep$trans$date)
                                                                                             , Produktname = NA
                                                                                             , DTProductNumber = as.numeric( unlist( unique(filep$trans[ , c("DTproductNumber", "MixerNumber"), with = F])[ , 1]))
                                                                                             , MixerNumber = unique(filep$trans[ , c("DTproductNumber", "MixerNumber"), with = F])[ , 2])

        for(o in 1 : nrow( filep$export)) if(any(filep$DTProductNumber$DTProductNumber %in% filep$export$DTProductNumber[ o ]))  filep$export$Produktname[ o ] <- filep$DTProductNumber$Produktname[ filep$DTProductNumber$DTProductNumber %in% filep$export$DTProductNumber[ o ] ]

      }

      if(LG == "2"){

        if( length( grep("Produktnummer", names(filep$trans)) ) > 0 )
          filep$export <- data.frame(date = as.character( unique( filep$trans$date))
                                     , MixerNumber = as.numeric( unlist( unique(filep$trans[ , grep("Produktnummer", names(filep$trans)), with = F])))) else{

                                       filep$export <- data.frame(date = as.character( unique( filep$trans$date))
                                                                  , MixerNumber = as.numeric( unlist( unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F]))))

                                     }
      }

      if(LG == "SG"){

        filep$export <- data.frame(date = as.character( unique( filep$trans$date))
                                   , MixerNumber = as.character( unlist( unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F]))))
      }

      # # new MixerID or DTProductNumber ? ----
      # dt_Mixernummer_sub <- dt_Mixernummer[ dt_Mixernummer$customer == customer & dt_Mixernummer$location == location,]
      # if( length(dt_Mixernummer_sub$line) > 1) dt_Mixernummer_sub <- dt_Mixernummer_sub[ dt_Mixernummer_sub$line == line, ]
      #
      # dt_Mixernummer_new <- unlist( lapply( filep$export$MixerNumber
      #                                       , function( x ) any(
      #                                         c(
      #                                           dt_Mixernummer_sub$Mixer_ID_1, dt_Mixernummer_sub$Mixer_ID_2, dt_Mixernummer_sub$Mixer_ID_3
      #                                         ) %in% x)
      # )
      # )
      #
      # if(!is.null(dt_Mixernummer_new)) if( !any( dt_Mixernummer_new )){
      #
      #   file.copy( paste0(wd$master, "/r4dt_beverage.ods")
      #              , paste0(wd$master, "/", date.dt(), "_", "r4dt_beverage.ods"))
      #
      #   for(i in which( !dt_Mixernummer_new )){
      #
      #     dt_Mixernummer <-  rbind(c(as.character(customer), location, LG, line
      #                                , ifelse( is.null( filep$export$Produktname[ i ] ), NA, filep$export$Produktname[ i ])
      #                                , filep$export$MixerNumber[ i ], 0, 0
      #                                , ifelse( is.null( filep$export$Produktname[ i ] ), NA, filep$export$Produktname[ i ]))
      #                              , dt_Mixernummer)
      #
      #   }
      #
      #   dt_Mixernummer$LG <- factor( dt_Mixernummer$LG, levels = c( "3", "SG", "2" ))
      #
      #   dt_Mixernummer <- dt_Mixernummer[ order(dt_Mixernummer$customer, dt_Mixernummer$LG
      #                                           , dt_Mixernummer$location, dt_Mixernummer$line
      #                                           , dt_Mixernummer$DTProductNumber, dt_Mixernummer$beverage) , ]
      #
      #   readODS::write_ods(dt_Mixernummer
      #                      , paste0(wd$master, "/", substr(gsub("-", "", Sys.Date()), 3, 8), "_r4dt_beverage.ods"))
      #
      # }

      # write to ServiceBackup ----
      setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))
      names( filep$export )[ 1 ] <- "date"

      filep$name <- paste0(paste(filep$year[k], customer, location, line, sep = "_"),".csv")

      if(!filep$name %in% dir()) write.csv2(filep$export, filep$name, row.names = F)

      if(filep$name %in% dir()){
        filep$csv_exist <- read.csv2(filep$name)
        filep$csv_exist <- plyr::rbind.fill(filep$csv_exist, filep$export)

        filep$csv_exist <- filep$csv_exist[which(substr(filep$csv_exist$date, 1, 4) %in% filep$year[k] == T) , ]

        if(any(which(duplicated.data.frame(filep$csv_exist)))) filep$csv_exist <- filep$csv_exist[ -which(duplicated.data.frame(filep$csv_exist)) , ]

        filep$csv_exist <- filep$csv_exist[order(filep$csv_exist[ , 1]), ]

        write.csv2(filep$csv_exist, filep$name, row.names = F)
      }
    }
  }
}
