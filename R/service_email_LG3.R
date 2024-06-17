service_email_LG3 <- function(today, yesterday
                              , systems
                              , wd_export
                              , servicemail, serviceimap
                              , pw
                              , verbose = T
                              , ssl = T
                              , folder = "INBOX"
                              , delete_files = F
                              , delete_emails = F
                              , unzip_server = T
                              , unzip_type = NA){

  library(mRpostman)
  library(zip)
  library(data.table)

  lg3_service_email <- list()
  # convert date to outlook-date ####
  lg3_service_email$today_outlook <- format(today, "%d-%b-%Y")
  lg3_service_email$today_outlook <- gsub(substr(lg3_service_email$today_outlook, 4, 6)
                                          , month.abb[as.numeric(format(today,"%m"))]
                                          , lg3_service_email$today_outlook)

  lg3_service_email$yesterday_outlook <- format(yesterday, "%d-%b-%Y")
  lg3_service_email$yesterday_outlook <- gsub(substr(lg3_service_email$yesterday_outlook, 4, 6)
                                              , month.abb[as.numeric(format(yesterday,"%m"))]
                                              , lg3_service_email$yesterday_outlook)

  # Configure Email ####
  con <- configure_imap(url=serviceimap,
                        username=servicemail,
                        password=pw,
                        use_ssl = ssl,
                        verbose = verbose,
                        # buffersize = 512000,
                        # timeout_ms = 10000
                        )

  con$reset_timeout_ms(x = 30000) # Long timeout to ensure the system has enough time to process the emails
  con$reset_verbose(x = FALSE) # The responding text is just too long, therefore suppress it

  # con$list_mail_folders() # List folders

  # Loop to run script for all LG3 and SG3 ####

    con$select_folder(name = folder) # Select Inbox

    # Create and navigate to folder customer/location/line
    dir.create(wd_export, showWarnings = F)
    setwd(wd_export)
    dir.create(systems$customer, showWarnings = F)
    setwd(paste0("./",systems$customer))
    dir.create(systems$short, showWarnings = F)
    setwd(paste0("./",systems$short))
    dir.create(systems$line, showWarnings = F)
    setwd(paste0("./",systems$line))

    lg3_service_email$wdc <- getwd() # Save current path

    # Search for log_zip emails ####
    lg3_service_email$search_log.drk <- paste0(yesterday, "_", systems$customer, "_", systems$location, "_", systems$line, "_drk")
    lg3_service_email$find_log.drk <- con$search(request = AND(mRpostman::string(expr = lg3_service_email$search_log.drk, where = "SUBJECT"),
                                                               since(date_char = lg3_service_email$today_outlook)), use_uid = T)
    lg3_service_email$find_log.drk <- lg3_service_email$find_log.drk[ length( lg3_service_email$find_log.drk ) ]

    lg3_service_email$search_log.ref <- paste0(yesterday, "_", systems$customer, "_", systems$location, "_", systems$line, "_ref")
    lg3_service_email$find_log.ref <- con$search(request = AND(mRpostman::string(expr = lg3_service_email$search_log.ref, where = "SUBJECT"),
                                                               since(date_char = lg3_service_email$today_outlook)), use_uid = T)
    lg3_service_email$find_log.ref <- lg3_service_email$find_log.ref[ length( lg3_service_email$find_log.ref )]

    lg3_service_email$search_log.spc <- paste0(yesterday, "_", systems$customer, "_", systems$location, "_", systems$line, "_spc")
    lg3_service_email$find_log.spc <- con$search(request = AND(mRpostman::string(expr = lg3_service_email$search_log.spc, where = "SUBJECT"),
                                                               since(date_char = lg3_service_email$today_outlook)), use_uid = T)
    lg3_service_email$find_log.spc <- sort( lg3_service_email$find_log.spc )

    message("Found "
            , ifelse( is.na(lg3_service_email$find_log.drk), 0, length( lg3_service_email$find_log.drk )), " Dark spectra, "
            , ifelse( is.na(lg3_service_email$find_log.ref), 0, length( lg3_service_email$find_log.ref )), " Reference spectra and "
            , ifelse( length( lg3_service_email$find_log.spc ) == 0, 0, ifelse( is.na(lg3_service_email$find_log.spc), 0, length( lg3_service_email$find_log.spc ))), " Production spectra"
            , " from ", paste0(systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG)
            , ", date = ", yesterday)

    lg3_service_email$find_log <- c(lg3_service_email$find_log.drk, lg3_service_email$find_log.ref, lg3_service_email$find_log.spc)
    lg3_service_email$find_log <- sort( lg3_service_email$find_log )

    # Download csv ####
    setwd(lg3_service_email$wdc)

    # next loop if no attachments were found
    if(!any(!is.na(lg3_service_email$find_log))) stop("No attachments were found")
    unlink(dir(path = lg3_service_email$wdc, recursive = T), force = T, recursive = T)

    if(any(!is.na(lg3_service_email$find_log))){
      lg3_service_email$find_log %>%
        con$fetch_attachments(use_uid = T)
    }
    # Sys.sleep(60) # The system can be kind of busy, this is just to feel more secure

    # read data
    if( length( dir( pattern = "ref R export.csv", recursive = T) ) != 0){
      read.ref <- fread(dir( pattern = "ref R export.csv", recursive = T), sep = ";", dec = ",", fill = F)
      read.ref$time <- strftime(read.ref$datetime, format = "%H:%M:%S", tz = "UTC")
    }

    if( length( dir( pattern = "drk R export.csv", recursive = T) ) != 0){
      read.drk <- fread(dir( pattern = "drk R export.csv", recursive = T), sep = ";", dec = ",", fill = F)
      read.drk$time <- strftime(read.drk$datetime, format = "%H:%M:%S", tz = "UTC")
    }

    if( length( dir( pattern = "spc R export", recursive = T) ) != 0){
      read.spc <- lapply(dir( pattern = "spc R export", recursive = T), function( x ) fread(x, sep = ";", dec = ",", fill = F))

      if(systems$LG == "SG3"){
        # Timestamp Bug in SG3
        for(k in 1 : length( read.spc)){
          read.spc[[ k ]]$spectrumTimestamp <- as.character( read.spc[[ k ]]$spectrumTimestamp )
          read.spc[[ k ]]$statusTimestamp <- as.character( read.spc[[ k ]]$statusTimestamp )
          # read.spc <- lapply(read.spc, function( x ) x[ , spectrumTimestamp := as.character(spectrumTimestamp)])
          # read.spc <- lapply(read.spc, function( x ) x[ , statusTimestamp := as.character(statusTimestamp)])
        }
      }

      read.spc <- rbindlist(read.spc, fill = T)
      read.spc$time <- strftime(read.spc$datetime, format = "%H:%M:%S", tz = "UTC")
      read.spc$location <- systems$location
      read.spc$line <- systems$line
    }

    # Move csv ####
    export_directory <- service_backup_path(systems$customer
                                            , systems$location
                                            , systems$line, dir_wd = wd)

    if( length( lg3_service_email$find_log.ref ) != 0)
      if( exists("read.ref")){
        for(k in 1:length(unique(read.ref$date))){
          reftoexport <- read.ref[which(read.ref$date==unique(read.ref$date)[k]),]
          setwd(export_directory)
          dir.create("ref", showWarnings = F)
          setwd("./ref")

          if(length(which(substr(dir(),1,10)==unique(reftoexport$date)))>0){
            reftomerge <- fread(dir()[which(gsub("_ref.csv","",dir())==unique(reftoexport$date))], sep = ";", dec = ",")

            if( nrow( reftomerge ) < 1) reftomerge <- reftoexport
            refmerged <- rbindlist(list(reftomerge,reftoexport), fill = T)

            refmerged <- refmerged[order(refmerged$datetime),]
            refmerged <- refmerged[which(!duplicated(refmerged$datetime)),]

            reftoexport <- refmerged
            if(is.na(as.numeric(gsub("X", "", names(reftoexport)[ncol(reftoexport)])))){
              suppressWarnings(refwl <- sort(as.numeric(gsub("X","",names(reftoexport)))))
              for(wl in paste0("X", refwl)) reftoexport <- reftoexport[ , moveme(names(reftoexport), paste(wl, "last")), with = F]}
          }
          reftoexport <- reftoexport[order(reftoexport$datetime),]
          fwrite(reftoexport,paste0(unique(reftoexport$date),"_",
                                    systems$customer,"_",
                                    systems$location,"_",
                                    systems$line,"_ref.csv"), sep = ";", dec = ",")
          rm(read.ref)
        }
      }

    if( length( lg3_service_email$find_log.drk ) != 0)
      if( exists("read.drk")){
        for(k in 1:length(unique(read.drk$date))){
          drktoexport <- read.drk[which(read.drk$date==unique(read.drk$date)[k]),]
          setwd(export_directory)
          dir.create("drk", showWarnings = F)
          setwd("./drk")

          if(length(which(substr(dir(),1,10)==unique(drktoexport$date)))>0){
            drktomerge <- fread(dir()[which(gsub("_drk.csv","",dir())==unique(drktoexport$date))], sep = ";", dec = ",")

            if( nrow( drktomerge ) < 1) drktomerge <- drktoexport
            drkmerged <- rbindlist(list(drktomerge,drktoexport), fill = T)

            drkmerged <- drkmerged[order(drkmerged$datetime),]
            drkmerged <- drkmerged[which(!duplicated(drkmerged$datetime)),]

            drktoexport <- drkmerged
            if(is.na(as.numeric(gsub("X", "", names(drktoexport)[ncol(drktoexport)])))){
              suppressWarnings(drkwl <- sort(as.numeric(gsub("X","",names(drktoexport)))))
              for(wl in paste0("X", drkwl)) drktoexport <- drktoexport[ , moveme(names(drktoexport), paste(wl, "last")), with = F]}
          }
          drktoexport <- drktoexport[order(drktoexport$datetime),]
          fwrite(drktoexport,paste0(unique(drktoexport$date),"_",
                                    systems$customer,"_",
                                    systems$location,"_",
                                    systems$line,"_drk.csv"), sep = ";", dec = ",")
          rm(read.drk)
        }
      }

    if( length( lg3_service_email$find_log.spc ) != 0)
      if( exists("read.spc")){

        spc.date <- read.spc

        for(k in 1:length(unique(read.spc$date))){
          spctoexport <- read.spc[which(read.spc$date==unique(read.spc$date)[k]),]
          setwd(export_directory)
          dir.create("spc", showWarnings = F)
          setwd("./spc")

          if(length(which(substr(dir(),1,10)==unique(spctoexport$date)))>0){
            spctomerge <- fread(dir()[which(gsub("_spc.csv","",dir())==unique(spctoexport$date))], sep = ";", dec = ",")
            if( nrow( spctomerge ) < 1) spctomerge <- spctoexport

            if(systems$LG == "SG3"){
              # Timestamp Bug in SG3
              spctomerge$spectrumTimestamp <- as.character(spctomerge$spectrumTimestamp)
              spctomerge$statusTimestamp <- as.character(spctomerge$statusTimestamp)
              # spctomerge <- spctomerge[ , spectrumTimestamp := as.character(spectrumTimestamp)]
              # spctomerge <- spctomerge[ , statusTimestamp := as.character(statusTimestamp)]
            }

            if( nrow( spctomerge ) < 1) spctomerge <- spctoexport
            spcmerged <- rbindlist(list(spctomerge,spctoexport), fill = T)

            spcmerged <- spcmerged[order(spcmerged$datetime),]
            spcmerged <- spcmerged[which(!duplicated(spcmerged$datetime)),]

            spctoexport <- spcmerged
            if(is.na(as.numeric(gsub("X", "", names(spctoexport)[ncol(spctoexport)])))){
              suppressWarnings(spcwl <- sort(as.numeric(gsub("X","",names(spctoexport)))))
              for(wl in paste0("X", spcwl)) spctoexport <- spctoexport[ , moveme(names(spctoexport), paste(wl, "last")), with = F]}
          }
          spctoexport <- spctoexport[order(spctoexport$datetime),]
          fwrite(spctoexport,paste0(unique(spctoexport$date),"_",
                                    systems$customer,"_",
                                    systems$location,"_",
                                    systems$line,"_spc.csv"), sep = ";", dec = ",")
          rm(read.spc)
        }

        produkt_per_day_year(customer = systems$customer
                             , location = systems$location
                             , line = systems$line
                             , LG = systems$LG
                             , year = as.numeric(substr(unique( spc.date$date ), 1, 4))
                             , dir_wd = wd
                             , date_file = unique( spc.date$date ))

      }
    setwd(lg3_service_email$wdc)
    unlink(dir(path = lg3_service_email$wdc, recursive = T), force = T, recursive = T)
    unlink( list.dirs(), recursive = T)

    if(delete_emails == T){

      lg3_service_email$delete$search$log.drk <- list()
      lg3_service_email$delete$search$log.ref <- list()
      lg3_service_email$delete$search$log.spc <- list()

      lg3_service_email$delete$find$log.drk <- list()
      lg3_service_email$delete$find$log.ref <- list()
      lg3_service_email$delete$find$log.spc <- list()

      for(d in 0:30){
      lg3_service_email$delete$search$log.drk[[ d + 1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$location, "_", systems$line, "_drk")
      lg3_service_email$delete$search$log.ref[[ d + 1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$location, "_", systems$line, "_ref")
      lg3_service_email$delete$search$log.spc[[ d + 1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$location, "_", systems$line, "_spc")

      lg3_service_email$delete$find$log.drk[[ d + 1 ]] <- con$search(request = string(expr = lg3_service_email$delete$search$log.drk[[ d + 1 ]], where = "SUBJECT"), use_uid = T)
      lg3_service_email$delete$find$log.ref[[ d + 1 ]] <- con$search(request = string(expr = lg3_service_email$delete$search$log.ref[[ d + 1 ]], where = "SUBJECT"), use_uid = T)
      lg3_service_email$delete$find$log.spc[[ d + 1 ]] <- con$search(request = string(expr = lg3_service_email$delete$search$log.spc[[ d + 1 ]], where = "SUBJECT"), use_uid = T)

      }

      con$delete$uid <- sort(as.numeric(unlist(lg3_service_email$delete$find)))
      con$delete$uid
      con$delete_msg(msg_id = con$delete$uid, use_uid = T, mute = F, retries = 5)
      con$expunge(msg_uid = con$delete$uid, mute = F, retries = 5)
      con$expunge()
    }

}

