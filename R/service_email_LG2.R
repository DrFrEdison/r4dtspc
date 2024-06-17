service_email_LG2 <- function(today, yesterday
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

  lg2_service_email <- list()
  # convert date to outlook-date ####
  lg2_service_email$today_outlook <- format(today, "%d-%b-%Y")
  lg2_service_email$today_outlook <- gsub(substr(lg2_service_email$today_outlook, 4, 6)
                                          , month.abb[as.numeric(format(today,"%m"))]
                                          , lg2_service_email$today_outlook)

  lg2_service_email$yesterday_outlook <- format(yesterday, "%d-%b-%Y")
  lg2_service_email$yesterday_outlook <- gsub(substr(lg2_service_email$yesterday_outlook, 4, 6)
                                              , month.abb[as.numeric(format(yesterday,"%m"))]
                                              , lg2_service_email$yesterday_outlook)

  # Configure Email ####
  con <- configure_imap(url=serviceimap,
                        username=servicemail,
                        password=pw,
                        use_ssl = ssl,
                        verbose = verbose,
                        buffersize = 512000,
                        timeout_ms = 10000)

  con$reset_timeout_ms(x = 30000) # Long timeout to ensure the system has enough time to process the emails
  con$reset_verbose(x = FALSE) # The responding text is just too long, therefore suppress it

  # con$list_mail_folders() # List folders

  # Loop to run script for all LG1 and LG2 ####
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

  lg2_service_email$wdc <- getwd() # Save current path

  # delete old files ####
  setwd(lg2_service_email$wdc)
  dir.create(servicemail, showWarnings = F)
  setwd(paste0("./", servicemail))
  dir.create(folder, showWarnings = F)
  setwd(paste0("./", folder))

  if(delete_files == T){
    unlink(dir(), recursive = T, force = T)
    if(length(dir())>0) file.remove(dir(), all.files = T, recursive = T)
  }

  setwd(wd_export)
  setwd(paste0("./",systems$customer))
  setwd(paste0("./",systems$short))
  setwd(paste0("./",systems$line))

  # Search for log_zip emails ####
  lg2_service_email$search_log1 <- paste0(yesterday, "_", systems$customer, "_", systems$short, "_", systems$line, "_log_zip")
  lg2_service_email$find_log1 <- con$search(request = AND(mRpostman::string(expr = lg2_service_email$search_log1, where = "SUBJECT"),
                                                          since(date_char = lg2_service_email$today_outlook)), use_uid = T)

  lg2_service_email$search_log2 <- paste0(yesterday, "_", systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG, "_spc_zip")
  lg2_service_email$find_log2 <- con$search(request = AND(mRpostman::string(expr = lg2_service_email$search_log2, where = "SUBJECT"),
                                                          since(date_char = lg2_service_email$today_outlook)), use_uid = T)
  lg2_service_email$find_log <- sort( unique( c(lg2_service_email$find_log1, lg2_service_email$find_log2)))

  message("Found "
          , ifelse( all(is.na( lg2_service_email$find_log)), 0, length(lg2_service_email$find_log))
          , " attachments with .log and .spc files in INBOX"
          , " from ", paste0(systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG))

  # Search for csv_zip emails ####
  lg2_service_email$search_csv1 <- paste0(yesterday, "_", systems$customer, "_", systems$short, "_", systems$line, "_csv_zip")
  lg2_service_email$find_csv1 <- con$search(request = mRpostman::string(expr = lg2_service_email$search_csv1, where = "SUBJECT")
                                            , use_uid = T)

  lg2_service_email$search_csv2 <- paste0(yesterday, "_", systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG, "_spc_csv")
  lg2_service_email$find_csv2 <- con$search(request = mRpostman::string(expr = lg2_service_email$search_csv2, where = "SUBJECT")
                                            , use_uid = T)

  lg2_service_email$find_csv <- sort( unique( c(lg2_service_email$find_csv1, lg2_service_email$find_csv2)))

  message("Found "
          , ifelse(all(is.na(lg2_service_email$find_csv)), 0, length(lg2_service_email$find_csv))
          , " attachments with .csv files in INBOX"
          , " from ", paste0(systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG))

  # Download log ####
  setwd(lg2_service_email$wdc)

  if(any(!is.na(lg2_service_email$find_log))){
    lg2_service_email$find_log %>%
      con$fetch_attachments(use_uid = T)
    # message("\n.log and .spc files downloaded")
  }
  # Sys.sleep(60) # The system can be kind of busy, this is just to feel more secure

  # Download csv ####
  if(any(!is.na(lg2_service_email$find_csv))){
    lg2_service_email$find_csv %>%
      con$fetch_attachments(use_uid = T)
    # message("\n.csv ", ifelse(length(lg2_service_email$find_csv) > 1, "files", "file"), " downloaded")
  }

  # Unzip ####
  setwd(lg2_service_email$wdc)
  dir.create(servicemail, showWarnings = F)
  setwd(paste0("./", servicemail))
  dir.create(folder, showWarnings = F)
  setwd(paste0("./", folder))

  dir.create("spclog", showWarnings = F) # Unzip folder
  if(any(!is.na(lg2_service_email$find_log))){
    for(j in lg2_service_email$find_log){
      if(!paste0("UID",j) %in% dir()) next
      unzip(paste0("./UID", j, "/", dir(paste0("./UID", j))), exdir = "./spclog")}
    # message(".log and .spc files unzipped into folder spclog")
  }

  dir.create("csv", showWarnings = F) # Unzip folder
  if(any(!is.na(lg2_service_email$find_csv))){
    for(j in lg2_service_email$find_csv) unzip(paste0("./UID", j, "/", dir(paste0("./UID", j))), exdir = "./csv")
    # message(".csv files unzipped into folder csv")
  }

  # Delete Files from folder ####
  unlink(dir()[which(dir() != "csv" & dir() != "spclog")], force = T, recursive = T)
  # message("delete all files except folders csv and spclog")

  # Move csv ####
  lg2_service_email$export_path <- service_backup_path(customer = systems$customer
                                                       , location = systems$location
                                                       , line = systems$line
                                                       , dir_wd = wd)

  setwd(lg2_service_email$wdc)
  setwd(paste0("./", servicemail))
  setwd(paste0("./", folder))
  setwd("csv")

  if(length(dir()[grep(yesterday, dir())]) > 0){
    file.copy(dir()[grep(yesterday, dir())]
              , paste0(lg2_service_email$export_path, "CSV/", dir()[grep(yesterday, dir())]), overwrite = T)
    # message( toString(dir()[grep(yesterday, dir())]), " copied to ", paste0(lg2_service_email$export_path, "CSV/"))
  }

  setwd(lg2_service_email$wdc)
  setwd(paste0("./", servicemail))
  setwd(paste0("./", folder))
  setwd("spclog")

  if(length(dir()[grep(yesterday, dir())]) > 0){
    zip(zipfile = paste0(yesterday, ".zip")
        , files = dir()[grep(yesterday, dir())])
    # message(".log and .spc files zipped to ", paste0(yesterday, ".zip"))
    file.copy(paste0(yesterday, ".zip")
              , paste0(lg2_service_email$export_path, "ZIP/", yesterday, ".zip")
              , overwrite = T)
    # message(paste0(yesterday, ".zip"), " copied to ", paste0(lg2_service_email$export_path, "ZIP/"))
  }

  # Delete Files####
  setwd(lg2_service_email$wdc)
  setwd(paste0("./", servicemail))
  setwd(paste0("./", folder))

  if(delete_files == T){
    unlink(dir(), recursive = T, force = T)
    if(length(dir())>0) file.remove(dir(), all.files = T, recursive = T)
  }

  # Delete last 30 Emails ####
  if(delete_emails == T){

    lg2_service_email$delete$search$log_zip1 <- list()
    lg2_service_email$delete$search$log_zip2 <- list()
    lg2_service_email$delete$search$csv_zip1 <- list()
    lg2_service_email$delete$search$csv_zip2 <- list()

    lg2_service_email$delete$find$log_zip1 <- list()
    lg2_service_email$delete$find$log_zip2 <- list()
    lg2_service_email$delete$find$csv_zip1 <- list()
    lg2_service_email$delete$find$csv_zip2 <- list()

    for(d in 0:30){
      lg2_service_email$delete$search$log_zip1[[ d+1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$short, "_", systems$line, "_log_zip")
      lg2_service_email$delete$search$log_zip2[[ d+1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG, "_spc_zip")
      lg2_service_email$delete$search$csv_zip1[[ d+1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$short, "_", systems$line, "_csv_zip")
      lg2_service_email$delete$search$csv_zip2[[ d+1 ]] <- paste0(yesterday - d, "_", systems$customer, "_", systems$location, "_", systems$line, "_", systems$LG, "_csv_zip")

      lg2_service_email$delete$find$log_zip1[[ d+1 ]] <- con$search(request = string(expr = lg2_service_email$delete$search$log_zip1[[ d+1 ]], where = "SUBJECT"), use_uid = T)
      lg2_service_email$delete$find$log_zip2[[ d+1 ]] <- con$search(request = string(expr = lg2_service_email$delete$search$log_zip2[[ d+1 ]], where = "SUBJECT"), use_uid = T)
      lg2_service_email$delete$find$csv_zip1[[ d+1 ]] <- con$search(request = string(expr = lg2_service_email$delete$search$csv_zip1[[ d+1 ]], where = "SUBJECT"), use_uid = T)
      lg2_service_email$delete$find$csv_zip2[[ d+1 ]] <- con$search(request = string(expr = lg2_service_email$delete$search$csv_zip2[[ d+1 ]], where = "SUBJECT"), use_uid = T)
    }

    con$delete$uid <- sort(as.numeric(unlist(lg2_service_email$delete$find)))
    con$delete$uid
    con$delete_msg(msg_id = con$delete$uid, use_uid = T, mute = F, retries = 5)
    con$expunge(msg_uid = con$delete$uid, mute = F, retries = 5)
    con$expunge()
      }
}

