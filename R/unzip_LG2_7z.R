unzip.LG2.7z <- function(date = NA # Which date should be unzipped? If NA then all
                         , unzip.dir = "C:/csvtemp"
                         , customerlist # List with lines
                         , dir_wd){ # wd

  # Folder paths ####
  zip.dir <- paste0(service_backup_path(as.character(customerlist$customer), as.character(customerlist$location), as.character(customerlist$line), dir_wd = dir_wd),"ZIP/")
  csv.dir <- paste0(service_backup_path(as.character(customerlist$customer), as.character(customerlist$location), as.character(customerlist$line), dir_wd = dir_wd),"CSV/")
  ref.dir <- paste0(service_backup_path(as.character(customerlist$customer), as.character(customerlist$location), as.character(customerlist$line), dir_wd = dir_wd), "ref/")
  drk.dir <- paste0(service_backup_path(as.character(customerlist$customer), as.character(customerlist$location), as.character(customerlist$line), dir_wd = dir_wd), "drk/")
  spc.dir <- paste0(service_backup_path(as.character(customerlist$customer), as.character(customerlist$location), as.character(customerlist$line), dir_wd = dir_wd), "spc/")

  # List zip.files ####
  message(zip.dir)
  setwd(zip.dir)
  zip.files <- dir(pattern = ".zip$")
  zip.files.date <- substr(zip.files,1,10)

  # List csv.files ####
  setwd(csv.dir)
  csv.files <- dir(pattern = ".csv$")
  csv.files.date <- substr(csv.files,1,10)

  # List exported csvfiles ####
  setwd(csv.dir)
  ref.files <- list.files(ref.dir, pattern = "ref.csv$")
  drk.files <- list.files(drk.dir, pattern = "drk.csv$")
  spc.files <- list.files(spc.dir, pattern = "spc.csv$")

  ref.drk.spc.files <- c(ref.files, drk.files, spc.files)
  ref.drk.spc.files.date <- substr(ref.drk.spc.files, 1, 10)

  # Filter date ####
  if(!is.na(date)){
    ref.drk.spc.files.date <- ref.drk.spc.files.date[grep(date,ref.drk.spc.files.date)]

    zip.files <- zip.files[grep(date,zip.files.date)]
    zip.files.date <- zip.files.date[grep(date,zip.files.date)]

    csv.files <- csv.files[ grep(date, csv.files.date) ]
    csv.files.date <- csv.files.date[ grep(date, csv.files.date) ]

  }

  zip.files.date <- sort(zip.files.date)
  zip.files <- sort(zip.files)
  ref.drk.spc.files.date <- sort(ref.drk.spc.files.date)
  csv.files <- sort(csv.files)
  csv.files.date <- sort(csv.files.date)

  # remove 1kb files
  # this is a bug in zip files, sometimes very small zipfiles are created by LG2 systems which do not have any data
  zip.files.size <- file.info( paste0(zip.dir, zip.files[ !is.na(zip.files.date) ] ))$size
  zip.files <- zip.files[ which( zip.files.size != 22) ]
  zip.files.date <- zip.files.date[ which( zip.files.size != 22) ]

  # Check if last 60 zip.files were completely extracted
  # Only proceed this during night as it takes a while
  if(strftime(Sys.time(), format = "%H:%M:%S") > "01:00:00" & strftime(Sys.time(), format = "%H:%M:%S") < "08:30:00"){

    spc.files.to.check <- spc.files[ which(substr(spc.files, 1, 10) > Sys.Date( ) - 60) ]
    zip.files.to.check <- zip.files[ which( substr(zip.files, 1, 10) > Sys.Date( ) - 60) ]

    zip.files.nrow <- lapply(zip.files.to.check, function( x ) zip::zip_list( paste0(zip.dir, x)))
    zip.files.nrow <- lapply(zip.files.nrow, function( x ) length(x$filename[ grep("-M.log", x$filename)]))

    spc.files.nrow <- lapply( paste0(spc.dir, spc.files.to.check), freadr4dt)
    spc.files.nrow <- lapply(spc.files.nrow, nrow)

    zip.files.to.check <- zip.files.to.check[ which( unlist( zip.files.nrow ) != unlist( spc.files.nrow )) ]
    spc.files.to.check <- spc.files.to.check[ which( unlist( zip.files.nrow ) != unlist( spc.files.nrow )) ]
  }

  # Stop when all files are already processed ####
  if( all(is.na(unique(zip.files.date))) ){message(paste0("No new files found for ",
                                                          as.character(customerlist$customer)," ",
                                                          as.character(customerlist$location)," ",
                                                          as.character(customerlist$line)))
    setwd(csv.dir)
    stop_quietly()
  }

  # Check if files are already existing ####
  for(i in 1:length(sort(zip.files.date))){
    if(length(pmatch(zip.files.date[i] , sort(ref.drk.spc.files.date))) == 1 &
       !is.na(pmatch(zip.files.date[i] , sort(ref.drk.spc.files.date)))){
      zip.files.date[i] <- NA}
  }

  zip.files <- zip.files[!is.na(zip.files.date)]
  zip.files.date <- zip.files.date[!is.na(zip.files.date)]

  if( exists( "zip.files.to.check" )){

    zip.files <- c(zip.files, zip.files.to.check)
    zip.files.date <- c(zip.files.date, substr(zip.files.to.check, 1, 10))

    rm(zip.files.to.check)
  }

  # Stop when all files are already processed ####
  if(length(zip.files.date)==0){message(paste0("No new files found for ",
                                               as.character(customerlist$customer)," ",
                                               as.character(customerlist$location)," ",
                                               as.character(customerlist$line)))
    setwd(csv.dir)
    stop_quietly()
  }

  # zip.files ####
  zip.files <- zip.files[zip.files %in% paste0(zip.files.date, ".zip")]

  # csv.files ####
  csv.files <- csv.files[substr(csv.files, 1, 10) %in% zip.files.date]

  # create unzip.dir ####
  setwd(unzip.dir)
  suppressWarnings(dir.create(unzip.dir))
  suppressWarnings(dir.create(paste(unzip.dir, "unzip", sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[ ,"customer"]), sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[ ,"customer"]),as.character(customerlist[ , "location"]), sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[ ,"customer"]),as.character(customerlist[ , "location"]),as.character(customerlist[ , "line"]), sep = "/")))

  suppressWarnings(dir.create(unzip.dir))
  suppressWarnings(dir.create(paste(unzip.dir, "zip", sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[ , "customer"]), sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[ , "customer"]),as.character(customerlist[ , "location"]), sep = "/")))
  suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[ , "customer"]),as.character(customerlist[ , "location"]),as.character(customerlist[ , "line"]), sep = "/")))

  zip.dir.copy <- paste(unzip.dir, "zip",as.character(customerlist[ , "customer"]),as.character(customerlist[ , "location"]),as.character(customerlist[ , "line"]), sep = "/")
  unzip.dir.copy <- paste(unzip.dir, "unzip",as.character(customerlist[ , "customer"]),as.character(customerlist[ , "location"]),as.character(customerlist[ , "line"]), sep = "/")

  if(identical(zip.files, character(0))) stop_quietly()

  message(paste0(as.character(customerlist$customer)," ",
                 as.character(customerlist$location)," ",
                 as.character(customerlist$line)," ",
                 zip.files," to progress \n"))

  setwd(wd$master)
  if( !dir( pattern = "7z.exe")[1] == "7z.exe" ) stop( "7z.exe is missing")

  # unzip ####
  for(j in 1:length(zip.files)){

    setwd(wd$master)

    dir.create( paste(unzip.dir.copy,gsub(".zip$","",unlist(zip.files)[j]),sep="/") )

    suppressMessages(
      system(paste0("7z.exe e "
                    , zip.dir, unlist( zip.files[ j ] )
                    , " -o"
                    , paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/")
                    , " *.log -r -aoa")
             , show.output.on.console = F)
    )


    # ref ####
    setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

    ref <- list()
    ref$files.dir <- list.files(pattern="-R.log$", recursive = T)

    ref$dat <-  suppressWarnings(lapply(ref$files.dir,function(x) fread(x
                                                                        , header = F
                                                                        , sep = "="
                                                                        , dec = ","
                                                                        , encoding = "Latin-1")))

    ref$files.dir <- ref$files.dir[ which( unlist( lapply( ref$dat, length)) > 0 ) ]
    ref$dat <- ref$dat[ which( unlist( lapply( ref$dat, length)) > 0 ) ]

    ref$dat <- lapply(ref$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

    zerotime <- which(do.call(rbind,lapply(ref$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
    ref$date <- lapply(ref$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        ref$date[[zerotime[i]]] <- format(round(ref$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
    }

    ref$dat <- lapply(ref$dat,function(x) x[-1,])
    ref$para <- lapply(ref$dat,function(x) x$V1)

    ref$dat <-  lapply(ref$dat,function(x) x$V2 <- gsub(",",".",x$V2))

    ref$Produktnummer <- lapply(ref$para,function(x) grep("Produkt",x))
    ref$Produktnummer <- mapply(function(x , y) x[y],ref$dat,ref$Produktnummer)

    if(customerlist$LG=="SG"){
      suppressWarnings(ref$spc <- lapply(unlist(ref$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

      ref$wl <- lapply(ref$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
      ref$spc <- lapply(ref$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
    } else{
      suppressWarnings(ref$spc <- lapply(unlist(ref$files.dir),function(x) readLines(x)))

      ref$wl <- lapply(ref$spc, function(x) x[grep("Spektrum=", x)])
      ref$spc <- lapply(ref$spc, function(x) x[grep("Spektrum=", x)+1])

      ref$wl <- lapply(ref$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
      ref$spc <- lapply(ref$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
    }

    if(length(ref$dat) > 0){
      if(customerlist$LG!="SG"){ ref$dat <- lapply(ref$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
      for(i in 1:length(ref$dat)) colnames(ref$dat[[i]]) <- ref$para[[i]]

      }

      if(customerlist$LG=="SG"){
        sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
        ref$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                           , dat = ref$dat
                           , para = ref$para
                           , SIMPLIFY = F)

        for(i in 1:length(ref$dat1)) colnames(ref$dat1[[i]]) <- ref$para[[i]][ref$para[[i]] %in% sgnames]

        ref$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                           , dat = ref$dat
                           , para = ref$para
                           , SIMPLIFY = F)

        for(i in 1:length(ref$dat2)) colnames(ref$dat2[[i]]) <- ref$para[[i]][!ref$para[[i]] %in% sgnames]

        ref$dat <- mapply( function( x , y) data.table( x,y )
                           , x = ref$dat1
                           , y = ref$dat2
                           , SIMPLIFY = F)
      }

      ref$final <- data.table(datetime = do.call(rbind,lapply(ref$date,as.character))
                              , date = as.Date.character(do.call(rbind,lapply(ref$date,as.character)),tz="UTC",format="%Y-%m-%d")
                              , time = substr(do.call(rbind,lapply(ref$date,as.character)),12,19)
                              , rbindlist(ref$dat, fill = T)
                              , do.call(rbind, ref$spc))

      colnames(ref$final)[1:3] <- c("datetime","date","time")
      rangep <- which(colnames(ref$final) == "Mittelungen") : ncol(ref$final)
      suppressWarnings(colnames(ref$final)[rangep][which(substr(colnames(ref$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(ref$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(ref$wl))))

      as.character( unlist( customerlist[o , "customer"]) )
      customerlist %>% pull(customer)

      fwrite(ref$final
             , paste0(ref.dir
                      , gsub(".zip","",unlist(zip.files)[j]), "_"
                      , as.character( unlist( customerlist[ , "customer"])), "_"
                      , as.character( unlist( customerlist[ , "location"])), "_"
                      , as.character( unlist( customerlist[ , "line"])), "_ref.csv")
             , sep = ";", dec = ",")
    }
    # drk ####
    setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

    drk <- list()
    drk$files.dir <- list.files(pattern="-D.log$", recursive = T)

    drk$dat <-  suppressWarnings(lapply(drk$files.dir,function(x) fread(x
                                                                        , header = F
                                                                        , sep = "="
                                                                        , dec = ","
                                                                        , encoding = "Latin-1")))

    drk$files.dir <- drk$files.dir[ which( unlist( lapply( drk$dat, length)) > 0 ) ]
    drk$dat <- drk$dat[ which( unlist( lapply( drk$dat, length)) > 0 ) ]

    drk$dat <- lapply(drk$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

    zerotime <- which(do.call(rbind,lapply(drk$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
    drk$date <- lapply(drk$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        drk$date[[zerotime[i]]] <- format(round(drk$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
    }

    drk$dat <- lapply(drk$dat,function(x) x[-1,])
    drk$para <- lapply(drk$dat,function(x) x$V1)

    drk$dat <-  lapply(drk$dat,function(x) x$V2 <- gsub(",",".",x$V2))

    drk$Produktnummer <- lapply(drk$para,function(x) grep("Produkt",x))
    drk$Produktnummer <- mapply(function(x , y) x[y],drk$dat,drk$Produktnummer)

    if(customerlist$LG=="SG"){
      suppressWarnings(drk$spc <- lapply(unlist(drk$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

      drk$wl <- lapply(drk$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
      drk$spc <- lapply(drk$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
    } else{
      suppressWarnings(drk$spc <- lapply(unlist(drk$files.dir),function(x) readLines(x)))

      drk$wl <- lapply(drk$spc, function(x) x[grep("Spektrum=", x)])
      drk$spc <- lapply(drk$spc, function(x) x[grep("Spektrum=", x)+1])

      drk$wl <- lapply(drk$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
      drk$spc <- lapply(drk$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
    }

    if(length(drk$dat) > 0){

      if(customerlist$LG!="SG"){ drk$dat <- lapply(drk$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
      for(i in 1:length(drk$dat)) colnames(drk$dat[[i]]) <- drk$para[[i]]
      }

      if(customerlist$LG=="SG"){
        sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
        drk$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                           , dat = drk$dat
                           , para = drk$para
                           , SIMPLIFY = F)

        for(i in 1:length(drk$dat1)) colnames(drk$dat1[[i]]) <- drk$para[[i]][drk$para[[i]] %in% sgnames]

        drk$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                           , dat = drk$dat
                           , para = drk$para
                           , SIMPLIFY = F)

        for(i in 1:length(drk$dat2)) colnames(drk$dat2[[i]]) <- drk$para[[i]][!drk$para[[i]] %in% sgnames]

        drk$dat <- mapply( function( x , y) data.table( x,y )
                           , x = drk$dat1
                           , y = drk$dat2
                           , SIMPLIFY = F)
      }

      drk$final <- data.table(datetime = do.call(rbind,lapply(drk$date,as.character))
                              , date = as.Date.character(do.call(rbind,lapply(drk$date,as.character)),tz="UTC",format="%Y-%m-%d")
                              , time = substr(do.call(rbind,lapply(drk$date,as.character)),12,19)
                              , rbindlist(drk$dat, fill = T)
                              , do.call(rbind, drk$spc))

      colnames(drk$final)[1:3] <- c("datetime","date","time")
      rangep <- which(colnames(drk$final) == "Mittelungen") : ncol(drk$final)
      suppressWarnings(colnames(drk$final)[rangep][which(substr(colnames(drk$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(drk$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(drk$wl))))

      fwrite(drk$final
             , paste0(drk.dir
                      , gsub(".zip","",unlist(zip.files)[j]), "_"
                      , as.character( unlist( customerlist[ , "customer"])), "_"
                      , as.character( unlist( customerlist[ , "location"])), "_"
                      , as.character( unlist( customerlist[ , "line"])), "_drk.csv")
             , sep = ";", dec = ",")
    }
    # spc ####
    setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

    spc <- list()
    spc$files.dir <- list.files(pattern="-M.log$", recursive = T)

    spc$dat <-  suppressWarnings(lapply(spc$files.dir,function(x) fread(x
                                                                        , header = F
                                                                        , sep = "="
                                                                        , dec = ","
                                                                        , encoding = "Latin-1")))

    spc$files.dir <- spc$files.dir[ which( unlist( lapply( spc$dat, length)) > 0 ) ]
    spc$dat <- spc$dat[ which( unlist( lapply( spc$dat, length)) > 0 ) ]

    spc$dat <- lapply(spc$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

    zerotime <- which(do.call(rbind,lapply(spc$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
    spc$date <- lapply(spc$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        spc$date[[zerotime[i]]] <- format(round(spc$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
    }

    spc$dat <- lapply(spc$dat,function(x) x[-1,])
    spc$para <- lapply(spc$dat,function(x) x$V1)

    spc$dat <-  lapply(spc$dat,function(x) x$V2 <- gsub(",",".",x$V2))

    spc$Produktnummer <- lapply(spc$para,function(x) grep("Produkt",x))
    spc$Produktnummer <- mapply(function(x , y) x[y],spc$dat,spc$Produktnummer)

    if(customerlist$LG=="SG"){
      suppressWarnings(spc$spc <- lapply(unlist(spc$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

      spc$wl <- lapply(spc$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
      spc$spc <- lapply(spc$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
    } else{
      suppressWarnings(spc$spc <- lapply(unlist(spc$files.dir),function(x) readLines(x)))

      spc$wl <- lapply(spc$spc, function(x) x[grep("Spektrum=", x)])
      spc$spc <- lapply(spc$spc, function(x) x[grep("Spektrum=", x)+1])

      spc$wl <- lapply(spc$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
      spc$spc <- lapply(spc$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
    }

    if(length(spc$dat) > 0){
      if(customerlist$LG!="SG"){ spc$dat <- lapply(spc$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
      for(i in 1:length(spc$dat)) colnames(spc$dat[[i]]) <- spc$para[[i]]
      }

      if(customerlist$LG=="SG"){
        sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
        spc$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                           , dat = spc$dat
                           , para = spc$para
                           , SIMPLIFY = F)

        for(i in 1:length(spc$dat1)) colnames(spc$dat1[[i]]) <- spc$para[[i]][spc$para[[i]] %in% sgnames]

        spc$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                           , dat = spc$dat
                           , para = spc$para
                           , SIMPLIFY = F)
        for(i in 1:length(spc$dat2)) colnames(spc$dat2[[i]]) <- spc$para[[i]][!spc$para[[i]] %in% sgnames]

        spc$dat <- mapply( function( x , y) data.table( x,y )
                           , x = spc$dat1
                           , y = spc$dat2
                           , SIMPLIFY = F)
      }

      spc$final <- data.table(datetime = do.call(rbind,lapply(spc$date,as.character))
                              , date = as.Date.character(do.call(rbind,lapply(spc$date,as.character)),tz="UTC",format="%Y-%m-%d")
                              , time = substr(do.call(rbind,lapply(spc$date,as.character)),12,19)
                              , rbindlist(spc$dat, fill = T)
                              , do.call(rbind, spc$spc))

      colnames(spc$final)[1:3] <- c("datetime","date","time")
      rangep <- which(colnames(spc$final) == "Mittelungen") : ncol(spc$final)
      suppressWarnings(colnames(spc$final)[rangep][which(substr(colnames(spc$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(spc$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(spc$wl))))

      fwrite(spc$final
             , paste0(spc.dir
                      , gsub(".zip","",unlist(zip.files)[j]), "_"
                      , as.character( unlist( customerlist[ , "customer"])), "_"
                      , as.character( unlist( customerlist[ , "location"])), "_"
                      , as.character( unlist( customerlist[ , "line"])), "_spc.csv")
             , sep = ";", dec = ",")
    }
    message(paste(gsub(".zip","",unlist(zip.files)[j]),"for",as.character(customerlist$customer),as.character(customerlist$location),as.character(customerlist$line),"completed, time =",substr(Sys.time(), 1, 19),length(zip.files)-j,"to go"))

  }

  setwd(csv.dir)
  unallowed <- c("I:","K:","L:","M:","N:","P:","R:","S:","W:")
  if(length(unique(match(unallowed,substr(zip.dir.copy,1,2))))<2){
    unlink(paste(zip.dir.copy,"*",sep="/"),recursive = T,force=T)
    unlink(paste(unzip.dir.copy,"*",sep="/"),recursive = T,force=T)
  } # else {textreturn <- "No files are removed as you try to automatically remove files from the server"}

  message(paste(as.character(customerlist$customer),as.character(customerlist$location),as.character(customerlist$line),"finished"))

  message("Create production list per day")

  for(i in substr(unique(unlist(zip.files)), 1, 10)){
    produkt_per_day_year(customer = customerlist$customer
                         , location = customerlist$location
                         , line = customerlist$line
                         , LG = "2"
                         , year = year( i )
                         , date_file = i
                         , dir_wd = dir_wd)
  }
  message("finished")

}

# debug(unzip.LG2.7z)
# unzip.LG2.7z(date = dt$date
#              , line = NA
#              , unzip.dir = "C:/csvtemp"
#              , dir_wd = wd
#              , customerlist = dt_customer[ dt_customer$customer != "Roche" , ])
# undebug(unzip.LG2.7z)
