# Column names of your SQL-Export file
namessql_base <- c("DateTime","measurementTypeCode","DTproductName","MixerNumber","UNSB_Name","Modell",
                   "accumulations","lightPath","Reported_Value","Scores","Y_Predicted","YDeviation",
                   "Y_Predicted_Corr","Bias","Hotellingt2","Hotellingt2Lim","FResXSamp","FResLims","integrationTime","SpectrometerTemperature","FluidFlow","FluidTemperature",
                   "FluidPressure","TimeSinceBGD","Signal_ProductFlows")
wl <- 190:598

LG3_transform_SQL_to_csv <- function(csv_file,
                                     file_directory,
                                     namessql = namessql_base,
                                     wl = 190:598,
                                     export_server = T,
                                     G10 = F,
                                     sqlquestion = NA){

  customer <- input$customer
  location <- input$location
  line <- input$unit

  if(sqlquestion %in% c("drk","production","reference","mea")!=T) stop("No sqlquestion defined")

  export_directory <- service_backup_path(input$customer,input$location,input$line, dir_wd = wd)
  setwd(file_directory)

  # sql list
  sql <- list()

  sqlt <- lapply(csv_file,function(x) fread(x,head=F,sep=";",dec=",",fill=T, check.names = F, colClasses = "character", nrows = 1))

  # Test for NULL
  for(i in 1:length(sqlt)) if(length(grep("NULL",sqlt[[i]][1,]))==0 & G10==F) sql[[i]] <- lapply(csv_file,function(x) fread(x,head=F,sep=";",dec=",",fill=T, check.names = F, colClasses = "character"))
  for(i in 1:length(sqlt)) if(length(grep("NULL",sqlt[[i]][1,]))==0 & G10==T) sql[[i]] <- lapply(csv_file,function(x) fread(x))

  for(i in 1:length(sqlt)){
    if(length(grep("NULL",sqlt[[i]][1,]))>0 & G10==F){
      # nrows <- nrow(read.csv2(csv_file,head=F))
      # for(i in 1:nrows){
      #   sql <- lapply(csv_file,function(x) read.csv2(x,head=F,sep=";",dec=",",fill=T, check.names = F, colClasses = c("character",NULL),
      #                                                skip=i,nrows = 1))
      #   if(length(grep("NULL",sql[[1]][1,]))==0) break
      # }
      sql <- lapply(csv_file,function(x) fread(x,head=F,sep=";",dec=",",fill=T, check.names = F, colClasses = c("character",NULL),
                                               skip=i+1))
    }
  }

  for(i in 1:length(sqlt)){ if(length(grep("NULL",sqlt[[i]][1,]))>0 & G10==T){
    nrows <- nrow(read.csv2(csv_file,head=F))
    for(i in 1:nrows){
      sql <- lapply(csv_file,function(x) read.csv2(x,skip=i,nrows = 1))
      if(length(grep("NULL",sql[[1]][1,]))==0) break
    }
    sql <- lapply(csv_file,function(x) read.csv2(x,head=F,sep=";",dec=",",fill=T, check.names = F, colClasses = c("character",NULL),
                                                 skip=i+1))
  }
  }


  # rbind ####
  sql_raw <- do.call(plyr::rbind.fill,sql)
  sql_raw <- data.frame(sql_raw)
  iii <- which(sql_raw[ , ncol(sql_raw)] != "")[1]
  if(G10==T){sql_raw <- sql_raw[-1,]}

  # search for # ####
  # ppp <- apply(sql_raw,1,function(x) length(which(as.numeric(gregexpr("#",x))>0)))
  # if(length(which(ppp>0))>0) sql_raw <- sql_raw[-which(ppp>0),]

  # remove {} ####
  subbrace <- which(!is.na(as.numeric(apply(sql_raw[iii,],2,function(x) grep("\\{",x)))))
  sql_raw[,subbrace] <- suppressWarnings(apply(data.frame(sql_raw[,subbrace]),2,function(x) as.numeric(gsub(",",".",gsub("\\{","",x)))))
  subbrace2 <- which(!is.na(as.numeric(apply(sql_raw[iii,],2,function(x) grep("\\}",x)))))
  sql_raw[,subbrace2] <- suppressWarnings(apply(data.frame(sql_raw[,subbrace2]),2,function(x) as.numeric(gsub(",",".",gsub("\\}","",x)))))
  if(G10==F){colnames(sql_raw) <- c(namessql,rep(wl,length(sqlquestion)))} else {colnames(sql_raw) <- c("ID",namessql,rep(wl,length(sqlquestion)))}
  # make columns numeric
  numcol <- suppressWarnings(which(!is.na(as.numeric(gsub(",",".",apply(sql_raw[iii ,],2,paste))))))
  suppressWarnings(sql_raw[,numcol] <- apply(sql_raw[,numcol],2,function(x) as.numeric(gsub(",",".",as.character(x)))))

  # order by date ####
  sql_raw <- sql_raw[order(sql_raw$DateTime),]

  # split data
  if(G10==F){sql$data <- sql_raw[,1:(subbrace[1]-1)]}else{sql$data <- sql_raw[,2:(subbrace[1]-1)]}

  if(length(sqlquestion)==1 && sqlquestion %in% "production") sql$spc <- sql_raw[,subbrace[1]:subbrace2[1]]
  if(length(sqlquestion)==1 && sqlquestion %in% "drk") sql$drk <- sql_raw[,subbrace[1]:subbrace2[1]]
  if(length(sqlquestion)==1 && sqlquestion %in% "reference") sql$ref <- sql_raw[,subbrace[1]:subbrace2[1]]
  if(length(sqlquestion)==1 && sqlquestion %in% "mea") sql$mea <- sql_raw[,subbrace[1]:subbrace2[1]]

  if(length(sqlquestion)>1) stop("Skript does not yet work for more than one spectra type, i.e. abs, drk, bgd or mea")
  rm(sql_raw)

  sql$data$datetime <- as.POSIXct(as.character(sql$data$DateTime),format=c("%Y-%m-%d %H:%M:%OS"),tz="Europe/Berlin")
  sql$data$date <- as.Date(sql$data$DateTime,tz="Europe/Berlin")
  sql$data$time <- strftime(sql$data$DateTime, format = "%H:%M:%S", tz = "Europe/Berlin")

  sql$data$DateTime <- NULL

  sql$date$location <- location
  sql$date$line <- line

  sql$data <- sql$data[moveme(names(sql$data), "location line datetime date time first")]
  suppressWarnings(sql$data[,which(names(sql$data)=="accumulations"):ncol(sql$data)] <- apply(sql$data[,which(names(sql$data)=="accumulations"):ncol(sql$data)],2,function(x) as.numeric(gsub(",",".",x))))

  if(length(sqlquestion)==1 && sqlquestion %in% "production") sql$spc <- t(sql$spc)
  if(length(sqlquestion)==1 && sqlquestion %in% "drk") sql$drk <- t(sql$drk)
  if(length(sqlquestion)==1 && sqlquestion %in% "reference") sql$ref <- t(sql$ref)
  if(length(sqlquestion)==1 && sqlquestion %in% "mea") sql$mea <- t(sql$mea)

  # export ####
  if(length(which(sqlquestion=="reference"))==1){
    refdatamean <- apply(sql$ref,2,mean)
    refdatasd <- apply(sql$ref,2,sd)
    if(length(which(!duplicated(refdatamean)))!=length(which(!duplicated(refdatasd)))){
      if(length(which(!duplicated(refdatamean)))>length(which(!duplicated(refdatasd)))){ dupref <- which(!duplicated(refdatasd)) } else{ dupref <- which(!duplicated(refdatamean))}
    } else dupref <- as.numeric(which(!duplicated(refdatamean)))

    refdata <- sql$data[dupref,]
    exportref <- t(sql$ref[,dupref]);rm(refdatamean,refdatasd,dupref)
    suppressWarnings(export_ref <- data.frame(refdata,exportref))
    colnames(export_ref)[grep("NA",colnames(export_ref))] <- wl

    # export_ref$Modell[which(export_ref$SampleName == "Coca-Cola" & export_ref$Modell == "NULL")] <- "LG3_GS2_CC_CC(CP)_V01"

    suppressWarnings(colnames(export_ref)[which(!is.na(as.numeric(gsub("X","",colnames(export_ref)))))] <- gsub("X","",colnames(export_ref)[which(!is.na(as.numeric(gsub("X","",colnames(export_ref)))))]))
  }

  if(length(which(sqlquestion=="drk"))==1){
    drkdatamean <- apply(sql$drk,2,mean)
    drkdatasd <- apply(sql$drk,2,sd)
    if(length(which(!duplicated(drkdatamean)))!=length(which(!duplicated(drkdatasd)))){
      if(length(which(!duplicated(drkdatamean)))>length(which(!duplicated(drkdatasd)))){ dupdrk <- which(!duplicated(drkdatasd)) } else{ dupdrk <- which(!duplicated(drkdatamean))}
    } else dupdrk <- as.numeric(which(!duplicated(drkdatamean)))

    drkdata <- sql$data[dupdrk,]
    exportdrk <- t(sql$drk[,dupdrk]);rm(drkdatamean,drkdatasd,dupdrk)
    suppressWarnings(export_drk <- data.frame(drkdata,exportdrk))
    colnames(export_drk)[grep("NA",colnames(export_drk))] <- wl

    # export_drk$Modell[which(export_drk$SampleName == "Coca-Cola" & export_drk$Modell == "NULL")] <- "LG3_GS2_CC_CC(CP)_V01"


    suppressWarnings(colnames(export_drk)[which(!is.na(as.numeric(gsub("X","",colnames(export_drk)))))] <- gsub("X","",colnames(export_drk)[which(!is.na(as.numeric(gsub("X","",colnames(export_drk)))))]))

  }

  if(length(which(sqlquestion=="production"))==1){
    export_spc <- data.frame(sql$data,t(sql$spc))

    # export_spc$Modell[which(export_spc$SampleName == "Coca-Cola" & export_spc$Modell == "NULL")] <- "LG3_GS2_CC_CC(CP)_V01"

    colnames(export_spc)[grep("NA",colnames(export_spc))] <- wl
    suppressWarnings(colnames(export_spc)[which(!is.na(as.numeric(gsub("X","",colnames(export_spc)))))] <- gsub("X","",colnames(export_spc)[which(!is.na(as.numeric(gsub("X","",colnames(export_spc)))))]))
  }

  if(export_server==T){
    if(length(which(sqlquestion=="reference"))==1){
      for(i in 1:length(unique(export_ref$date))){
        reftoexport <- export_ref[which(export_ref$date==unique(export_ref$date)[i]),]
        setwd(export_directory)
        setwd("./ref")
        if(length(which(substr(dir(),1,10)==unique(reftoexport$date)))>0){ next }
        # reftomerge <- read.csv2(dir()[which(gsub("_ref.csv","",dir())==unique(reftoexport$date))])
        # suppressWarnings(colnames(reftomerge)[which(!is.na(as.numeric(gsub("X","",colnames(reftomerge)))))] <- gsub("X","",colnames(reftomerge)[which(!is.na(as.numeric(gsub("X","",colnames(reftomerge)))))]))
        #
        # reftomerge$date <- as.Date(reftomerge$date)
        # reftomerge$datetime <- as.POSIXct(reftomerge$datetime,format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")
        #
        # refmerged <- rbind(reftomerge,reftoexport)
        # refmerged <- refmerged[order(refmerged$datetime),]
        #
        # refmerged <- refmerged[which(!duplicated.data.frame(refmerged[,150:350])),]
        #
        # reftoexport <- refmerged
        # }
        fwrite(reftoexport,paste0(unique(reftoexport$date),"_",
                                  customer,"_",
                                  location,"_",
                                  line,"_ref.csv"),row.names = F, sep = ";", dec = ",")
      }
      message(paste("REF exported to",getwd()))
    }
    if(length(which(sqlquestion=="drk"))==1){
      for(i in 1:length(unique(export_drk$date))){
        drktoexport <- export_drk[which(export_drk$date==unique(export_drk$date)[i]),]
        setwd(export_directory)
        setwd("./drk")
        if(length(which(substr(dir(),1,10)==unique(drktoexport$date)))>0){ next }
        # drktomerge <- read.csv2(dir()[which(gsub("_drk.csv","",dir())==unique(drktoexport$date))])
        # suppressWarnings(colnames(drktomerge)[which(!is.na(as.numeric(gsub("X","",colnames(drktomerge)))))] <- gsub("X","",colnames(drktomerge)[which(!is.na(as.numeric(gsub("X","",colnames(drktomerge)))))]))
        #
        # drktomerge$date <- as.Date(drktomerge$date)
        # drktomerge$datetime <- as.POSIXct(drktomerge$datetime,format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")
        #
        # drkmerged <- rbind(drktomerge,drktoexport)
        # drkmerged <- drkmerged[order(drkmerged$datetime),]
        #
        # drkmerged <- drkmerged[which(!duplicated.data.frame(drkmerged[,150:350])),]
        #
        # drktoexport <- drkmerged
        # }
        fwrite(drktoexport,paste0(unique(drktoexport$date),"_",
                                  customer,"_",
                                  location,"_",
                                  line,"_drk.csv"),row.names = F, sep = ";", dec = ",")
      }
      message(paste("drk exported to",getwd()))
    }
    if(length(which(sqlquestion=="production"))==1){
      for(i in 1:length(unique(export_spc$date))){
        spctoexport <- export_spc[which(export_spc$date==unique(export_spc$date)[i]),]
        setwd(export_directory)
        setwd("./spc")
        if(length(which(substr(dir(),1,10)==unique(spctoexport$date)))>0){
          spctomerge <- read.csv2(dir()[which(gsub("_spc.csv","",dir())==unique(spctoexport$date))])

          #ppp <- apply(sql_raw,1,function(x) length(which(as.numeric(gregexpr("#",x))>0)))
          #if(length(which(ppp>0))>0) sql_raw <- sql_raw[-which(ppp>0),]

          suppressWarnings(colnames(spctomerge)[which(!is.na(as.numeric(gsub("X","",colnames(spctomerge)))))] <- gsub("X","",colnames(spctomerge)[which(!is.na(as.numeric(gsub("X","",colnames(spctomerge)))))]))

          spctomerge$date <- as.Date(spctomerge$date)
          spctomerge$datetime <- as.POSIXct(spctomerge$datetime,format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")

          spcmerged <- rbind(spctomerge,spctoexport)
          spcmerged <- spcmerged[order(spcmerged$datetime),]

          spcmerged <- spcmerged[which(!duplicated.data.frame(spcmerged[,150:350])),]

          spctoexport <- spcmerged
        }
        write.csv2(spctoexport,paste0(unique(spctoexport$date),"_",
                                      customer,"_",
                                      location,"_",
                                      line,"_spc.csv"),row.names = F)
      }
      message(paste("SPC exported to",getwd()))
    }
  }
}
