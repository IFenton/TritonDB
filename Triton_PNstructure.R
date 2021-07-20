
# function for putting data into the correct structure for the PN  --------

# input: 
# - excel / csv file in one sheet; - details on which columns contain sp. info (and genus if separate); - details on ODP or not. If so, then leg, site, hole, core, section, interval.top; - depth; - age; - age model; - lat; - long; - paleolat; - paleolong; - abundance; - sample group abundance; - preservation; - source; - Database

# output: a dataframe with a predictable set of headings:
# Database level:
# - Database; - date stamp; - inputter
# site level:
# - Age model; - latitude; - longitude; - ODP site?; - leg; - site; - hole; - core; - Water depth; - Source
# Sample level:
# - Age; - Depth; - Section; - Interval top; - Interval bottom; - Paleolat; - Paleolong; - Sample type; - Preservation; - Processing; - Total IDd; 
# Species level:
# - Species (corrected for synonyms); - Abundance; - Abundance categories; - Sample.Group.Abundance

PNstructure <- function(data, input.init, database.source, ODP = "Y", sheet.no = 1, model = "MATTHEWS2016", choices = NULL, pal.lat.full = TRUE, multiple.abun = FALSE, db.ID = database.source) {
  require("svDialogs")
  library(devtools)
  #install_github("macroecology/mapast")
  library(mapast)
  
  library(sp)
  #source("Code/Triton_ForamSynonyms.R")
  
  # create a choices list
  if (is.null(choices)) {
    choices <- list()
  }
  # read in the file
  if (is.character(data)) {
    data <- read.xlsx(data, sheet = sheet.no)
  }
  
  # # append database information ----------------
  # (database source, date stamp, inputter)
  data <- cbind(data, db.source = database.source, date = Sys.Date(), person = input.init, dbID = db.ID, stringsAsFactors = FALSE)
  
  # add a column of why the data was collected
  if (is.null(choices$reason)) {
    choices$reason <- dlg_list(c("Community analysis", "Selected species", "Proxies", "Extreme event", "Biostratigraphy", "Unspecified"), title = "What is the reason for the data collection?")$res
  }
  data$reason <- factor(choices$reason, levels = c("Community analysis", "Selected species", "Proxies", "Extreme event", "Biostratigraphy", "Unspecified"))
  
  # # rename site-level columns ----------------
  # Source ----------------------------------
  if (is.null(choices$source)) {
    check <- FALSE
    while(check == FALSE) {
      print(head(data))
      choices$source <- dlg_list(c("Yes", "No"), title = "Does the dataset contain the source reference?")$res
      if (choices$source == "No") {
        choices$source <- readline("Enter the source: ")
        print(paste("So source: ", choices$source, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      } else { # the source reference is one / more columns
        source.cols <- NULL
        source.loop <- "Yes"
        while(source.loop == "Yes") {
          source.cols <- c(source.cols, dlg_list(names(data), title = "Enter the source column")$res)
          source.loop <- dlg_list(c("Yes", "No"), title = "Do you want to enter another column?")$res
        }
        print(paste(c("So source: ", source.cols)))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        choices$source <- source.cols
      }
    }
  }
  
  # if the source is one/more of the data columns
  if (all(choices$source %in% names(data))) {
    if (length(choices$source) == 1) {
      names(data)[names(data) == choices$source] <- "source"
    } else {
      data$source <- apply(data[,choices$source], 1, paste, collapse = ",")
    }
  } else {
    data <- cbind(data, source = choices$source, stringsAsFactors = FALSE)
  }
  
  if (is.null(choices$year)) {
    # get the year from the citation
    choices$year <- as.numeric(gsub("^.*(19\\d{2}|20\\d{2}).*$", "\\1", data$source))
  }
  data <- cbind(data, year = choices$year)
  
  # age model -----------------------------------------
  if (is.null(choices$age.model)) {
    while(is.null(choices$age.model)) {
      # print metadata with potentially relevant info for the age model
      print(c(unique(data[,grep("zone", names(data), ignore.case = TRUE)])))
      choices$age.model <- dlg_list(c("Wade2011 (P,E,O,M,Pl,Pt)", "Berggren2005 (P,E,O)", "Berggren1995 (P,M,Pl,Pt)", "Berggren1977 (Pl)", "GTSBlow1969 (P,N)", "C&P1997/P&C1997 (P,N)", "B&M1988 (P)", "K&S1983 (N)", "Blow1969 (P13-N12)", "Ericson1968 (P-Z)", "Huber2005 (AP,AE,AO)", "Huber1991 (AP)", "Raffi2006 (NN)", "GTS2012 (MoveDB uses this)", "Other", "Extra"), title = "What is the age model?")$res
      if (length(choices$age.model) == 0)
        choices$age.model <- "Other"
      # remove the bit in brackets (i.e. after the space)
      choices$age.model <- gsub( "\\ .*$", "", choices$age.model)
    }
  }
  data <- cbind(data, age.model = choices$age.model, stringsAsFactors = FALSE)
  

  # site info ---------------------------------------------------------------
  # If not an ODP site, create blank columns
  if (is.null(choices$holeID) & !is.null(choices$coreID)) {
    choices$holeID <- choices$coreID
    choices$coreID <- NULL
  }
  if (ODP == "N") {
    data$leg <- data$site <- data$hole <- data$core <- data$section <- data$sample.top <- as.character(NA)
    if (is.null(choices$holeID)) {
      check <- FALSE
      while(check == FALSE) {
        choices$holeID <- dlg_list(names(data), title = "Which column is the hole ID?")$res
        print(paste("So hole ID: ", database.source, "$", choices$holeID, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    names(data)[names(data) == choices$holeID] <- "holeID"
  } else { 
    # if it is an ODP site, then identify columns
    if (is.null(choices$leg) | is.null(choices$site) | is.null(choices$hole) | is.null(choices$core)) {
      check <- FALSE
      while(check == FALSE) {
        choices$leg <- dlg_list(names(data), title = "Which column is the ODP leg?")$res
        if (length(choices$leg) == 0)
          choices$leg <- readline("Enter the leg: ")
        choices$site <- dlg_list(names(data), title = "Which column is the ODP site?")$res
        if (length(choices$site) == 0)
          choices$site <- readline("Enter the site: ")
        choices$hole <- dlg_list(names(data), title = "Which column is the ODP hole?")$res
        if (length(choices$hole) == 0)
          choices$hole <- readline("Enter the hole: ")
        choices$core <- dlg_list(names(data), title = "Which column is the ODP core?")$res
        if (length(choices$core) == 0)
          choices$core <- readline("Enter the core: ")
        print(paste("So leg: ", choices$leg, sep = ""))
        print(paste("site: ", choices$site, sep = ""))
        print(paste("hole: ", choices$hole, sep = ""))
        print(paste("core: ", choices$core, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    # if the odp info is in columns
    if (choices$leg %in% names(data) & choices$site %in% names(data) & choices$hole %in% names(data)) {
      names(data)[names(data) == choices$leg] <- "leg"
      names(data)[names(data) == choices$site] <- "site"
      names(data)[names(data) == choices$hole] <- "hole"
      data$hole <- gsub("^.*_", "", data$hole) # remove the site info from the hole info
      data$leg <- as.character(data$leg)
      data$site <- as.character(data$site)
    } else {
      # otherwise add it in
      data <- cbind(data, leg = as.character(choices$leg), site = as.character(choices$site), hole = as.character(choices$hole), stringsAsFactors = FALSE)
    }
    # core info
    if (choices$core %in% names(data)) {
      names(data)[names(data) == choices$core] <- "core"
      data$core <- as.character(data$core)
    } else {
      data <- cbind(data, core = as.character(choices$core), stringsAsFactors = FALSE)
    }
    
    # hole ID (if it exists apart from the leg / hole)
    if (is.null(choices$holeID)) {
      check <- FALSE
      while(check == FALSE) {
        choices$holeID <- dlg_list(names(data), title = "Is any column the hole ID (and not leg/hole etc.)?")$res
        print(paste("So hole ID: ", database.source, "$", choices$holeID, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    if (length(choices$holeID) > 0) {
      names(data)[names(data) == choices$holeID] <- "holeID"
    } else{
      data$holeID <- paste(data$leg, data$hole, sep = "-")
    }
  }

  # coordinates -------------------------------------------------------------
  # latitude
  if (is.null(choices$latitude) | is.null(choices$longitude)) {
    check <- FALSE
    while(check == FALSE) {
      choices$latitude <- dlg_list(names(data), title = "Which column is the latitude?")$res
      choices$longitude <- dlg_list(names(data), title = "Which column is the longitude?")$res
      if ((length(choices$latitude)|length(choices$longitude)) == 0) {
        choices$latitude <- as.numeric(readline("Enter the latitude: "))
        choices$longitude <- as.numeric(readline("And the longitude: "))
        print(paste("So latitude: ", choices$latitude, sep = ""))
        print(paste("longitude: ", choices$longitude, sep = ""))
      } else {
        print(paste("So latitude: ", database.source, "$", choices$latitude, sep = ""))
        print(paste("longitude: ", database.source, "$", choices$longitude, sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$latitude %in% names(data)) {
    names(data)[names(data) == choices$latitude] <- "latitude"
    names(data)[names(data) == choices$longitude] <- "longitude"
  } else {
    data <- cbind(data, latitude = choices$latitude, longitude = choices$longitude)
  }
  
  # Water depth
  if(is.null(choices$water.depth)) {
    check <- FALSE
    while(check == FALSE) {
      choices$water.depth <- dlg_list(names(data), title = "Does any column contain the water depth?")$res
      if (length(choices$water.depth) == 0) {
        choices$water.depth <- readline("Enter the water depth: ")
        if (choices$water.depth == "")
          choices$water.depth <- NA
        print(paste("So water depth: ", choices$water.depth, " m", sep = ""))
      } else {
        print(paste("So water depth: ", database.source, "$", choices$water.depth, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$water.depth %in% names(data)) {
    names(data)[names(data) == choices$water.depth] <- "water.depth"
    data$water.depth <- as.numeric(data$water.depth)
  } else {
    data <- cbind(data, water.depth = as.numeric(choices$water.depth), stringsAsFactors = FALSE)
  }
  
  # Fill Sample-level information ------------------
  # add row ID
  if (!is.null(data$row.num)) {
    data$sampleID <- paste(data$holeID, gsub("R", "", data$row.num), sep = "_")
  } else {
    if (is.null(choices$sampleID)) {
      check <- FALSE
      while(check == FALSE) {
        choices$sampleID <- dlg_list(names(data), title = "Is any column the sample ID (and not leg/hole etc.)?")$res
        print(paste("So sample ID: ", database.source, "$", choices$sampleID, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
      data$sampleID <- data[, names(data) == choices$sampleID]
    } else if (length(choices$sampleID) > 0) {
      data$sampleID <- paste(data$holeID, data[, names(data) == choices$sampleID], sep = "_")
    } else if (sum(!is.na(data$sample.depth)) > 0) {
      data$sampleID <- paste(data$holeID, data$sample.depth, sep = "_")
      if (any(is.na(data$sample.depth))) { # use age rather than depth where necessary
        data$sampleID[is.na(data$sample.depth)] <- paste(data$holeID[is.na(data$sample.depth)], data$age[is.na(data$sample.depth)], sep = ".")
      }
    } else {
      data$sampleID <- data$holeID
    }
  }
  tmp.rowID <- paste(database.source, data$sampleID, sep = "_")
  data$rowID[order(tmp.rowID)] <- paste(sort(tmp.rowID), "_R", unlist(tapply(tmp.rowID, tmp.rowID, function(x) 1:length(x))), sep = "")
  
  # Age  / depth -------------------------------
  if (is.null(choices$sample.age) | is.null(choices$sample.depth)) {
    check <- FALSE
    while(check == FALSE) {
      choices$sample.age <- dlg_list(names(data), title = "Which column is the numeric age?")$res
      if (length(choices$sample.age) == 0) {
        choices$sample.age <- readline("Enter the age: ")
        choices$sample.age <- ifelse(choices$sample.age == "", NA, as.numeric(choices$sample.age))
        # convert the age to a number
        if(choices$age.model == 1) {
          choices$sample.age <- as.numeric(as.character(choices$sample.age))
        }
      }
      choices$sample.depth <- dlg_list(names(data), title = "Which column is the sample depth?")$res
      if (length(choices$sample.depth) == 0)
        choices$sample.depth <- NA
      print(paste("So age: ", choices$sample.age))
      print(paste("sample depth: ", database.source, "$", choices$sample.depth, sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  # what is the zone age
  if (is.null(choices$sample.zone)) {
    check <- FALSE
    while(check == FALSE) {
      choices$sample.zone <- dlg_list(names(data), title = "Which column is the zone age?")$res
      if (length(choices$sample.zone) == 0) {
        choices$sample.zone <- NA
      }
      print(paste("So zone: ", choices$sample.zone, sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  # add age / depth / zone columns
  if (choices$sample.age[1] %in% names(data)) {
    names(data)[names(data) == choices$sample.age] <- "age"
  } else {
    data <- cbind(data, age = as.numeric(as.character(choices$sample.age)))
  }
  if (choices$sample.depth %in% names(data)) {
    names(data)[names(data) == choices$sample.depth] <- "sample.depth"
    data$sample.depth <- as.numeric(data$sample.depth)
  } else {
    data <- cbind(data, sample.depth = as.numeric(choices$sample.depth))
  }
  if (choices$sample.zone %in% names(data)) {
    names(data)[names(data) == choices$sample.zone] <- "zone"
  } else {
    data <- cbind(data, zone = choices$sample.zone, stringsAsFactors = FALSE)
  }
  # for which rows should the zones be used to estimate age
  if (is.null(choices$no.num.age))
    choices$no.num.age <- is.na(data$age)
  
  # set up columns for zone based age data
  data$rng.age <- NA
  data$AM.type <- as.character(NA)
  data$age.st <- as.numeric(NA)
  data$age.en <- as.numeric(NA)
  data$segment <- "s"
  data$age.calc <- factor("Orig", levels = c("Interp", "Magneto", "Model", "Orig", "Zone"))
  data$zon.age <- as.numeric(NA)
  data$age.err <- as.numeric(NA)
  data$int.age <- as.numeric(NA)
  data$err.int.age <- as.numeric(NA)
  data$mod.age <- as.numeric(NA)
  data$r2 <- as.numeric(NA)
  data$n.pts <- as.numeric(NA)
  
  # zone type
  if (is.null(choices$zone.type) & !is.na(choices$sample.zone)) {
    print(unique(data$zone))
    choices$zone.type <- dlg_list(c("Foram", "Nanno", "Magneto", "Other", "Mixed"), title = "What is the age model type?")$res
  }
  if (any(choices$zone.type == "Chrono"))
    choices$zone.type[choices$zone.type == "Chrono"] <- "Magneto"
  
  # for samples where we don't have numeric ages but do have zones, then calculate the ages
  if (any(!is.na(data$zone)) & (any(is.na(choices$sample.age)) | !is.null(choices$zone.ck) | any(is.na(data[, names(data) == "age"])))) {
    dat.zone <- data.frame(zone = data$zone, ocean = factor("IndoPac", levels = c("Atl", "IndoPac")), region = factor("Temp", levels = c("Temp", "Trop")), segment = data$segment, stringsAsFactors = FALSE)
    # get ocean polygons and work out origin of sites to get ocean specific ages
    load("Data/OceanPolygons.RData")
    dat.zone$ocean[point.in.polygon(data$longitude, data$latitude, atlantic.1$x, atlantic.1$y) == 1] <- "Atl"
    
    # work out site region
    if (any(data$latitude < 23.5))
      dat.zone$region[data$latitude < 23.5] <- "Trop"
    
    # identify which zones are not known so they can be added / re-run
    check <- "Yes"
    while(check == "Yes") {
      # get a unique list of zones
      tmp.age <- zones.fun(unique(dat.zone), scheme = choices$age.model, type = choices$zone.type)
      print(tmp.age)
      print("Unknown zones:")
      print(tmp.age$zone[is.na(tmp.age$zon.age)])
      if (is.null(choices$zone.ck))
        check <- dlg_list(c("Yes", "No"), title = "Do you want to make changes and re-run this?")$res
      else
        check <- "No"
    }
    choices$zone.ck <- "No"
    dat.zone$ID <- 1:nrow(dat.zone)
    dat.zone <- merge(dat.zone, tmp.age)
    dat.zone <- dat.zone[order(dat.zone$ID), ]
    dat.zone$sch[dat.zone$sch == "NA"] <- NA
    dat.zone$type[dat.zone$type == "NA"] <- NA
    # # if (any(dat.zone$ocean == "IndoPac")) {
    # #   tmp.zn <- tmp.age[tmp.age$ocean == "IndoPac",]
    # #   dat.zone$age[dat.zone$ocean == "IndoPac"] <- tmp.zn$zon.age[match(dat.zone$zone[dat.zone$ocean == "IndoPac"], tmp.zn$zone)]
    # #   dat.zone$age.rng[dat.zone$ocean == "IndoPac"] <- tmp.zn$age.rng[match(dat.zone$zone[dat.zone$ocean == "IndoPac"], tmp.zn$zone[tmp.zn$ocean == "IndoPac"])]
    # #   dat.zone$AM.type[dat.zone$ocean == "IndoPac"] <- tmp.zn$type[match(dat.zone$zone[dat.zone$ocean == "IndoPac"], tmp.zn$zone[tmp.zn$ocean == "IndoPac"])]
    # #   dat.zone$age.model[dat.zone$ocean == "IndoPac"] <- tmp.zn$sch[match(dat.zone$zone[dat.zone$ocean == "IndoPac"], tmp.zn$zone[tmp.zn$ocean == "IndoPac"])]
    # # }
    # # if (any(dat.zone$ocean == "Atl")) {
    # #   tmp.zn <- tmp.age[tmp.age$ocean == "Atl",]
    # #   dat.zone$age[dat.zone$ocean == "Atl"] <- tmp.zn$zon.age[match(dat.zone$zone[dat.zone$ocean == "Atl"], tmp.zn$zone[tmp.zn$ocean == "Atl"])]
    # #   dat.zone$age.rng[dat.zone$ocean == "Atl"] <- tmp.zn$age.rng[match(dat.zone$zone[dat.zone$ocean == "Atl"], tmp.zn$zone[tmp.zn$ocean == "Atl"])]
    # #   dat.zone$AM.type[dat.zone$ocean == "Atl"] <- tmp.zn$type[match(dat.zone$zone[dat.zone$ocean == "Atl"], tmp.zn$zone[tmp.zn$ocean == "Atl"])]
    # # }
    data$zon.age <- dat.zone$zon.age
    data$age.st <- dat.zone$st
    data$age.en <- dat.zone$en
    data$rng.age <- dat.zone$age.rng
    if (!is.null(choices$no.num.age)) {
      choices$est.zone.type <- choices$age.st <- choices$age.en <- choices$range.age <- choices$est.age.model <- rep(NA, nrow(data))
      choices$est.age <- data$age
      data$AM.type[choices$no.num.age] <- choices$est.zone.type[choices$no.num.age] <- dat.zone$type[choices$no.num.age]
      #data$age.st[choices$no.num.age] <- choices$age.st[choices$no.num.age] <- dat.zone$st[choices$no.num.age]
      #data$age.en[choices$no.num.age] <- choices$age.en[choices$no.num.age] <- dat.zone$en[choices$no.num.age]
      #data$rng.age[choices$no.num.age] <- choices$range.age[choices$no.num.age] <- dat.zone$age.rng[choices$no.num.age]
      data$age.model[choices$no.num.age] <- choices$est.age.model[choices$no.num.age] <- dat.zone$sch[choices$no.num.age]
      #data$age[choices$no.num.age] <- choices$est.age[choices$no.num.age] <- dat.zone$zon.age[choices$no.num.age]
      #data$age.calc[choices$no.num.age] <- "Zone"
    } else {
      data$age <- choices$sample.age <- dat.zone$age
      data$rng.age <- choices$range.age <- dat.zone$age.rng
      data$AM.type <- choices$type <- dat.zone$AM.type
      data$age.model <- choices$age.model <- dat.zone$sch
      data$age.st <- choices$age.st <- dat.zone$st
      data$age.en <- choices$age.en <- dat.zone$en
    }
    choices$zones.tab <- tmp.age
    
    # calculate interpolated / modelled ages
    for (i in unique(data$holeID)) {
      if (any(!is.na(data$zon.age[data$holeID == i]) & !is.na(data$sample.depth[data$holeID == i]))) {
        # interpolated ages
        tmp.zon <- data[!duplicated(data$sampleID) & data$holeID == i, grep("core|hole|sample|age|zone", names(data))]
        tmp.int <- age.interp(tmp.zon)
        tmp.int <- tmp.int[tmp.int$interp == "TRUE",]
        if (nrow(tmp.int) != 0) {
          data$int.age[data$holeID == i] <- tmp.int$int.age[match(data$sampleID[data$holeID == i], tmp.int$sampleID)]
          data$err.int.age[data$holeID == i] <- tmp.int$err.int.age[match(data$sampleID[data$holeID == i], tmp.int$sampleID)]
        }
       
        # create a age-depth chart
        tmp.dat.age <- data.frame(Chron = c(paste("T", unique(tmp.zon$zone)), paste("B", unique(tmp.zon$zone))), Type = c(rep("T", length(unique(tmp.zon$zone))), rep("B", length(unique(tmp.zon$zone)))),  Zone = rep(unique(tmp.zon$zone), 2), stringsAsFactors = FALSE)
        tmp.dat.age$Age[tmp.dat.age$Type == "B"] <- tmp.zon$age.st[match(tmp.dat.age$Zone[tmp.dat.age$Type == "B"], tmp.zon$zone)]
        tmp.dat.age$Age[tmp.dat.age$Type == "T"] <- tmp.zon$age.en[match(tmp.dat.age$Zone[tmp.dat.age$Type == "T"], tmp.zon$zone)]
        # remove zones with identical ages
        tmp.dat.age <- tmp.dat.age[!duplicated(paste(tmp.dat.age$Age, tmp.dat.age$Type)), ]
        # calculate depths
        tmp.max <- tapply(tmp.zon$sample.depth, tmp.zon$age.st, max)
        tmp.dat.age$Depth[tmp.dat.age$Type == "B"] <- tmp.max[match(tmp.dat.age$Age[tmp.dat.age$Type == "B"], names(tmp.max))]
        tmp.min <- tapply(tmp.zon$sample.depth, tmp.zon$age.en, min)
        tmp.dat.age$Depth[tmp.dat.age$Type == "T"] <- tmp.min[match(tmp.dat.age$Age[tmp.dat.age$Type == "T"], names(tmp.min))]
        tmp.dat.age <- tmp.dat.age[order(tmp.dat.age$Depth), ]

        # set first / last depth to NA, as not clear they are the start / end of zones
        tmp.dat.age$Depth[c(1, nrow(tmp.dat.age))] <- NA
        tmp.dat.age <- tmp.dat.age[!is.na(tmp.dat.age$Depth) & !is.na(tmp.dat.age$Age), ]

        if (nrow(tmp.dat.age) > 1) {
          # add in gaps
          tmp.gap <- tmp.dat.age$Chron[1:(nrow(tmp.dat.age) - 1)][diff(tmp.dat.age$Depth) < 10 & diff(tmp.dat.age$Age) > 2 & tmp.dat.age$Type[1:(nrow(tmp.dat.age) - 1)] != "T"]
          if (length(tmp.gap) > 0) {
            gap <- c("Gap", NA, NA, NA, NA)
            for (j in tmp.gap) {
              gap.id <- which(tmp.dat.age$Chron == j)
              tmp.dat.age <- rbind(tmp.dat.age[1:gap.id,], gap, gap, tmp.dat.age[(gap.id+1):nrow(tmp.dat.age), ])
            }
            tmp.dat.age$Depth[is.na(tmp.dat.age$Depth)] <- as.numeric(tmp.dat.age$Depth[which(is.na(tmp.dat.age$Depth)) - 1]) + 0.1
            tmp.dat.age$Depth[is.na(tmp.dat.age$Depth)] <- as.numeric(tmp.dat.age$Depth[which(is.na(tmp.dat.age$Depth)) + 1]) - 0.1
            tmp.dat.age$Depth <- as.numeric(tmp.dat.age$Depth)
            tmp.dat.age$Age <- as.numeric(tmp.dat.age$Age)
          }

          choices[[paste("data.age", i, sep = "_")]] <- tmp.dat.age

          # calculate the modelled ages
          tmp.age.res <- age.model(tmp.dat.age, choices$zones.tab, data[!duplicated(data$sampleID) & data$holeID == i,], hole = i, plots = FALSE, pan = TRUE)
          data$mod.age[data$holeID == i] <- tmp.age.res$mod.age[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
          data$r2[data$holeID == i] <- tmp.age.res$r2[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
          data$n.pts[data$holeID == i] <- tmp.age.res$n.pts[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
        }
        # add in the estimated age
        data$age[choices$no.num.age & data$holeID == i] <- NA
        if (length(unique(data$age[data$holeID == i])) < length(unique(data$sample.depth[data$holeID == i])) & !all(is.na(data$age[data$holeID == i]))) {
          tmp.orig <- data$age[data$holeID == i]
          data$age[data$holeID == i] <- NA
          tmp.no.num.age <- choices$no.num.age[data$holeID == i]
          choices$no.num.age <- rep(TRUE, nrow(data[data$holeID == i,]))
        }

        uni.dat <- data[!duplicated(data$sampleID) & data$holeID == i, c("sampleID", "sample.depth", grep("age", names(data), value = TRUE), "zone")]
        uni.dat <- rbind(uni.dat, uni.dat[nrow(uni.dat),])
        if (grepl("MoveDB", db.ID))
          age.mod <- movedb.model.type[movedb.model.type$Hole == i, ]
        else 
          age.mod <- data.frame(Hole = i, Original = 0, Model = 0, Interp = 0, Zone = 1)
         
        if (nrow(age.mod) == 1) {
          if (age.mod$Model == 1) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$mod.age) & data$holeID == i & data$mod.age >= 0
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$mod.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$mod.age[tmp.row]
            data$age.err[tmp.row] <- 0
            data$age.calc[tmp.row] <- "Model"
            uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
          }
          if (age.mod$Interp == 1|age.mod$Interp == 2) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$int.age) & data$holeID == i
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$int.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              #tmp.ex <- data$int.age < max(uni.dat$int.age[tmp.ex], na.rm = TRUE) & data$sample.depth > min(uni.dat$sample.depth[tmp.ex], na.rm = TRUE)
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$int.age[tmp.row]
            data$age.err[tmp.row] <- data$err.int.age[tmp.row]
            data$age.calc[tmp.row] <- "Interp"
          }
          if (age.mod$Model == 2) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$mod.age) & data$holeID == i & data$mod.age >= 0
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$mod.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$mod.age[tmp.row]
            data$age.err[tmp.row] <- 0
            data$age.calc[tmp.row] <- "Model"
            uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
          }

        }
        rm(tmp.dat.age)
        
      }

      tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$zon.age) & data$holeID == i
      data$age[tmp.row] <- data$zon.age[tmp.row]
      data$age.err[tmp.row] <- data$rng.age[tmp.row]
      data$age.calc[tmp.row] <- "Zone"

      if (any(grepl("tmp.orig", ls()))) {
        data$age[data$holeID == i][is.na(data$age)] <- tmp.orig[data$holeID == i][is.na(data$age)]
        choices$no.num.age[data$holeID == i] <- tmp.no.num.age
      }
      data$age.calc[is.na(data$age) & data$holeID == i] <- NA
      data$age.calc <- factor(data$age.calc, levels = c("Interp", "Magneto", "Model", "Orig", "Zone"))

      
    }
  }

  # sample level info -------------------------------------------------------

  # - Section
  # - Interval top
  # - Interval bottom
  if (ODP == "N") {
    data$section <- data$sample.top <- as.character(NA)
  } else {
    if (is.null(choices$section) | is.null(choices$sample.top)) {
      check <- FALSE
      while(check == FALSE) {
        choices$section <- dlg_list(names(data), title = "Which column is the ODP section?")$res
        if (length(choices$section) == 0)
          choices$section <- readline("Enter the section: ")
        choices$sample.top <- dlg_list(names(data), title = "Which column is the ODP sample.top?")$res
        if (length(choices$sample.top) == 0)
          choices$sample.top <- readline("Enter the sample top: ")
        print(paste("So section: ", choices$section, sep = ""))
        print(paste("sample.top: ", choices$sample.top, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    if (choices$sample.top %in% names(data) | choices$section %in% names(data)) {
      names(data)[names(data) == choices$section] <- "section"
      names(data)[names(data) == choices$sample.top] <- "sample.top"
      data$section <- as.character(data$section)
      data$sample.top <- as.character(data$sample.top)
    } else {
      data <- cbind(data, section = as.character(choices$section), sample.top = as.character(choices$sample.top), stringsAsFactors = FALSE)
    }

  }
  
  # - Sample type
  if (is.null(choices$sample.type)) {
    check <- FALSE
    while(check == FALSE) {
      choices$sample.type <- dlg_list(names(data), title = "Does any column contain the sample type?")$res
      if (length(choices$sample.type) == 0) {
        choices$sample.type <- readline("Enter the sample type: ")
        print(paste("So sample type: ", choices$sample.type, sep = ""))
      } else {
        print(paste("So sample type: ", database.source, "$", choices$sample.type, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$sample.type %in% names(data)) {
    names(data)[names(data) == choices$sample.type] <- "sample.type"
  } else {
    data <- cbind(data, sample.type = choices$sample.type, stringsAsFactors = FALSE)
  }
  
  # - Preservation
  if(is.null(choices$preservation)) {
    check <- FALSE
    while(check == FALSE) {
      choices$preservation <- dlg_list(names(data), title = "Does any column contain the preservation?")$res
      if (length(choices$preservation) == 0) {
        choices$preservation <- readline("Enter the preservation: ")
        print(paste("So preservation: ", choices$preservation, sep = ""))
      } else {
        print(paste("So preservation: ", database.source, "$", choices$preservation, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$preservation %in% names(data)) {
    names(data)[names(data) == choices$preservation] <- "preservation"
    data$preservation <- as.character(data$preservation)
  } else {
    data <- cbind(data, preservation = choices$preservation, stringsAsFactors = FALSE)
  }
  
  # - Processing
  if (is.null(choices$processing)) {
    check <- FALSE
    while(check == FALSE) {
      choices$processing <- dlg_list(names(data), title = "Does any column contain the sample processing information (e.g. sieve fraction)?")$res
      if (length(choices$processing) == 0) {
        choices$processing <- readline("Enter the sample processing information: ")
        if (choices$processing == "")
          choices$processing <- NA
        print(paste("So sample processing information: ", choices$processing, sep = ""))
      } else {
        print(paste("So sample processing information: ", database.source, "$", choices$processing, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$processing %in% names(data)) {
    names(data)[names(data) == choices$processing] <- "processing"
  } else {
    data <- cbind(data, processing = as.character(choices$processing), stringsAsFactors = FALSE)
  }
  
  # - Total IDd
  if(is.null(choices$total.IDd)) {
    check <- FALSE
    while(check == FALSE) {
      choices$total.IDd <- dlg_list(names(data), title = "Does any column contain the total specimens IDd?")$res
      if (length(choices$total.IDd) == 0) {
        choices$total.IDd <- readline("Enter the total specimens IDd: ")
        if (choices$total.IDd == "")
          choices$total.IDd <- NA
        print(paste("So total specimens IDd: ", choices$total.IDd, sep = ""))
      } else {
        print(paste("So total specimens IDd: ", database.source, "$", choices$total.IDd, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$total.IDd %in% names(data)) {
    names(data)[names(data) == choices$total.IDd] <- "total.IDd"
    data$total.IDd <- as.character(data$total.IDd)
  } else {
    data <- cbind(data, total.IDd = as.character(choices$total.IDd), stringsAsFactors = FALSE)
  }
  
  # paleolatitude ------------------------------
  
  if (is.null(choices$pal.lat.full))
    choices$pal.lat.full <- pal.lat.full
  if (any(!is.na(data$age))|!is.null(choices$pal.lat)) {
    if (max(as.numeric(as.character(data$age)), na.rm = TRUE) == 0) {
      # if all the data is extant, then no need for palaeolatitude
      data$pal.lat <- data$latitude
      data$pal.long <- data$longitude
    } else {
      
      if (is.null(choices$pal.lat) | is.null(choices$pal.long)) {
        # if there is no palaeolatitude information
        print(head(data))
        pl.check <- dlg_list(c("Yes", "No"), title = "Is there palaeolatitude information?")$res
        if (length(pl.check) == 0)
          pl.check <- "No"
        if (pl.check == "Yes") {
          check <- FALSE
          while(check == FALSE) {
            choices$pal.lat <- dlg_list(names(data), title = "Which column is the palaeolatitude?")$res
            choices$pal.long <- dlg_list(names(data), title = "Which column is the palaeolongitude?")$res
            print(paste("So palaeolatitude: data$", choices$pal.lat, sep = ""))
            print(paste("palaeolongitude: data$", choices$pal.long, sep = ""))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
          names(data)[names(data) == choices$pal.lat] <- "pal.lat"
          names(data)[names(data) == choices$pal.long] <- "pal.long"
        } else {
          # # if necessary extract palaeo lat information
          ## this is from palaeocoords, which is a function in mapast
          if (!pal.lat.full) { # i.e. if only testing
            # extract 5 random ages to test it on
            tmp.uni.ages <- unique(data$age[!is.na(data$age)])[1:5]
            tmp.pal.rows <- which(data$age %in% tmp.uni.ages[!is.na(tmp.uni.ages)])
            tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- as.numeric(rep(NA, nrow(data)))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
          } else {
            tmp.pal.rows <- which(!is.na(data$age))
            tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- as.numeric(rep(NA, nrow(data)))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
          }
        } 
        
      } else if (!(choices$pal.lat.full) & pal.lat.full) {
        # if the choices$pal.lat are only a subset and you want a full run
        tmp.pal.rows <- which(!is.na(data$age))
        tmp <- pal.coord(data[tmp.pal.rows,], model = model)
        choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- as.numeric(rep(NA, nrow(data)))
        choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
        choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
      } else {
        if (choices$pal.lat[1] %in% names(data)) {
          names(data)[names(data) == choices$pal.lat] <- "pal.lat"
          names(data)[names(data) == choices$pal.long] <- "pal.long"
        } else {
          data$pal.lat <- NA
          data$pal.long <- NA
          data$pal.lat <- as.numeric(choices$pal.lat)
          data$pal.long <- as.numeric(choices$pal.long)
        }
      }
    }
  }
  choices$pal.lat.full <- pal.lat.full

  # species names -----------------------------------------------------------
  # # Extract species level information
  sp.check <- "unknown"
  if(is.null(choices$species) & is.null(choices$genus)) {
    print(head(data))
    sp.check <- dlg_list(c("Yes", "No"), title = "Is the species data currently in the form of a binomial?")$res
    if(sp.check == "Yes") {
      check <- FALSE
      while(check == FALSE) {
        choices$species <- dlg_list(names(data), title = "Which column is the species binomial?")$res
        print(paste("So species: ", database.source, "$", choices$species, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
      names(data)[names(data) == choices$species] <- "orig.species"
    } else {
      check <- FALSE
      while(check == FALSE) {
        choices$genus <- dlg_list(names(data), title = "Which column is the genus?")$res
        choices$sp <- dlg_list(names(data), title = "Which column is the species?")$res
        choices$q.sp <- dlg_list(names(data), title = "Which column is the species qualifier?")$res
        choices$subsp <- dlg_list(names(data), title = "Which column is the subspecies?")$res
        print(paste("So genus is: ", database.source, "$", choices$genus, sep = ""))
        print(paste("species: ", database.source, "$", choices$sp, sep = ""))
        print(paste("species qualifier: ", database.source, "$", choices$q.sp, sep = ""))
        print(paste("subspecies: ", database.source, "$", choices$subsp, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  if (!is.null(choices$species) | sp.check == "Yes") {
    names(data)[names(data) == choices$species] <- "orig.species"
  } 
  if (!is.null(choices$genus) | sp.check == "No") {
    species <- paste(data[, names(data) == choices$genus], data[, names(data) == choices$sp])
    if (length(choices$subsp) != 0) {
      sub.sp.col <- data[, names(data) == choices$subsp]
      species[!is.na(sub.sp.col)] <- paste(species[!is.na(sub.sp.col)], sub.sp.col[!is.na(sub.sp.col)])
    }
    if (length(choices$q.sp) != 0) {
      sp.q.col <- data[, names(data) == choices$q.sp]
      species[!is.na(sp.q.col)] <- paste(species[!is.na(sp.q.col)], sp.q.col[!is.na(sp.q.col)])
    }
    data$orig.species <- species
  }
  
    
  # # Query database to correct for synonyms
  check <- "Yes"
  new.species <- NULL
  while(check == "Yes") {
    if (is.null(new.species)) {
      comp.list <- unique(data$orig.species)
      comp.rerun <- 1:length(comp.list)
    } else {
      comp.rerun <- which(new.species %in% c("Cretaceous", "Unsure", "unknown"))
      comp.list <- unique(data$orig.species)[comp.rerun]
    }
    new.species[comp.rerun] <- compare(comp.list, micro = TRUE)
    print("The following species were classified as 'unknown'")
    print(sort(unique(data$orig.species)[new.species == "unknown"]))
    print("The following species were classified as 'Cretaceous'")
    print(sort(unique(data$orig.species)[new.species == "Cretaceous"]))
    print("The following species were classified as 'Unsure'")
    print(sort(unique(data$orig.species)[new.species == "Unsure"]))
    print("The following species were classified as 'Vague'")
    print(sort(unique(data$orig.species)[new.species == "Vague"]))
    if (!is.null(choices$synonymy)) {
      check <- "No"
    } else {
      check <- dlg_list(c("Yes", "No"), title = "Do you want to make some corrections and re-run this")$res
    }
  }
  
  data$species <- new.species[match(data$orig.species, unique(data$orig.species))]
  choices$synonymy <- "checked"
  
  # abundance information
  if (is.null(choices$orig.abundance)) {
    check <- FALSE
    while(check == FALSE) {
      choices$orig.abundance <- dlg_list(names(data), title = "Does any column contain the abundance?")$res
      if (length(choices$orig.abundance) == 0) {
        choices$orig.abundance <- "P"
      } else {
        print(paste("So abundance: ", database.source, "$", choices$orig.abundance, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$orig.abundance %in% names(data)) {
    names(data)[names(data) == choices$orig.abundance] <- "orig.abundance"
  } else {
    data <- cbind(data, orig.abundance = choices$orig.abundance, stringsAsFactors = FALSE)
  }
  if (is.null(choices$abun.calc)) {
    choices$abun.calc <- dlg_list(c(TRUE, FALSE), title = "Should the numeric abundance be calculated?")$res
  }
  
  # abundance ---------------------------------------------------------------
  if (choices$abun.calc == TRUE) {
    if (is.null(choices$full.abun)) {
      if (multiple.abun == FALSE) {
        # What are the abundance units
        print("Numeric characters:")
        print(summary(as.numeric(gsub("#|\\?|>|<", "", grep("\\d", data$orig.abundance, value = TRUE)))))
        print("Non numeric characters:")
        print(grep("[^(0-9)|\\.]", unique(data$orig.abundance), value = TRUE))
        if (is.null(choices$abun.units)) {
          # what are the abundance units
          choices$abun.units <- dlg_list(c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed") , title = "What are the units of abundance?")$res
        }
        data$abun.units <- factor(choices$abun.units, levels = c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed"))
        # convert everything to numbers
        data$abundance <- data$orig.abundance
        # where there are mixed units, identify numeric / non-numeric rows
        if (any(choices$abun.units == "Mixed")) {
          mix.tab <- tapply(data$abundance, data$holeID, max)
          mix.tab[!is.na(mix.tab)] <- "Num"
          mix.tab[is.na(mix.tab)] <- "NotNum"
          mix.row <- rep(NA, nrow(data))
          mix.row[data$holeID %in% names(mix.tab)[mix.tab == "Num"]] <- "Num"
          mix.row[data$holeID %in% names(mix.tab)[mix.tab == "NotNum"]] <- "NotNum"
          mix.row[mix.row == "Num"] <- dlg_list(c("Relative abundance", "Count", "Number per gram") , title = "What are the units of abundance for the numeric data?")$res
          mix.row[mix.row == "NotNum"] <- dlg_list(c("Binned", "P/A") , title = "What are the units of abundance for the non-numeric data?")$res
          data$abun.units <- choices$abun.units <- mix.row
        }
        if (any(choices$abun.units %in% c("Relative abundance", "Count", "Number per gram"))) {
          tmp.row <- which(data$abun.units %in% c("Relative abundance", "Count", "Number per gram"))
          tmp.abun <- data$abundance[tmp.row]
          if (any(grepl("[^(0-9)|\\.]", unique(tmp.abun))) & any(is.na(as.numeric(grepl("[^(0-9)|\\.]", unique(tmp.abun)))))) {
            if(is.null(choices$abun.num)) {
              print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
              choices$abun.num <- as.numeric(readline("What should this be replaced with? "))
            }
            tmp.abun[grep("[^(0-9)|\\.]", tmp.abun)] <- choices$abun.num
          }
          data$abundance[tmp.row] <- as.numeric(tmp.abun)
        } 
        if (any(choices$abun.units == "P/A")) {
          tmp.row <- which(data$abun.units == "P/A")
          tmp.abun <- data$abundance[tmp.row]
          if (is.null(choices$abun.p)) {
            print(unique(tmp.abun))
            choices$abun.p <- readline("What symbol indicates presence? ")
            choices$abun.a <- readline("What symbol indicates absence? ")
            for (i in unique(tmp.abun)[!(unique(tmp.abun) %in% c(choices$abun.p, choices$abun.a))]) {
              tmp.pa <- dlg_list(c("Presence", "Absence") , title = paste("Should", i, "be presence or absence?"))$res
              if (tmp.pa == "Presence"){
                choices$abun.p <- c(choices$abun.p, i)
              } else if (tmp.pa == "Absence") {
                choices$abun.a <- c(choices$abun.a, i)
              }
            }
          }
          tmp.abun[tmp.abun %in% choices$abun.p] <- 1
          tmp.abun[tmp.abun %in% choices$abun.a] <- 0
          data$abundance[tmp.row] <- tmp.abun
        } 
        if (any(choices$abun.units == "Binned")) {
          tmp.row <- which(data$abun.units == "Binned")
          tmp.abun <- data$abundance[tmp.row]
          if (is.null(choices$abun.bin)) {
            print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
            choices$abun.bin <- readline("Enter the binned abundances starting with absent (e.g. n,r,c,a), leave reworked out: ")
            choices$abun.bin <- unlist(strsplit(choices$abun.bin, ","))
          } 
          tmp.abun <- match(tmp.abun, choices$abun.bin) - 1
          tmp.abun[is.na(tmp.abun)] <- 0
          data$abundance[tmp.row] <- tmp.abun 
        }
      } else {
        # add in the necessary additional columns
        if (is.null(choices$abun)) {
          choices$abun <- tibble(source = unique(data$source), abun = NA, units = NA)
          choices$abun.smID <- tibble(source = unique(data$sampleID), abun = NA, units = NA)
        }
        data$abun.units <- NA
        # calculate abundances based initially on the source
        for (i in unique(data$source)) {
          if (any(is.na(data$abun.units[data$source == i & !is.na(data$source)]))) {
            tmp <- num.abun(i, data = data, choices = choices, sample_source = "source")
            if (!is.null(tmp)) {
              data$abundance[data$source == i & !is.na(data$source)] <- tmp$data
              data$abun.units[data$source == i & !is.na(data$source)] <- choices$abun$units[choices$abun$source == i] <- tmp$abun.units
              choices$abun$abun[choices$abun$source == i] <- list(tmp$choices)
            }
            #save(data, choices, file = paste("Outputs/tmp_dat/", substr(gsub("[[:punct:]]| ", "", i), 0, 40), ".RData", sep = ""))
          }
        }
        rm(i, tmp)

        # where there is no source, use the sample ID
        for (i in unique(data$sampleID)) {
          if (any(is.na(data$abun.units[data$sampleID == i]) | data$abun.units[data$sampleID == i] == "Mixed" | choices$abun$units[choices$abun$source == unique(data$source[match(i, data$sampleID)])] == "Mixed")) {
            tmp <- num.abun(i, data = data, choices = choices, sample_source = "sample")
            data$abundance[data$sampleID == i] <- tmp$data
            data$abun.units[data$sampleID == i] <- choices$abun.smID$units[choices$abun.smID$source == i] <- tmp$abun.units
            choices$abun.smID$abun[choices$abun.smID$source == i] <- list(tmp$choices)
            save(data, choices, file = paste("Outputs/tmp_dat/", i, ".RData", sep = ""))
          }
        }
        names(choices$abun.smID)[1] <- "sampleID"
        rm(i, tmp)
        data$abun.units <- factor(data$abun.units, levels = c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed"))
        
        choices$full.abun <- data$abundance
        choices$full.abun.units <- data$abun.units
        
      }
      #data$abundance <- as.numeric(as.character(data$abundance))
      
    } else {
      choices$full.abun <- choices$full.abun[!is.na(choices$full.abun)]
      if (length(choices$full.abun) == nrow(data)) {
        data$abundance <- choices$full.abun
        data$abun.units <- choices$full.abun.units
      } else {
        data$abundance <- NA
        data$abun.units <- NA
        # working through the sources
        for (i in unique(data$source[!is.na(data$source)])) {
          # what is the abundance type
          tmp.type <- c("abun.num", "abun.p", "abun.a", "abun.bin")[unlist(lapply(choices$abun$abun[choices$abun$source == i & !is.na(choices$abun$source)][[1]], function(x) !is.null(x)))]
          # which rows belong to that data type
          tmp.row <- which(data$source == i & !is.na(data$source))
          # if there is an abundance type
          if (length(tmp.type) > 0 & tmp.type[1] != "abun.num") {
            # identify the abundance type
            tmp.abun <- unlist(choices$abun$abun[choices$abun$source == i & !is.na(choices$abun$source)][[1]][tmp.type])
            if (tmp.type[1] == "abun.p") {
              data$abun.units[tmp.row] <- "P/A"
              tmp.orig.abun <- data$orig.abundance[tmp.row]
              tmp.new.abun <- rep(NA, length(tmp.orig.abun))
              tmp.new.abun[tmp.orig.abun %in% tmp.abun[grep("abun.p", names(tmp.abun))]] <- 1
              tmp.new.abun[tmp.orig.abun %in% tmp.abun[grep("abun.a", names(tmp.abun))]] <- 0
              data$abundance[tmp.row] <- tmp.new.abun
              rm(tmp.orig.abun, tmp.new.abun)
            }
            if (tmp.type[1] == "abun.bin") {
              # add data units
              data$abun.units[tmp.row] <- "Binned"
              tmp.orig.abun <- data$orig.abundance[tmp.row]
              # convert binned abundances
              tmp.new.abun <- match(tmp.orig.abun, tmp.abun) - 1
              tmp.new.abun[is.na(tmp.new.abun)] <- 0 # convert any that don't match to 0
              data$abundance[tmp.row] <- tmp.new.abun
              rm(tmp.orig.abun, tmp.new.abun)
            }
            rm(tmp.abun)
          } else {
            # if all of them are numeric
            data$abundance[tmp.row] <- suppressWarnings(as.numeric(data$orig.abundance[tmp.row]))
            if (length(tmp.type) > 0|sum(is.na(data$abundance[tmp.row] > 1))) {
              data$abundance[tmp.row][is.na(data$abundance[tmp.row])] <- 0.01
            }
            if (max(with(data[data$source == i & !is.na(data$source), ], tapply(abundance, sampleID, sum))) <= 100){
              data$abun.units[tmp.row] <- "Relative abundance"
            } else if (sum(data$abundance[tmp.row] %% 1) == 0) {
              data$abun.units[tmp.row] <- "Count"
            }
          }
          # if there are actually mixed abundance units
          if (tmp.type[1] == "abun.bin" & !is.na(tmp.type[1]) & any(with(data[data$source == i & !is.na(data$source),], tapply(orig.abundance, sampleID, function (x) length(grep("[0-9]", x)) > 0)))) {
            # which should be numeric
            tmp.num <- names(which(with(data[data$source == i & !is.na(data$source),], tapply(orig.abundance, sampleID, function (x) length(grep("[0-9]", x)) > 0))))
            # convert to numbers
            data$abundance[data$sampleID %in% tmp.num] <- suppressWarnings(as.numeric(data$orig.abundance[data$sampleID %in% tmp.num]))
            data$abundance[data$sampleID %in% tmp.num & is.na(data$abundance)] <- 0.01
            if (max(with(data[data$source == i & !is.na(data$source), ], tapply(abundance, sampleID, sum))) <= 115){
              data$abun.units[data$sampleID %in% tmp.num] <- "Relative abundance"
            } else if (sum(data$abundance[tmp.row] %% 1) == 0) {
              data$abun.units[data$sampleID %in% tmp.num] <- "Count"
            }
          }
          rm(tmp.row, tmp.type)
        }
        # where there aren't sources, working through the sample.ID
        for (i in sort(unique(data$Sample.ID[!is.na(data$Sample.ID)]))) {
          if (any(is.na(data$abun.units[data$Sample.ID == i]))) {
            # what is the abundance type
            tmp.type <- c("abun.num", "abun.p", "abun.a", "abun.bin")[unlist(lapply(choices$abun.smID$abun[choices$abun.smID$sampleID == i & !is.na(choices$abun.smID$sampleID)][[1]], function(x) !is.null(x)))]
            # which rows belong to that data type
            tmp.row <- which(data$Sample.ID == i & !is.na(data$Sample.ID))
            # if there is an abundance type
            if (length(tmp.type) > 0 & tmp.type[1] != "abun.num") {
              # identify the abundance type
              tmp.abun <- unlist(choices$abun.smID$abun[choices$abun.smID$sampleID == i & !is.na(choices$abun.smID$sampleID)][[1]][tmp.type])
              if (tmp.type[1] == "abun.p") {
                data$abun.units[tmp.row] <- "P/A"
                tmp.orig.abun <- data$orig.abundance[tmp.row]
                tmp.new.abun <- rep(NA, length(tmp.orig.abun))
                tmp.new.abun[tmp.orig.abun %in% tmp.abun[grep("abun.p", names(tmp.abun))]] <- 1
                tmp.new.abun[tmp.orig.abun %in% tmp.abun[grep("abun.a", names(tmp.abun))]] <- 0
                data$abundance[tmp.row] <- tmp.new.abun
                rm(tmp.orig.abun, tmp.new.abun)
              }
              if (tmp.type[1] == "abun.bin") {
                # add data units
                data$abun.units[tmp.row] <- "Binned"
                tmp.orig.abun <- data$orig.abundance[tmp.row]
                # convert binned abundances
                tmp.new.abun <- match(tmp.orig.abun, tmp.abun) - 1
                tmp.new.abun[is.na(tmp.new.abun)] <- 0 # convert any that don't match to 0
                data$abundance[tmp.row] <- tmp.new.abun
                rm(tmp.orig.abun, tmp.new.abun)
              }
              rm(tmp.abun)
            } else {
              # if all of them are numeric
              data$abundance[tmp.row] <- suppressWarnings(as.numeric(data$orig.abundance[tmp.row]))
              if (length(tmp.type) > 0|sum(is.na(data$abundance[tmp.row] > 1))) {
                data$abundance[tmp.row][is.na(data$abundance[tmp.row])] <- 0.01
              }
              if (max(data$abundance[tmp.row]) <= 100){
                data$abun.units[tmp.row] <- "Relative abundance"
              } else if (sum(data$abundance[tmp.row] %% 1) == 0) {
                data$abun.units[tmp.row] <- "Count"
              }
            }
            rm(tmp.row, tmp.type)
          }
        }
        choices$full.abun <- data$abundance
        choices$full.abun.units <- data$abun.units
      }
    }
  }
  
  # remove those with NA abundances
  data <- data[!is.na(data$abundance),]
  # make sure it is numeric
  data$abundance <- as.numeric(data$abundance)
  
  # remove those not at species level
  data <- data[grepl("\\s", data$species),]
  # # Where synonyms identify duplicates, then merge them
  # dup.sp <- which(duplicated(paste(data$age, data$depth, data$latitude, data$longitude, data$species, data$zone)))
  dup.sp <- which(duplicated(paste(data$species, data$sampleID)))
  # if (length(dup.sp) > 0) {
  #   data <- data[-dup.sp,]
  # }
  if (length(dup.sp) > 0) {
    # get merged species
    tmp.sp <- tapply(data$orig.species, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    tmp.orig.abun <- tapply(data$orig.abundance, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    # calculate the summed abundances given the duplicates
    # if (!is.null(data$row.num)) {
    #   browser() # should I be using this version?
    #   tmp.abun <- tapply(data$abundance, list(data$row.num , data$species), sum, na.rm = TRUE)
    #   data <- data[-dup.sp,]
    #   # match the summed abundances with the data
    #   data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$row.num [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    # } else {
      tmp.abun <- tapply(data$abundance, list(data$sampleID , data$species), sum, na.rm = TRUE)
      tmp.abun2 <- with(data[data$abun.units == "Binned", ], tapply(abundance, list(sampleID , species), max, na.rm = TRUE))
      data <- data[-dup.sp,]
      # match the summed abundances with the data
      data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$sampleID [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    # }
    # if there is presence absence data, then reset the maximum to 1
    if (any(data$abun.units == "P/A")) {
      data$abundance[data$abun.units == "P/A" & data$abundance > 0] <- 1
    }
    
    # ditto for binned data
    if (any(data$abun.units == "Binned")) {
      data$abundance[data$abun.units == "Binned"] <- with(data[data$abun.units == "Binned",], sapply(1:sum(data$abun.units == "Binned"), function (x) tmp.abun2[match(sampleID[x], dimnames(tmp.abun2)[[1]]), match(species[x], dimnames(tmp.abun2)[[2]])]))
      # when there are multiple abundance values, e.g. in neptune
    #   if (multiple.abun == TRUE) {
    #     # if abun.bin hasn't already been calculated
    #     if (is.null(choices$abun.bin)) {
    #       # get the maximum value for each source / sampleID
    #       tmp.abun.bin.source <- unlist(lapply(1:nrow(choices$abun), function(x) length(choices$abun$abun[[x]]$abun.bin)))
    #       names(tmp.abun.bin.source) <- choices$abun$source
    #       tmp.abun.bin.smID <- unlist(lapply(1:nrow(choices$abun.smID[!is.na(choices$abun.smID$abun),]), function(x) length(choices$abun.smID$abun[!is.na(choices$abun.smID$abun)][[x]]$abun.bin)))
    #       names(tmp.abun.bin.smID) <- choices$abun.smID$sampleID[!is.na(choices$abun.smID$abun)]
    #       choices$abun.bin <- rep(NA, nrow(data))
    #       choices$abun.bin[!is.na(data$source)] <- tmp.abun.bin.source[match(data$source[!is.na(data$source)], names(tmp.abun.bin.source))]
    #       if (length(tmp.abun.bin.smID[match(data$Sample.ID[is.na(data$source)], names(tmp.abun.bin.smID))]) > 0) {
    #         choices$abun.bin[is.na(data$source)] <- tmp.abun.bin.smID[match(data$Sample.ID[is.na(data$source)], names(tmp.abun.bin.smID))]
    #       } else {
    #         choices$abun.bin[is.na(choices$abun.bin)] <- tmp.abun.bin.smID[match(data$sampleID[is.na(choices$abun.bin)], names(tmp.abun.bin.smID))]
    #       }
    #     }
    #     data$abundance[data$abun.units == "Binned" & data$abundance > (choices$abun.bin - 1)] <- choices$abun.bin[data$abun.units == "Binned" & data$abundance > (choices$abun.bin - 1)] - 1
    #   } else {
    #     data$abundance[data$abun.units == "Binned" & data$abundance > (length(choices$abun.bin) - 1)] <- length(choices$abun.bin) - 1
    #   }
      }
    # merged original species
    data$orig.species <- sapply(1:nrow(data), function (x) tmp.sp[match(data$sampleID [x], dimnames(tmp.sp)[[1]]), match(data$species[x], dimnames(tmp.sp)[[2]])])
    data$orig.abundance <- sapply(1:nrow(data), function (x) tmp.orig.abun[match(data$sampleID [x], dimnames(tmp.orig.abun)[[1]]), match(data$species[x], dimnames(tmp.orig.abun)[[2]])])
  }
  
  # convert factors to strings
  data$rng.age <- as.numeric(data$rng.age)
  data$zone <- as.character(data$zone)
  #data$water.depth <- ifelse(data$water.depth == "NA", as.numeric(NA), as.numeric(data$water.depth))
  # complete factor levels
  data$abun.units <- factor(data$abun.units, levels = c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed"))
  
  # add in extra age columns
  data$mag.zone <- as.numeric(NA)
  data$mag.age.st <- as.numeric(NA)
  data$mag.age.en <- as.numeric(NA)
  data$mag.age <- as.numeric(NA)
  data$int.mag.age <- as.numeric(NA)
  data$err.int.mag.age <- as.numeric(NA)
  
  # return restructured dataframe
  col.inc <- c("rowID", "species", "orig.species", "abundance", "orig.abundance", "abun.units", "sample.depth", "segment", "age", "age.err", "age.calc", "zone", "zon.age", "age.st", "age.en", "rng.age", "int.age", "err.int.age", "mag.zone", "mag.age.st", "mag.age.en", "mag.age", "int.mag.age", "err.int.mag.age", "mod.age", "r2", "n.pts", "age.model", "AM.type", "latitude", "longitude", "pal.lat", "pal.long", "water.depth", "db.source", "dbID", "holeID", "sampleID", "reason", "leg", "site", "hole", "core", "section", "sample.top", "sample.type", "total.IDd", "preservation", "processing", "person", "date", "year", "source")
  final.data <- list(data = data[, col.inc], choices = choices)
  return(final.data)
}



# pangaea PNstructure -----------------------------------------------------
pan.PNstructure <- function(pan.data, input.init, choices = NULL, model = "MATTHEWS2016", pal.lat.full = TRUE) {
  # pal.lat.full only runs paleolat for the first 5 data points to check that the function is working / get the choices. 
  # load the relevant libraries
  require("svDialogs")
  library(devtools)
  library(openxlsx)
  #install_github("macroecology/mapast")
  library(mapast)
  #source("Code/Triton_ForamSynonyms.R")
  library(tidyverse)
  library(readxl)
  library(sp)
  
  # create a list - choices - that once populated allows the function to run automatically
  if (is.null(choices)) {
    choices <- list()
    choices$pal.lat.full <- pal.lat.full
  }
  
  # get the data
  tmp.data <- pan.data$data[[1]]
  # reshape: work out the columns to reshape into a long format data frame ----------------------
  if (is.null(choices$var.col)) {
    # assume all columns are varying except depth, age, lat, long, etc.
    choices$var.col <- which(!grepl("Depth|Age|zone|Epoch|Latitude|Longitude|Sample label|Foram|foram|Event", names(tmp.data)))
    # check the automation is correct
    print(paste(c("Suggested varying columns are: ", names(tmp.data)[choices$var.col])))
    print(paste(c("Non varying are: ", names(tmp.data)[-choices$var.col])))
    check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    while(check == FALSE) {
      # if the automation didn't work, then do this by hand
      print(names(tmp.data))
      choices$var.col <- readline("Which column numbers vary (i.e. should be rows): " )
      choices$var.col <- eval(parse(text = (choices$var.col)))
      print(paste(c("So varying columns are: ", names(tmp.data)[choices$var.col])))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  # if some of the columns are duplicated
  col.var <- choices$var.col
  if (length(unique(colnames(tmp.data)[choices$var.col])) != length(choices$var.col)) {
    dup.col <- which(duplicated(colnames(tmp.data)))
    uniq.col <- unique(colnames(tmp.data)[choices$var.col])
    print("Duplicated columns:")
    print(colnames(tmp.data)[dup.col])
    if (is.null(choices$merge))
      choices$merge <- dlg_list(c("Yes", "No"), title = "Do you want to merge the duplicated columns?")$res
    if (choices$merge == "Yes") {
      colnames(tmp.data)[dup.col] <- paste(colnames(tmp.data)[dup.col], "[1]")
  #     tmp.data[, choices$var.col] <- apply(tmp.data[, choices$var.col], 2, as.numeric)
  #     tmp.data <- as_tibble(sapply(unique(colnames(tmp.data)), function(x) ifelse(is.numeric(tmp.data[,x]), rowSums(tmp.data[,grepl(x, colnames(tmp.data))]), tmp.data[,x])))
  #     col.var <- match(uniq.col, colnames(tmp.data))
    }
  }
  
  tmp.data$row.num  <- paste("R", 1:nrow(tmp.data), sep = "")
  # reshape the data
  data <- gather(tmp.data, Species, rel.abun, colnames(tmp.data)[col.var])
  
  # print to check
  print(head(data))
  
  # database information ----------------
  # (database source, date stamp, inputter)
  data <- cbind(data, db.source = "Pangaea", date = Sys.Date(), person = input.init, dbID = pan.data$doi, stringsAsFactors = FALSE)
  
  # add a column of why the data was collected
  # if the citation / supplement mentions census, then assume it was collected for community analysis
  if (grepl("census", paste(pan.data$citation, pan.data$supplement_to))) {
    choices$reason <- "Community analysis"
  }
  # if automation doesn't give a reason
  if (is.null(choices$reason)) {
    print(paste(pan.data$citation, pan.data$supplement_to))
    print(pan.data$doi)
    print(names(pan.data$data[[1]]))
    choices$reason <- dlg_list(c("Community analysis", "Selected species", "Proxies", "Extreme event", "Biostratigraphy", "Unspecified"), title = "What is the reason for the data collection?")$res
  }
  data$reason <- factor(choices$reason, levels = c("Community analysis", "Selected species", "Proxies", "Extreme event", "Biostratigraphy", "Unspecified"))
  
  ## Site-level columns -----------------------------
  # source/year -----------
  if (is.null(choices$source)) {
    # get the source from the citation
    choices$source <- pan.data$citation
  }
  data <- cbind(data, source = choices$source, stringsAsFactors = FALSE)
  if (is.null(choices$year)) {
    # get the source from the citation
    choices$year <- gsub("^[^\\()]*\\((\\d*).*$", "\\1", pan.data$citation)
  }
  data <- cbind(data, year = as.numeric(choices$year))
  
  # age model -------------------
  if (is.null(choices$age.model)) {
    while(is.null(choices$age.model)) {
      # print metadata with potentially relevant info for the age model
      print(grep("zone|age", pan.data$meta.data[[1]], value = TRUE, ignore.case = TRUE))
      print("Zones:")
      print(c(unique(data[,grep("zone", names(data), ignore.case = TRUE)])))
      print(paste("Year:", data$year[1]))
      choices$age.model <- dlg_list(c("Wade2011 (P,E,O,M,Pl,Pt)", "Berggren2005 (P,E,O)", "Berggren1995 (P,M,Pl,Pt)", "Berggren1977 (Pl)", "GTSBlow1969 (P,N)", "C&P1997/P&C1997 (P,N)", "B&M1988 (P)", "K&S1983 (N)", "Blow1969 (P13-N12)", "Ericson1968 (P-Z)", "Huber2005 (AP,AE,AO)", "Huber1991 (AP)", "Raffi2006 (NN)", "GTS2012 (MoveDB uses this)", "Other", "Extra"), title = "What is the age model?")$res
      if (length(choices$age.model) == 0)
        choices$age.model <- "Other"
      choices$age.model <- gsub( "\\ .*$", "", choices$age.model)
    }
  }
  data <- cbind(data, age.model = choices$age.model, stringsAsFactors = FALSE)
  
  # odp sites: If not an ODP site, create blank columns -----------------------
  if (is.null(choices$holeID) & !is.null(choices$coreID)) {
    choices$holeID <- choices$coreID
    choices$coreID <- NULL
  }
  if (!(grepl("ODP|DSDP", paste(pan.data$citation, pan.data$supplement_to)))) {
    data$leg <- data$site <- data$hole <- data$core <- data$section <- data$sample.top <- as.character(NA)
    # extract the holeID (assume that if none of the columns are latitude, then there is only one sample)
    if (all(colnames(data) != "Latitude")) {
      if (is.null(choices$holeID)) {
        # get the event info from the meta data as this has the hole ID
        tmp.ID <- paste(grep("Event", pan.data$meta.data[[1]], value = TRUE, ignore.case = TRUE), collapse = ";")
        print(tmp.ID)
        choices$holeID <- gsub("^Event\\(s\\):\\\t *(.*?)\\s.*$", "\\1", tmp.ID, ignore.case = TRUE)
        print(paste("So hole ID: ", choices$holeID))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        # if the automation doesn't work
        if (check == FALSE) {
          print(grep("Citation|Related to|Event", pan.data$meta.data[[1]], value = TRUE))
          choices$holeID <- readline("What is the hole ID: " )
        }
      }
      data$holeID <- choices$holeID
    } 
    # if, however, there are multiple hole IDs, so it is a column
    if (is.null(choices$holeID)) {
      check <- FALSE
      while(check == FALSE) {
        choices$holeID <- dlg_list(names(data), title = "Which column is the hole ID?")$res
        print(paste("So hole ID: data$", choices$holeID, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    names(data)[names(data) == choices$holeID] <- "holeID"
    
  } else { # if it is an ODP site
    
    iodp.col <- NULL
    tmp.split <- NULL
    if (is.null(choices$leg) | is.null(choices$site) | is.null(choices$hole) | is.null(choices$core)) {
      print(grep("DSDP|ODP", pan.data$meta.data[[1]], value = TRUE))
      print(head(data))
      iodp <- dlg_list(c("One", "Multiple", "Metadata"), title = "Is the ODP information (leg, site, hole, core) in one / multiple columns or the meta data?")$res
      if (iodp == "One") {
        check <- FALSE
        while(check == FALSE) {
          iodp.col <- dlg_list(names(data), title = "Which column is the ODP info?")$res
          print(paste("So info: ", iodp.col, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
        # extract all the iodp info from the column
        tmp.iodp <- gsub("--", "-", data[, iodp.col])
        tmp.split <- strsplit(tmp.iodp, "\\-|,/")
        # if not all rows have iodp data, add some NAs
        tmp.split[which(lapply(tmp.split, length) == 0)] <- list(c(NA, NA, NA, NA))
        # if data is missing the leg
        if (any(unlist(lapply(tmp.split, function(x) as.numeric(gsub("[A-z]", "", x[1])) > as.numeric(gsub("[A-z]", "", x[2])))), na.rm = TRUE)) {
          leg.mis <- which(unlist(lapply(tmp.split, function(x) as.numeric(gsub("[A-z]", "", x[1])) > as.numeric(gsub("[A-z]", "", x[2])))))
          if (is.null(choices$add.leg)) {
            choices$add.leg <- readline("It looks like the leg is missing from from the sample label. Enter the leg: ")
          }
          if (length(choices$add.leg) > 0)
            tmp.split[leg.mis] <- lapply(tmp.split[leg.mis], function (x) c(choices$add.leg, x))
        }
          
        choices$leg <- unlist(lapply(tmp.split, "[[", 1))
        choices$site <- gsub("[A-Z]", "", unlist(lapply(tmp.split, "[[", 2)))
        choices$hole <- unlist(lapply(tmp.split, "[[", 2))
        if (any(lapply(tmp.split, length) > 2)) {
          choices$core <- unlist(lapply(tmp.split, "[[", 3))
        } else {
          choices$core <- NA
        }
      } else if (iodp == "Multiple") { # if the IODP data is in multiple columns
        check <- FALSE
        while(check == FALSE) {
          choices$leg <- dlg_list(names(data), title = "Which column is the ODP leg?")$res
          if (length(choices$leg) == 0) {
            choices$leg <- readline("Enter the leg: ")
            if (choices$leg == "")
              choices$leg <- NA
          }
          choices$site <- dlg_list(names(data), title = "Which column is the ODP site?")$res
          if (length(choices$site) == 0) {
            choices$site <- readline("Enter the site: ")
            if (choices$site == "")
              choices$site <- NA
          }
          choices$hole <- dlg_list(names(data), title = "Which column is the ODP hole?")$res
          if (length(choices$hole) == 0) {
            choices$hole <- readline("Enter the hole: ")
            if (choices$hole == "")
              choices$hole <- NA
          }
          choices$core <- dlg_list(names(data), title = "Which column is the ODP core?")$res
          if (length(choices$core) == 0) {
            choices$core <- readline("Enter the core: ")
            if (choices$core == "")
              choices$core <- NA
          }
          # check these
          print(paste("So leg: ", choices$leg, sep = ""))
          print(paste("site: ", choices$site, sep = ""))
          print(paste("hole: ", choices$hole, sep = ""))
          print(paste("core: ", choices$core, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      } else if (iodp == "Metadata") {
        check <- FALSE 
        while(check == FALSE) {
          tmp.odp <- grep("DSDP|ODP", pan.data$meta.data[[1]], value = TRUE)
          print(tmp.odp)
          choices$leg <- unique(gsub("^.*Leg ([[:digit:]]*)\\W.*$", "\\1", grep("Leg ", tmp.odp, value = TRUE)))
          print(paste("Suggested leg: ", choices$leg, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          if (check == "FALSE") {
            choices$leg <- readline("Enter the leg: ")
            if (choices$leg == "")
              choices$leg <- NA
          }
          choices$site <- unique(gsub("^.*Site ([[:digit:]]*)\\W.*$", "\\1", grep("Site ", tmp.odp, value = TRUE)))
          print(paste("Suggested site: ", choices$site, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          if (check == "FALSE") {
            choices$site <- readline("Enter the site: ")
            if (choices$site == "")
              choices$site <- NA
          }
          tmp.hole <- unique(gsub("^.*Hole ([[:digit:]]*|[[:digit:]]*-[[:digit:]]*)\\W.*$", "\\1", grep("Hole ", tmp.odp, value = TRUE)))
          print(paste("Suggested hole: ", choices$hole, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          if (check == "FALSE") {
            choices$hole <- readline("Enter the hole: ")
            if (choices$hole == "")
              choices$hole <- NA
          }
          tmp.core <- unique(gsub("^.*Core ([[:alnum:]]*)\\W.*$", "\\1", grep("Core ", tmp.odp, value = TRUE)))
          print(paste("Suggested core: ", choices$core, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          if (check == "FALSE") {
            choices$core <- readline("Enter the core: ")
            if (choices$core == "")
              choices$core <- NA
          }
          # check these
          print(paste("So leg: ", choices$leg, sep = ""))
          print(paste("site: ", choices$site, sep = ""))
          print(paste("hole: ", choices$hole, sep = ""))
          print(paste("core: ", choices$core, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
      # work out the hole ID
      # is the hole ID in a column?
      if (is.null(choices$holeID)) {
        check <- FALSE
        while(check == FALSE) {
          choices$holeID <- dlg_list(names(data), title = "Is any column the hole ID (and not leg/hole etc.)?")$res
          if (length(choices$holeID) == 0) {
            tmp.ID <- paste(grep("Event", pan.data$meta.data[[1]], value = TRUE, ignore.case = TRUE), collapse = ";")
            print(tmp.ID)
            choices$holeID <- gsub("^Event\\(s\\):\\\t *(.*?)\\s.*$", "\\1", tmp.ID, ignore.case = TRUE)
          }
          print(paste("So hole ID: ", choices$holeID))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          # if the automation doesn't work
          if (check == FALSE) {
            choices$holeID <- paste(choices$leg, choices$hole, sep = "-")
            print(paste("So hole ID: ", choices$holeID[1]))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
          if (check == FALSE) {
            print(grep("Citation|Related to|Event", pan.data$meta.data[[1]], value = TRUE))
            choices$holeID <- readline("What is the hole ID: " )
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
        }
      }
    }
    # add the IODP data to the data frame
    if (choices$leg[1] %in% names(data) & choices$site[1] %in% names(data) & choices$hole[1] %in% names(data) & choices$core[1] %in% names(data)) {
      names(data)[names(data) == choices$leg] <- "leg"
      names(data)[names(data) == choices$site] <- "site"
      names(data)[names(data) == choices$hole] <- "hole"
      names(data)[names(data) == choices$core] <- "core"
    } else {
      data$leg <- as.character(choices$leg)
      data$site <- choices$site
      data$hole <- choices$hole
      data$core <- choices$core
    }
    if (choices$holeID[1] %in% names(data)) {
      names(data)[names(data) == choices$holeID] <- "holeID"
    } else{ 
      data$holeID <- choices$holeID 
    }
  }
  
  # if some ODP info is not complete see if you want to add it
  if (sum(is.na(data$leg)) != nrow(data) & sum(is.na(data$leg)) > 0) {
    if (is.null(choices$fill.iodp))
      choices$fill.iodp <- dlg_list(c("Yes", "No"), title = "Do you want to replace NAs in leg / hole?")$res
    if (choices$fill.iodp == "Yes") {
      data$leg[is.na(data$leg)] <- data$leg[!is.na(data$leg)][1]
      data$site[is.na(data$site)] <- data$site[!is.na(data$site)][1]
    }
  }
  
  
  # latitude / longitude ---------------
  if (any(colnames(data) == "Latitude")) {
    # if it is already a column
    choices$latitude <- "Latitude"
    choices$longitude <- "Longitude"
  } else {
    # otherwise extract the lat / long from the metadata
    # # tmp.coord <- grep("Coverage:", pan.data$meta.data[[1]], value = TRUE)
    # # choices$latitude <- gsub(".*latitude: *(.*?) \\*\\slongitude.*", "\\1", tmp.coord, ignore.case = TRUE)
    # # choices$longitude <- gsub("^.*longitude: *(.*?)$", "\\1", tmp.coord, ignore.case = TRUE)
    # # if (grepl("[A-z]", choices$latitude)|grepl("[A-z]", choices$longitude)) {
    # #   cores <- data.frame(core = unique(data$holeID), lat = NA, long = NA)
    # #   for (i in 1:nrow(cores)) {
    # #     cores$lat[i] <- gsub(".*latitude: *(\\S*).*$", "\\1", grep("latitude", grep(cores$core[i], pan.data$meta.data[[1]], value = TRUE), value = TRUE, ignore.case = TRUE), ignore.case = TRUE)
    # #     cores$long[i] <-gsub("^.*Longitude: (\\S*).*$", "\\1", grep("latitude", grep(cores$core[i], pan.data$meta.data[[1]], value = TRUE), value = TRUE, ignore.case = TRUE), ignore.case = TRUE)
    # #   }
    # #   choices$latitude <- cores$lat[match(data$holeID, cores$core)]
    # #   choices$longitude <- cores$lat[match(data$holeID, cores$core)]
    # # }
    #browser()
    choices$site.details <- data.frame(holeID = unique(data$holeID), lat = NA, long = NA, depth = NA, type = NA, stringsAsFactors = FALSE)
    for (i in choices$site.details$holeID) {
      tmp.coord <- grep(paste(i, ".*\\*"), pan.data$meta.data[[1]], value = TRUE)
      if (length(tmp.coord) == 0) {
        tmp.coord <- grep("^Event.*", pan.data$meta.data[[1]], value = TRUE)
      }
      choices$site.details$lat[choices$site.details$holeID == i] <- as.numeric(gsub(".*latitude: *(.*?) \\*\\slongitude.*", "\\1", tmp.coord, ignore.case = TRUE))
      choices$site.details$long[choices$site.details$holeID == i] <- as.numeric(gsub("^.*longitude: *([^ ]*).*$", "\\1", tmp.coord, ignore.case = TRUE))
      choices$site.details$depth[choices$site.details$holeID == i] <- as.numeric(gsub(".*ELEVATION: \\-*(.*?) m \\*\\s.*$", "\\1", tmp.coord, ignore.case = TRUE))
      if (grepl("DEVICE", tmp.coord, ignore.case = TRUE))
        choices$site.details$type[choices$site.details$holeID == i] <- gsub("^.*DEVICE: (.*?) \\* .*$|^.*DEVICE: (.*?)$", "\\1\\2", tmp.coord)
    }
    
    choices$latitude <- choices$site.details$lat[match(data$holeID, choices$site.details$holeID)]
    choices$longitude <- choices$site.details$long[match(data$holeID, choices$site.details$holeID)]
    
  }
  # add the data
  if (choices$latitude[1] %in% names(data)) {
    names(data)[names(data) == choices$latitude] <- "latitude"
    names(data)[names(data) == choices$longitude] <- "longitude"
  } else {
    data <- cbind(data, latitude = choices$latitude, longitude = choices$longitude)
  }
  data$latitude <- as.numeric(as.character(data$latitude))
  data$longitude <- as.numeric(as.character(data$longitude))
  
  # water depth -----------------
  if (!is.null(choices$site.details)) {
    if (any(!is.na(choices$site.details$depth))) {
      choices$water.depth <- choices$site.details$depth[match(data$holeID, choices$site.details$holeID)]
    }
  }
  if(is.null(choices$water.depth)) {
    choices$water.depth <- dlg_list(names(data), title = "Does any column contain the water depth?")$res
    if (length(choices$water.depth) == 0) {
      
    # if there is no explicit column of water depth, extract it from the metadata (-elevation)
      tmp.wd <- grep("Event\\(s\\):", pan.data$meta.data[[1]], value = TRUE)
      print(tmp.wd)
      choices$water.depth <- gsub(".*ELEVATION: \\-*(.*?) m \\*\\s.*$", "\\1", tmp.wd, ignore.case = TRUE)
      print(paste("So water depth: ", choices$water.depth, " m", sep = ""))
    } else {
      print(paste("So water depth: data$", choices$water.depth, " ", sep = ""))
    }
  }
  # add water depth to the data frame
  if (choices$water.depth[1] %in% names(data)) {
    names(data)[names(data) == choices$water.depth] <- "water.depth"
  } else {
    data <- cbind(data, water.depth = as.numeric(choices$water.depth))
  }
  
  # Sample-level information ---------------------------------------
  # - Age
  # - Depth
  
  # add in the sample ID
  data$sampleID <- paste(data$dbID, data$holeID, gsub("R", "", data$row.num ), sep = "_")
  
  
  # depth -------------
  if (is.null(choices$sample.depth)) {
    # is depth a column?
    if (length(grep("Depth", names(data))) == 1) {
      choices$sample.depth <- grep("Depth", names(data), value = TRUE)
    } else { # if not, what should we use
      check <- FALSE
      while(check == FALSE) {
        choices$sample.depth <- dlg_list(names(data), title = "Which column is the sample depth?")$res
        if (length(choices$sample.depth) == 0) {
          choices$sample.depth <- NA
        }
        print(paste("sample depth: data$", choices$sample.depth, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  # add the sample depth
  if (choices$sample.depth[1] %in% names(data)) {
    names(data)[names(data) == choices$sample.depth] <- "sample.depth"
  } else {
    data <- cbind(data, sample.depth = choices$sample.depth)
  }
  # odp details ------------------------
  # - Section
  # - Interval top
  # - Interval bottom
  if (!(grepl("ODP|DSDP", paste(pan.data$citation, pan.data$supplement_to)))) {
    section <- sample.top <- sample.bottom <- NA
  } else { # if it is ODP data
    if (is.null(choices$section) | is.null(choices$sample.top)) {
      # 
      if (length(iodp.col) > 0 & any(lapply(tmp.split, length) > 3)) {
        # if we have already got the iodp info from one column
        choices$section <- unlist(lapply(tmp.split, function(x) ifelse(length(x) == 3, NA, x[[4]])))
        # if the sample was a core catcher it won't have a sample top, so set that to NA
        if (max(unlist(lapply(tmp.split, length))) >= 5)
          choices$sample.top <- unlist(lapply(tmp.split, function(x) ifelse(length(x) == 3, NA, ifelse(length(x) == 4, ifelse(x[[4]] == "CC", 150, 0), x[[5]]))))
        else
          choices$sample.top <- NA
      } else {
        check <- FALSE
        while(check == FALSE) {
          choices$section <- dlg_list(names(data), title = "Which column is the ODP section?")$res
          if (length(choices$section) == 0) {
            choices$section <- readline("Enter the section: ")
            if (choices$section == "")
              choices$section <- NA
          }
          choices$sample.top <- dlg_list(names(data), title = "Which column is the ODP sample.top?")$res
          if (length(choices$sample.top) == 0) {
            choices$sample.top <- readline("Enter the sample top: ")
            if (choices$sample.top == "")
              choices$sample.top <- NA
          }
          print(paste("So section: ", choices$section, sep = ""))
          print(paste("sample.top: ", choices$sample.top, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
    }
    if (choices$section[1] %in% names(data)) {
      names(data)[names(data) == choices$section] <- "section"
    } else {
      data <- cbind(data, section = choices$section)
    }
    if (choices$sample.top[1] %in% names(data)) {
      names(data)[names(data) == choices$sample.top] <- "sample.top"
    } else {
      choices$sample.top[choices$sample.top == "NA"] <- NA
      data <- cbind(data, sample.top = as.numeric(as.character(gsub("CC", "0", choices$sample.top))))
    }
  }
  # if (is.na(choices$sample.depth) & all(is.na(choices$age)) & any(!is.na(choices$sample.top))) {
  #   data$sample.depth <- (as.numeric(as.character(gsub("[A-z]", "", data$core))) - 1) * 9 + (as.numeric(as.character(gsub("CC", "7", data$section))) - 1) * 1.5 + ifelse(is.na(data$sample.top), 0, data$sample.top/100)
  # }
  
  # if some sample depths are missing, recalculate them
  if (any(is.na(data$sample.depth) & !is.na(choices$section))) {
    # create a dataframe for the depths
    tmp.depth <- data.frame(row.num = data$row.num[!duplicated(data$row.num)], orig.depth = data$sample.depth[!duplicated(data$row.num)], core = data$core[!duplicated(data$row.num)], section = data$section[!duplicated(data$row.num)], sample.top = data$sample.top[!duplicated(data$row.num)])
    # estimate these based on the sampling
    tmp.depth$samples <- (as.numeric(as.character(gsub("[A-z]", "", tmp.depth$core))) - 1) * 9 + (as.numeric(as.character(gsub("\\D", "", gsub("CC", "7", tmp.depth$section)))) - 1) * 1.5 + ifelse((is.na(tmp.depth$sample.top)|tmp.depth$section == "CC"|tmp.depth$sample.top == "CC"), 0, suppressWarnings(as.numeric(as.character(tmp.depth$sample.top))/100))
  tmp.depth$scaled <- sapply(1:nrow(tmp.depth), depth.calc, tmp.depth)
    tmp.depth$scaled[!is.na(tmp.depth$orig.depth)] <- tmp.depth$orig.depth[!is.na(tmp.depth$orig.depth)]
    if (all(is.na(tmp.depth$orig.depth)))
      tmp.depth$scaled <- tmp.depth$samples

    data$sample.depth[is.na(data$sample.depth)] <- tmp.depth$scaled[match(data$row.num[is.na(data$sample.depth)], tmp.depth$row.num)]
  }
  
  
  # age ----------------------
  data$rng.age <- as.numeric(NA)
  data$AM.type <- as.character(NA)
  data$age.st <- as.numeric(NA)
  data$age.en <- as.numeric(NA)
  data$segment <- "s"
  data$age.calc <- factor("Orig", levels = c("Interp", "Magneto", "Model", "Orig", "Zone"))
  data$zon.age <- as.numeric(NA)
  data$age.err <- as.numeric(NA)
  data$int.age <- as.numeric(NA)
  data$err.int.age <- as.numeric(NA)
  data$mod.age <- as.numeric(NA)
  data$r2 <- as.numeric(NA)
  data$n.pts <- as.numeric(NA)
  
  
  if (is.null(choices$sample.age)) {
    # if there is an age column
    if (sum(grepl("Age \\[ka\\]", names(data))) == 1) {
      choices$sample.age <- grep("Age \\[ka\\]", names(data), value = TRUE)
    } else { # if automation doesn't work
      check <- FALSE
      while(check == FALSE) {
        choices$sample.age <- dlg_list(names(data), title = "Which column is the numeric age?")$res
        if (length(choices$sample.age) == 0) {
          two.ages <- dlg_list(c("Yes", "No"), title = "Is the age given as a range in two columns?")$res
          if (two.ages == "Yes") {
            max.age.col <- dlg_list(names(data), title = "Which columns is the max age?")$res
            min.age.col <- dlg_list(names(data), title = "Which columns is the min age?")$res
            choices$sample.age <- apply(data[, c(max.age.col, min.age.col)], 1, mean)
            choices$range.age <- abs(data[, max.age.col] - data[, min.age.col])
          } else {
            choices$sample.age <- readline("Enter the age: ")
            choices$sample.age <- ifelse(choices$sample.age == "", NA, as.numeric(choices$sample.age))
          }
        }
        print(paste("So age: ", choices$sample.age))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  if (!is.null(choices$range.age)) {
    data$rng.age <- choices$range.age
  }
  # check whether the age is in ka / Ma 
  if (is.null(choices$age.units)) {
    if (is.na(choices$sample.age)) {
      choices$age.units <- "Ma"
    } else {
      print(choices$sample.age)
      choices$age.units <- "ka"
      choices$age.units <- dlg_list(c("Ma", "ka", "None"), title = "What are the units of age?")$res
    }
  }
  # add zones
  if (is.null(choices$sample.zone)) {
    check <- FALSE
      while(check == FALSE) {
        choices$sample.zone <- dlg_list(names(data), title = "Which column contains the zone age?")$res
        if (length(choices$sample.zone) == 0) {
          choices$sample.zone <- NA
        }
        print(paste("So zone: ", choices$sample.zone))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    #}
  }
  
  # add the age column
  if (choices$sample.age[1] %in% names(data)) {
    names(data)[names(data) == choices$sample.age] <- "age"
  } else {
    data <- cbind(data, age = as.numeric(as.character(choices$sample.age)))
  }
  if (is.null(choices$no.num.age))
    choices$no.num.age <- is.na(data$age)
  
  # # if there are NAs in the age column, then drop those rows
  # if (sum(is.na(data$age)) > 0) {
  #   print(paste("Dropping ", sum(is.na(data$age)), " rows that have age NA"))
  #   data <- data[!is.na(data$age),]
  # }
  
  # if the age is in ka, then convert it to Ma
  if (choices$age.units == "ka") {
    data$age <- data$age / 1000 
  }
  # add zones
  if (choices$sample.zone %in% names(data)) {
    names(data)[names(data) == choices$sample.zone] <- "zone"
  } else {
    data <- cbind(data, zone = as.character(choices$sample.zone), stringsAsFactors = FALSE)
  }
  
 
  
  # zone type
  if (is.null(choices$zone.type) & !is.na(choices$sample.zone)) {
    print(unique(data$zone))
    choices$zone.type <- dlg_list(c("Foram", "Nanno", "Magneto", "Other", "Mixed"), title = "What is the age model type?")$res
  }
  if (any(choices$zone.type == "Chrono"))
    choices$zone.type[choices$zone.type == "Chrono"] <- "Magneto"
  
  # if we only have zones then use those to get the age
  if (any(!is.na(data$zone)) & (any(is.na(choices$sample.age)) | !is.null(choices$zone.ck) | any(is.na(data[, names(data) == "age"])))) {
    dat.zone <- data.frame(zone = data$zone, ocean = factor("IndoPac", levels = c("Atl", "IndoPac")), region = factor("Temp", levels = c("Temp", "Trop")), segment = data$segment, stringsAsFactors = FALSE)
    # get ocean polygons and work out origin of sites
    load("Data/OceanPolygons.RData")
    dat.zone$ocean[point.in.polygon(data$longitude, data$latitude, atlantic.1$x, atlantic.1$y) == 1] <- "Atl"
    
    # work out site region
    if (any(data$latitude < 23.5))
      dat.zone$region[data$latitude < 23.5] <- "Trop"
    
    check <- "Yes"
    while(check == "Yes") {
      tmp.age <- zones.fun(unique(dat.zone), scheme = choices$age.model, type = choices$zone.type)
      print(tmp.age)
      print("Unknown zones:")
      print(tmp.age$zone[is.na(tmp.age$zon.age)])
      if (is.null(choices$zone.ck))
        check <- dlg_list(c("Yes", "No"), title = "Do you want to make changes and re-run this?")$res
      else
        check <- "No"
    }
    choices$zone.ck <- "No"
    dat.zone$ID <- 1:nrow(dat.zone)
    dat.zone <- merge(dat.zone, tmp.age)
    dat.zone <- dat.zone[order(dat.zone$ID), ]
    dat.zone$sch[dat.zone$sch == "NA"] <- NA
    dat.zone$type[dat.zone$type == "NA"] <- NA
    
    data$zon.age <- dat.zone$zon.age
    data$age.st <- dat.zone$st
    data$age.en <- dat.zone$en
    data$rng.age <- dat.zone$age.rng
    if (!is.null(choices$no.num.age)) {
      choices$est.zone.type <- choices$age.st <- choices$age.en <- choices$range.age <- choices$est.age.model <- rep(NA, nrow(data))
      choices$est.age <- data$age
      data$AM.type[choices$no.num.age] <- choices$est.zone.type[choices$no.num.age] <- dat.zone$type[choices$no.num.age]
      data$age.model[choices$no.num.age] <- dat.zone$sch[choices$no.num.age]
      # data$age[choices$no.num.age] <- choices$est.age[choices$no.num.age] <- dat.zone$zon.age[choices$no.num.age]
      # data$age.err[choices$no.num.age] <- data$rng.age[choices$no.num.age]
      # data$age.calc[choices$no.num.age] <- "Zone"
    } else {
      data$age <- choices$sample.age <- dat.zone$age
      data$rng.age <- choices$range.age <- dat.zone$age.rng
      data$AM.type <- choices$type <- dat.zone$AM.type
      data$age.model <- choices$age.model <- dat.zone$sch
      data$age.st <- choices$age.st <- dat.zone$st
      data$age.en <- choices$age.en <- dat.zone$en
    }
    choices$zones.tab <- tmp.age
    
    # calculate interpolated / modelled ages
    for (i in unique(data$holeID)) {
      if (any(!is.na(data$zon.age[data$holeID == i]))) {
        # interpolated ages
        tmp.zon <- data[!duplicated(data$sampleID) & data$holeID == i, grep("core|hole|sample|age|zone", names(data))]
        tmp.int <- age.interp(tmp.zon)
        tmp.int <- tmp.int[tmp.int$interp == "TRUE",]
        if (nrow(tmp.int) != 0) {
          data$int.age[data$holeID == i] <- tmp.int$int.age[match(data$sampleID[data$holeID == i], tmp.int$sampleID)]
          data$err.int.age[data$holeID == i] <- tmp.int$err.int.age[match(data$sampleID[data$holeID == i], tmp.int$sampleID)]
        }
        
        # create a age-depth chart
        tmp.dat.age <- data.frame(Chron = c(paste("T", unique(tmp.zon$zone)), paste("B", unique(tmp.zon$zone))), Type = c(rep("T", length(unique(tmp.zon$zone))), rep("B", length(unique(tmp.zon$zone)))),  Zone = rep(unique(tmp.zon$zone), 2), stringsAsFactors = FALSE)
        tmp.dat.age$Age[tmp.dat.age$Type == "B"] <- tmp.zon$age.st[match(tmp.dat.age$Zone[tmp.dat.age$Type == "B"], tmp.zon$zone)]
        tmp.dat.age$Age[tmp.dat.age$Type == "T"] <- tmp.zon$age.en[match(tmp.dat.age$Zone[tmp.dat.age$Type == "T"], tmp.zon$zone)]
        # remove zones with identical ages
        tmp.dat.age <- tmp.dat.age[!duplicated(paste(tmp.dat.age$Age, tmp.dat.age$Type)), ]
        # calculate depths
        tmp.max <- tapply(tmp.zon$sample.depth, tmp.zon$age.st, max)
        tmp.dat.age$Depth[tmp.dat.age$Type == "B"] <- tmp.max[match(tmp.dat.age$Age[tmp.dat.age$Type == "B"], names(tmp.max))]
        tmp.min <- tapply(tmp.zon$sample.depth, tmp.zon$age.en, min)
        tmp.dat.age$Depth[tmp.dat.age$Type == "T"] <- tmp.min[match(tmp.dat.age$Age[tmp.dat.age$Type == "T"], names(tmp.min))]
        tmp.dat.age <- tmp.dat.age[order(tmp.dat.age$Depth), ]
        
        # set first / last depth to NA, as not clear they are the start / end of zones
        tmp.dat.age$Depth[c(1, nrow(tmp.dat.age))] <- NA
        tmp.dat.age <- tmp.dat.age[!is.na(tmp.dat.age$Depth) & !is.na(tmp.dat.age$Age), ]
        
        if (nrow(tmp.dat.age) > 1) {
          # add in gaps
          tmp.gap <- tmp.dat.age$Chron[1:(nrow(tmp.dat.age) - 1)][diff(tmp.dat.age$Depth) < 10 & diff(tmp.dat.age$Age) > 2 & tmp.dat.age$Type[1:(nrow(tmp.dat.age) - 1)] != "T"]
          if (length(tmp.gap) > 0) {
            gap <- c("Gap", NA, NA, NA, NA)
            for (j in tmp.gap) {
              gap.id <- which(tmp.dat.age$Chron == j)
              tmp.dat.age <- rbind(tmp.dat.age[1:gap.id,], gap, gap, tmp.dat.age[(gap.id+1):nrow(tmp.dat.age), ])
            }
            tmp.dat.age$Depth[is.na(tmp.dat.age$Depth)] <- as.numeric(tmp.dat.age$Depth[which(is.na(tmp.dat.age$Depth)) - 1]) + 0.1
            tmp.dat.age$Depth[is.na(tmp.dat.age$Depth)] <- as.numeric(tmp.dat.age$Depth[which(is.na(tmp.dat.age$Depth)) + 1]) - 0.1
            tmp.dat.age$Depth <- as.numeric(tmp.dat.age$Depth)
            tmp.dat.age$Age <- as.numeric(tmp.dat.age$Age)
          }
        
          choices[[paste("data.age", i, sep = "_")]] <- tmp.dat.age
          
          # calculate the modelled ages
          tmp.age.res <- age.model(tmp.dat.age, choices$zones.tab, data[!duplicated(data$sampleID) & data$holeID == i,], hole = i, plots = FALSE, pan = TRUE)
          data$mod.age[data$holeID == i] <- tmp.age.res$mod.age[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
          data$r2[data$holeID == i] <- tmp.age.res$r2[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
          data$n.pts[data$holeID == i] <- tmp.age.res$n.pts[match(data$sampleID[data$holeID == i], tmp.age.res$sampleID)]
        }
       # add in the estimated age
        data$age[choices$no.num.age & data$holeID == i] <- NA
        if (length(unique(data$age[data$holeID == i])) < length(unique(data$sample.depth[data$holeID == i])) & !all(is.na(data$age[data$holeID == i]))) {
          tmp.orig <- data$age[data$holeID == i]
          data$age[data$holeID == i] <- NA
          tmp.no.num.age <- choices$no.num.age[data$holeID == i]
          choices$no.num.age <- rep(TRUE, nrow(data[data$holeID == i,]))
        }
        
        uni.dat <- data[!duplicated(data$sampleID) & data$holeID == i, c("sampleID", "sample.depth", grep("age", names(data), value = TRUE), "zone")]
        uni.dat <- rbind(uni.dat, uni.dat[nrow(uni.dat),])
        age.mod <- pan.model.type[pan.model.type$DB == unique(data$dbID) & pan.model.type$hole == i, ]
        
        if (nrow(age.mod) == 1) {
          if (age.mod$Model == 1) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$mod.age) & data$holeID == i & data$mod.age >= 0
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$mod.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$mod.age[tmp.row]
            data$age.err[tmp.row] <- 0
            data$age.calc[tmp.row] <- "Model"
            uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
          }
          if (age.mod$Interp == 1|age.mod$Interp == 2) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$int.age) & data$holeID == i
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$int.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              #tmp.ex <- data$int.age < max(uni.dat$int.age[tmp.ex], na.rm = TRUE) & data$sample.depth > min(uni.dat$sample.depth[tmp.ex], na.rm = TRUE)
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$int.age[tmp.row]
            data$age.err[tmp.row] <- data$err.int.age[tmp.row]
            data$age.calc[tmp.row] <- "Interp"
          }
          if (age.mod$Model == 2) {
            tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$mod.age) & data$holeID == i & data$mod.age >= 0
            uni.dat$age[is.na(uni.dat$age)] <- uni.dat$mod.age[is.na(uni.dat$age)]
            if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
              tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
              tmp.ex <- tmp.ex[!is.na(tmp.ex)]
              tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
              tmp.row <- tmp.row & !tmp.ex
            }
            data$age[tmp.row] <- data$mod.age[tmp.row]
            data$age.err[tmp.row] <- 0
            data$age.calc[tmp.row] <- "Model"
            uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
          }
          
        }

        tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$zon.age) & data$holeID == i
        data$age[tmp.row] <- data$zon.age[tmp.row]
        data$age.err[tmp.row] <- data$rng.age[tmp.row]
        data$age.calc[tmp.row] <- "Zone"

        if (any(grepl("tmp.orig", ls()))) {
          data$age[data$holeID == i][is.na(data$age)] <- tmp.orig[data$holeID == i][is.na(data$age)]
          choices$no.num.age[data$holeID == i] <- tmp.no.num.age
        }
        data$age.calc[is.na(data$age) & data$holeID == i] <- NA
        data$age.calc <- factor(data$age.calc, levels = c("Interp", "Magneto", "Model", "Orig", "Zone"))

        rm(tmp.dat.age)
      }
    }
  }
  
  
 
  # sample type ---------------------
  if (!is.null(choices$site.details)) {
    if (any(!is.na(choices$site.details$type))) {
      choices$sample.type <- choices$site.details$type[match(data$holeID, choices$site.details$holeID)]
    }
  }
  if (is.null(choices$sample.type)) {
    # enter the sample type
    check <- FALSE
    while(check == FALSE) {
      choices$sample.type <- dlg_list(names(data), title = "Does any column contain the sample type (e.g. coring method)?")$res
      if (length(choices$sample.type) == 0) {
        # try and automate this
        tmp.dev <- grep("DEVICE: ", pan.data$meta.data[[1]], value = TRUE)
        print(tmp.dev)
        tmp.dev <- gsub("^.*DEVICE: (.*?) \\* .*$|^.*DEVICE: (.*?)$", "\\1\\2", tmp.dev)[1]
        if (length(tmp.dev) == 0 | is.na(tmp.dev)) {
          # if it can't be automated, see if the details are there anyway
          print(grep("^Event", pan.data$meta.data[[1]], value = TRUE))
          choices$sample.type <- readline("Enter the sample type: ")
          if (choices$sample.type == "")
            choices$sample.type <- NA
        } else if (nchar(tmp.dev) == 0) {
          # if it can't be automated, see if the details are there anyway
          print(grep("^Event", pan.data$meta.data[[1]], value = TRUE))
          choices$sample.type <- readline("Enter the sample type: ")
          if (choices$sample.type == "")
            choices$sample.type <- NA
        } else {
          choices$sample.type <- tmp.dev
        }
        print(paste("So sample type: ", choices$sample.type, sep = ""))
      } else {
        print(paste("So sample type: data$", choices$sample.type, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        tmp.check <- dlg_list(c(TRUE, FALSE), title = "Do you want to enter the sample type by hand (FALSE will cycle back through the options)")$res
        if (tmp.check) {
          choices$sample.type <- readline("Enter the sample type: ")
          if (choices$sample.type == "")
            choices$sample.type <- NA
          print(paste("So sample type: ", choices$sample.type, sep = ""))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
    }
  }
  if (choices$sample.type[1] %in% names(data)) {
    names(data)[names(data) == choices$sample.type] <- "sample.type"
  } else {
    data <- cbind(data, sample.type = choices$sample.type, stringsAsFactors = FALSE)
  }
  
  # - preservation -----------------
  if(is.null(choices$preservation)) {
    choices$preservation <- grep("preserv|Fragm plankt foram \\[%\\]", names(data), value = TRUE)[1]
    check <- FALSE
    while(check == FALSE) {
      if (is.null(choices$preservation)) 
        choices$preservation <- dlg_list(names(data), title = "Does any column contain the preservation?")$res
      if (length(choices$preservation) == 0) {
        # if it isn't a column, then see if the details are in the meta data
        print(grep("^Event", pan.data$meta.data[[1]], value = TRUE))
        choices$preservation <- readline("Enter the preservation: ")
        if (choices$preservation == "")
          choices$preservation <- NA
        print(paste("So preservation: ", choices$preservation, sep = ""))
      } else {
        print(paste("So preservation: data$", choices$preservation, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        choices$preservation <- NULL
      }
    }
  }
  if (choices$preservation %in% names(data)) {
    names(data)[names(data) == choices$preservation] <- "preservation"
    data$preservation <- as.character(data$preservation)
  } else {
    data <- cbind(data, preservation = choices$preservation, stringsAsFactors = FALSE)
  }
  
  # - processing --------------------
  if (is.null(choices$processing)) {
    check <- FALSE
    while(check == FALSE) {
      choices$processing <- dlg_list(names(data), title = "Does any column contain the sample processing information (e.g. sieve fraction)?")$res
      if (length(choices$processing) == 0) {
        # see if there are details of the processing method
        tmp.pro <- grep("method", pan.data$meta.data[[1]], ignore.case = TRUE, value = TRUE)
        print("Methods: ")
        print(unique(gsub("^.*method\\: ", "", tmp.pro, ignore.case = TRUE)))
        choices$processing <- readline("Enter the sample processing information: ")
        if (choices$processing == "")
          choices$processing <- NA
        print(paste("So sample processing information: ", choices$processing, sep = ""))
      } else {
        print(paste("So sample processing information: data$", choices$processing, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$processing %in% names(data)) {
    names(data)[names(data) == choices$processing] <- "processing"
  } else {
    data <- cbind(data, processing = choices$processing, stringsAsFactors = FALSE)
  }
  
  # paleolatitude ------------------
  if (is.null(choices$pal.lat.full))
    choices$pal.lat.full <- pal.lat.full
  if (any(!is.na(data$age))|!is.null(choices$pal.lat)) {
    if (max(as.numeric(as.character(data$age)), na.rm = TRUE) == 0) {
      # if all the data is extant, then no need for palaeolatitude
      data$pal.lat <- data$latitude
      data$pal.long <- data$longitude
    } else {
      
      if (is.null(choices$pal.lat) | is.null(choices$pal.long)) {
        # if there is no palaeolatitude information
        print(head(data))
        pl.check <- dlg_list(c("Yes", "No"), title = "Is there palaeolatitude information?")$res
        if (length(pl.check) == 0)
          pl.check <- "No"
        if (pl.check == "Yes") {
          check <- FALSE
          while(check == FALSE) {
            choices$pal.lat <- dlg_list(names(data), title = "Which column is the palaeolatitude?")$res
            choices$pal.long <- dlg_list(names(data), title = "Which column is the palaeolongitude?")$res
            print(paste("So palaeolatitude: data$", choices$pal.lat, sep = ""))
            print(paste("palaeolongitude: data$", choices$pal.long, sep = ""))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
          names(data)[names(data) == choices$pal.lat] <- "pal.lat"
          names(data)[names(data) == choices$pal.long] <- "pal.long"
        } else {
          # # if necessary extract palaeo lat information
          ## this is from palaeocoords, which is a function in mapast
          if (!pal.lat.full) { # i.e. if only testing
            # extract 5 random ages to test it on
            tmp.uni.ages <- unique(data$age[!is.na(data$age)])[1:5]
            tmp.pal.rows <- which(data$age %in% tmp.uni.ages[!is.na(tmp.uni.ages)])
            tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
          } else {
            tmp.pal.rows <- which(!is.na(data$age))
            tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
          }
        } 
        
      } else if (!(choices$pal.lat.full) & pal.lat.full) {
        # if the choices$pal.lat are only a subset and you want a full run
        tmp.pal.rows <- which(!is.na(data$age))
        tmp <- pal.coord(data[tmp.pal.rows,], model = model)
        choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
        choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
        choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
      } else {
        if (choices$pal.lat[1] %in% names(data)) {
          names(data)[names(data) == choices$pal.lat] <- "pal.lat"
          names(data)[names(data) == choices$pal.long] <- "pal.long"
        } else {
          data$pal.lat <- NA
          data$pal.long <- NA
          data$pal.lat <- choices$pal.lat
          data$pal.long <- choices$pal.long
        }
      }
    }
  }
  choices$pal.lat.full <- pal.lat.full
  # # if necessary convert ages to updated dates (Gradstein 2012)
  
  # # Extract species level information
  # species names ------------------------
  sp.check <- "unknown"
  if(is.null(choices$species) & is.null(choices$genus)) {
    print(head(data))
    ## sp.check <- dlg_list(c("Yes", "No"), title = "Is the species data currently in the form of a binomial?")$res
    ## if(sp.check == "Yes") {
     check <- FALSE
     while(check == FALSE) {
       choices$species <- dlg_list(names(data), title = "Which column is the species binomial?")$res
       if (length(choices$species) > 0) {
         print(paste("So species: data$", choices$species, sep = ""))
         check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
       } else {
         choices$genus <- dlg_list(names(data), title = "Which column is the genus?")$res
         choices$sp <- dlg_list(names(data), title = "Which column is the species?")$res
         choices$subsp <- dlg_list(names(data), title = "Which column is the subspecies?")$res
         print(paste("So genus is: data$", choices$genus, sep = ""))
         print(paste("species: data$", choices$sp, sep = ""))
         print(paste("subspecies: data$", choices$subsp, sep = ""))
         check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
       }
     }
     names(data)[names(data) == choices$species] <- "orig.species"
    # } else {
    #   check <- FALSE
    #   while(check == FALSE) {
    #     choices$genus <- dlg_list(names(data), title = "Which column is the genus?")$res
    #     choices$sp <- dlg_list(names(data), title = "Which column is the species?")$res
    #     choices$subsp <- dlg_list(names(data), title = "Which column is the subspecies?")$res
    #     print(paste("So genus is: data$", choices$genus, sep = ""))
    #     print(paste("species: data$", choices$sp, sep = ""))
    #     print(paste("subspecies: data$", choices$subsp, sep = ""))
    #     check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    #   }
    #}
  }
  if (!is.null(choices$species) | choices$species %in% names(data)) {
    names(data)[names(data) == choices$species] <- "orig.species"
  } 
  if (!is.null(choices$genus)) {
    species <- paste(data[, names(data) == choices$genus], data[, names(data) == choices$sp])
    if (length(choices$subsp) != 0) {
      sub.sp.col <- data[, names(data) == choices$subsp]
      species[!is.na(sub.sp.col)] <- paste(species[!is.na(sub.sp.col)], sub.sp.col[!is.na(sub.sp.col)])
    }
    data$orig.species <- species
  }
  # extract the full names from the meta data, and remove extra characters e.g. [%]
  sp.list <- sapply(unique(data$orig.species), function(x) unique(gsub("\t([^\\(\\[]*).*$", "\\1", grep(gsub("*$", "\\) \\\\*", gsub(" \\[.*$", "", x)), pan.data$meta.data[[1]], value = TRUE))), simplify = TRUE)
  # if there are direct duplicates
  if (is.list(sp.list)) {
    # solve any ones with no answers
    no.sp <- which(lapply(sp.list, length) == 0)
    if (length(no.sp) > 0) {
      # remove the brackets and try again
      sp.list[no.sp] <- sapply(names(sp.list)[no.sp], function(x) unique(gsub("\t([^\\(\\[]*).*$", "\\1", grep(gsub(" \\(.*$", "\\) \\\\*", gsub(" \\[.*$", "", x)), pan.data$meta.data[[1]], value = TRUE))), simplify = TRUE)
    }
    dup.sp <- which(lapply(sp.list, length) == 2)
    if (all(sp.list[[dup.sp[1]]] == sp.list[[dup.sp[2]]])) {
      sp.list[dup.sp[1]] <- sp.list[[dup.sp[1]]][1]
      sp.list[dup.sp[2]] <- sp.list[[dup.sp[2]]][2]
      if (length(unlist(sp.list)) == length(unique(data$orig.species)))
        sp.list <- unlist(sp.list)
    }
  }
  data$orig.species <- sp.list[match(data$orig.species, names(sp.list))]
  # remove the trailing space
  data$orig.species <- sapply(data$orig.species, function(x) gsub("(\\s+$)", "", x))
  
  # # Query database to correct for synonyms
  check <- "Yes"
  new.species <- NULL
  while(check == "Yes") {
    if (is.null(new.species)) {
      comp.list <- unique(data$orig.species)
      comp.rerun <- 1:length(comp.list)
    } else {
      comp.rerun <- which(new.species %in% c("Cretaceous", "Unsure", "unknown"))
      comp.list <- unique(data$orig.species)[comp.rerun]
    }
    if (is.null(choices$synonymy)) 
      choices$ages.sp <- dlg_list(c("Yes", "No"), title = "Should the synonymy list use age controls?")$res
    if (choices$ages.sp == "Yes") {
      new.species <- compare(unique(data$orig.species), micro = TRUE, age.check = TRUE)
    } else {
      new.species[comp.rerun] <- compare(comp.list, micro = TRUE)
    }
    print("The following species were classified as 'unknown'")
    print(sort(unique(data$orig.species)[new.species == "unknown"]))
    print("The following species were classified as 'Cretaceous'")
    print(sort(unique(data$orig.species)[new.species == "Cretaceous"]))
    print("The following species were classified as 'Unsure'")
    print(sort(unique(data$orig.species)[new.species == "Unsure"]))
    print("The following species were classified as 'Vague'")
    print(sort(unique(data$orig.species)[new.species == "Vague"]))
    if (!is.null(choices$synonymy)) {
      check <- "No"
    } else {
      check <- dlg_list(c("Yes", "No"), title = "Do you want to make some corrections and re-run this")$res
    }
  }
  data$species <- new.species[match(data$orig.species, unique(data$orig.species))]
  choices$synonymy <- "checked"
  # abundance --------------------
  # - Total IDd
  if(is.null(choices$total.IDd)) {
    check <- FALSE
    while(check == FALSE) {
    choices$total.IDd <- dlg_list(names(data), title = "Does any column contain information on the amount searched (e.g. total specimens IDd)?")$res
    if (length(choices$total.IDd) == 0) {
      # see if there is anything useful in the methods
      tmp.pro <- grep("method", pan.data$meta.data[[1]], ignore.case = TRUE, value = TRUE)
      print("Methods: ")
      print(unique(gsub("^.*method\\: ", "", tmp.pro, ignore.case = TRUE)))
      choices$total.IDd <- readline("Enter the total specimens IDd: ")
      if (choices$total.IDd == "")
        choices$total.IDd <- NA
      print(paste("So total specimens IDd: ", choices$total.IDd, sep = ""))
      } else {
        print(paste("So total specimens IDd: data$", choices$total.IDd, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$total.IDd %in% names(data)) {
    names(data)[names(data) == choices$total.IDd] <- "total.IDd"
  } else {
    data <- cbind(data, total.IDd = choices$total.IDd, stringsAsFactors = FALSE)
  }
  
  # abundance information
  if (is.null(choices$abundance)) {
    check <- FALSE
    while(check == FALSE) {
      if ("rel.abun" %in% names(data))
        choices$abundance <- "rel.abun"
      else 
        choices$abundance <- dlg_list(names(data), title = "Does any column contain the abundance?")$res
      if (length(choices$abundance) == 0) {
        choices$abundance <- "P"
      } else {
        print(paste("So abundance: data$", choices$abundance, sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$abundance %in% names(data)) {
    names(data)[names(data) == choices$abundance] <- "abundance"
  } else {
    data <- cbind(data, abundance = choices$abundance)
  }
  
  # convert FALSE to F where it was incorrectly changed
  if (any(data$abundance == "FALSE", na.rm = TRUE)) {
    data$abundance[is.na(data$abundance)] <- ""
    data$abundance[data$abundance == "FALSE"] <- "F"
  }
    
  # What are the abundance units
  print("Numeric characters:")
  print(summary(suppressWarnings(as.numeric(gsub("#|\\?|>|<", "", grep("\\d", data$abundance, value = TRUE))))))
  print("Non numeric characters:")
  print(grep("[^(0-9)|\\.]", unique(data$abundance), value = TRUE))
  if (is.null(choices$abun.units)) {
    # what are the abundance units
    print("Abundance units:")
    print(unique(gsub("^.*\\[(.*)\\].*$", "\\1", grep("\\[", pan.data$meta.data[[1]], value = TRUE))))
    choices$abun.units <- dlg_list(c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed") , title = "What are the units of abundance?")$res
  }
  data$abun.units <- factor(choices$abun.units, levels = c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed"))
  # convert everything to numbers
  data$orig.abundance <- data$abundance
  # where there are mixed units, identify numeric / non-numeric rows
  if (any(choices$abun.units == "Mixed")) {
    mix.tab <- tapply(data$abundance, data$holeID, max)
    mix.tab[!is.na(mix.tab)] <- "Num"
    mix.tab[is.na(mix.tab)] <- "NotNum"
    mix.row <- rep(NA, nrow(data))
    mix.row[data$holeID %in% names(mix.tab)[mix.tab == "Num"]] <- "Num"
    mix.row[data$holeID %in% names(mix.tab)[mix.tab == "NotNum"]] <- "NotNum"
    mix.row[mix.row == "Num"] <- dlg_list(c("Relative abundance", "Count", "Number per gram") , title = "What are the units of abundance for the numeric data?")$res
    mix.row[mix.row == "NotNum"] <- dlg_list(c("Binned", "P/A") , title = "What are the units of abundance for the non-numeric data?")$res
    data$abun.units <- choices$abun.units <- mix.row
  }
  if (any(choices$abun.units %in% c("Relative abundance", "Count", "Number per gram"))) {
    tmp.row <- which(data$abun.units %in% c("Relative abundance", "Count", "Number per gram"))
    tmp.abun <- data$abundance[tmp.row]
    if (any(grepl("[^(0-9)|\\.]", unique(tmp.abun)))) {
      if(is.null(choices$abun.num)) {
        print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
        choices$abun.num <- as.numeric(readline("What should this be replaced with? "))
      }
      tmp.abun[grep("[^(0-9)|\\.]", tmp.abun)] <- choices$abun.num
    }
    data$abundance[tmp.row] <- tmp.abun
  } 
  if (any(choices$abun.units == "P/A")) {
    tmp.row <- which(data$abun.units == "P/A")
    tmp.abun <- data$abundance[tmp.row]
    if (is.null(choices$abun.p)) {
      print(unique(tmp.abun))
      choices$abun.p <- readline("What symbol indicates presence? ")
      choices$abun.a <- readline("What symbol indicates absence? ")
      for (i in unique(tmp.abun)[!(unique(tmp.abun) %in% c(choices$abun.p, choices$abun.a))]) {
        tmp.pa <- dlg_list(c("Presence", "Absence") , title = paste("Should", i, "be presence or absence?"))$res
        if (tmp.pa == "Presence"){
          choices$abun.p <- c(choices$abun.p, i)
        } else if (tmp.pa == "Absence") {
          choices$abun.a <- c(choices$abun.a, i)
        }
      }
    }
    tmp.abun[tmp.abun %in% choices$abun.p] <- 1
    tmp.abun[tmp.abun %in% choices$abun.a] <- 0
    data$abundance[tmp.row] <- tmp.abun
  } 
  if (any(choices$abun.units == "Binned")) {
    tmp.row <- which(data$abun.units == "Binned")
    tmp.abun <- data$abundance[tmp.row]
    if (is.null(choices$abun.bin)) {
      print(grep("Citation", pan.data$meta.data[[1]], value = TRUE))
      print(grep("supplement", pan.data$meta.data[[1]], value = TRUE))
      print("")
      print(grep("abundance", pan.data$meta.data[[1]], value = TRUE))
      print("")
      print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
      choices$abun.bin <- readline("Enter the binned abundances starting with absent (e.g. n,r,c,a), leave reworked out: ")
      choices$abun.bin <- unlist(strsplit(choices$abun.bin, ","))
    } 
    tmp.abun <- match(tmp.abun, choices$abun.bin) - 1
    tmp.abun[is.na(tmp.abun)] <- 0
    data$abundance[tmp.row] <- tmp.abun 
  }
  data$abundance <- as.numeric(as.character(data$abundance))
  
  # remove those with NA abundances
  data <- data[!is.na(data$abundance),]
  
  # remove those not at species level
  data <- data[grepl("\\s", data$species),]
  # Where synonyms identify duplicates, then merge them 
  dup.sp <- which(duplicated(paste(data$age, data$sample.depth, data$latitude, data$longitude, data$species)))
  
 if (length(dup.sp) > 0) {
    tmp.sp <- tapply(data$orig.species, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    tmp.orig.abun <- tapply(data$orig.abundance, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    # calculate the summed abundances given the duplicates
    # if (!is.null(data$row.num)) {
    #   browser() # should I be using this version?
    #   tmp.abun <- tapply(data$abundance, list(data$row.num , data$species), sum, na.rm = TRUE)
    #   data <- data[-dup.sp,]
    #   # match the summed abundances with the data
    #   data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$row.num [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    # } else {
      tmp.abun <- tapply(data$abundance, list(data$sampleID , data$species), sum, na.rm = TRUE)
      tmp.abun2 <- with(data[data$abun.units == "Binned", ], tapply(abundance, list(sampleID , species), max, na.rm = TRUE))
      data <- data[-dup.sp,]
      # match the summed abundances with the data
      data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$sampleID [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    # }
    # if there is presence absence data, then reset the maximum to 1
    if (any(data$abun.units == "P/A")) {
      data$abundance[data$abun.units == "P/A" & data$abundance > 0] <- 1
    }
    
    # ditto for binned data
    if (any(data$abun.units == "Binned")) {
      data$abundance[data$abun.units == "Binned"] <- with(data[data$abun.units == "Binned",], sapply(1:sum(data$abun.units == "Binned"), function (x) tmp.abun2[match(sampleID[x], dimnames(tmp.abun2)[[1]]), match(species[x], dimnames(tmp.abun2)[[2]])]))
    }
    # merged original species
    data$orig.species <- sapply(1:nrow(data), function (x) tmp.sp[match(data$sampleID [x], dimnames(tmp.sp)[[1]]), match(data$species[x], dimnames(tmp.sp)[[2]])])
    data$orig.abundance <- sapply(1:nrow(data), function (x) tmp.orig.abun[match(data$sampleID [x], dimnames(tmp.orig.abun)[[1]]), match(data$species[x], dimnames(tmp.orig.abun)[[2]])])
    # if (is.numeric(data$abundance)) {
    #   # calculate the summed abundances given the duplicates
    #   tmp.abun <- tapply(data$abundance, list(data$row.num , data$species), sum, na.rm = TRUE)
    #   data <- data[-dup.sp,]
    #   # match the summed abundances with the data
    #   data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$row.num [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    #   # if there is presence absence data, then reset the maximum to 1
    #   if (any(choices$abun.units == "P/A")) {
    #     data$abundance[data$abun.units == "P/A" & data$abundance > 0] <- 1
    #   }
    #   # ditto for binned data
    #   if (any(choices$abun.units == "Binned")) {
    #     data$abundance[data$abun.units == "Binned" & data$abundance > (length(choices$abun.bin) - 1)] <- length(choices$abun.bin) - 1
    #   }
    # } else {
    #   stop("Non numeric data")
    # }
  }
  
  # add row ID
  data$rowID[order(data$sampleID)] <- paste(sort(data$sampleID), "_R", unlist(tapply(data$sampleID, data$sampleID, function(x) 1:length(x))), sep = "")
  
  # add in extra age columns
  data$mag.zone <- as.numeric(NA)
  data$mag.age.st <- as.numeric(NA)
  data$mag.age.en <- as.numeric(NA)
  data$mag.age <- as.numeric(NA)
  data$int.mag.age <- as.numeric(NA)
  data$err.int.mag.age <- as.numeric(NA)
  
  # return restructured dataframe
  col.inc <- c("rowID", "species", "orig.species", "abundance", "orig.abundance", "abun.units", "sample.depth", "segment", "age", "age.err", "age.calc", "zone", "zon.age", "age.st", "age.en", "rng.age", "int.age", "err.int.age", "mag.zone", "mag.age.st", "mag.age.en", "mag.age", "int.mag.age", "err.int.mag.age", "mod.age", "r2", "n.pts", "age.model", "AM.type", "latitude", "longitude", "pal.lat", "pal.long", "water.depth", "db.source", "dbID", "holeID", "sampleID", "reason", "leg", "site", "hole", "core", "section", "sample.top", "sample.type", "total.IDd", "preservation", "processing", "person", "date", "year", "source")
  
  final.data <- list(data = data[, col.inc], choices = choices)
  return(final.data)
}



# PNstructure for IODP ----------------------------
iodp.PNstructure <- function (file, input.init, choices = NULL, model = "MATTHEWS2016", pal.lat.full = TRUE, database.source = "IODP", iodp.info, rm.bk = FALSE, recalc.age = FALSE, chrons.full = all.chrons, age.chron = NA, iodp.orig.data = NA) {
  # pal.lat.full only runs paleolat for the first 5 data points to check that the function is working / get the choices. 
  # load the relevant libraries
  require("svDialogs")
  library(devtools)
  library(openxlsx)
  #install_github("macroecology/mapast")
  library(mapast)
  #source("Code/Triton_ForamSynonyms.R")
  library(tidyverse)
  library(readxl)
  library(sp)
  if (is.null(choices$sheets)) {
    # choices$sheets <- dlg_list(excel_sheets(file), title = "Which sheets should be used?")$res
    print(excel_sheets(file))
    choices$sheets <- readline("Which sheets should be used? " ) # need readline to be able to get more than one sheet
    choices$sheets <- excel_sheets(file)[eval(parse(text = (choices$sheets)))] 
  }
  if (!is.data.frame(iodp.orig.data)) {
    for (j in choices$sheets)  {
      tmp.data <- read_excel(file, sheet = j)
      # if the column headings are numbers
      if (!any(grepl("[A-z]", colnames(tmp.data)))) {
        tmp.data <- read_excel(file, sheet = j, skip = 1)
      }
      # if the first row is still the column headings
      if (length(grep("[\\(]", unlist(tmp.data[1,]))) == sum(!is.na(tmp.data[1,]))) {
        names(tmp.data)[grepl(paste(gsub("\\.{3}[0-9]*$", "", names(tmp.data)[!is.na(tmp.data[1,])]), collapse = "|"), names(tmp.data))] <- gsub("\\.{3}[0-9]*$", "", names(tmp.data)[grepl(paste(gsub("\\.{3}[0-9]*$", "", names(tmp.data)[!is.na(tmp.data[1,])]), collapse = "|"), names(tmp.data))])
        names(tmp.data)[!is.na(tmp.data[1,])] <- paste(names(tmp.data)[!is.na(tmp.data[1,])], tmp.data[1,c(!is.na(tmp.data[1,]))])
        tmp.data <- tmp.data[2:nrow(tmp.data), ]
      }
      names(tmp.data) <- gsub("\\.{3}[0-9]*$", "", names(tmp.data))
      # if necessary remove square brackets
      if (rm.bk) {
        tmp.data[] <- lapply(tmp.data, function(x) gsub("\\s+\\[.*\\]$", "", x))
        names(tmp.data) <- gsub(" _pink_", " (pink)", names(tmp.data))
        names(tmp.data) <- gsub(" _white_", " (white)", names(tmp.data))
        names(tmp.data) <- gsub("Sinistral _", "Sinistral (", names(tmp.data))
        names(tmp.data) <- gsub(" _.*$", "", names(tmp.data))
        names(tmp.data) <- gsub("\"", "'", names(tmp.data))
        names(tmp.data) <- gsub("\\s+\\[.*\\]$", "", names(tmp.data))
      }
      #View(tmp.data)
      
      # create a list - choices - that once populated allows the function to run automatically
      if (is.null(choices)) {
        choices <- list()
        choices$pal.lat.full <- pal.lat.full
      }
      # get the data
      # reshape: work out the columns to reshape into a long format data frame ----------------------
      if (!(gsub("_| ", "", paste("var.col", j, sep = ".")) %in% names(choices)) & (length(j) == 1 & length(grep("var.col$", names(choices), value = TRUE)) == 0)) {
        # assume all columns are varying except depth, age, lat, long, etc.
        tmp.var.col <- which(!grepl("Depth|Age|zone|Epoch|Latitude|Longitude|Sample label|Foram|foram|Event|Stratigraphy|Comment", names(tmp.data)))
        # check the automation is correct
        print(paste(c("Suggested varying columns are: ", names(tmp.data)[tmp.var.col])))
        print(paste(c("Non varying are: ", names(tmp.data)[-tmp.var.col])))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        while(check == FALSE) {
          # if the automation didn't work, then do this by hand
          print(names(tmp.data))
          tmp.var.col <- eval(parse(text = readline("Which column numbers vary (i.e. are species): " )))
          print(paste(c("So varying columns are: ", names(tmp.data)[tmp.var.col])))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      } else {
        if (!is.null(choices$var.col)) {
          # if one sheet
          tmp.var.col <- choices$var.col
        } else {
          # if multiple sheets
          tmp.var.col <- choices[paste("var.col.", gsub("_|\\s", "", j), sep = "")][[1]]
        }
      }
      names(tmp.data) <- gsub("__.$", "", names(tmp.data))
      # if some of the columns are duplicated
      if (length(unique(colnames(tmp.data)[tmp.var.col])) != length(tmp.var.col)) {
        dup.col <- which(duplicated(colnames(tmp.data)))
        uniq.col <- unique(colnames(tmp.data)[tmp.var.col])
        print("Duplicated columns:")
        print(colnames(tmp.data)[dup.col])
        if (is.null(choices$merge))
          choices$merge <- dlg_list(c("Yes", "No"), title = "Do you want to merge the duplicated columns?")$res
        if (choices$merge == "Yes") {
          for (i in unique(colnames(tmp.data)[dup.col])) {
            colnames(tmp.data)[colnames(tmp.data) == i] <- paste(i, " [", 1:length(colnames(tmp.data)[colnames(tmp.data) == i]), "]", sep = "")
          }
        }
      }
      if (length(choices$sheets) > 1) {
        tmp.data$row.num <- paste("R", which(choices$sheets == j), ".", 1:nrow(tmp.data), sep = "")
      } else {
        tmp.data$row.num <- paste("R", 1:nrow(tmp.data), sep = "")
      }
      # reshape the data
      names(tmp.data)[duplicated(names(tmp.data))] <- paste(names(tmp.data)[duplicated(names(tmp.data))], "1", sep = "_")
      if (length(tmp.var.col) > 1) {
        assign(paste("dataset", gsub(" ", "_", j), sep = "."), gather(tmp.data, Species, rel.abun, colnames(tmp.data)[tmp.var.col]))
      } else {
        assign(paste("dataset", gsub(" ", "_", j), sep = "."), gather(tmp.data, Species, rel.abun, colnames(tmp.data)[eval(parse(text = tmp.var.col))]))
      }
      if (!any(grepl(paste("var.col", j, sep = "."), names(choices)))) {
        choices <- c(choices, list(tmp.var.col))
        names(choices)[length(choices)] <- gsub("_| ", "", paste("var.col", j, sep = "."))
      }
    }
    orig.data <- eval(parse(text = paste("bind_rows(", paste(grep("dataset", ls(), value = TRUE), collapse = ","), ")", sep = "")))
  } else {
    orig.data <- iodp.orig.data
  }
  
  data <- orig.data
  
  # print to check
  print(head(data))
  
  # database information ----------------
  # (database source, date stamp, inputter)
  
  data <- cbind(data, db.source = "IODP", date = Sys.Date(), person = input.init, dbID = "IODP", stringsAsFactors = FALSE)
  
  # add a column of why the data was collected
  if (is.null(choices$reason)) {
    choices$reason <- "Unspecified"
  }
  data$reason <- factor(choices$reason, levels = c("Community analysis", "Selected species", "Proxies", "Extreme event", "Biostratigraphy", "Unspecified"))
  
  ## Site-level columns -----------------------------
  # source/year -----------
  if (is.null(choices$source)) {
    check <- FALSE
    while(check == FALSE) {
      choices$source <- readline("Enter the source: ")
      choices$source <- paste("Core Report ", gsub("^.*\\)/([^/]*)/.*$", "\\1", file), " ", gsub("^.*/([^/]*)/.*$", "\\1", file) ,", doi:", choices$source, sep = "")
      print(paste("So source: ", choices$source, sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  
  data <- cbind(data, source = as.character(choices$source))
  if (is.null(choices$year)) {
    # check <- FALSE
    # while(check == FALSE) {
    #   choices$year <- readline("Enter the year: ")
    #   
    #   check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    # }
    choices$year <- gsub(".*(.{4})$", "\\1", choices$source)
    if (choices$year > 2020 | choices$year < 1850) {
      check <- FALSE
      while(check == FALSE) {
        choices$year <- readline("Enter the year: ")

        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
    print(paste("So year: ", choices$year, sep = ""))
  }
  data <- cbind(data, year = as.numeric(choices$year))
  # age model -------------------
  if (is.data.frame(age.chron)) {
    age.file <- grep(paste("^[^~].*", gsub("^.*(.)\\.xlsx$|^.*(.)\\.xls$", "\\1", tmp.file), "_chrono", sep = ""), grep(gsub("[^/]*$", "", file), age.chron$file, value = TRUE), value = TRUE)
    if (any(!grepl("xlsx", age.file))) {
      browser()
      age.file <- grep(gsub("[^/]*$", "", file), age.chron$file, value = TRUE)
    }
    if (length(age.file) > 1) {
      age.file <- grep(paste("^[^~].*", gsub("^.*([A-Z])\\.xl.*$", "\\1", file), "_chrono", sep = ""), grep(gsub("[^/]*$", "", file), age.chron$file, value = TRUE), value = TRUE)
    }
    if (any(!grepl("\\.xl.*$", age.file))) {
      browser()      
      age.file <- grep(paste("^[^~][^A-Z]*", "_chrono", sep = ""), grep(gsub("[^/]*$", "", file), age.chron$file, value = TRUE), value = TRUE)
    }
    if (any(grepl("xls", age.file))) {
      data.age <- age.chron$chron[age.chron$file == age.file[1]][[1]]
      #View(data.age)
    }
  } else {
    age.file <- paste(gsub("[^/]*$", "", file), grep(paste("^[^~].*", gsub("^.*(.)\\.xlsx$|^.*(.)\\.xls$", "\\1", tmp.file), "_chrono", sep = ""), list.files(gsub("[^/]*$", "", file)), value = TRUE), sep = "")
    if (any(!grepl("xlsx", age.file)))
      age.file <- paste(gsub("[^/]*$", "", file), grep(paste("^[^~].*", "_chrono", sep = ""), list.files(gsub("[^/]*$", "", file)), value = TRUE), sep = "")
    if (length(age.file) > 1)
      age.file <- paste(gsub("[^/]*$", "", file), grep(paste("^[^~].*", gsub("^.*([A-Z])\\.xl.*$", "\\1", file), "_chrono", sep = ""), list.files(gsub("[^/]*$", "", file)), value = TRUE), sep = "")
    if (any(!grepl("\\.xl.*$", age.file)))
      age.file <- paste(gsub("[^/]*$", "", file), grep(paste("^[^~][^A-Z]*", "_chrono", sep = ""), list.files(gsub("[^/]*$", "", file)), value = TRUE), sep = "")
    if (any(grepl("xls", age.file))) {
      data.age <- read_excel(age.file[1])
    #View(data.age)
    }
  }
  if (is.null(choices$age.model)) {
    if (any(grepl("data.age", ls()))) {
      if (length(unique(data.age$Scheme)) == 1) {
        choices$age.model <- gsub(" ", "", unique(data.age$Scheme))
        print(paste("So age model:", choices$age.model))
      }
    }
    while(is.null(choices$age.model)) {
      if (any(grepl("data.age", ls())))
        print(unique(data.age$Scheme))
      choices$age.model <- dlg_list(c("Wade2011 (P,E,O,M,Pl,Pt)", "Berggren2005 (P,E,O)", "Berggren1995 (P,M,Pl,Pt)", "Berggren1977 (Pl)", "GTSBlow1969 (P,N)", "C&P1997/P&C1997 (P,N)", "B&M1988 (P)", "K&S1983 (N)", "Blow1969 (P13-N12)", "Ericson1968 (P-Z)", "Huber2005 (AP,AE,AO)", "Huber1995 (AP)", "Raffi2006 (NN)", "GTS2004 (MoveDB uses this)", "Other", "Extra"), title = "What is the age model?")$res
      if (length(choices$age.model) == 0)
        choices$age.model <- "Other"
      choices$age.model <- gsub( "\\ .*$", "", choices$age.model)
    }
  }
  data <- cbind(data, age.model = choices$age.model)
  
  # odp sites -----------------------
  if (is.null(choices$holeID) & !is.null(choices$coreID)) {
    choices$holeID <- choices$coreID
    choices$coreID <- NULL
  }
  choices$iodp.col <- NULL
  tmp.split <- NULL
  if (is.null(choices$leg) | is.null(choices$site) | is.null(choices$hole) | is.null(choices$core)) {
    print(head(data))
    choices$iodp.lvl <- dlg_list(c("leg", "site", "hole", "core", "none"), title = "Which level of ODP data is first in the column?")$res
    if (choices$iodp.lvl == "none") {
      tmp.split <- vector("list", nrow(data))
    } else {
      check <- FALSE
      while(check == FALSE) {
        if (is.null(choices$iodp.col)) {
          print(names(data))
          choices$iodp.col <- "Sample label"
        } else {
          choices$iodp.col <- dlg_list(names(data), title = "Which column is the ODP info?")$res
        }
        print(paste("So info: ", choices$iodp.col, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
      # extract all the iodp info from the column
      tmp.split <- strsplit(data[,choices$iodp.col], "\\-|,|/")
    }
    if (choices$iodp.lvl != "leg") {
      check <- FALSE
      while(check == FALSE) {
        choices$leg <- gsub("^.*\\)/([^/]*)/.*$", "\\1", file)
        print(paste("Suggested leg is:", choices$leg))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        if (check == FALSE) {
          choices$leg <- dlg_list(names(data), title = "Which column is the leg?")$res
          if (length(choices$leg) == 0) {
            print(paste("The leg is:", choices$leg))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
        }
        if (check == FALSE) {
          choices$leg <- readline("Enter the leg: ")
          print(paste("The leg is:", choices$leg))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
    }
    if (!(choices$iodp.lvl %in% c("site", "leg"))) {
      check <- FALSE
      while(check == FALSE) {
        choices$site <- gsub("[A-Z]$", "", gsub("^.*/([^/]*)/.*$", "\\1", file))
        print(paste("Suggested site is:", choices$site))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        if (check == FALSE) {
          choices$site <- dlg_list(names(data), title = "Which column is the site?")$res
          if (length(choices$leg) == 0) {
            print(paste("The site is:", choices$site))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
        }
        if (check == FALSE) {
          choices$site <- readline("Enter the site: ")
          print(paste("The site is:", choices$site))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
    }
    if (!(choices$iodp.lvl %in% c("site", "leg", "hole"))) {
      check <- FALSE
      while(check == FALSE) {
        choices$hole <- gsub("^.*/([^/]*)/.*$", "\\1", file)
        print(paste("Suggested hole is:", choices$hole))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        if (check == FALSE) {
          choices$hole <- dlg_list(names(data), title = "Which column is the hole?")$res
          if (length(choices$hole) != 0) {
            print(paste("The hole is:", choices$hole))
            check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
          }
        }
        if (check == FALSE) {
          choices$hole <- readline("Enter the hole: ")
          print(paste("The hole is:", choices$hole))
          check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        }
      }
    }
    check <- FALSE
    while(check == FALSE) {
      tmp.split <- lapply(tmp.split, function(x) c(choices$leg, choices$hole, x))
    
      if (is.null(choices$leg))
        choices$leg <- unlist(lapply(tmp.split, "[[", 1))
      if (is.null(choices$site)) {
        rm.space <- unlist(lapply(tmp.split, "[[", 2))
        rm.space <- gsub("\\s+", "", rm.space)
        choices$site <- gsub("[A-z]{1}$", "", rm.space)
      }
      if (is.null(choices$hole)) {
        rm.space <- unlist(lapply(tmp.split, "[[", 2))
        choices$hole <- gsub("\\s+", "", rm.space)
      }
      if (any(lapply(tmp.split, length) > 2)) {
        choices$core <- unlist(lapply(tmp.split, "[[", 3))
      } else {
        choices$core <- NA
      }
      print(paste("So leg: ", unique(choices$leg)))
      print(paste("site: ", unique(choices$site)))
      print(paste("hole: ", unique(choices$hole)))
      print(c("core: ", choices$core[1:ifelse(length(choices$core) > 10, 10, length(choices$core))]))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        choices$leg <- readline("Enter the leg: ")
        choices$site <- readline("Enter the site: ")
        choices$hole <- readline("Enter the hole: ")
        choices$core <- readline("Enter the core: ")
        print(paste("So leg: ", choices$leg[1]))
        print(paste("site: ", choices$site[1]))
        print(paste("hole: ", choices$hole[1]))
        print(c("core: ", choices$core[1:ifelse(length(choices$core) > 10, 10, length(choices$core))]))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  # add the IODP data to the data frame
  data$leg <- as.character(choices$leg)
  data$site <- choices$site
  data$hole <- choices$hole
  data$core <- as.character(choices$core)
  
  # work out the hole ID
  # is the hole ID in a column?
  data$holeID <- choices$holeID <- gsub("_", ".", gsub("iodp|.xls|x$", "", tmp.file))
  # add in the sample ID
  data$sampleID <- paste(data$dbID, data$holeID, gsub("R", "", data$row.num ), sep = "_")
  
  if (length(unique(data$sampleID)) < length(unique(data$sample.depth))) {
    browser()
  }
  
  # latitude / longitude ---------------
  if (is.null(choices$latitude)) {
    check <- FALSE
    while(check == FALSE) {
      if (any(iodp.info$Leg == unique(choices$leg))) {
        tmp.leg <- unique(choices$leg)
      } else {
        tmp.leg <- unique(grep(unique(choices$leg), iodp.info$Leg, value = TRUE))
      }
      if (any(iodp.info$Site == unique(choices$site))) {
        tmp.site <- unique(choices$site)
      } else {
        tmp.site <- unique(grep(gsub("^[A-z]", "", unique(choices$site)), iodp.info$Site, value = TRUE))
      }
      tmp.hole <- gsub("^[A-z]", "", unique(choices$hole))
      # if there is more than one hole
      if (length(tmp.hole) > 1) {
        for(i in 1:length(tmp.hole)) 
          tmp.hole[i] <- ifelse(length(unique(grep(tmp.hole[i], iodp.info$Hole, value = TRUE))) > 0, unique(grep(tmp.hole[i], iodp.info$Hole, value = TRUE)), NA)
        tmp.hole <- tmp.hole[!is.na(tmp.hole)]
        choices$longitude <- choices$latitude <- rep(NA, length(tmp.hole))
        for(j in 1:length(tmp.hole)) {
          choices$latitude[j] <- iodp.info$DecLat[iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole[j], "$", sep = ""), iodp.info$Hole)]
          choices$longitude[j] <- iodp.info$DecLong[iodp.info$Leg == tmp.leg &  grepl(paste(tmp.hole[j], "$", sep = ""), iodp.info$Hole)] 
          names(choices$latitude)[j] <- names(choices$longitude)[j] <- iodp.info$Hole[iodp.info$Leg == tmp.leg &  grepl(paste(tmp.hole[j], "$", sep = ""), iodp.info$Hole)]
        }
      } else {
        if (any(iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole, collapse = "|"), iodp.info$Hole))) {
          if (length(grep(paste(tmp.hole, collapse = "|"), iodp.info$Hole)) > 1) {
            choices$latitude <- iodp.info$DecLat[iodp.info$Leg == tmp.leg & iodp.info$Hole == tmp.hole]
            choices$longitude <- iodp.info$DecLong[iodp.info$Leg == tmp.leg & iodp.info$Hole == tmp.hole] 
            names(choices$latitude) <- names(choices$longitude) <- iodp.info$Hole[iodp.info$Leg == tmp.leg & iodp.info$Hole == tmp.hole]
          } else {
            choices$latitude <- iodp.info$DecLat[iodp.info$Leg == tmp.leg & grepl(tmp.hole, iodp.info$Hole)]
            choices$longitude <- iodp.info$DecLong[iodp.info$Leg == tmp.leg &  grepl(tmp.hole, iodp.info$Hole)] 
            names(choices$latitude) <- names(choices$longitude) <- iodp.info$Hole[iodp.info$Leg == tmp.leg &  grepl(tmp.hole, iodp.info$Hole)]
          }
        } else {
          choices$latitude <- iodp.info$DecLat[iodp.info$Leg == tmp.leg & iodp.info$Site == tmp.site]
          choices$longitude <- iodp.info$DecLong[iodp.info$Leg == tmp.leg & iodp.info$Site == tmp.site] 
          names(choices$latitude) <- names(choices$longitude) <- iodp.info$Hole[iodp.info$Leg == tmp.leg & iodp.info$Site == tmp.site]
        }
      }
      print(paste("So latitude: ", choices$latitude, sep = ""))
      print(paste("longitude: ", choices$longitude, sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        choices$latitude <- readline("Enter the latitude: ")
        choices$longitude <- readline("And the longitude: ")
        print(paste("So latitude: ", choices$latitude, sep = ""))
        print(paste("longitude: ", choices$longitude, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  # add the data
  if (length(choices$latitude) > 1 & length(choices$longitude) > 1) {
    data$latitude <- NA
    data$longitude <- NA
    for (i in 1:length(unique(choices$hole))) {
      data$latitude[grepl(unique(choices$hole)[i], data$hole)] <- as.numeric(choices$latitude[names(choices$latitude) == gsub("^[A-z]", "", unique(choices$hole)[i])])
      data$longitude[grepl(unique(choices$hole)[i], data$hole)] <- as.numeric(choices$longitude[names(choices$longitude) == gsub("^[A-z]", "", unique(choices$hole)[i])])
    }
  } else {
    data <- cbind(data, latitude = as.numeric(choices$latitude), longitude = as.numeric(choices$longitude), stringsAsFactors = FALSE)
  }
  # water depth -----------------
  if(is.null(choices$water.depth)) {
    check <- FALSE
    while(check == FALSE) {
      # if there is more than one hole
      if (length(tmp.hole) > 1) {
        choices$water.depth <- rep(NA, length(tmp.hole))
        for(j in 1:length(tmp.hole)) {
          choices$water.depth[j] <- iodp.info$mbsl[iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole[j], "$", sep = ""), iodp.info$Hole)]
          names(choices$water.depth)[j] <- iodp.info$Hole[iodp.info$Leg == tmp.leg &  grepl(paste(tmp.hole[j], "$", sep = ""), iodp.info$Hole)]
        }
      } else {
        if (any(iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole, collapse = "|"), iodp.info$Hole))) {
          if (length(grepl(tmp.hole, iodp.info$Hole)) > 1) {
            choices$water.depth <- iodp.info$mbsl[iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole, "$", sep = ""), iodp.info$Hole)]
            names(choices$water.depth) <- iodp.info$Hole[iodp.info$Leg == tmp.leg & grepl(paste(tmp.hole, "$", sep = ""), iodp.info$Hole)]
          } else {
            choices$water.depth <- iodp.info$mbsl[iodp.info$Leg == tmp.leg & grepl(tmp.hole, iodp.info$Hole)]
            names(choices$water.depth) <- iodp.info$Hole[iodp.info$Leg == tmp.leg & grepl(tmp.hole, iodp.info$Hole)]
          }
        } else {
          choices$water.depth <- iodp.info$mbsl[iodp.info$Leg == tmp.leg & iodp.info$Site == tmp.site]
          names(choices$water.depth) <- iodp.info$Hole[iodp.info$Leg == tmp.leg & iodp.info$Site == tmp.site]
        }
      }
      print(paste("So water depth: ", unique(choices$water.depth), " m", sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        choices$water.depth <- readline("Enter the water depth: ")
        print(paste("So water depth: ", choices$water.depth, " m", sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  if (length(choices$water.depth) > 1) {
    data$water.depth <- NA
    for (i in 1:length(unique(choices$hole)))
      data$water.depth[grepl(unique(choices$hole)[i], data$hole)] <- as.numeric(choices$water.depth[names(choices$latitude) == gsub("^[A-z]", "", unique(choices$hole)[i])])
  } else {
    data <- cbind(data, water.depth = as.numeric(choices$water.depth), stringsAsFactors = FALSE)
  }
  
  # Sample-level information ---------------------------------------
  # - Age
  # - Depth
  # depth -------------
  if (is.null(choices$sample.depth)) {
    # is depth a column?
    check <- FALSE
    while(check == FALSE) {
      if (is.null(choices$sample.depth)) {
        choices$sample.depth <- grep("Depth", names(data), value = TRUE)
      } else {
        print(names(data))
        choices$sample.depth <- names(data)[eval(parse(text = readline("Which column(s) are sample depth: " )))]
      }
      if (length(choices$sample.depth) == 0) {
        choices$sample.depth <- NA
      }
      print(paste("sample depth: data$", choices$sample.depth, sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  # add the sample depth
  if (all(!is.na(choices$sample.depth))) {
    if (length(setdiff(choices$sample.depth, names(data))) > 0) {
      choices$sample.depth <- gsub("\\s\\[.*$", "", choices$sample.depth)
    }
    if (length(choices$sample.depth) > 1) {
      data$sample.depth <- rowMeans(apply(data[, choices$sample.depth], 2, as.numeric))
    } else {
      names(data)[names(data) == choices$sample.depth] <- "sample.depth"
    }
    data$sample.depth <- as.numeric(data$sample.depth)
  } else {
    data$sample.depth <- NA
  }
    
  
  # odp details ------------------------
  # - Section
  # - Interval top
  # - Interval bottom
  if ((is.null(choices$section) | is.null(choices$sample.top))) {
    if (length(choices$iodp.col) > 0 & any(lapply(tmp.split, length) > 3)) {
      # if we have already got the iodp info from one column
      choices$section <- unlist(lapply(tmp.split, function(x) ifelse(length(x) == 3, NA, x[[4]])))
      # if the sample was a core catcher it won't have a sample top, so set that to NA
      if (max(unlist(lapply(tmp.split, length))) >= 5)
        choices$sample.top <- unlist(lapply(tmp.split, function(x) ifelse(length(x) == 3, NA, ifelse(length(x) == 4, ifelse(x[[4]] == "CC", 150, 0), x[[5]]))))
      else
        choices$sample.top <- NA
    } else {
      check <- FALSE
      while(check == FALSE) {
        choices$section <- dlg_list(names(data), title = "Which column is the ODP section?")$res
        if (length(choices$section) == 0) {
          choices$section <- readline("Enter the section: ")
          if (choices$section == "")
            choices$section <- NA
        }
        choices$sample.top <- dlg_list(names(data), title = "Which column is the ODP sample.top?")$res
        if (length(choices$sample.top) == 0) {
          choices$sample.top <- readline("Enter the sample top: ")
          if (choices$sample.top == "")
            choices$sample.top <- NA
        }
        print(paste("So section: ", choices$section, sep = ""))
        print(paste("sample.top: ", choices$sample.top, sep = ""))
        check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      }
    }
  }
  choices$sample.top <- gsub(" ", "", choices$sample.top)
  data <- cbind(data, section = choices$section)
  data <- cbind(data, sample.top = choices$sample.top)
  
  # if some sample depths are missing, recalculate them
  if (any(is.na(data$sample.depth) & !is.na(choices$sample.top))) {
    # create a dataframe for the depths
    tmp.depth <- data.frame(row.num = data$row.num[!duplicated(data$row.num)], orig.depth = data$sample.depth[!duplicated(data$row.num)], core = data$core[!duplicated(data$row.num)], section = data$section[!duplicated(data$row.num)], sample.top = data$sample.top[!duplicated(data$row.num)])
    # estimate these based on the sampling
    tmp.depth$samples <- (as.numeric(as.character(gsub("[A-z]", "", tmp.depth$core))) - 1) * 9 + (as.numeric(as.character(gsub("\\D", "", gsub("CC", "7", tmp.depth$section)))) - 1) * 1.5 + ifelse((is.na(tmp.depth$sample.top)|tmp.depth$section == "CC"|tmp.depth$sample.top == "CC"), 0, suppressWarnings(as.numeric(as.character(tmp.depth$sample.top))/100))
    tmp.depth$scaled <- sapply(1:nrow(tmp.depth), depth.calc, tmp.depth)
    tmp.depth$scaled[!is.na(tmp.depth$orig.depth)] <- tmp.depth$orig.depth[!is.na(tmp.depth$orig.depth)]
    if (all(is.na(tmp.depth$orig.depth)))
      tmp.depth$scaled <- tmp.depth$samples
    
    data$sample.depth[is.na(data$sample.depth)] <- tmp.depth$scaled[match(data$row.num[is.na(data$sample.depth)], tmp.depth$row.num)]
  }
  
  # age ----------------------
  if (is.null(choices$sample.age)) {
    # if there is an age column
    check <- FALSE
    while(check == FALSE) {
      choices$sample.age <- dlg_list(names(data), title = "Which column is the numeric age?")$res
      if (length(choices$sample.age) == 0) {
        # choices$sample.age <- readline("Enter the age: ")
        # choices$sample.age <- ifelse(choices$sample.age == "", NA, as.numeric(choices$sample.age))
        choices$sample.age <- NA
      }
      print(paste("So age: ", choices$sample.age))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (is.null(choices$range.age)) {
    data$rng.age <- NA
  } else {
    data$rng.age <- choices$range.age
  }
  # check whether the age is in ka / Ma
  if (is.null(choices$age.units)) {
    if (is.na(choices$sample.age)) {
      choices$age.units <- "Ma"
    } else {
      print(choices$sample.age)
      choices$age.units <- "ka"
      choices$age.units <- dlg_list(c("Ma", "ka", "None"), title = "What are the units of age?")$res
    }
  }
  # add foram zones
  if (is.null(choices$sample.zone)) {
    check <- FALSE
    while(check == FALSE) {
      choices$sample.zone <- dlg_list(names(data), title = "Which column contains the zone age?")$res
      if (length(choices$sample.zone) == 0) {
        choices$sample.zone <- NA
      }
      print(paste("So zone: ", choices$sample.zone))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
    #}
  }
  # add the age column
  if (choices$sample.age[1] %in% names(data)) {
    names(data)[names(data) == choices$sample.age] <- "age"
  } else {
    data <- cbind(data, age = as.numeric(as.character(choices$sample.age)))
  }
  data$age.err <- NA
  if (is.null(choices$no.num.age))
    choices$no.num.age <- is.na(data$age)
  
  # if the age is in ka, then convert it to Ma
  if (choices$age.units == "ka") {
    data$age <- as.numeric(data$age)
    data$age <- data$age / 1000 
  }
  # add zones
  if (all(choices$sample.zone %in% names(data))) {
    data$zone <- as.character(data[, names(data) == choices$sample.zone])
  } else {
    data <- cbind(data, zone = as.character(choices$sample.zone), stringsAsFactors = FALSE)
  }
  
  # add in segment code
  data$segment <- "s"
  
  # add in the mag zone
  if (is.null(data$mag.zone))
    data$mag.zone <- as.character(NA)
  
  # if the age is in an external table -----------------------
  if (all(is.na(data$age)) & all (is.na(data$zone)) | recalc.age) {
    tmp.hole <- gsub("^[A-z]", "", unique(choices$hole))
    age.file <- grep(paste("^[^~].*_chrono", sep = ""), grep(gsub("[^/]*$", "", file), age.chron$file, value = TRUE), value = TRUE)
    
    # for each hole, 
      for (i in 1:length(tmp.hole)) {
        # get the external age files
        if (is.data.frame(age.chron)) {
          if ((length(tmp.hole) != 1 | length(age.file[grep(paste(tmp.hole[1], "chrono", sep = ""), gsub("_", "", age.file))]) != 1) & length(age.file) > 1) {
            data.age <- age.chron$chron[age.chron$file == age.file[1]][[1]]
            for (j in 2:length(age.file)) {
              data.age <- merge(data.age, age.chron$chron[age.chron$file == age.file[j]][[1]], all.x = TRUE, all.y = TRUE)
            }
          } else {
            if (length(age.file) > 1) {
              data.age <- age.chron$chron[age.chron$file == age.file[grep(paste(tmp.hole[1], "chrono", sep = ""), gsub("_", "", age.file))]][[1]]
            } else {
              data.age <- age.chron$chron[age.chron$file == age.file][[1]]
            }
          }
        } else {
          if ((length(tmp.hole) != 1 | length(age.file[grep(paste(tmp.hole[1], "chrono", sep = ""), gsub("_", "", age.file))]) != 1) & length(age.file) > 1) {
            data.age <- read_excel(age.file[1])
            for (j in 2:length(age.file))
              data.age <- merge(data.age, read_excel(age.file[j]), all.x = TRUE, all.y = TRUE)
          } else {
            if (length(age.file) > 1) {
              data.age <- read_excel(age.file[grep(paste(tmp.hole[1], "chrono", sep = ""), gsub("_", "", age.file))])
            } else {
              data.age <- read_excel(age.file)
            }
          }
        }
        # if one of the columns is the hole
        if (tmp.hole[i] %in% names(data.age)) {
          # remove non-numeric
          data.age[, tmp.hole[i]] <- round(suppressWarnings(as.numeric(pull(data.age, tmp.hole[i]))), 4)
          # data.age$`Age (Ma)` <- as.numeric(pull(data.age, `Age (Ma)`)) 29/7 don't think I want this
          names(data.age)[names(data.age) == "Age (Ma)"] <- "orig.age"
          names(data.age)[names(data.age) == "Chron boundary"] <- "orig.chron"
          if (any(grepl("\\?", data.age$orig.chron)))
            data.age <- data.age[-grep("\\?", data.age$orig.chron), ]
          
          
          # remove rounding errors
          data.age$orig.age[suppressWarnings(!is.na(as.numeric(data.age$orig.age)))] <- round(suppressWarnings(as.numeric(data.age$orig.age)[!is.na(as.numeric(data.age$orig.age))]), 4)
          
          # if there are any segments
          data.age$segment <- 1
          if (any(data.age$orig.chron == "Gap")) {
            if (any(grepl("Gap", data.age$orig.chron))) {
              # calculate position of any gaps
              chrono.segment <- grep("Gap", data.age$orig.chron)
              if (length(chrono.segment) > 0) {
                # identify the first split
                data.age$segment[(chrono.segment[1] + 1):length(data.age$segment)] <- data.age$segment[(chrono.segment[1] + 1):length(data.age$segment)] + 1
                if (length(chrono.segment) > 1) {
                  # identify later splits (where it's not just the start / end of the same gap)
                  for (j in 2:length(chrono.segment)) {
                    if (chrono.segment[j] - chrono.segment[j - 1] > 1) {
                      data.age$segment[(chrono.segment[j] + 1):length(data.age$segment)] <- data.age$segment[(chrono.segment[j] + 1):length(data.age$segment)] + 1
                    }
                  }
                }
              }
            }

          #   data.age$segment <- 1
          #   for (k in 1:(length(which(data.age$orig.chron == "Gap")) / 2)){
          #     data.age$segment[which(data.age$orig.chron == "Gap")[k + 1]:ifelse(length(which(data.age$orig.chron == "Gap")) > (k*2 + 1), which(data.age$orig.chron == "Gap")[k + 2], length(data.age$orig.chron))] <- k + 1
          #   }
          #   rm(k)
          }
          data.age <- merge(data.age, chrons.full, by.x = c("orig.chron", "orig.age"), by.y = c("orig.chron", "Original.age"), all.x = TRUE)
          if (any(is.na(data.age$Original.event) & data.age$orig.chron != "Gap"))
            browser()
          # remove events which don't have accurate dates (except "Gap")
          if (any(is.na(data.age$Age) & data.age$orig.chron != "Gap"))
            data.age <- data.age[-which(is.na(data.age$Age) & data.age$orig.chron != "Gap"), ]
          
          # re-order rows by depth then age
          data.age <- data.age[order(pull(data.age, tmp.hole[i]), data.age$Age), ]
          
          # get unique events (e.g. remove those that have different dates for different oceans)
          data.age <- unique(data.age[, !(names(data.age) %in% c("Age.notes", "Age"))])
          
          # remove rows that have NA
          rm.rows <- intersect(which(data.age$orig.chron != "Gap" | data.age$orig.chron != "SF" | is.na(data.age$orig.chron)), unique(unlist(apply(data.age[, c("orig.chron", "orig.age", tmp.hole[i])], 2, function (x) which(is.na(x))))))
          if (length(rm.rows) > 0)
            data.age <- data.age[-rm.rows, ]
          
          # provided the hole / site exists as a column in data.age get the zones by depth
          data.zone <- tibble(chron = paste(data.age$corr.chron[1:(nrow(data.age) - 1)], data.age$corr.chron[2:(nrow(data.age))], sep = "_"), start = pull(data.age, tmp.hole[i])[2:nrow(data.age)], end = c(pull(data.age, tmp.hole[i])[1:(nrow(data.age) - 1)]), segment = data.age$segment[1:(nrow(data.age) - 1)])
          if (!is.null(data$zone))
            data$zone <- as.character(data$zone)
          # get the new zone based on sample depths
          calc.zone <- sapply(data$sample.depth[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)], function (x) ifelse(length(data.zone$chron[x > data.zone$end & x <= data.zone$start]) > 0, data.zone$chron[x > data.zone$end & x <= data.zone$start], NA))
          # where the new zone is NA use the old zone if available
          data$zone[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)][!is.na(calc.zone)] <- calc.zone[!is.na(calc.zone)]
          
          # add in the segments
          #data$segment[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)] <- "s"
          calc.seg <- sapply(data$sample.depth[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)], function (x) ifelse(length(data.zone$chron[x > data.zone$end & x <= data.zone$start]) > 0, data.zone$segment[x > data.zone$end & x <= data.zone$start], NA))
          # where the new zone is NA use the old zone if available
          data$segment[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)][!is.na(calc.seg)] <- calc.seg[!is.na(calc.seg)]
          print(data.zone)
          choices[[paste("data.age", tmp.hole[i], sep = "_")]] <- data.age
          
          # calculate magneto zones
          # provided the hole / site exists as a column in data.age get the zones by depth
          data.age.mag <- data.age[(data.age$Corrected.Type == "Magneto" | is.na(data.age$Corrected.Type) | data.age$Corrected.Type == "None"), ]
          if (nrow(data.age.mag) > 1) {
            data.zone.mag <- tibble(chron = paste(data.age.mag$corr.chron[1:(nrow(data.age.mag) - 1)], data.age.mag$corr.chron[2:(nrow(data.age.mag))], sep = "_"), start = pull(data.age.mag, tmp.hole[i])[2:nrow(data.age.mag)], end = c(pull(data.age.mag, tmp.hole[i])[1:(nrow(data.age.mag) - 1)]), segment = data.age.mag$segment[1:(nrow(data.age.mag) - 1)])
            # get the new zone based on sample depths
            calc.mag.zone <- sapply(data$sample.depth[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)], function (x) ifelse(length(data.zone.mag$chron[x > data.zone.mag$end & x <= data.zone.mag$start]) > 0, data.zone.mag$chron[x > data.zone.mag$end & x <= data.zone.mag$start], NA))
            # where the new zone is NA use the old zone if available
            data$mag.zone[grep(paste(tmp.hole[i], "$", sep = ""), data$hole)][!is.na(calc.mag.zone)] <- calc.mag.zone[!is.na(calc.mag.zone)]
            print(data.zone.mag)
            choices[[paste("data.mag.age", tmp.hole[i], sep = "_")]] <- data.age.mag
          } else {
            data$mag.zone <- as.character(NA)
          }
        }
      }
    choices$calc.zone <- data$zone
  } else if (!is.null(choices$calc.zone)) {
    data$zone <- as.character(choices$calc.zone)
  }
  data$rng.age <- as.numeric(NA)
  data$AM.type <- as.character(NA)
  data$zon.age <- data$age.st <- data$age.en <- as.numeric(NA)
  data$int.age <- data$err.int.age <- as.numeric(NA)
  data$mod.age <- data$r2 <- data$n.pts <- as.numeric(NA)
  data$age.calc <- "Orig"
  data$mag.age.st <- data$mag.age.en <- data$mag.age.rng <- as.numeric(NA)
  data$int.mag.age <- data$err.int.mag.age <- as.numeric(NA)
  
  
  # zone type
  if (is.null(choices$zone.type) & (any(!is.na(choices$sample.zone)) | any(!is.na(choices$calc.zone)))) {
    print(unique(data$zone))
    choices$zone.type <- dlg_list(c("Foram", "Nanno", "Magneto", "Other", "Mixed"), title = "What is the age model type?")$res
  }
  if (any(choices$zone.type == "Chrono") & !is.na(choices$zone.type))
    choices$zone.type[choices$zone.type == "Chrono"] <- "Magneto"
  
  # if we only have zones then use those to get the age
  if (any(!is.na(data$zone) | !is.null(choices$zone.ck) | any(is.na(data[, names(data) == "age"])) | recalc.age)) {
    dat.zone <- data.frame(zone = data$zone, mag.zone = data$mag.zone, ocean = factor("IndoPac", levels = c("Atl", "IndoPac")), region = factor("Temp", levels = c("Temp", "Trop")), segment = data$segment, stringsAsFactors = FALSE)
    
    # get ocean polygons and work out origin of sites
    load("Data/OceanPolygons.RData")
    dat.zone$ocean[point.in.polygon(data$longitude, data$latitude, atlantic.1$x, atlantic.1$y) == 1] <- "Atl"
    
    # work out site region
    if (any(data$latitude < 23.5))
      dat.zone$region[data$latitude < 23.5] <- "Trop"
    check <- "Yes"
    while(check == "Yes") {
      if (is.null(choices$zones.tab) | recalc.age) {
        tmp.age <- zones.fun(unique(dat.zone), scheme = choices$age.model, type = choices$zone.type)
        tmp.age.mag <- zones.fun(unique(dat.zone), scheme = choices$age.model, type = choices$zone.type, mag = TRUE)
      } else {
        tmp.age <- choices$zones.tab
      }
      print(tmp.age)
      print("Unknown zones:")
      print(tmp.age$zone[is.na(tmp.age$zon.age)])
      if (is.null(choices$zone.ck))
        check <- dlg_list(c("Yes", "No"), title = "Do you want to make changes and re-run this?")$res
      else
        check <- "No"
    }
    choices$zone.ck <- "No"
    dat.zone$ID <- 1:nrow(dat.zone)
    dat.zone <- merge(dat.zone, tmp.age)
    if (nrow(tmp.age.mag) != 0)
      dat.zone <- merge(dat.zone, tmp.age.mag[, grep("zone|mag", names(tmp.age.mag))], by.x = "mag.zone", by.y = "zone")
    dat.zone <- dat.zone[order(dat.zone$ID), ]
    dat.zone$sch[dat.zone$sch == "NA"] <- NA
    dat.zone$type[dat.zone$type == "NA"] <- NA
    if (!is.null(choices$no.num.age)) {
      choices$est.zone.type <- choices$age.st <- choices$age.en <- choices$range.age <- choices$est.age.model <- rep(NA, nrow(data))
      choices$est.age <- data$age
      data$AM.type <- choices$est.zone.type <- dat.zone$type
      data$age.st <- choices$age.st <- dat.zone$st
      data$age.en <- choices$age.en <- dat.zone$en
      data$rng.age <- choices$range.age <- dat.zone$age.rng
      data$age.model <- as.character(data$age.model)
      data$age.model <- choices$est.age.model <- dat.zone$sch
      data$zon.age <- choices$zon.age <- dat.zone$zon.age
      data$mag.zone <- choices$mag.zone <- dat.zone$mag.zone
      data$mag.age.st <- choices$mag.age.st <- dat.zone$mag.st
      data$mag.age.en <- choices$mag.age.en <- dat.zone$mag.en
      data$mag.age <- choices$mag.age <- dat.zone$mag.age
      data$mag.age.rng <- choices$mag.age.rng <- dat.zone$mag.age.rng
    } else {
      data$zon.age <- choices$sample.age <- dat.zone$age
      data$rng.age <- choices$range.age <- dat.zone$age.rng
      data$AM.type <- choices$type <- dat.zone$AM.type
      data$age.model <- choices$age.model <- dat.zone$sch
      data$age.st <- choices$age.st <- dat.zone$st
      data$age.en <- choices$age.en <- dat.zone$en
    }
    choices$zones.tab <- tmp.age
    choices$zones.mag.tab <- tmp.age.mag
    data$zone <- as.character(data$zone)
    data$age.model <- as.character(data$age.model)
  }
  # calculate the interpolated ages ----------------------
  if (any(!is.na(data$zone))) {
    if (any(grepl("data.age", names(choices)))) {
      for (i in unique(data$hole)) {
        tmp.dat.age <- choices[paste("data.age", i, sep = "_")][[1]]
        if (is.null(tmp.dat.age))
          tmp.dat.age <- choices[paste("data.age", gsub("^U|^C|^M", "", i), sep = "_")][[1]]
        if (!is.null(tmp.dat.age)) {
          if (length(intersect(tmp.dat.age$segment, choices$zones.tab$segment)) > 0) {
            # calculate interpolated ages
            tmp.int <- age.interp.chrono(tmp.dat.age, choices$zones.tab, data[!duplicated(data$sampleID) & data$hole == i,], hole = i)
            data$int.age[data$hole == i] <- tmp.int$int.age[match(data$sampleID[data$hole == i], tmp.int$sampleID)]
            data$err.int.age[data$hole == i] <- tmp.int$err.int.age[match(data$sampleID[data$hole == i], tmp.int$sampleID)]
            # calculate ages based on magnetostratigraphy
            tmp.dat.mag <- choices[paste("data.mag.age", i, sep = "_")][[1]]
            if (is.null(tmp.dat.mag))
              tmp.dat.mag <- choices[paste("data.mag.age", gsub("^U|^C|^M", "", i), sep = "_")][[1]]
            if (!is.null(tmp.dat.mag)) {
              if (nrow(tmp.dat.mag) > 1 & any(!is.na(data$mag.zone[!duplicated(data$sampleID) & data$hole == i]))) {
                tmp.chron.int <- age.interp.chrono(tmp.dat.mag, choices$zones.mag.tab, data[!duplicated(data$sampleID) & data$hole == i,], hole = i, mag = TRUE)
                data$int.mag.age[data$hole == i] <- tmp.chron.int$int.age[match(data$sampleID[data$hole == i], tmp.chron.int$sampleID)]
                data$err.int.mag.age[data$hole == i] <- tmp.chron.int$err.int.age[match(data$sampleID[data$hole == i], tmp.chron.int$sampleID)]
              }
            }
          }
        }
        rm(tmp.dat.age)
      }
    } 
    if (any(is.na(data$int.age) & !is.na(data$zon.age))) {
      tmp.int <- age.interp(data[!duplicated(data$sampleID) & is.na(data$int.age),])
      tmp.int <- tmp.int[tmp.int$interp == "TRUE",]
      if (nrow(tmp.int) != 0) {
        data$err.int.age[is.na(data$int.age)] <- tmp.int$err.int.age[match(data$sampleID[is.na(data$int.age)], tmp.int$sampleID)]
        data$int.age[is.na(data$int.age)] <- tmp.int$int.age[match(data$sampleID[is.na(data$int.age)], tmp.int$sampleID)]
      }
    }
  }
  

  # calculate the modelled ages
  if (any(grepl("data.age", names(choices)))) {
    for (i in unique(data$hole)) {
      tmp.dat.age <- choices[paste("data.age", i, sep = "_")][[1]]
      if (is.null(tmp.dat.age))
        tmp.dat.age <- choices[paste("data.age", gsub("^U|^C|^M", "", i), sep = "_")][[1]]
      if (!is.null(tmp.dat.age)) {
        tmp.age.res <- age.model(tmp.dat.age, choices$zones.tab, data[!duplicated(data$sampleID) & data$hole == i,], hole = i, plots = FALSE)
        data$mod.age[data$hole == i] <- tmp.age.res$mod.age[match(data$sampleID[data$hole == i], tmp.age.res$sampleID)]
        data$r2[data$hole == i] <- tmp.age.res$r2[match(data$sampleID[data$hole == i], tmp.age.res$sampleID)]
        data$n.pts[data$hole == i] <- tmp.age.res$n.pts[match(data$sampleID[data$hole == i], tmp.age.res$sampleID)]
      }
      rm(tmp.dat.age)
    }
  }
  # add in the relevant ages
  for (i in unique(data$hole)) {
    uni.dat <- data[!duplicated(data$sampleID), c("sampleID", "sample.depth", grep("age", names(data), value = TRUE), "zone")]
    uni.dat <- rbind(uni.dat, uni.dat[nrow(uni.dat),])
    age.mod <- model.type[model.type$File == tmp.file & model.type$Hole == i, ]
    if (nrow(age.mod) == 1) {
      if (length(uni.dat$age) < length(uni.dat$sample.depth) & !all(is.na(uni.dat$age)) | age.mod$Original == 0) {
        tmp.orig <- data$age
        data$age <- NA
        tmp.no.num.age <- choices$no.num.age
        choices$no.num.age <- TRUE
      }
      if (age.mod$Mag == 1) {
        tmp.row <- choices$no.num.age & !is.na(data$int.mag.age) & data$hole == i
        data$age[tmp.row] <- data$int.mag.age[tmp.row]
        data$age.err[tmp.row] <- 0
        data$age.calc[tmp.row] <- "Magneto"
        uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
      }
      
      if (age.mod$Model == 1) {
        tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$mod.age) & data$hole == i & data$mod.age >= 0
        uni.dat$age[is.na(uni.dat$age)] <- uni.dat$mod.age[is.na(uni.dat$age)]
        if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
          tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
          tmp.ex <- tmp.ex[!is.na(tmp.ex)]
          tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
          tmp.row <- tmp.row & !tmp.ex
        }
        data$age[tmp.row] <- data$mod.age[tmp.row]
        data$age.err[tmp.row] <- 0
        data$age.calc[tmp.row] <- "Model"
        uni.dat$age <- data$age[match(uni.dat$sampleID, data$sampleID)]
      }
      if (age.mod$Interp == 1) {
        tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$int.age) & data$hole == i
        uni.dat$age[is.na(uni.dat$age)] <- uni.dat$int.age[is.na(uni.dat$age)]
        if (any(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < -0.01, na.rm = TRUE)) {
          tmp.ex <- which(diff(uni.dat$sample.depth) > 0 & diff(uni.dat$age) < 0) + 1
          tmp.ex <- tmp.ex[!is.na(tmp.ex)]
          #tmp.ex <- data$int.age < max(uni.dat$int.age[tmp.ex], na.rm = TRUE) & data$sample.depth > min(uni.dat$sample.depth[tmp.ex], na.rm = TRUE)
          tmp.ex <- data$zone %in% uni.dat$zone[tmp.ex]
          tmp.row <- tmp.row & !tmp.ex
        }
        data$age[tmp.row] <- data$int.age[tmp.row]
        data$age.err[tmp.row] <- data$err.int.age[tmp.row]
        data$age.calc[tmp.row] <- "Interp"
      }
    }
    
    tmp.row <- choices$no.num.age & is.na(data$age) & !is.na(data$zon.age) & data$hole == i
    data$age[tmp.row] <- data$zon.age[tmp.row]
    data$age.err[tmp.row] <- data$rng.age[tmp.row]
    data$age.calc[tmp.row] <- "Zone"
    
    if (any(grepl("tmp.orig", ls()))) {
      data$age[is.na(data$age)] <- tmp.orig[is.na(data$age)]
      choices$no.num.age <- tmp.no.num.age
    }
  }
  data$age.calc[is.na(data$age)] <- NA
  data$age.calc <- factor(data$age.calc, levels = c("Interp", "Magneto", "Model", "Orig", "Zone"))
  
    # sample type ---------------------
  if (is.null(choices$sample.type)) {
    # enter the sample type
    choices$sample.type <- "ODP"
  }
  data <- cbind(data, sample.type = as.character(choices$sample.type))
  
  
  # - preservation -----------------
  if(is.null(choices$preservation)) {
    check <- FALSE
    while(check == FALSE) {
      if (is.null(choices$preservation)) 
        choices$preservation <- dlg_list(names(data), title = "Does any column contain the preservation?")$res
      if (length(choices$preservation) == 0) {
        # if it isn't a column, then see if the details are in the meta data
        choices$preservation <- readline("Enter the preservation: ")
        if (choices$preservation == "")
          choices$preservation <- NA
        print(paste("So preservation: ", choices$preservation, sep = ""))
      } else {
        print(paste("So preservation: data$", choices$preservation, " ", sep = ""))
      }
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
      if (check == FALSE) {
        choices$preservation <- NULL
      }
    }
  }
  if (choices$preservation %in% names(data)) {
    names(data)[names(data) == choices$preservation] <- "preservation"
  } else {
    data <- cbind(data, preservation = as.character(choices$preservation), stringsAsFactors = FALSE)
  }
  
  # - processing --------------------
  if (is.null(choices$processing)) {
    # check <- FALSE
    # while(check == FALSE) {
    #   choices$processing <- dlg_list(names(data), title = "Does any column contain the sample processing information (e.g. sieve fraction)?")$res
    #   if (length(choices$processing) == 0) {
    #     choices$processing <- readline("Enter the sample processing information: ")
    #     if (choices$processing == "")
    choices$processing <- NA
    #     print(paste("So sample processing information: ", choices$processing, sep = ""))
    #   } else {
    #     print(paste("So sample processing information: data$", choices$processing, " ", sep = ""))
    #   }
    #   check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    # }
  }
  if (choices$processing %in% names(data)) {
    names(data)[names(data) == choices$processing] <- "processing"
  } else {
    data <- cbind(data, processing = as.character(choices$processing), stringsAsFactors = FALSE)
  }
  
  # paleolatitude ------------------
  if (is.null(choices$pal.lat.full))
    choices$pal.lat.full <- pal.lat.full
  if (any(!is.na(data$age))|!is.null(choices$pal.lat)) {
    if (max(as.numeric(as.character(data$age)), na.rm = TRUE) == 0) {
      # if all the data is extant, then no need for palaeolatitude
      data$pal.lat <- data$latitude
      data$pal.long <- data$longitude
    } else {
      
      if (is.null(choices$pal.lat) | is.null(choices$pal.long)) {
        # if there is no palaeolatitude information
        print(head(data))
        # pl.check <- dlg_list(c("Yes", "No"), title = "Is there palaeolatitude information?")$res
        # if (length(pl.check) == 0)
        #  pl.check <- "No"
        # if (pl.check == "Yes") {
        #   check <- FALSE
        #   while(check == FALSE) {
        #     choices$pal.lat <- dlg_list(names(data), title = "Which column is the palaeolatitude?")$res
        #     choices$pal.long <- dlg_list(names(data), title = "Which column is the palaeolongitude?")$res
        #     print(paste("So palaeolatitude: data$", choices$pal.lat, sep = ""))
        #     print(paste("palaeolongitude: data$", choices$pal.long, sep = ""))
        #     check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
        #   }
        #   names(data)[names(data) == choices$pal.lat] <- "pal.lat"
        #   names(data)[names(data) == choices$pal.long] <- "pal.long"
        # } else {
          # # if necessary extract palaeo lat information
          ## this is from palaeocoords, which is a function in mapast
          if (!pal.lat.full) { # i.e. if only testing
            # extract 5 random ages to test it on
            tmp.uni.ages <- unique(data$age[!is.na(data$age)])[1]
            tmp.pal.rows <- which(data$age %in% tmp.uni.ages[!is.na(tmp.uni.ages)])
            # tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- NA#tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- NA#tmp$paleolng
          } else {
            tmp.pal.rows <- which(!is.na(data$age))
            tmp <- pal.coord(data[tmp.pal.rows,], model = model)
            choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
            choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
            choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
          }
       # } 
        
      } else if (!(choices$pal.lat.full) & pal.lat.full) {
        # if the choices$pal.lat are only a subset and you want a full run
        tmp.pal.rows <- which(!is.na(data$age))
        tmp <- pal.coord(data[tmp.pal.rows,], model = model)
        choices$pal.lat <- choices$pal.long <- data$pal.lat <- data$pal.long <- rep(NA, nrow(data))
        choices$pal.lat[tmp.pal.rows] <- data$pal.lat[tmp.pal.rows] <- tmp$paleolat
        choices$pal.long[tmp.pal.rows] <- data$pal.long[tmp.pal.rows] <- tmp$paleolng
      } else {
        if (choices$pal.lat[1] %in% names(data)) {
          names(data)[names(data) == choices$pal.lat] <- "pal.lat"
          names(data)[names(data) == choices$pal.long] <- "pal.long"
        } else {
          data$pal.lat <- NA
          data$pal.long <- NA
          data$pal.lat <- as.numeric(choices$pal.lat)
          data$pal.long <- as.numeric(choices$pal.long)
        }
      }
    }
  }
  choices$pal.lat.full <- pal.lat.full
  # # if necessary convert ages to updated dates (Gradstein 2012)
  
  # # Extract species level information
  # species names ------------------------
  sp.check <- "unknown"
  if(is.null(choices$species) & is.null(choices$genus)) {
    print(head(data))
    ## sp.check <- dlg_list(c("Yes", "No"), title = "Is the species data currently in the form of a binomial?")$res
    ## if(sp.check == "Yes") {
    # check <- FALSE
    # while(check == FALSE) {
    #   choices$species <- dlg_list(names(data), title = "Which column is the species binomial?")$res
    #   if (length(choices$species) > 0) {
    #     print(paste("So species: data$", choices$species, sep = ""))
    #     check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    #   } else {
    #     choices$genus <- dlg_list(names(data), title = "Which column is the genus?")$res
    #     choices$sp <- dlg_list(names(data), title = "Which column is the species?")$res
    #     choices$subsp <- dlg_list(names(data), title = "Which column is the subspecies?")$res
    #     print(paste("So genus is: data$", choices$genus, sep = ""))
    #     print(paste("species: data$", choices$sp, sep = ""))
    #     print(paste("subspecies: data$", choices$subsp, sep = ""))
    #     check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    #   }
    # }
    choices$species <- "Species"
    names(data)[names(data) == choices$species] <- "orig.species"
    # } else {
    #   check <- FALSE
    #   while(check == FALSE) {
    #     choices$genus <- dlg_list(names(data), title = "Which column is the genus?")$res
    #     choices$sp <- dlg_list(names(data), title = "Which column is the species?")$res
    #     choices$subsp <- dlg_list(names(data), title = "Which column is the subspecies?")$res
    #     print(paste("So genus is: data$", choices$genus, sep = ""))
    #     print(paste("species: data$", choices$sp, sep = ""))
    #     print(paste("subspecies: data$", choices$subsp, sep = ""))
    #     check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    #   }
    #}
  }
  if (!is.null(choices$species) | choices$species %in% names(data)) {
    names(data)[names(data) == choices$species] <- "orig.species"
  } 
  if (!is.null(choices$genus)) {
    species <- paste(data[, names(data) == choices$genus], data[, names(data) == choices$sp])
    if (length(choices$subsp) != 0) {
      sub.sp.col <- data[, names(data) == choices$subsp]
      species[!is.na(sub.sp.col)] <- paste(species[!is.na(sub.sp.col)], sub.sp.col[!is.na(sub.sp.col)])
    }
    data$orig.species <- species
  }
  # extract the full names from the meta data, and remove extra characters e.g. [%]
  # if there are direct duplicates
  data$orig.species <- gsub(" \\[.*$", "", data$orig.species)
  
  # remove the trailing space
  data$orig.species <- sapply(data$orig.species, function(x) gsub("(\\s+$)", "", x))
  
  # # Query database to correct for synonyms
  check <- "Yes"
  new.species <- NULL
  while(check == "Yes") {
    if (is.null(new.species)) {
      comp.list <- unique(data$orig.species)
      comp.rerun <- 1:length(comp.list)
    } else {
      comp.rerun <- which(new.species %in% c("Cretaceous", "Unsure", "unknown"))
      comp.list <- unique(data$orig.species)[comp.rerun]
    }
    # if (is.null(choices$synonymy)) 
    #   choices$ages.sp <- dlg_list(c("Yes", "No"), title = "Should the synonymy list use age controls?")$res
    choices$ages.sp <- "Yes"
    if (choices$ages.sp == "Yes") {
      new.species <- compare(unique(data$orig.species), micro = TRUE, age.check = TRUE)
    } else {
      new.species[comp.rerun] <- compare(comp.list, micro = TRUE)
    }
    print("The following species were classified as 'unknown'")
    print(sort(unique(data$orig.species)[new.species == "unknown"]))
    print("The following species were classified as 'Cretaceous'")
    print(sort(unique(data$orig.species)[new.species == "Cretaceous"]))
    print("The following species were classified as 'Unsure'")
    print(sort(unique(data$orig.species)[new.species == "Unsure"]))
    print("The following species were classified as 'Vague'")
    print(sort(unique(data$orig.species)[new.species == "Vague"]))
    if (!is.null(choices$synonymy)) {
      check <- "No"
    } else {
      check <- dlg_list(c("Yes", "No"), title = "Do you want to make some corrections and re-run this")$res
    }
  }
  data$species <- new.species[match(data$orig.species, unique(data$orig.species))]
  choices$synonymy <- "checked"
  # abundance --------------------
  # - Total IDd
  if(is.null(choices$total.IDd)) {
    check <- FALSE
    while(check == FALSE) {
      choices$total.IDd <- dlg_list(names(data), title = "Does any column contain information on the amount searched (e.g. total specimens IDd)?")$res
      if (length(choices$total.IDd) == 0)
        choices$total.IDd <- NA
      print(paste("So total specimens IDd: data$", choices$total.IDd, " ", sep = ""))
      check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    }
  }
  if (choices$total.IDd %in% names(data)) {
    names(data)[names(data) == choices$total.IDd] <- "total.IDd"
  } else {
    data <- cbind(data, total.IDd = choices$total.IDd)
  }
  data$total.IDd <- gsub(" ", "", data$total.IDd)
  
  # abundance information
  if (is.null(choices$abundance)) {
    # check <- FALSE
    # while(check == FALSE) {
    #   if ("rel.abun" %in% names(data))
        choices$abundance <- "rel.abun"
    #   else 
    #     choices$abundance <- dlg_list(names(data), title = "Does any column contain the abundance?")$res
    #   if (length(choices$abundance) == 0) {
    #     choices$abundance <- "P"
    #   } else {
    #     print(paste("So abundance: data$", choices$abundance, sep = ""))
    #   }
    #   check <- dlg_list(c(TRUE, FALSE), title = "Is this correct?")$res
    # }
  }
  if (choices$abundance %in% names(data)) {
    names(data)[names(data) == choices$abundance] <- "abundance"
  } else {
    data <- cbind(data, abundance = choices$abundance)
  }
  data$abundance <- gsub(" ", "", data$abundance)
  
  # What are the abundance units
  print("Numeric characters:")
  print(suppressWarnings(summary(as.numeric(gsub("#|\\?|>|<|:", "", grep("\\d", data$abundance, value = TRUE))))))
  print("Non numeric characters:")
  print(grep("[^(0-9)|\\.]", unique(data$abundance), value = TRUE))
  if (is.null(choices$abun.units)) {
    # what are the abundance units
    print("Abundance units:")
    choices$abun.units <- dlg_list(c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed") , title = "What are the units of abundance?")$res
  }
  data$abun.units <- factor(choices$abun.units, levels = c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed"))
  # convert everything to numbers
  data$orig.abundance <- data$abundance
  # where there are mixed units, identify numeric / non-numeric rows
  if (any(choices$abun.units == "Mixed")) {
    mix.tab <- tapply(data$abundance, data$holeID, max)
    mix.tab[!is.na(mix.tab)] <- "Num"
    mix.tab[is.na(mix.tab)] <- "NotNum"
    mix.row <- rep(NA, nrow(data))
    mix.row[data$holeID %in% names(mix.tab)[mix.tab == "Num"]] <- "Num"
    mix.row[data$holeID %in% names(mix.tab)[mix.tab == "NotNum"]] <- "NotNum"
    mix.row[mix.row == "Num"] <- dlg_list(c("Relative abundance", "Count", "Number per gram") , title = "What are the units of abundance for the numeric data?")$res
    mix.row[mix.row == "NotNum"] <- dlg_list(c("Binned", "P/A") , title = "What are the units of abundance for the non-numeric data?")$res
    data$abun.units <- choices$abun.units <- mix.row
  }
  if (any(choices$abun.units %in% c("Relative abundance", "Count", "Number per gram"))) {
    tmp.row <- which(data$abun.units %in% c("Relative abundance", "Count", "Number per gram"))
    tmp.abun <- data$abundance[tmp.row]
    if (any(grepl("[^(0-9)|\\.]", unique(tmp.abun)))) {
      if(is.null(choices$abun.num)) {
        choices$abun.num <- NULL
        for (i in grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE)) {
          tmp.pa <- dlg_list(c("Presence", "Absence") , title = paste("Should", i, "be presence or absence?"))$res
          if (tmp.pa == "Presence"){
            choices$abun.num <- c(choices$abun.num, as.numeric(0.01))
            names(choices$abun.num)[length(choices$abun.num)] <- i
          } else if (tmp.pa == "Absence") {
            choices$abun.num <- c(choices$abun.num, as.numeric(0))
            names(choices$abun.num)[length(choices$abun.num)] <- i
          }
        }
        # print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
      }
      tmp.abun[grep("[^(0-9)|\\.]", tmp.abun)] <- as.numeric(choices$abun.num[match(tmp.abun[grep("[^(0-9)|\\.]", tmp.abun)], names(choices$abun.num))])
      
    }
    data$abundance[tmp.row] <- as.numeric(tmp.abun)
  } 
  if (any(choices$abun.units == "P/A")) {
    tmp.row <- which(data$abun.units == "P/A")
    tmp.abun <- data$abundance[tmp.row]
    if (is.null(choices$abun.p)) {
      print(unique(tmp.abun))
      choices$abun.p <- readline("What symbol indicates presence? ")
      choices$abun.a <- readline("What symbol indicates absence? ")
      for (i in unique(tmp.abun)[!(unique(tmp.abun) %in% c(choices$abun.p, choices$abun.a))]) {
        tmp.pa <- dlg_list(c("Presence", "Absence") , title = paste("Should", i, "be presence or absence?"))$res
        if (tmp.pa == "Presence"){
          choices$abun.p <- c(choices$abun.p, i)
        } else if (tmp.pa == "Absence") {
          choices$abun.a <- c(choices$abun.a, i)
        }
      }
    }
    tmp.abun[tmp.abun %in% choices$abun.p] <- 1
    tmp.abun[tmp.abun %in% choices$abun.a] <- 0
    data$abundance[tmp.row] <- tmp.abun
  } 
  if (any(choices$abun.units == "Binned")) {
    tmp.row <- which(data$abun.units == "Binned")
    tmp.abun <- data$abundance[tmp.row]
    if (is.null(choices$abun.bin)) {
      print(grep("[^(0-9)|\\.]", unique(tmp.abun), value = TRUE))
      choices$abun.bin <- readline("Enter the binned abundances starting with absent (e.g. n,r,c,a), leave reworked out: ")
      choices$abun.bin <- unlist(strsplit(choices$abun.bin, ","))
    } 
    tmp.abun <- match(tmp.abun, choices$abun.bin) - 1
    tmp.abun[is.na(tmp.abun)] <- 0
    data$abundance[tmp.row] <- tmp.abun 
  }
  data$abundance <- as.numeric(as.character(data$abundance))
  
  # remove those not at species level
  data <- data[grepl("\\s", data$species),]
  # Where synonyms identify duplicates, then merge them 
  dup.sp <- which(duplicated(paste(data$age, data$sample.depth, data$latitude, data$longitude, data$species)))
  
  # if (length(dup.sp) > 0) {
  #   if (is.numeric(data$abundance)) {
  #     # calculate the summed abundances given the duplicates
  #     tmp.abun <- tapply(data$abundance, list(data$row.num, data$species), sum, na.rm = TRUE)
  #     data <- data[-dup.sp,]
  #     # match the summed abundances with the data
  #     data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$row.num[x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
  #     # if there is presence absence data, then reset the maximum to 1
  #     if (any(choices$abun.units == "P/A")) {
  #       data$abundance[data$abun.units == "P/A" & data$abundance > 0] <- 1
  #     }
  #     # ditto for binned data
  #     if (any(choices$abun.units == "Binned")) {
  #       data$abundance[data$abun.units == "Binned" & data$abundance > (length(choices$abun.bin) - 1)] <- length(choices$abun.bin) - 1
  #     }
  #   } else {
  #     stop("Non numeric data")
  #   }
  # }
  
  if (length(dup.sp) > 0) {
    tmp.sp <- tapply(data$orig.species, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    tmp.orig.abun <- tapply(data$orig.abundance, list(data$sampleID, data$species), function(x) paste(x, collapse = ", "))
    tmp.abun <- tapply(data$abundance, list(data$sampleID , data$species), sum, na.rm = TRUE)
    tmp.abun2 <- with(data[data$abun.units == "Binned", ], tapply(abundance, list(sampleID , species), max, na.rm = TRUE))
    data <- data[-dup.sp,]
    # match the summed abundances with the data
    data$abundance <- sapply(1:nrow(data), function (x) tmp.abun[match(data$sampleID [x], dimnames(tmp.abun)[[1]]), match(data$species[x], dimnames(tmp.abun)[[2]])])
    # if there is presence absence data, then reset the maximum to 1
    if (any(data$abun.units == "P/A")) {
      data$abundance[data$abun.units == "P/A" & data$abundance > 0] <- 1
    }
    
    # ditto for binned data
    if (any(data$abun.units == "Binned")) {
      data$abundance[data$abun.units == "Binned"] <- with(data[data$abun.units == "Binned",], sapply(1:sum(data$abun.units == "Binned"), function (x) tmp.abun2[match(sampleID[x], dimnames(tmp.abun2)[[1]]), match(species[x], dimnames(tmp.abun2)[[2]])]))
    }
    # merged original species
    data$orig.species <- sapply(1:nrow(data), function (x) tmp.sp[match(data$sampleID [x], dimnames(tmp.sp)[[1]]), match(data$species[x], dimnames(tmp.sp)[[2]])])
    data$orig.abundance <- sapply(1:nrow(data), function (x) tmp.orig.abun[match(data$sampleID [x], dimnames(tmp.orig.abun)[[1]]), match(data$species[x], dimnames(tmp.orig.abun)[[2]])])
  }
  
  
  
  tmp.rowID <- paste(data$dbID, data$sampleID, sep = "_")
  data$rowID[order(tmp.rowID)] <- paste(sort(tmp.rowID), "_R", unlist(tapply(tmp.rowID, tmp.rowID, function(x) 1:length(x))), sep = "")
  data$source <- as.character(data$source)
  data$year <- as.numeric(as.character(data$year))
  data$sample.type <- as.character(data$sample.type)
  data$sample.top <- as.character(data$sample.top)
  data$section <- as.character(data$section)
  data$age.model <- as.character(data$age.model)
  
  
  # return restructured dataframe
  col.inc <- c("rowID", "species", "orig.species", "abundance", "orig.abundance", "abun.units", "sample.depth", "segment", "age", "age.err", "age.calc", "zone", "zon.age", "age.st", "age.en", "rng.age", "int.age", "err.int.age", "mag.zone", "mag.age.st", "mag.age.en", "mag.age", "int.mag.age", "err.int.mag.age", "mod.age", "r2", "n.pts", "age.model", "AM.type", "latitude", "longitude", "pal.lat", "pal.long", "water.depth", "db.source", "dbID", "holeID", "sampleID", "reason", "leg", "site", "hole", "core", "section", "sample.top", "sample.type", "total.IDd", "preservation", "processing", "person", "date", "year", "source")
  
  final.data <- list(data = data[, col.inc], choices = choices, orig.data = orig.data)
  return(final.data)
}



# Function for paleocoords ------------------------------------------------
pal.coord <- function(data, model) {
  ## this is from palaeocoords, which is a function in mapast
  # it assumes the age column is 'age'
  paleolng <- c()
  paleolat <- c()
  data$order <- seq(1:nrow(data))
  data <- data[order(data$age), ]
  uma <- unique(data$age)
  for (curma in 1:length(uma)) {
    # subset the data for unique ages
    part <- subset(data, data$age == uma[curma])
    pts <- ""
    if (base::length(part$age) > 200) {
      # if there is lots of data in that age category, then run it in subsets
      num <- base::ceiling(base::length(part$age)/200)
      round <- 1
      while (round <= num) {
        pts <- ""
        if (round < num) {
          pts <- ""
          part2 <- part[((round - 1) * 200 + 1):(round * 200), ]
          for (j in 1:base::length(part2$age)) {
            pts <- base::paste0(pts, ",", part2$longitude[j], ",", part2$latitude[j])
          }
          pts <- base::substring(pts, 2)
          url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=",
                              uma[curma], "&model=", model, "&return_null_points")
          paleopts <- rjson::fromJSON(file = url)
          for (k in 1:base::length(paleopts$coordinates)) {
            if (base::is.null(paleopts$coordinates[[k]])) {
              paleolng <- c(paleolng, NA)
              paleolat <- c(paleolat, NA)
            } else {
              paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
              paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
            }
          }
        } else {
          pts <- ""
          part2 <- part[((round - 1) * 200 + 1):base::length(part$age), 
                        ]
          for (j in 1:base::length(part2$age)) {
            pts <- base::paste0(pts, ",", part2$longitude[j], 
                                ",", part2$latitude[j])
          }
          pts <- base::substring(pts, 2)
          url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", 
                              pts, "&time=", uma[curma], "&model=", 
                              model, "&return_null_points")
          paleopts <- rjson::fromJSON(file = url)
          for (k in 1:base::length(paleopts$coordinates)) {
            if (base::is.null(paleopts$coordinates[[k]])) {
              paleolng <- c(paleolng, NA)
              paleolat <- c(paleolat, NA)
            } else {
              paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
              paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
            }
          }
        }
        round <- round + 1
      }
    } else {
      # if there isn't >200 data points, then run the whole thing
      for (j in 1:base::length(part$age)) {
        pts <- base::paste0(pts, ",", part$longitude[j], 
                            ",", part$latitude[j])
      }
      pts <- base::substring(pts, 2)
      url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", 
                          pts, "&time=", uma[curma], "&model=", model, 
                          "&return_null_points")
      paleopts <- rjson::fromJSON(file = url)
      for (k in 1:base::length(paleopts$coordinates)) {
        if (base::is.null(paleopts$coordinates[[k]])) {
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        } else {
          paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
          paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
        }
      }
    }
    print(paste(curma, "out of", length(uma), sep = " "))
  }
  paleolat <- paleolat[order(data$order)]
  paleolng <- paleolng[order(data$order)]
  return(list(paleolat = paleolat, paleolng = paleolng))
}

# pal.coord.round ---------------------------------------------------------
pal.coord.round <- function(data, model) {
  ## this is from palaeocoords, which is a function in mapast
  # it assumes the age column is 'age'
  paleolng <- c()
  paleolat <- c()
  data$order <- seq(1:nrow(data))
  data <- data[order(data$round.age), ]
  uma <- unique(data$round.age)
  for (curma in 1:length(uma)) {
    # subset the data for unique ages
    part <- subset(data, data$round.age == uma[curma])
    pts <- ""
    if (base::length(part$round.age) > 200) {
      # if there is lots of data in that age category, then run it in subsets
      num <- base::ceiling(base::length(part$round.age)/200)
      round <- 1
      while (round <= num) {
        pts <- ""
        if (round < num) {
          pts <- ""
          part2 <- part[((round - 1) * 200 + 1):(round * 200), ]
          for (j in 1:base::length(part2$round.age)) {
            pts <- base::paste0(pts, ",", part2$longitude[j], ",", part2$latitude[j])
          }
          pts <- base::substring(pts, 2)
          url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=",
                              uma[curma], "&model=", model, "&return_null_points")
          paleopts <- rjson::fromJSON(file = url)
          for (k in 1:base::length(paleopts$coordinates)) {
            if (base::is.null(paleopts$coordinates[[k]])) {
              paleolng <- c(paleolng, NA)
              paleolat <- c(paleolat, NA)
            } else {
              paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
              paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
            }
          }
        } else {
          pts <- ""
          part2 <- part[((round - 1) * 200 + 1):base::length(part$round.age), 
                        ]
          for (j in 1:base::length(part2$round.age)) {
            pts <- base::paste0(pts, ",", part2$longitude[j], 
                                ",", part2$latitude[j])
          }
          pts <- base::substring(pts, 2)
          url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", 
                              pts, "&time=", uma[curma], "&model=", 
                              model, "&return_null_points")
          paleopts <- rjson::fromJSON(file = url)
          for (k in 1:base::length(paleopts$coordinates)) {
            if (base::is.null(paleopts$coordinates[[k]])) {
              paleolng <- c(paleolng, NA)
              paleolat <- c(paleolat, NA)
            } else {
              paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
              paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
            }
          }
        }
        round <- round + 1
      }
    } else {
      # if there isn't >200 data points, then run the whole thing
      for (j in 1:base::length(part$round.age)) {
        pts <- base::paste0(pts, ",", part$longitude[j], 
                            ",", part$latitude[j])
      }
      pts <- base::substring(pts, 2)
      url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", 
                          pts, "&time=", uma[curma], "&model=", model, 
                          "&return_null_points")
      paleopts <- rjson::fromJSON(file = url)
      for (k in 1:base::length(paleopts$coordinates)) {
        if (base::is.null(paleopts$coordinates[[k]])) {
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        } else {
          paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
          paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
        }
      }
    }
    print(paste(curma, "out of", length(uma), sep = " "))
  }
  paleolat <- paleolat[order(data$order)]
  paleolng <- paleolng[order(data$order)]
  return(list(paleolat = paleolat, paleolng = paleolng))
}


# Numerical abundance -----------------------------------------------------
num.abun <- function(ID, data, choices, sample_source = "source") { 
 print(ID)
  if (sample_source == "source") {
    d.abun <- data$orig.abundance[data$source == ID & !is.na(data$source)]
    choices.sub <- choices$abun[choices$abun$source == ID,]
  }
  else if (sample_source == "sample") {
    d.abun <- data$orig.abundance[data$sampleID == ID]
    choices.sub <- choices$abun.smID[choices$abun.smID$source == ID,]
  }
  if (!is.na(choices.sub$abun)) {
    c.abun.num <- choices.sub$abun[[1]]$abun.num
    c.abun.p <- choices.sub$abun[[1]]$abun.p
    c.abun.a <- choices.sub$abun[[1]]$abun.a
    c.abun.bin <- choices.sub$abun[[1]]$abun.bin
  } else {
    c.abun.num <- c.abun.p <- c.abun.a <- c.abun.bin <- NULL
  }
  
  # if the abundance units are NA
  if (is.na(choices.sub$units)) {
    print("Numeric characters:")
    print(summary(as.numeric(gsub("#|\\?|>|<", "", grep("\\d", d.abun, value = TRUE)))))
    print("sum:")
    print(sum(as.numeric(gsub("#|\\?|>|<", "", grep("\\d", d.abun, value = TRUE)))))
    print("Non numeric characters:")
    print(grep("[^(0-9)|\\.]", unique(d.abun), value = TRUE))
    # what are the abundance units
    print("Abundance units:")
    c.abun.units <- dlg_list(c("Relative abundance", "Count", "Number per gram", "Binned", "P/A", "Mixed") , title = "What are the units of abundance?")$res
  } else {
    c.abun.units <- choices.sub$units
  }
  if (c.abun.units %in% "Mixed")
    d.abun <- rep(NA, length(d.abun))
  
  if (c.abun.units %in% c("Relative abundance", "Count", "Number per gram")) {
    if (any(grepl("[^(0-9)|\\.]", unique(d.abun)))) {
      if(is.null(c.abun.num)) {
        print(grep("[^(0-9)|\\.]", unique(d.abun), value = TRUE))
        c.abun.num <- as.numeric(readline("What should this be replaced with? "))
      }
      d.abun[grep("[^(0-9)|\\.]", d.abun)] <- c.abun.num
    }
  } 
  if (c.abun.units == "P/A") {
    if (is.null(c.abun.p)) {
      print(unique(d.abun))
      c.abun.p <- readline("What symbol indicates presence? ")
      c.abun.a <- readline("What symbol indicates absence? ")
      for (i in unique(d.abun)[!(unique(d.abun) %in% c(c.abun.p, c.abun.a))]) {
        tmp.pa <- dlg_list(c("Presence", "Absence") , title = paste("Should", i, "be presence or absence?"))$res
        if (tmp.pa == "Presence"){
          c.abun.p <- c(c.abun.p, i)
        } else if (tmp.pa == "Absence") {
          c.abun.a <- c(c.abun.a, i)
        }
      }
      rm(i)
    }
    d.abun[d.abun %in% c.abun.p] <- 1
    d.abun[d.abun %in% c.abun.a] <- 0
  } 
  if (c.abun.units == "Binned") {
    print(grep("[^(0-9)|\\.]", unique(d.abun), value = TRUE))
    if (is.null(c.abun.bin))
      c.abun.bin <- readline("Enter the binned abundances starting with absent (e.g. n,r,c,a), leave reworked out: ")
    c.abun.bin <- unlist(strsplit(c.abun.bin, ","))
    d.abun <- match(d.abun, c.abun.bin) - 1
    d.abun[is.na(d.abun)] <- 0
  }
  d.abun <- as.numeric(as.character(d.abun))
  
  choices <- list(abun.num = c.abun.num, abun.p = c.abun.p, abun.a = c.abun.a, abun.bin = c.abun.bin)
  return(list(data = d.abun, abun.units = c.abun.units, choices = choices))
}


# zones function ----------------------------------------------------------
# identify the column with the zone

# if known, identify the zone scheme, otherwise leave blank (to be guessed later)

# identify the species, or if multiple, indicate it is mixed

# run zones.fun on all those sites that have NAs for the age (this takes in a dataframe of unique zones / oceans, and arguments of scheme and type)

# calculate a continuous age estimate based on samples with continuous zone schemes

# function for obtaining ages of zones
zones.fun <- function(dat, scheme = NA, type, mag = FALSE, full.events = all.chrons, full.zones = all.zones) {
  library(readxl)
  # create a subset of the chosen scheme
  if (!is.na(scheme) & scheme != "Other" & scheme != "Pre2012" & scheme != "Post2012") {
    zones <- full.zones[full.zones$Scheme == scheme,]
  } else {
    zones <- full.zones
  }
  
  # read in the events spreadsheet
  full.events$corr.chron[!is.na(full.events$Age.notes)] <- paste(full.events$corr.chron[!is.na(full.events$Age.notes)], full.events$Age.notes[!is.na(full.events$Age.notes)])
  tmp.events <- tibble(Zone = full.events$corr.chron, EventS = NA, Start = full.events$Age, EventE = NA, End = full.events$Age, Mean = NA, Scheme = NA, Type = full.events$Corrected.Type, Event = "Event", Comment = "Post2020", OceanS = NA, OceanE = NA, orig.st = NA, orig.en = NA, )
  
  # merge zones and events
  zones <- rbind(zones, tmp.events)
  
  # convert zones to upper case / remove spaces
  zones$Zone <- toupper(zones$Zone)
  zones$Zone <- gsub("\\s+", "", zones$Zone)
  full.zones$Zone <- toupper(full.zones$Zone)
  full.zones$Zone <- gsub("\\s+", "", full.zones$Zone)
  
  # if magneto, then set these as zone
  if (mag) {
    dat$zone <- dat$mag.zone
    dat <- dat[!duplicated(dat$zone),]
  }
  
  ## tidy up the zones, so they will be recognised
  # where necessary, split the zones (n.b. this cannot be done for Magneto)
  if (!is.na(type) & type == "Foram") {
    tmp.age <- dat$zone
    # excluding magneto
    tmp.age[!grepl(" C[0-9]", tmp.age)] <- gsub("([0-9])([A-Z])", "\\1-\\2", tmp.age[!grepl(" C[0-9]", tmp.age)])
    tmp.age[!grepl(" C[0-9]", tmp.age)] <- gsub("([a-z])([A-Z])", "\\1-\\2", tmp.age[!grepl(" C[0-9]", tmp.age)])
    tmp.age <- toupper(tmp.age)
  } else {
    tmp.age <- toupper(dat$zone)
  }
  tmp.age <- gsub("TOP ", "T", tmp.age)
  # where the second age is missing the period
  if (any(grepl("[0-9]", tmp.age)))
    tmp.age <- ifelse(grepl("[0-9]", tmp.age), gsub("/|-| TO |,| OR ", "/", tmp.age), tmp.age)
  tmp.age <- gsub("\\s+", "", tmp.age)
  # if the zone is incomplete
  if (any(grepl("/[0-9]", tmp.age))) {
    # if there are two sets of incomplete zones
    if (any(grepl("/[0-9].+$", tmp.age) & grepl("/[0-9]$", tmp.age))) {
      tmp.age[grepl("/[0-9]", tmp.age)] <- sapply(tmp.age[grepl("/[0-9]", tmp.age)], function (x) gsub("(^.*/.*/.*/.*?)", paste("\\1", gsub("^[A-Z]*[0-9]/[0-9]/([^0-9]*).*$", "\\1", x), sep = ""), sub("/", paste("/", gsub("^([^0-9]*).*$", "\\1", x), sep = ""), x)), USE.NAMES = FALSE)
    } else {
      tmp.age[grepl("/[0-9]", tmp.age)] <- sapply(tmp.age[grepl("/[0-9]", tmp.age)], function (x) gsub("/([0-9])", paste("/", gsub("^([^0-9]*).*$", "\\1", x), "\\1", sep = ""), x), USE.NAMES = FALSE)
    }
  }
  if (any(grepl("/[A-Z]$", tmp.age)))
    tmp.age[grepl("/[A-Z]$", tmp.age)] <- sapply(tmp.age[grepl("/[A-Z]$", tmp.age)], function (x) gsub("/", paste("/", gsub("^([^0-9]*[^A-Z]*).*$", "\\1", x), sep = ""), x), USE.NAMES = FALSE)
  if (any(grepl("\\(INDO", tmp.age)))
    tmp.age[grepl("\\(INDO", tmp.age)] <- sapply(tmp.age[grepl("\\(INDO", tmp.age)], function (x) gsub("\\(INDO", "", x), USE.NAMES = FALSE)
  # split those that have multiple zones
  tmp.age <- ifelse(grepl("[0-9]", tmp.age), strsplit(tmp.age, "/|_"), ifelse(grepl("_", tmp.age), strsplit(tmp.age, "_"), tmp.age))
  
  # remove those where there was a extra split in the middle of a zone (e.g. P-2)
  tmp.age <- lapply(tmp.age, function(x) if(any(grepl("[0-9]", x) & !grepl(">", x) & !grepl("\\(F\\)", x) & !grepl("[0-9]CN", x) & !grepl("C[0-9]+N|R|A", x) & !grepl("^T|^B", x))) {grep("[0-9]|^TSF$", x, value = TRUE)} else {x})
  
  if (is.null(dat$region))
    browser()
  if (!any(c("Trop", "Temp") %in% (dat$region)))
    browser()
  if(any(dat$ocean == "Atl" & dat$region == "Trop")) {
    # for atlantic sites in tropical regions
    tmp.zon <- gsub("ATL$", "", zones$Zone)
    tmp.zon <- gsub("TROP$", "", tmp.zon)
    # tmp.zon <- toupper(tmp.zon)
    # tmp.zon <- gsub("\\s+", "", tmp.zon)
    tmpf.zon <- gsub("ATL$", "", full.zones$Zone)
    tmpf.zon <- gsub("TROP$", "", tmpf.zon)
    # tmpf.zon <- toupper(tmpf.zon)
    # tmpf.zon <- gsub("\\s+", "", tmpf.zon)
    
    # calculate start / end dates
    zon.st <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Start[match(x, tmp.zon)], full.zones$Start[match(x, tmpf.zon)]))
    dat$st[dat$ocean == "Atl" & dat$region == "Trop"] <- unlist(lapply(zon.st, max))
    zon.en <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$End[match(x, tmp.zon)], full.zones$End[match(x, tmpf.zon)]))
    dat$en[dat$ocean == "Atl" & dat$region == "Trop"] <- unlist(lapply(zon.en, min))
    zon.sch <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Scheme[match(x, tmp.zon)], full.zones$Scheme[match(x, tmpf.zon)]))
    zon.sch <- unlist(lapply(zon.sch, function(x) paste(unique(x), collapse = ", ")))
    dat$sch[dat$ocean == "Atl" & dat$region == "Trop"] <- gsub("NA, ", "", zon.sch)
    zon.typ <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Type[match(x, tmp.zon)], full.zones$Type[match(x, tmpf.zon)]))
    zon.typ <- unlist(lapply(zon.typ, function(x) paste(unique(x), collapse = ", ")))
    dat$type[dat$ocean == "Atl" & dat$region == "Trop"] <- gsub("None, ", "", zon.typ)
    
  }
  if(any(dat$ocean == "Atl" & dat$region == "Temp")) {
    # for atlantic sites in temperate regions
    tmp.zon <- gsub("ATL$", "", zones$Zone)
    tmp.zon <- gsub("TEMP$", "", tmp.zon)
    # tmp.zon <- toupper(tmp.zon)
    # tmp.zon <- gsub("\\s+", "", tmp.zon)
    tmpf.zon <- gsub("ATL$", "", full.zones$Zone)
    tmpf.zon <- gsub("TEMP$", "", tmpf.zon)
    # tmpf.zon <- toupper(tmpf.zon)
    # tmpf.zon <- gsub("\\s+", "", tmpf.zon)
    
    # calculate start / end dates
    zon.st <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Start[match(x, tmp.zon)], full.zones$Start[match(x, tmpf.zon)]))
    dat$st[dat$ocean == "Atl" & dat$region == "Temp"] <- unlist(lapply(zon.st, max))
    zon.en <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$End[match(x, tmp.zon)], full.zones$End[match(x, tmpf.zon)]))
    dat$en[dat$ocean == "Atl" & dat$region == "Temp"] <- unlist(lapply(zon.en, min))
    zon.sch <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Scheme[match(x, tmp.zon)], full.zones$Scheme[match(x, tmpf.zon)]))
    zon.sch <- unlist(lapply(zon.sch, function(x) paste(unique(x), collapse = ", ")))
    dat$sch[dat$ocean == "Atl" & dat$region == "Temp"] <- gsub("NA, ", "", zon.sch)
    zon.typ <- lapply(tmp.age[dat$ocean == "Atl" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Type[match(x, tmp.zon)], full.zones$Type[match(x, tmpf.zon)]))
    zon.typ <- unlist(lapply(zon.typ, function(x) paste(unique(x), collapse = ", ")))
    dat$type[dat$ocean == "Atl" & dat$region == "Temp"] <- gsub("None, ", "", zon.typ)
    
  }
  if(any(dat$ocean == "IndoPac" & dat$region == "Trop")) {
    # for Indo/Pacific sites in tropical regions
    tmp.zon <- gsub("INDOPAC$", "", zones$Zone)
    tmp.zon <- gsub("TROP$", "", tmp.zon)
    # tmp.zon <- toupper(tmp.zon)
    # tmp.zon <- gsub("\\s+", "", tmp.zon)
    tmpf.zon <- gsub("INDOPAC$", "", full.zones$Zone)
    tmpf.zon <- gsub("TROP$", "", tmpf.zon)
    # tmpf.zon <- toupper(tmpf.zon)
    # tmpf.zon <- gsub("\\s+", "", tmpf.zon)
    
    # calculate start / end dates
    zon.st <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Start[match(x, tmp.zon)], full.zones$Start[match(x, tmpf.zon)]))
    dat$st[dat$ocean == "IndoPac" & dat$region == "Trop"] <- unlist(lapply(zon.st, max))
    zon.en <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$End[match(x, tmp.zon)], full.zones$End[match(x, tmpf.zon)]))
    dat$en[dat$ocean == "IndoPac" & dat$region == "Trop"] <- unlist(lapply(zon.en, min))
    zon.sch <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Scheme[match(x, tmp.zon)], full.zones$Scheme[match(x, tmpf.zon)]))
    zon.sch <- unlist(lapply(zon.sch, function(x) paste(unique(x), collapse = ", ")))
    dat$sch[dat$ocean == "IndoPac" & dat$region == "Trop"] <- gsub("NA, ", "", zon.sch)
    zon.typ <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Trop"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Type[match(x, tmp.zon)], full.zones$Type[match(x, tmpf.zon)]))
    zon.typ <- unlist(lapply(zon.typ, function(x) paste(unique(x), collapse = ", ")))
    dat$type[dat$ocean == "IndoPac" & dat$region == "Trop"] <- gsub("None, ", "", zon.typ)
    
  }
  if(any(dat$ocean == "IndoPac" & dat$region == "Temp")) {
    # for Indo/Pacific sites in temperate regions
    tmp.zon <- gsub("INDOPAC$", "", zones$Zone)
    tmp.zon <- gsub("TEMP$", "", tmp.zon)
    # tmp.zon <- toupper(tmp.zon)
    # tmp.zon <- gsub("\\s+", "", tmp.zon)
    tmpf.zon <- gsub("INDOPAC$", "", full.zones$Zone)
    tmpf.zon <- gsub("TEMP$", "", tmpf.zon)
    # tmpf.zon <- toupper(tmpf.zon)
    # tmpf.zon <- gsub("\\s+", "", tmpf.zon)
    
    # calculate start / end dates
    zon.st <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Start[match(x, tmp.zon)], full.zones$Start[match(x, tmpf.zon)]))
    dat$st[dat$ocean == "IndoPac" & dat$region == "Temp"] <- unlist(lapply(zon.st, max))
    zon.en <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$End[match(x, tmp.zon)], full.zones$End[match(x, tmpf.zon)]))
    dat$en[dat$ocean == "IndoPac" & dat$region == "Temp"] <- unlist(lapply(zon.en, min))
    zon.sch <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Scheme[match(x, tmp.zon)], full.zones$Scheme[match(x, tmpf.zon)]))
    zon.sch <- unlist(lapply(zon.sch, function(x) paste(unique(x), collapse = ", ")))
    dat$sch[dat$ocean == "IndoPac" & dat$region == "Temp"] <- gsub("NA, ", "", zon.sch)
    zon.typ <- lapply(tmp.age[dat$ocean == "IndoPac" & dat$region == "Temp"], function(x) ifelse(!is.na(match(x, tmp.zon)), zones$Type[match(x, tmp.zon)], full.zones$Type[match(x, tmpf.zon)]))
    zon.typ <- unlist(lapply(zon.typ, function(x) paste(unique(x), collapse = ", ")))
    dat$type[dat$ocean == "IndoPac" & dat$region == "Temp"] <- gsub("None, ", "", zon.typ)
    
  }
  
  dat$zon.age <- apply(dat[, c("st", "en")], 1, mean)
  dat$age.rng <- dat$st - dat$en
  if (mag) {
    names(dat)[names(dat) == "st"] <- "mag.st"
    names(dat)[names(dat) == "en"] <- "mag.en"
    names(dat)[names(dat) == "zon.age"] <- "mag.age"
    names(dat)[names(dat) == "age.rng"] <- "mag.age.rng"
    dat <- dat[, c("zone", "sch", "type", "ocean", "region", "segment", "mag.st", "mag.en", "mag.age", "mag.age.rng")]
  } else {
    dat <- dat[, c("zone", "sch", "type", "ocean", "region", "segment", "st", "en", "zon.age", "age.rng")]
  }
  return(dat)
}



# Age estimates -----------------------------------------------------------
age.interp <- function(data) {
  # sort this by depth
  tmp <- data[order(data$age, data$sample.depth),]
  
  # get the unique age info for the samples
  tmp.age <- unique(tmp[!is.na(tmp$zon.age), grep("age", names(tmp))])
  # check if the zones are continuous 
  tmp.age$interp <- FALSE
  if (nrow(tmp.age) > 2)
    tmp.age$interp[2:(nrow(tmp.age) - 1)] <- tmp.age$age.st[1:(nrow(tmp.age) - 2)] == tmp.age$age.en[2:(nrow(tmp.age) - 1)] & tmp.age$age.st[2:(nrow(tmp.age) - 1)] == tmp.age$age.en[3:(nrow(tmp.age))]
  
  # where zones are continuous then use the depth to interpolate ages
  tmp <- merge(tmp, tmp.age)
  
  tmp.sample <- tmp[!duplicated(tmp$sampleID) & !is.na(tmp$zone), c("sampleID", "zone", "interp", "age", "zon.age", "rng.age", "age.st", "age.en", "int.age", "err.int.age", "sample.depth")]
  # for each unique age
  for (i in unique(tmp.sample$zon.age)) {
    # if that age should be interpolated (and there is more than one row)
    if (any(tmp.sample$interp[tmp.sample$zon.age == i] == TRUE) & nrow(tmp.sample[tmp.sample$zon.age == i, ]) > 1) {
      # if there is sample depth
      if (all(!is.na(tmp.sample$sample.depth[tmp.sample$zon.age == i]))) 
        tmp.sample$int.age[tmp.sample$zon.age == i] <- tmp.sample$age.en[tmp.sample$zon.age == i][1] + (tmp.sample$age.st[tmp.sample$zon.age == i][1] - tmp.sample$age.en[tmp.sample$zon.age == i][1]) * ((tmp.sample$sample.depth[tmp.sample$zon.age == i] - min(tmp.sample$sample.depth[tmp.sample$zon.age == i])) / (max(tmp.sample$sample.depth[tmp.sample$zon.age == i]) - min(tmp.sample$sample.depth[tmp.sample$zon.age == i])))
      else
        tmp.sample$int.age[tmp.sample$zon.age == i] <- tmp.sample$age.en[tmp.sample$zon.age == i][1] + ((tmp.sample$age.st[tmp.sample$zon.age == i][1] - tmp.sample$age.en[tmp.sample$zon.age == i][1]) / nrow(tmp.sample[tmp.sample$zon.age == i,])) * 1:nrow(tmp.sample[tmp.sample$zon.age == i,])
      if (all(!is.na(tmp.sample$int.age[tmp.sample$zon.age == i])))
        tmp.sample$err.int.age[tmp.sample$zon.age == i] <- 0
      else
        tmp.sample$err.int.age[tmp.sample$zon.age == i] <- NA
    } else {
      tmp.sample$int.age[tmp.sample$zon.age == i] <- tmp.sample$zon.age[tmp.sample$zon.age == i]
      tmp.sample$err.int.age[tmp.sample$zon.age == i] <- tmp.sample$rng.age[tmp.sample$zon.age == i]
      tmp.sample$interp[tmp.sample$zon.age == i] <- FALSE
    }
  }
  tmp <- merge(tmp[, !names(tmp) %in% c("int.age", "err.int.age")], tmp.sample, all.x = TRUE)
  tmp <- tmp[order(tmp$sample.depth),]
  return(tmp[, c("sampleID", "int.age", "err.int.age", "interp", "sample.depth")])
}

age.interp.chrono <- function(chrono, zones.tab, data, hole, mag = FALSE) {
  # sort this by depth
  tmp <- data[order(data$age, data$sample.depth),]
  # if(is.list(chrono))
  #   chrono <- chrono[[1]]
  # 
  if (mag) {
    tmp$zone <- tmp$mag.zone
    tmp$age.st <- tmp$mag.age.st
    tmp$age.en <- tmp$mag.age.en
    tmp$rng.age <- tmp$mag.age.rng
    tmp$zon.age <- tmp$mag.age
  }
  
  # add in segments
  # tmp$segment <- 1
  # chrono$segment <- 1
  # if (any(grepl("Gap", tmp$zone))) {
  #   # calculate position of any gaps
  #   tmp.segment <- grep("Gap", tmp$zone)
  #   chrono.segment <- grep("Gap", chrono$orig.chron)
  #   if (length(tmp.segment) > 0) {
  #     # identify the first split
  #     tmp$segment[(tmp.segment[1] + 1):length(tmp$segment)] <- tmp$segment[(tmp.segment[1] + 1):length(tmp$segment)] + 1
  #     chrono$segment[(chrono.segment[1] + 1):length(chrono$segment)] <- chrono$segment[(chrono.segment[1] + 1):length(chrono$segment)] + 1
  #     if (length(tmp.segment) > 1) {
  #       # identify later splits (where it's not just the start / end of the same gap)
  #       for (i in 2:length(tmp.segment)) {
  #         if (tmp.segment[i] - tmp.segment[i - 1] > 1) {
  #           tmp$segment[(tmp.segment[i] + 1):length(tmp$segment)] <- tmp$segment[(tmp.segment[i] + 1):length(tmp$segment)] + 1
  #           chrono$segment[(chrono.segment[i] + 1):length(chrono$segment)] <- chrono$segment[(chrono.segment[i] + 1):length(chrono$segment)] + 1
  #         }
  #       }
  #     }
  #   }
  # }
  
  # get the unique age info for the samples
  tmp.age <- unique(tmp[!is.na(tmp$zone), grep("age|zone|segment", names(tmp))])
  if (any(!(names(chrono) == hole)))
    hole <- gsub("^U|^C|^M", "", hole)
  
  for (i in unique(tmp.age$segment)) {
    if (any(!is.na(chrono[chrono$segment == i, names(chrono) == hole]))) {
      # get the min depth for each zone
      tmp.age$min.depth[tmp.age$segment == i] <- sapply(tmp.age$zone[tmp.age$segment == i], function (x) min(chrono[chrono$segment == i,][match(strsplit(x, "_")[[1]], chrono$corr.chron[chrono$segment == i]), names(chrono) == hole]), USE.NAMES = FALSE)
      # get the max depth for each zone
      tmp.age$max.depth[tmp.age$segment == i] <- sapply(tmp.age$zone[tmp.age$segment == i], function (x) max(chrono[chrono$segment == i,][match(strsplit(x, "_")[[1]], chrono$corr.chron[chrono$segment == i]), names(chrono) == hole]), USE.NAMES = FALSE)
    }
  }
  
  # calculate the scaling factor for each zone
  tmp.age$scaling <- (tmp.age$age.st - tmp.age$age.en) / (tmp.age$max.depth - tmp.age$min.depth)
  
  # get the scaled age for each sample.depth
  tmp.sample <- tmp[, c("sampleID", "zone", "zon.age", "rng.age", "age.st", "age.en", "sample.depth", "segment")]
  tmp.sample$int.age <- NA
  tmp.sample$err.int.age <- NA
  tmp.match <- match(tmp.sample$zone, tmp.age$zone)
  tmp.sample$int.age <- (tmp.sample$sample.depth - tmp.age$min.depth[tmp.match]) * tmp.age$scaling[tmp.match] + tmp.age$age.en[tmp.match]
  tmp.sample$err.int.age[!is.na(tmp.sample$int.age)] <- 0
    
  return(tmp.sample)
}


# age.model ---------------------------------------------------------------
# for each hole where there isn't a unique age for each depth / the age error is NA
age.model <- function(chrono, zones.tab, dataset, hole, plots = FALSE, pan = FALSE, chrons.full = all.chrons) {
  # get the correct ages
  # if(is.list(chrono))
  #   chrono <- chrono[[1]]
  if (any(!(names(chrono) == hole)))
    hole <- gsub("^U|^C|^M", "", hole)
  names(chrono)[names(chrono) == hole] <- "Depth"
  
  if (pan == FALSE) {
    tmp.full <- chrons.full[chrons.full$Age.notes %in% c(NA, unique(as.character(zones.tab$ocean)), unique(as.character(zones.tab$region))), ]
    tmp.age <- merge(chrono, tmp.full, all.x = TRUE)
  } else {
    tmp.age <- chrono
    names(tmp.age)[names(tmp.age) == "Chron"] <- "orig.chron"
  }
  tmp.age <- tmp.age[order(tmp.age$Depth, tmp.age$Age), ]
  
  library(mgcv)
  
  par(ask = plots)
  
  if (plots & !is.null(tmp.age$orig.age)) {
    plot(tmp.age$orig.age, tmp.age$Age, pch = 16, xlab = "Original age", ylab = "Updated age", main = hole)
    abline(0, 1)
  }
  
  # remove na's
  #tmp.age <- tmp.age[!(is.na(tmp.age$corr.age) | is.na(tmp.age$Depth)),]
  
  # plot age vs. depth
  if (plots) {
    plot(tmp.age$Age, tmp.age$Depth, type = "b")
  }
  
  # create the sample dataset
  tmp.pf <- dataset[, c("sampleID", "sample.depth", "segment", "zone", "zon.age", "rng.age", "age.st", "age.en")]
  tmp.pf$mod.age <- NA
  tmp.pf$r2 <- NA
  tmp.pf$n.pts <- NA
  
  # if the depths are identical and the ages are relatively distant (> 0.1x age range), then split the dataset at those points
  tmp.split <- NULL
  if ((tmp.age$Depth[1] - tmp.age$Depth[2]) == 0)
    tmp.split <- 1
  tmp.split <- c(tmp.split, which(diff(tmp.age$Age) > 5 & diff(tmp.age$Depth) < 5 | diff(tmp.age$Age) > 10))
  if (any(tmp.age$orig.chron == "Gap")) {
    tmp.split <- c(tmp.split, which(tmp.age$orig.chron == "Gap"))
  }
  if ((tmp.age$Depth[nrow(tmp.age)] - tmp.age$Depth[nrow(tmp.age) - 1]) == 0)
    tmp.split <- c(tmp.split, (nrow(tmp.age) - 1))
  for (j in 1:(length(tmp.split) + 1)) {
    tmp.st.row <- ifelse(j == 1, j, tmp.split[j - 1] + 1)
    tmp.en.row <- ifelse(j > length(tmp.split), nrow(tmp.age), tmp.split[j])
    tmp.dat <- tmp.age[tmp.st.row:tmp.en.row,]
    # gam / lm
    tmp.pf.rows <- which(tmp.pf$sample.depth >= min(tmp.dat$Depth) & tmp.pf$sample.depth <= max(tmp.dat$Depth))
    if (sum(!is.na(unique(tmp.dat$Depth[!is.na(tmp.dat$Age)]))) > 1 ) { # & length(tmp.pf.rows) > 1) {
      tmp.mod <- tryCatch(gam(Age ~ s(Depth), data = tmp.dat, gamma = 1.1), error = function (e) print("no"))
      
      # from data, extract estimated age for the depths in the data
      if (tmp.mod[1] == "no") {
        if (any(tmp.dat$corr.chron[!is.na(tmp.dat$corr.chron)] == "T SF")) {
          tmp.mod <- lm(Age ~ Depth - 1, data = tmp.dat)
        } else {
          tmp.mod <- lm(Age ~ Depth, data = tmp.dat)
        }
        if (plots) {
          par(ask = plot)
          plot(tmp.dat$Age ~ tmp.dat$Depth, pch = 16, xlab = "Depth", ylab = "Age", main = hole)
          abline(tmp.mod)
        }
        tmp.pf$mod.age[tmp.pf.rows] <- as.numeric(predict(tmp.mod, newdata = data.frame(Depth = tmp.pf$sample.depth[tmp.pf.rows])))
        tmp.pf$r2[tmp.pf.rows] <- suppressWarnings(summary(tmp.mod)$r.squared)
      } else {
        if (plots) {
          par(ask = plots)
          plot(tmp.mod, residuals = TRUE, pch = 16, xlab = "Depth", ylab = "Age", main = hole)
        }
        tmp.pf$mod.age[tmp.pf.rows] <- as.numeric(predict(tmp.mod, newdata = data.frame(Depth = tmp.pf$sample.depth[tmp.pf.rows])))
        tmp.pf$r2[tmp.pf.rows] <- summary(tmp.mod)$r.sq
      }
    tmp.pf$n.pts[tmp.pf.rows] <- nrow(tmp.dat)
    }
  }
  # remove sample depths beyond the depths in the age model
  for (i in unique(tmp.pf$segment[!is.na(tmp.pf$segment)])) {
    seg.row <- which(tmp.pf$segment == i)
    if (sum(tmp.age$segment == i) > 0) {
      tmp.pf$mod.age[seg.row][tmp.pf$sample.depth[seg.row] < min(tmp.age$Depth[tmp.age$segment == i], na.rm = TRUE) | tmp.pf$sample.depth[seg.row] > max(tmp.age$Depth[tmp.age$segment == i], na.rm = TRUE)] <- NA
    }
  }
  
  # check how it matches to the current age estimate
  if (!all(is.na(tmp.pf$zon.age | tmp.pf$mod.age)) & plots) {
    plot(tmp.pf$zon.age, tmp.pf$mod.age, pch = 16, xlab = "Original age", ylab = "Modelled age", xlim = c(min(c(tmp.pf$zon.age, tmp.pf$mod.age), na.rm = TRUE), max(c(tmp.pf$zon.age, tmp.pf$mod.age), na.rm = TRUE)), ylim = c(min(c(tmp.pf$zon.age, tmp.pf$mod.age), na.rm = TRUE), max(c(tmp.pf$zon.age, tmp.pf$mod.age), na.rm = TRUE)), main = hole)
    for (i in 1:nrow(tmp.pf)) {
     lines(c(tmp.pf$age.st[i], tmp.pf$age.en[i]), c(tmp.pf$mod.age[i], tmp.pf$mod.age[i]))
    }
    abline(0,1)
  }
  
  # add summary stats
  #View(tmp.pf)
  
  return(tmp.pf)
}

# estimating depths from hole / section ---------------
# calculate the scaling
depth.calc <- function(x, tmp.depth) {
  if (!is.na(tmp.depth$orig.depth[x]))
    return(x)
  lower.d <- suppressWarnings(max(which(!is.na(tmp.depth$orig.depth)[1:x])))
  upper.d <- suppressWarnings(min(which(!is.na(tmp.depth$orig.depth)[x:nrow(tmp.depth)]) + (x-1)))
  if (!is.infinite(lower.d) & !is.infinite(upper.d))
    return((tmp.depth$samples[x] - tmp.depth$samples[lower.d]) * (tmp.depth$orig.depth[upper.d] - tmp.depth$orig.depth[lower.d]) / (tmp.depth$samples[upper.d] - tmp.depth$samples[lower.d]) + tmp.depth$orig.depth[lower.d])
  if (is.infinite(lower.d))
    return((tmp.depth$samples[x]) * (tmp.depth$orig.depth[upper.d]) / (tmp.depth$samples[upper.d]))
  if (is.infinite(upper.d))
    return((tmp.depth$samples[x] - tmp.depth$samples[lower.d]) * (tmp.depth$orig.depth[lower.d]) / (tmp.depth$samples[lower.d]) + tmp.depth$orig.depth[lower.d])
}


# Function for fraction of range occupied ---------------------------------
age.frac <- function(species, sp.data, occ.data, bin.width) {
  # for a species, identify start and end dates
  t.start <- sp.data$Speciation[sp.data$species == species]
  t.end <- sp.data$Extinction[sp.data$species == species]
  
  # generate a sequence based on bin widths from start to end
  sp.seq <- seq(t.end, t.start, by = 1)
  if (bin.width > 1) {
    sp.seq <- unique(round(sp.seq / bin.width) * bin.width)
  }
  
  # round occurrence data to the bin categories
  bin.occ <- unique(round(occ.data$age[occ.data$species == species] / bin.width) * bin.width)
  
  # only include ages within the expected range
  bin.occ <- bin.occ[bin.occ >= min(sp.seq) & bin.occ <= max(sp.seq)]
  
  # check what fraction of possible ages are filled by the bin categories for that species
  return(length(bin.occ) / length(sp.seq))
}
