# Extra IODP data
# Adding the extra data from IODP

# Previous file: Triton_4MoveDB.R
# Next file: Triton_6Combined_data.R

# libraries ---------------------------------------------------------------
library(readxl) # read_excel
library(measurements) # convert to decimal degrees
library(svDialogs) # dlg_list
library(tidyverse)
library(maps)
library(gsheet)

# 1. Load in the IODP info ------------------------------------------------
IODP.info <- read_excel("Data/Sites IODP.xlsx")
head(IODP.info)
# remove the summary rows
IODP.info <- IODP.info[!is.na(IODP.info$Site),]

# leg / site / hole
IODP.info <- fill(IODP.info, Expedition)
names(IODP.info)[names(IODP.info) == "Expedition"] <- "Leg"
names(IODP.info)[names(IODP.info) == "Site"] <- "Hole"
IODP.info$Site <- gsub("[A-Z]$|[A-Z]/[A-Z]/{,2}", "", IODP.info$Hole)
IODP.info <- IODP.info[, c(1, ncol(IODP.info), 2:(ncol(IODP.info)-1))]

# convert lat / long
IODP.info$Latitude <- gsub("°|\\'", " ", IODP.info$Latitude)
IODP.info$Latitude <- gsub("(S|N).", "\\1", IODP.info$Latitude)
IODP.info$Latitude <- gsub("  ", " ", IODP.info$Latitude)
IODP.info$Latitude <- gsub(" N|N", "", IODP.info$Latitude)
IODP.info$Latitude[grep("S", IODP.info$Latitude)] <- paste("-", IODP.info$Latitude[grep("S", IODP.info$Latitude)], sep = "")
IODP.info$Latitude <- gsub(" S|S", "", IODP.info$Latitude)
IODP.info$Longitude <- gsub("°|\\'", " ", IODP.info$Longitude)
IODP.info$Longitude <- gsub("(E|W).", "\\1", IODP.info$Longitude)
IODP.info$Longitude <- gsub("  ", " ", IODP.info$Longitude)
IODP.info$Longitude <- gsub(" E|E", "", IODP.info$Longitude)
IODP.info$Longitude[grep("W", IODP.info$Longitude)] <- paste("-", IODP.info$Longitude[grep("W", IODP.info$Longitude)], sep = "")
IODP.info$Longitude <- gsub(" W|W", "", IODP.info$Longitude)

IODP.info$DecLat <- NA
IODP.info$DecLong <- NA
IODP.info$DecLat[grepl(" .* ", IODP.info$Latitude)] <- as.numeric(conv_unit(IODP.info$Latitude[grepl(" .* ", IODP.info$Latitude)], from = "deg_min_sec", to = "dec_deg"))
IODP.info$DecLong[grepl(" .* ", IODP.info$Longitude)] <- as.numeric(conv_unit(IODP.info$Longitude[grepl(" .* ", IODP.info$Longitude)], from = "deg_min_sec", to = "dec_deg"))
IODP.info$DecLat[grepl(" ", IODP.info$Latitude) & is.na(IODP.info$DecLat)] <- as.numeric(conv_unit(IODP.info$Latitude[grepl(" ", IODP.info$Latitude) & is.na(IODP.info$DecLat)], from = "deg_dec_min", to = "dec_deg"))
IODP.info$DecLong[grepl(" ", IODP.info$Latitude) & is.na(IODP.info$DecLong)] <- as.numeric(conv_unit(IODP.info$Longitude[grepl(" ", IODP.info$Latitude) & is.na(IODP.info$DecLong)], from = "deg_dec_min", to = "dec_deg"))
IODP.info$DecLat[is.na(IODP.info$DecLat)] <- as.numeric(IODP.info$Latitude[is.na(IODP.info$DecLat)])
IODP.info$DecLong[is.na(IODP.info$DecLong)] <- as.numeric(IODP.info$Longitude[is.na(IODP.info$DecLong)])

# check for errors in the data
summary(tapply(IODP.info$DecLat, IODP.info$Leg, function (x) max(x) - min (x))) # one NA
rev(sort(tapply(IODP.info$DecLat, IODP.info$Leg, function (x) max(x) - min (x))))

summary(tapply(IODP.info$DecLong, IODP.info$Leg, function (x) max(x) - min (x))) # one NA
rev(sort(tapply(IODP.info$DecLong, IODP.info$Leg, function (x) max(x) - min (x))))

# check these look sensible
map("world")
with(IODP.info[IODP.info$Leg == "4", ], points(DecLong, DecLat, type = "b", pch = 16, col = "red"))

summary(IODP.info$DecLat)
unique(IODP.info$Leg[IODP.info$DecLat > 90 & !is.na(IODP.info$DecLat)])
summary(IODP.info$DecLong)

# convert mbsl to numeric
IODP.info$mbsl <- as.numeric(IODP.info$mbsl)

# 2. Working through cores ------------------------------------------------
# if re-running, skip to 3
iodp.files <- grep("[^chrono].xls", list.files("Data/IODP extras/", recursive = TRUE), value = TRUE)

for (i in iodp.files) {
  tmp.path <- paste("Data/IODP extras/", i, sep = "")
  tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
  print(tmp.path)
  print(list.files(gsub("[^/]*$", "", tmp.path)))
  
  check <- "Yes"
  if (!any(grepl(tmp.file, ls()))) {
    View(read_excel(tmp.path, sheet = excel_sheets(tmp.path)[excel_sheets(tmp.path) != "188-1165B_forams"& excel_sheets(tmp.path) != "1145"][1]))
    View(read_excel(paste(gsub("[^/]*$", "", tmp.path), grep(paste("^[^~].*", gsub("^.*(.{1})\\.xlsx", "\\1", tmp.path), "_chrono", sep = ""), list.files(gsub("[^/]*$", "", tmp.path)), value = TRUE), sep = "")))
    
    check <- dlg_list(c("Yes", "No"), title = "Do you want to use this dataset?")$res
    if (check == "No") {
      assign(tmp.file, NA)
    } else {
      assign(tmp.file, 1)
    }
  }
  if (check == "Yes" & !is.na(eval(parse(text = tmp.file))[1]) & length(eval(parse(text = tmp.file))) < 2) {
    if (eval(parse(text = tmp.file)) == 1) {
      choices_iodp <- list()
      choices_iodp$pal.lat.full <- FALSE
    } else {
      assign(choices_iodp, eval(parse(text = paste(tmp.file, "$choices"))))
    }
    assign(tmp.file, iodp.PNstructure(tmp.path, input.init = "IF", pal.lat.full = FALSE, iodp.info = IODP.info, rm.bk = TRUE, choices = choices_iodp))
    rm(choices_iodp)
  }
  rm(tmp.path)
  save(list = grep(gsub("_.*$", "", tmp.file), ls(), value = TRUE), file = paste("Outputs/IODP_processing/IODPtmp_", gsub("_.*$", "", tmp.file), ".RData", sep = ""))
  print(grep(i, iodp.files) / length(iodp.files))
}
rm(i, check, tmp.file)

# check ones that are NA
iodp.na <- NULL
for (i in iodp.files) {
  tmp.path <- paste("../../../../../../Google Drive/Biogeography Project/NEPTUNE Updates (SEP 13)/", i, sep = "")
  tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
  if (any(grepl(tmp.file, ls()))) {
    if (is.na(eval(parse(text = tmp.file))[1])) {
      iodp.na <- c(iodp.na, i)
    }
  }
}
rm(i)

for (i in iodp.na) {
  print(i)
  tmp.path <- paste("../../../../../../Google Drive/Biogeography Project/NEPTUNE Updates (SEP 13)/", i, sep = "")
  tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
  if (any(grepl(tmp.file, ls()))) {
    if (is.na(eval(parse(text = tmp.file))[1])) {
      check <- dlg_list(c("Yes", "No"), title = "Do you want to use this dataset?")$res
      if (check == "No") {
        assign(tmp.file, NA)
      } else {
        tmp <- iodp.PNstructure(tmp.path, input.init = "IF", pal.lat.full = FALSE, iodp.info = IODP.info, rm.bk = TRUE, recalc.age = TRUE)
        if (all(is.na(tmp$data$age)))
          tmp <- NA
        assign(tmp.file, tmp)
      }
    }
  }
}
rm(iodp.na, i)

# 3. Re-running ------------
iodp.files <- grep("^(?=.*\\.xls)(?!.*chron)", list.files("Data/IODP extras/", recursive = TRUE), value = TRUE, perl = TRUE)
model.type <- read_xlsx("Data/iodp_age_models.xlsx")

# need to run chrons.R first
source("Code/chrons.R")

# if re-running load in the previous data
load("Outputs/IODP_separate.RData")
# to load the individual IODP files (though this should be unneccesary)
# lapply(paste("Outputs/IODP_processing/", grep("IODPtmp_", list.files("Outputs/IODP_processing"), value = TRUE), sep = ""), function (x) get(load(x, envir = .GlobalEnv)))


# 3a. Update to GTS 2020 --------------------------------------------------
# uses all.chrons, so assumes that Triton_1AgeModels.R has been run
for (i in iodp.files) {
  print(i)
  tmp.path <- paste("Data/IODP extras/", i, sep = "")
  tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
  if (any(grepl(tmp.file, ls()))) {
    if (!is.na(eval(parse(text = tmp.file))[1])) {
      assign("choices_iodp", eval(parse(text = paste(tmp.file, "$choices"))))
      # update the dataset
      assign(tmp.file, iodp.PNstructure(tmp.path, input.init = "IF", pal.lat.full = FALSE, iodp.info = IODP.info, rm.bk = TRUE, choices = choices_iodp, recalc.age = TRUE))
      # save the choices / data
      save(list = grep(gsub("_.*$", "", tmp.file), ls(), value = TRUE), file = paste("Outputs/IODP_processing/IODPtmp_", gsub("_.*$", "", tmp.file), ".RData", sep = ""))
      print(which(iodp.files == i) / length(iodp.files) * 100)
      rm(choices_iodp)
      
      # age plots
      tmp.dat <- eval(parse(text = paste0(tmp.file, "$data[!duplicated(", tmp.file, "$data$sampleID), ]")))
      tmp.dat <- tmp.dat[order(tmp.dat$sample.depth),]
      
      # chron.ages
      tmp.full <- all.chrons[all.chrons$Age.notes %in% c(NA, unique(as.character(eval(parse(text = paste0(tmp.file, "$choices$zones.tab$ocean"))))), unique(as.character(eval(parse(text = paste0(tmp.file, "$choices$zones.tab$region")))))), ]
      for (j in unique(tmp.dat$hole)) {
        j <- gsub("^U|^C|^M", "", j)
        tmp.age <- merge(eval(parse(text = paste0(tmp.file, "$choices$data.age_", j))), tmp.full, all.x = TRUE)
        if (nrow(tmp.age) != 0) {
          png(paste("Figures/Age models/", tmp.file, "_", j, ".png", sep = ""), 600, 600)
          tmp.age <- tmp.age[order(tmp.age[, names(tmp.age) == j], tmp.age$Age), ]
          plot(tmp.age[, names(tmp.age) == j] ~ tmp.age$Age, xlab = "Age", ylab = "Depth", pch = 16, type = "b", col = 4, main = j, xlim = c(min(c(unlist(tmp.dat[, c("age", "zon.age", "age.st", "age.en", "int.age", "mod.age")]), tmp.age$Age), na.rm = TRUE), max(c(unlist(tmp.dat[, c("age", "zon.age", "age.st", "age.en", "int.age", "mod.age")]), tmp.age$Age), na.rm = TRUE)))
          
          # mag.ages
          points(tmp.dat$sample.depth ~ tmp.dat$mag.age, pch = 16, type = "b", col = 5)
          for (k in 1:nrow(tmp.dat)) {
            lines(c(tmp.dat$mag.age.st[k], tmp.dat$mag.age.en[k]), c(tmp.dat$sample.depth[k], tmp.dat$sample.depth[k]), col = 5)
          }
          
          # zone ages
          points(tmp.dat$sample.depth ~ tmp.dat$zon.age, pch = 16, type = "b")
          for (k in 1:nrow(tmp.dat)) {
            lines(c(tmp.dat$age.st[k], tmp.dat$age.en[k]), c(tmp.dat$sample.depth[k], tmp.dat$sample.depth[k]))
          }
          
          # interpolated ages
          points(tmp.dat$sample.depth ~ tmp.dat$int.age, pch = 16, type = "b", col = 2, cex = 1.2)
          
          # modelled ages
          points(tmp.dat$sample.depth ~ tmp.dat$mod.age, pch = 16, type = "b", col = 3)
          
          # interpolated mag ages
          points(tmp.dat$sample.depth ~ tmp.dat$int.mag.age, pch = 16, type = "b", col = 6)
          
          # final ages
          points(tmp.dat$sample.depth ~ tmp.dat$age, pch = 16, type = "b", col = 7, cex = 1.3)
          
          legend("bottomright", pch = 16, legend = c("Zones", "Interp", "Model", "chrono", "mag", "int.mag", "age"), col = c(1,2,3,4,5,6,7))
          dev.off()
        }
      rm(tmp.age)  
      }
      rm(tmp.full, tmp.dat)
    }
  }
  rm(tmp.file, tmp.path)
}


rm(i, j, k, all.chrons)
# save(list = grep("^iodp[^\\.]", ls(), value = TRUE), file = "Outputs/IODP_separate.RData")

# combine all the datasets together
comp.iodp <- grep("^iodp[^\\.]", ls(), value = TRUE)

IODP_extra <- NULL

for (i in 1:length(comp.iodp)) {
  if (length(eval(parse(text = comp.iodp[i]))) > 1) {
    if (is.null(IODP_extra)) {
      IODP_extra <- eval(parse(text = paste(comp.iodp[i], "$data", sep = "")))
    } else {
      tmp <- eval(parse(text = comp.iodp[i]))
      if(!all(sapply(tmp$data, class) == sapply(IODP_extra, class)))
        break()
      IODP_extra <- bind_rows(IODP_extra, tmp$data)
    }
  }
  print(i)
}
rm(i, tmp)

res.sp4 <- IODP_extra

# remove NAs from age
res.sp4 <- res.sp4[!is.na(res.sp4$age) & res.sp4$age < 66,]
tapply(res.sp4$age, res.sp4$age.calc, summary)

# remove NAs from abundances
tapply(res.sp4$abundance, res.sp4$abun.units, summary)
unique(res.sp4$holeID[is.na(res.sp4$abundance)])

# where NAs are in addition to zero, then remove them, otherwise set them to 0
res.sp4 <- res.sp4[-which(is.na(res.sp4$abundance) & res.sp4$holeID == "188.1165.1165B"), ]
res.sp4$abundance[is.na(res.sp4$abundance)] <- 0

# relative abundances > 0
summary(with(res.sp4[res.sp4$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)))
unique(gsub("(_.*)_[^_]*$", "\\1", names(with(res.sp4[res.sp4$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)[which(tapply(abundance, sampleID, sum) > 100)]))))

sort(with(res.sp4[res.sp4$holeID == "117.722.722A.B", ], tapply(abundance, sampleID, sum))) # nothing obviously wrong with this
sort(with(res.sp4[res.sp4$holeID == "159T.958.958A.", ], tapply(abundance, sampleID, sum))) # nothing obviously wrong with this

summary(res.sp4)


# 4. Tidying data ---------------------------------------------------------
# save(res.sp4, file = "Outputs/res_sp4.RData")
# save(IODP_extra, IODP.info, file = "Outputs/IODPextra.RData")
rm(IODP_extra, IODP.info, iodp.files, model.type, comp.iodp)
rm(list = grep("^iodp[^\\.]", ls(), value = TRUE))
