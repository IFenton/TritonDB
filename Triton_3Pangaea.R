# Pangaea data for PN
# Extracting all the foram data from pangaea

# Previous file: Triton_2Neptune_plus.R
# Next file: Triton_4MoveDB.R

# libraries ---------------------------------------------------------------
# install.packages("pangaear")
library(pangaear) # e.g. pg_search
library(tidyverse) # map
library(readxl) # read_excel
library(openxlsx) # read.xlsx (n.b. read_xlsx couldn't handle the large data sizes)
library(svDialogs) # e.g. dlg_list

# if re-running, skip to section 8

# 1. Sample dataset -------------------------------------------------------
# an example pangaea dataset
# Wade et al (2007) Marine Micropal

# if re-running load the data
load("Outputs/WadeEtAl2007MarMic.RData")

# read in the data
readLines("Data/Datasets/199-1218_plank_foram.tab", 30) # starts on line 25
WadeEtAl2007MarMic.orig <- read.delim("Data/Datasets/199-1218_plank_foram.tab", skip = 24, header = TRUE)
head(WadeEtAl2007MarMic.orig)

# correct the names
names(WadeEtAl2007MarMic.orig) <- gsub("\\.+$", "", names(WadeEtAl2007MarMic.orig))
names(WadeEtAl2007MarMic.orig) <- gsub("\\.\\.", "\\. ", names(WadeEtAl2007MarMic.orig))
names(WadeEtAl2007MarMic.orig)[names(WadeEtAl2007MarMic.orig) == "Age. ka.BP"] <- "Age"
WadeEtAl2007MarMic.orig$Age <- WadeEtAl2007MarMic.orig$Age / 1000
WadeEtAl2007MarMic.orig$Leg <- as.numeric(gsub("-.*$", "", WadeEtAl2007MarMic.orig$Sample.label))
WadeEtAl2007MarMic.orig$Site <- as.numeric(gsub("^[^-]*-|[A-Z].*$", "", WadeEtAl2007MarMic.orig$Sample.label))
WadeEtAl2007MarMic.orig$Hole <- gsub("^[^-]*-([^-]*)-.*$", "\\1", WadeEtAl2007MarMic.orig$Sample.label)
WadeEtAl2007MarMic.orig$Core <- gsub("^[^-]*-[^-]*-(.*)-.*$", "\\1", WadeEtAl2007MarMic.orig$Sample.label)
WadeEtAl2007MarMic.orig$Section <- as.numeric(gsub("^.*-(.*),.*$", "\\1", WadeEtAl2007MarMic.orig$Sample.label))
WadeEtAl2007MarMic.orig$Sample.top <- as.numeric(gsub("^.*,", "", WadeEtAl2007MarMic.orig$Sample.label))
WadeEtAl2007MarMic.orig$Sample.ID <- paste("S", 1:nrow(WadeEtAl2007MarMic.orig), sep = "")
head(WadeEtAl2007MarMic.orig)

# reshape the data
WadeEtAl2007MarMic.long <- reshape(WadeEtAl2007MarMic.orig, direction = "long", varying = list(colnames(WadeEtAl2007MarMic.orig)[5:12]), idvar = "Sample.label", v.names = "rel.abun", times = names(WadeEtAl2007MarMic.orig)[5:12], timevar = "Species")
rownames(WadeEtAl2007MarMic.long) <- 1:nrow(WadeEtAl2007MarMic.long)
head(WadeEtAl2007MarMic.long)

#View(WadeEtAl2007MarMic.long)
# first time round
# WadeEtAl2007MarMic <- PNstructure(WadeEtAl2007MarMic.long, input.init = "IF", database.source = "Pangaea", ODP = "Y")
# if re-running load the data
WadeEtAl2007MarMic <- PNstructure(WadeEtAl2007MarMic.long, input.init = "IF", database.source = "Pangaea", ODP = "Y", choices = WadeEtAl2007MarMic$choices)

WadeEtAl2007MarMic$data$dbID <- "10.1594/PANGAEA.691327"
# if I've added any names, check that these are in all the relevant files
name.checking("../../../PhD/Project/Foraminifera/Data/")

# check the data
head(WadeEtAl2007MarMic$data)
summary(WadeEtAl2007MarMic$data)
str(WadeEtAl2007MarMic$data)

# save(WadeEtAl2007MarMic, WadeEtAl2007MarMic.long, WadeEtAl2007MarMic.orig, file = "Outputs/WadeEtAl2007MarMic.RData")



# 2. Extract the full dataset ---------------------------------------------
## extract full search results
# according to the website there are 8203 results - the same as the unique doi's in this list
res <- pg_search(query = "plank* AND foraminifera", bbox = c(-180, -90, 180, 90), count = 500)
for (i in seq(500, 8500, by = 500)) {
  res <- bind_rows(res, pg_search(query = "plank* AND foraminifera", bbox = c(-180, -90, 180, 90), count = 500, offset = i))
}
rm(i)

# 3. Remove those that can't be useful ------------------------------------
# ignore any that are datasets (i.e. only get the doi data points), as the datasets don't contain the actual data.
sum(res$size_measure == "data points")

res.dp <- res[res$size_measure == "data points", ] # 7324

# remove datasets based on key words
res.dp <- res.dp[-grep("multinet", res.dp$citation), ] # 6865
res.dp <- res.dp[-grep("sediment trap", res.dp$citation), ] # 6752
res.dp <- res.dp[-grep("box core", res.dp$citation), ] # 6750


# 4. Get column information -----------------------------------------------
# extract the column names for each dataset
for (i in 0:(ceiling(nrow(res.dp) / 20) - 1)) {
  res.dp$col.names[(i*20 + 1):(i*20 + 20)] <- map((i*20 + 1):(i*20 + 20), function(x) tryCatch(names(pg_data(res.dp$doi[x])[[1]]$data), error = function(e) return(NA))) # return NA when you get errors
  if (i == (ceiling(nrow(res.dp) / 20) - 1)) { # last time through
    res.dp$col.names[(i*20 + 1):nrow(res.dp)] <- map((i*20 + 1):nrow(res.dp), function(x) tryCatch(names(pg_data(res.dp$doi[x])[[1]]$data), error = function(e) return(NA))) 
  }
  Sys.sleep(10) # need to add in sleep time, as you can only download so many datasets at a time
} 
rm(i)

# which ones didn't work
which(is.na(res.dp$col.names))

# some of these run if the cache is emptied
# pg_cache_clear()
res.dp$col.names[is.na(res.dp$col.names)] <- map(which(is.na(res.dp$col.names)), function(x) tryCatch(names(pg_data(res.dp$doi[x])[[1]]$data), error = function(e) return(NA)))

# which ones still didn't work
which(is.na(res.dp$col.names))
# none of these contain relevant information (though interestingly both BFD and ForCens are in there - it may be a question of size)


# 5. Identify all those with relevant columns --------------------------
taxa <- read.xlsx("../../../PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "MacroperforateSpp")
taxa.micro <- read.xlsx("../../../PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "MicroperforateSpp")

# get a unique list of possible species / genus names
poss.sp <- unique(unlist(c(strsplit(taxa$Full, " "), strsplit(taxa.micro$Full, " "))))
# ideally I would then extract all that have species names in the column headings (which I can do based on the synonymy list)

res.sp <- filter(res.dp, grepl(paste(poss.sp, collapse = "|"), col.names))
dim(res.sp) # so 4972

# remove isotope records
res.sp <- filter(res.sp, !grepl("d13C|d18O", col.names))
dim(res.sp) # so 4199

res.sp2 <- filter(res.sp, grepl("Age|zone", col.names))
dim(res.sp2) # 626 have some (obvious/useable) form of age

# check whether I am likely to have missed any
res.dp$useful <- NA

for (i in 1:nrow(res.dp)) {
  if (!(res.dp$doi[i] %in% res.sp2$doi)) {
    print(res.dp$citation[i])
    print(res.dp$supplement_to[i])
    print(res.dp$col.names[i])
    res.dp$useful[i] <- dlg_list(c("Yes", "No"), title = "Does this dataset look useful?")$res
  }
}
rm(i)
# having gone through 380 (of the 4000), there is no obvious evidence that I missed any in my split. There are quite a lot with species data, but they only have age as stratigraphy. 

# save(res, res.dp, res.sp, file = "Outputs/pangaea_init.RData")

# 6. Extract info ---------------------------------------------------------
# add a column with the file name
res.sp2$file <- gsub("\\.|/", "_", res.sp2$doi)
# n.b. it looks like it doesn't re-download files once they are in the cache, so I should be able to get away with just using pg_data on the full dataset

# extract the dataset
res.sp2$data <- NA
res.sp2$data[1:nrow(res.sp2)] <- map(1:nrow(res.sp2), function(x) tryCatch(pg_data(res.sp2$doi[x])[[1]]$data, error = function(e) return(NA)))

# I then need to get the extra information in the heading (as that includes stuff like lat / long)

# sometimes this doesn't work (particularly if the doi matches more than one file)
# So write a function for extracting those
pg_dat_if <- function (file) {
  n.line <- 10
  t.line <- NULL
  # work out which line the data starts on - I need everything up to the */
  while(length(t.line) < 1) {
    n.line <- n.line + 20
    tmp <- readLines(file, n.line)
    t.line <- which(tmp == "*/")
  }
  # ignore everything past that point
  dat <- read_delim(file, delim = "\t", skip = (t.line)) 
  return(dat)
}

# read in the data description
pg_dat_desc <- function (file) {
  n.line <- 10
  t.line <- NULL
  # work out which line the data starts on - I need everything up to the */
  while(length(t.line) < 1) {
    n.line <- n.line + 20
    tmp <- readLines(file, n.line)
    t.line <- which(tmp == "*/")
  }
  # ignore everything past that point
  dat.desc <- readLines(file, (t.line - 1)) 
  return(dat.desc)
}

# run this for the dataset
res.sp2$meta.data <- NA
res.sp2$meta.data[1:nrow(res.sp2)] <- map(res.sp2$file, function (x) pg_dat_desc(paste("Data/PangaeaPF/", x, ".txt", sep = "")))

# 7. Obtain data ----------------------------------------------------------
# 7a. Initially test it on one dataset ------------------------------------
# actually a subset of one dataset as that is quicker for paleolat calculations
names(res.sp2)

tmp <- pan.PNstructure(res.sp2[4,], input.init = "IF", pal.lat.full = FALSE)
rm(tmp)

# 7b. Work through the datasets -------------------------------------------
# create columns for storing the data
# need to uncomment these
# res.sp2$new.dat <- rep(list(NA), nrow(res.sp2))
# res.sp2$choices <- rep(list(NA), nrow(res.sp2))
# res.sp2$comment <- NA

# doi: 10.1594/PANGAEA.202139, there are some errors in the column names, which differ from the original data
i <- which(res.sp2$doi == "10.1594/PANGAEA.202139")

names(res.sp2$data[[i]])[grep("grahami", names(res.sp2$data[[i]]))] <- "G. calida [%]"
names(res.sp2$data[[i]])[grep("P/D", names(res.sp2$data[[i]]))][1] <- "Neogloboquadrina pachyderma dextral and dutertrei integrade [%]"
names(res.sp2$data[[i]])[grep("angulata", names(res.sp2$data[[i]]))] <- "G. truncatulinoides [%]"
names(res.sp2$data[[i]])[grep("mentum", names(res.sp2$data[[i]]))] <- "G. menardii + G. tumida [%]"

res.sp2$meta.data[i][[1]] <- gsub("D. grahami", "G. calida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Deuterammina grahami", "Globigerinella calida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]][33] <- gsub("P/D int", "Neogloboquadrina pachyderma dextral and dutertrei integrade", res.sp2$meta.data[i][[1]][33])
res.sp2$meta.data[i][[1]][51] <- gsub("Neogloboquadrina pachyderma dextral and dutertrei integrade", "Neogloboquadrina pachyderma dutertrei integrade", res.sp2$meta.data[i][[1]][51])
res.sp2$meta.data[i][[1]] <- gsub("N. angulata", "G. truncatulinoides", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Nitzschia angulata", "Globorotalia truncatulinoides", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("N. mentum", "G. menardii + G. tumida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Globorotalia mentum", "Globorotalia menardii + Globorotalia tumida", res.sp2$meta.data[i][[1]])


# doi: 10.1594/PANGAEA.201814, there are some errors in the column names, which differ from the original data
i <- which(res.sp2$doi == "10.1594/PANGAEA.201814")

names(res.sp2$data[[i]])[grep("grahami", names(res.sp2$data[[i]]))] <- "G. calida [%]"
names(res.sp2$data[[i]])[grep("P/D", names(res.sp2$data[[i]]))][1] <- "Neogloboquadrina pachyderma dextral and dutertrei integrade [%]"
names(res.sp2$data[[i]])[grep("angulata", names(res.sp2$data[[i]]))] <- "G. truncatulinoides [%]"
names(res.sp2$data[[i]])[grep("mentum", names(res.sp2$data[[i]]))] <- "G. menardii + G. tumida [%]"

res.sp2$meta.data[i][[1]] <- gsub("D. grahami", "G. calida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Deuterammina grahami", "Globigerinella calida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]][33] <- gsub("P/D int", "Neogloboquadrina pachyderma dextral and dutertrei integrade", res.sp2$meta.data[i][[1]][33])
res.sp2$meta.data[i][[1]][51] <- gsub("Neogloboquadrina pachyderma dextral and dutertrei integrade", "Neogloboquadrina pachyderma dutertrei integrade", res.sp2$meta.data[i][[1]][51])
res.sp2$meta.data[i][[1]] <- gsub("N. angulata", "G. truncatulinoides", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Nitzschia angulata", "Globorotalia truncatulinoides", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("N. mentum", "G. menardii + G. tumida", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("Globorotalia mentum", "Globorotalia menardii + Globorotalia tumida", res.sp2$meta.data[i][[1]])


# 10.1594/PANGAEA.51969: some columns have the same name when they should be separate
i <- which(res.sp2$doi == "10.1594/PANGAEA.51969")

names(res.sp2$data[[i]])[grep("P/D int", names(res.sp2$data[[i]]))][1] <- "Neogloboquadrina pachyderma dextral and dutertrei integrade [%]"
res.sp2$meta.data[i][[1]][142] <- gsub("P/D int", "Neogloboquadrina pachyderma dextral dutertrei integrade", res.sp2$meta.data[i][[1]][142])
res.sp2$meta.data[i][[1]][142] <- gsub("Neogloboquadrina pachyderma dextral and dutertrei integrade", "Neogloboquadrina pachyderma dextral and dutertrei integrade", res.sp2$meta.data[i][[1]][142])
names(res.sp2$data[[i]])[grep("quinqueloba", names(res.sp2$data[[i]]))][1] <- "G. quinqueloba part [%]"
names(res.sp2$data[[i]])[grep("tumida", names(res.sp2$data[[i]]))][2] <- "G. tumida + menardii [%]"

# doi: 10.1594/PANGAEA.755092. There are two size fractions for the first row and they should not be counted separately. 
i <- which(res.sp2$doi == "10.1594/PANGAEA.755092")
res.sp2$data[[i]][1,res.sp2$choices[[i]]$var.col] <- res.sp2$data[[i]][1,res.sp2$choices[[i]]$var.col] / 2

# doi: 10.1594/PANGAEA.691462. One of the columns is mislabelled
i <- which(res.sp2$doi == "10.1594/PANGAEA.691462")
names(res.sp2$data[[i]])[grep("mg/kg", names(res.sp2$data[[i]]))] <- "Neogloboquadrina pachyderma dextral [%]"
res.sp2$meta.data[i][[1]] <- gsub("Erbium \\[mg\\/kg\\]", "Neogloboquadrina pachyderma dextral [%]", res.sp2$meta.data[i][[1]])
res.sp2$meta.data[i][[1]] <- gsub("\\(Er\\)", "(Neogloboquadrina pachyderma dextral [%])", res.sp2$meta.data[i][[1]])



for (i in 1:nrow(res.sp2)) {
  print(paste("i: ",i))
  View(res.sp2$meta.data[[i]])
  View(res.sp2$data[[i]])
  print(head(res.sp2$data[[i]]))
  # are there actually forams in this dataset
  tmp.spp <- colnames(res.sp2$data[[i]])
  tmp.spp <- gsub(" \\[.*$", "", tmp.spp)
  tmp.ns <- compare(unique(tmp.spp), micro = TRUE)
  print(grep("\\s", tmp.ns, value = TRUE))
  print("")
  # print the citation
  print(paste(res.sp2$citation[i], res.sp2$supplement_to[i]))
  # based on the above information, do I want to use this dataset?
  pan.fun <- dlg_list(c(TRUE, FALSE), title = "Do you want to use this dataset?")$res
  if (pan.fun) {
    tmp <- pan.PNstructure(res.sp2[i,], input.init = "IF", pal.lat.full = FALSE)
    res.sp2$new.dat[[i]] <- tmp$data
    res.sp2$choices[[i]] <- tmp$choices
  } else {
    res.sp2$comment[i] <- readline("Enter the reason for ignoring: ")
  }
  View(res.sp2$new.dat[[i]])
  if (i %% 10 == 0)
    save(res.sp2, file = paste0("Outputs/tmp_res_sp2_loop/tmp_res.sp2_", i, ".RData"))
}
rm(i, tmp.spp, tmp.ns, pan.fun)

# row 12 didn't extract the data, as the doi matched two files. So run
res.sp2$data[[12]] <- pg_dat_if(paste("Data/PangaeaPF/", res.sp2$file[[12]], ".txt", sep = ""))

# doi: 10.1594/PANGAEA.683874 has a relative abundance figure of 147 when it should (probably) be 14.7
res.sp2$data[[which(res.sp2$doi == "10.1594/PANGAEA.683874")]][27,"G. aquiensis [%]"] <- res.sp2$data[[which(res.sp2$doi == "10.1594/PANGAEA.683874")]][27,"G. aquiensis [%]"] / 10

# doi: 10.1594/PANGAEA.756787 # this was missing a couple of zeros, offsetting some of the data
res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`G. margaritae [%]` <- c(res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`G. margaritae [%]`[1:25], res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`G. margaritae [%]`[25:51])
res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`G. plesiotumida [%]` <- c(0, res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`G. plesiotumida [%]`[1:51])
res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`S. dehiscens [%]` <- c(0, res.sp2[res.sp2$doi == "10.1594/PANGAEA.756787",]$data[[1]]$`S. dehiscens [%]`[1:51])

# 8. Re-running the dataset --------------------------------------------------
load("Outputs/res_sp2.RData")
pan.model.type <- read_xlsx("Data/pan_age_models.xlsx")

# code for re-running (i.e. using choices)
for (i in 1:nrow(res.sp2)) { 
  print(paste("i: ",i))
  # print(head(res.sp2$data[[i]]))
  if (is.na(res.sp2$comment[i])) {
    tmp.choices <- res.sp2$choices[[i]]
    tmp <- pan.PNstructure(res.sp2[i,], input.init = "IF", pal.lat.full = FALSE, choices = tmp.choices)
    res.sp2$new.dat[[i]] <- tmp$data
    res.sp2$choices[[i]] <- tmp$choices
    
    for (j in unique(tmp$data$holeID)) {
      if (any(grepl("data.age", names(tmp$choices)))) {
        # add age model plots
        tmp.zon <- tmp$data[tmp$data$holeID == j,]
        tmp.zon <- tmp.zon[!duplicated(tmp.zon$sampleID), ]
        tmp.zon <- tmp.zon[, grep("core|sample|age|zone", names(tmp.zon))]
        tmp.zon <- tmp.zon[order(tmp.zon$sample.depth), ]
        
        if (any(!is.na(tmp.zon$age))) {
          png(paste("Figures/Age models/", gsub("/", ".", unique(tmp$data$dbID)), "_", j, ".png", sep = ""), 600, 600)
          plot(tmp.zon$sample.depth ~ tmp.zon$zon.age, pch = 16, xlim = c(min(tmp.zon$age.en, na.rm = TRUE), max(tmp.zon$age.st, na.rm = TRUE)), main = j, ylim = c(min(tmp.zon$sample.depth[!is.na(tmp.zon$zon.age)], na.rm = TRUE), max(tmp.zon$sample.depth[!is.na(tmp.zon$zon.age)], na.rm = TRUE)))
          for (k in 1:nrow(tmp.zon)) {
            lines(c(tmp.zon$age.st[k], tmp.zon$age.en[k]), c(tmp.zon$sample.depth[k], tmp.zon$sample.depth[k]))
          }
          if (any(grepl(paste("data.age", j, sep = "_"), names(tmp$choices))))
            points(tmp$choices[grep(paste("data.age", j, sep = "_"), names(tmp$choices))][[1]]$Depth ~ tmp$choices[grep(paste("data.age", j, sep = "_"), names(tmp$choices))][[1]]$Age, type = "b", col = 4, pch = 16)
          
          points(tmp.zon$sample.depth ~ tmp.zon$int.age, pch = 16, type = "b", col = 2)
          points(tmp.zon$sample.depth ~ tmp.zon$mod.age, pch = 16, col = 3, type = "b")
          points(tmp.zon$sample.depth ~ tmp.zon$age, pch = 16, col = 7, type = "b")
          
          legend("bottomright", pch = 16, legend = c("Zones", "Interp", "Model", "chrono", "age"), col = c(1,2,3,4,7))
          dev.off()
        }
      }
    }
  }
}
rm(i, j, k, tmp, tmp.choices, tmp.zon)

## -,P,R,F,C,A,D
# -,X,R,F,C,A,D
# -,/,R,F,C,A,D
# -,+,R,F,C,A,D
# -,P,S,R,F,C,A,D
# -,P,T,R,F,C,A,D
# -,X,VR,R,FR,F,CF,C,AC,A,DA,D
# -,X,VR,R,FR,F,CF,C,AC,A,AA,D
# -,X,VR,R,R/F,F,F/C,C,C/A,A,A/D,D
# -,P,T,TR,R,RF,F,CF,C,AC,A,AA,D
# -,X,VR,R,VF,F,CF,C,C/A,A,A/D,D
# -,P,VR,R,R/F,F,F/C,C,C/A,A,AA,D
# -,(P),P,(R),R,(F),F,(C),C,(A),A,(D),D
# -,X,cf. VR,VR,cf. R,R,cf. FR,FR,cf. F,F,cf. CF,CF,cf. C,C,cf. AC,AC,cf. A,A,cf. DA,DA,cf. D,D
# -,(P),P,(S),S,(R),R,(F),F,(C),C,(A),A,(D),D - brackets indicate only present in the smaller size fraction
## -,x,r,f,c,a,d

# convert it to one dataframe
res.sp2c <- do.call("rbind", res.sp2$new.dat[is.na(res.sp2$comment)])
str(res.sp2c)


# 9. Tidy the dataframe --------------------------------------------------
# check all the species names are actually valid
grep("^[^ ]*$", sort(unique(compare(unique(res.sp2c$species), micro = TRUE))), value = TRUE)

# tidying processing
res.sp2c$processing <- gsub("^.*METHOD:\\s()", "\\1", res.sp2c$processing)
res.sp2c$processing[res.sp2c$processing == "NA"] <- NA

# check abundances
tapply(res.sp2c$abundance, res.sp2c$abun.units, summary)
unique(res.sp2c$sampleID[res.sp2c$abun.units == "Relative abundance" & res.sp2c$abundance > 100]) # RA > 100 - none of these
summary(with(res.sp2c[res.sp2c$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)))
# relative abundances > 100 in a sample
unique(gsub("_.*$", "", names(with(res.sp2c[res.sp2c$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)[which(tapply(abundance, sampleID, sum) > 110)]))))
# View(res.sp2[res.sp2$doi == "10.1594/PANGAEA.754593",]$data[[1]]) # as reported in the original dataset, so not clear what the mistake is
# View(res.sp2[res.sp2$doi == "10.1594/PANGAEA.51969",]$data[[1]]) # as reported in the original dataset, so not clear what the mistake is
# View(res.sp2[res.sp2$doi == "10.1594/PANGAEA.778847",]$data[[1]])# the relative abundances are calculated without the G. glutinata, so sum to more than 100
# relative abundances <90 in a sample
unique(gsub("_.*$", "", names(with(res.sp2c[res.sp2c$abun.units == "Relative abundance", ], tapply(abundance, sampleID, sum)[which(tapply(abundance, sampleID, sum) < 90)]))))
# 32 of these - most are related to incomplete identifications, although some have no obvious errors (they are the same as originally reported)

# correct the negative water depths
res.sp2c$water.depth <- abs(res.sp2c$water.depth)

## preservation
# this record is % whole rather than % fragments so should be subtracted from 100 to make it comparable
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.807318"] <- 100 - as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.807318"])

# for those records that have number of fragments convert this into percentage
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702127"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702127"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702127"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702130"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702130"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702130"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702131"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702131"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702131"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702132"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702132"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702132"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702133"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702133"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702133"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702135"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702135"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702135"]) * 100
res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702136"] <- as.numeric(res.sp2c$preservation[res.sp2c$dbID =="10.1594/PANGAEA.702136"]) / as.numeric(res.sp2c$total.IDd[res.sp2c$dbID =="10.1594/PANGAEA.702136"]) * 100

# check for NAs
unique(res.sp2c$dbID[is.na(res.sp2c$abundance)]) # none of these

# convert "NA" to NA
res.sp2c$hole[res.sp2c$hole == "NA"] <- NA
res.sp2c$section[res.sp2c$section == "NA"] <- NA
res.sp2c$total.IDd[res.sp2c$total.IDd == "NA"] <- NA
res.sp2c$preservation[res.sp2c$preservation == "NA"] <- NA

# remove NAs in age
dim(res.sp2c)
sum(is.na(res.sp2c$age))
res.sp2c <- res.sp2c[!is.na(res.sp2c$age), ]

summary(res.sp2c)

tapply(res.sp2c$abundance, res.sp2c$abun.units, summary)
tapply(res.sp2c$age, res.sp2c$age.calc, summary)

# save(res.sp2, res.sp2c, file = "Outputs/res_sp2.RData")

rm(res.sp2, pan.model.type)

