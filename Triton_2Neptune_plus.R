# Sorting data for PN from Neptune, ForCens and a couple of other sources
# Loading in the data from a original set of sources
# 
# Previous file: Triton_1AgeModels.R
# Next file: Triton_2Neptune_plus.R

# Source files / libraries ------------------------------------------------
library(openxlsx) # read.xlsx (n.b. read_xlsx couldn't handle the large data sizes)
# install.packages("devtools")
# library(devtools)
# install_github("macroecology/mapast")
library(mapast)
library(svDialogs)
library(tidyverse)
library(measurements) # convert to decimal degrees


# 1. NEPTUNE -------------------------------------------------------------
load("Outputs/Neptune.RData")

# load in the neptune data
neptune.orig <- read.xlsx("Data/DB_NeptuneCenozoic.xlsx")

# check the abundances
sort(unique(strtrim(neptune.orig$Taxon.Abundance, 3)))
# first time
# neptune <- PNstructure(neptune.orig, input.init = "IF", database.source = "Neptune", ODP = "Y", multiple.abun = TRUE)
# If this is not being run for the first time
neptune <- PNstructure(neptune.orig, input.init = "IF", database.source = "Neptune", ODP = "Y", choices = neptune$choices, multiple.abun = TRUE)

# check the data
head(neptune$data)
summary(neptune$data)
str(neptune$data)

# binned abundance sequences
## N,P,R,F,C,A,D
# -,P,R,F,C,A,D
# -,X,R,F,C,A,D
# x,P,R,F,C,A,D
# -,+,R,F,C,A,D
# B,P,R,F,A,D
# B,P,R,F,C,A,D
# N,X,R,F,C,A,D
# N,P,S,R,F,C,A,D
# N,P,T,R,F,C,A,D
# -,P,T,R,F,C,A,D
# N,X,VR,R,FR,F,CF,C,AC,A,DA,D
# -,X,VR,R,FR,F,CF,C,AC,A,DA,D
# -,P,VR,R,R/F,F,F/C,C,C/A,A,A/D,D
# -,P,T,T/R,R,R/F,F,F/C,C,C/A,A,A/D,D
# N,(P),P,(R),R,(F),F,(C),C,(A),A,(D),D
# N,(X),X,(R),R,(F),F,(C),C,(A),A,(D),D
# N,(P),P,(S),S,(R),R,(F),F,(C),C,(A),A,(D),D - brackets indicate only present in the smaller size fraction
## n,x,r,f,c,a,d
## n,p,P,r,R,f,F,c,C,a,A,d,D
## n,p,s,r,f,c,a,d


# check the abundance units are working as expected
# Relative abundance
summary(unique(neptune$data$abundance[neptune$data$abun.units == "Relative abundance"]))

# which are > 100
sort(with(neptune$data[neptune$data$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)[tapply(abundance, sampleID, sum) > 100]))
unique(gsub("_.*$", "", names(sort(with(neptune$data[neptune$data$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)[tapply(abundance, sampleID, sum) > 100])))))
# automated guessing of one was wrong, so update this
neptune$data$abun.units[neptune$data$holeID == "120-749B" & neptune$data$abun.units == "Relative abundance"] <- "Count"

# Count
summary(unique(neptune$data$abundance[neptune$data$abun.units == "Count"]))

# P/A
unique(neptune$data$orig.abundance[neptune$data$abun.units == "P/A" & neptune$data$abundance == 1])

# binned
unique(grep("[0-9]", neptune$data$orig.abundance[neptune$data$abun.units == "Binned"], value = TRUE))
unique(neptune$data$orig.abundance[neptune$data$abun.units == "Binned"])

table(neptune$data$orig.abundance[neptune$data$abundance == 0]) # these are ones that were originally classified as extremely rare or questionable ID in a P/A analysis
table(neptune$data$abun.units)

## total.IDd
# remove spaces
neptune$data$total.IDd <- gsub(" |>|<", "", neptune$data$total.IDd)
# convert relative abundance to letters
suppressWarnings(as.numeric(neptune$data$total.IDd))
neptune$data$total.IDd[!is.na(suppressWarnings(as.numeric(neptune$data$total.IDd))) & suppressWarnings(as.numeric(neptune$data$total.IDd)) > 30] <- "A"
neptune$data$total.IDd[!is.na(suppressWarnings(as.numeric(neptune$data$total.IDd))) & suppressWarnings(as.numeric(neptune$data$total.IDd)) > 15] <- "C"
neptune$data$total.IDd[!is.na(suppressWarnings(as.numeric(neptune$data$total.IDd))) & suppressWarnings(as.numeric(neptune$data$total.IDd)) > 3] <- "P"
neptune$data$total.IDd[!is.na(suppressWarnings(as.numeric(neptune$data$total.IDd)))] <- "R"
sort(unique(neptune$data$total.IDd))
save(neptune, neptune.orig, file = "Outputs/Neptune.RData")

# 2. ForCenS -------------------------------------------------------------
# if re-running load in the data
load("Outputs/forcens.RData")

forcens.orig <- read.xlsx("Data/DB_ForCenS.xlsx", sheet = "ForCenSred", na.strings = c("N/A"))

names(forcens.orig) <- gsub("\\.", " ", names(forcens.orig))

forcens.orig$`Globorotalia truncatulinoides`[abs(forcens.orig$`Globorotalia truncatulinoides` - rowSums(cbind(forcens.orig$`Globorotalia truncatulinoides dextral coiling`, forcens.orig$`Globorotalia truncatulinoides sinistral coiling`), na.rm = TRUE)) < 0.02] <- NA
forcens.orig$`Trilobatus sacculifer`[abs(forcens.orig$`Trilobatus sacculifer` - rowSums(cbind(forcens.orig$`Trilobatus sacculifer w sac chamber`, forcens.orig$`Trilobatus sacculifer wo sac chamber`), na.rm = TRUE)) < 0.02] <- NA
forcens.orig$`Turborotalita quinqueloba`[abs(forcens.orig$`Turborotalita quinqueloba` - rowSums(cbind(forcens.orig$`Turborotalita quinqueloba dextral coiling`, forcens.orig$`Turborotalita quinqueloba sinistral coiling`), na.rm = TRUE)) < 0.02] <- NA
forcens.orig$`Turborotalita quinqueloba`[forcens.orig$`Turborotalita quinqueloba` < rowSums(cbind(forcens.orig$`Turborotalita quinqueloba dextral coiling`, forcens.orig$`Turborotalita quinqueloba sinistral coiling`), na.rm = TRUE) & !is.na(forcens.orig$`Turborotalita quinqueloba`)] <- NA

forcens.orig$`Globigerinoides ruber`[!is.na(forcens.orig$`Globigerinoides ruber & Globigerinoides white`)] <- NA
forcens.orig$`Globigerinoides white`[!is.na(forcens.orig$`Globigerinoides ruber & Globigerinoides white`)] <- NA
forcens.orig$`Globorotalia menardii`[!is.na(forcens.orig$`Globorotalia menardii & Globorotalia tumida`)] <- NA
forcens.orig$`Globorotalia tumida`[!is.na(forcens.orig$`Globorotalia menardii & Globorotalia tumida`)] <- NA

tmp.total <- rowSums(forcens.orig[22:72], na.rm = TRUE)
sum(tmp.total > 1.002)
summary(tmp.total*100)
# there is only one significantly over 100 now, and there is no obvious mistake in that one (also it's only 102 %)
rm(tmp.total)

# rotate the dataframe
forcens.long <- reshape(forcens.orig, direction = "long", varying = list(colnames(forcens.orig)[22:ncol(forcens.orig)]), idvar = "Sample ID", v.names = "rel.abun", times = names(forcens.orig)[22:ncol(forcens.orig)], timevar = "Species")
# remove those where the species abundance is NA (i.e. they weren't searched for)
forcens.long <- forcens.long[!is.na(forcens.long$rel.abun), ]
# rescale so that the relative abundances are percentage rather than decimals
forcens.long$rel.abun <- forcens.long$rel.abun*100
head(forcens.long)
rownames(forcens.long) <- 1:nrow(forcens.long)

# if running for the first time
# forcens <- PNstructure(forcens.long, input.init = "IF", database.source = "ForCenS", ODP = "N")
# if re-running 
forcens <- PNstructure(forcens.long, input.init = "IF", database.source = "ForCenS", ODP = "N", choices = forcens$choices)

# set 9999 year to NA
# forcens$data$year[forcens$data$year == 9999] <- NA
# only necessary first time

# check the data
head(forcens$data)
summary(forcens$data)
str(forcens$data)

# relative abundances
summary(with(forcens$data[forcens$data$abun.units == "Relative abundance",], tapply(abundance, sampleID, sum)))

save(forcens, forcens.long, forcens.orig, file = "Outputs/forcens.RData")

# 3. Eocene data --------------------------------------------------
# if re-running
load("Outputs/FentonEtAl2016.RData")

eoc <- read.xlsx("Data/DB_FentonEtAl2016.xlsx")
eoc$sample.ID <- NA
eoc$sample.ID[order(eoc$Location)] <- paste("S", unlist(lapply(table(eoc$Location), function(x) seq(1, x, 1))), sep = "")

# separate species and abundance
eoc2 <- gather(eoc, orig.species, ID, colnames(eoc)[grep("X",colnames(eoc))])
tmp <- sapply(eoc2$ID, strsplit, ";")
eoc2$orig.species <- unlist(lapply(tmp, "[[", 1))
eoc2$abundance <- unlist(lapply(tmp, "[", 2))
eoc2 <- eoc2[!is.na(eoc2$orig.species), ] # because of the structure of the data there are lots of NAs
rm(tmp)

# convert to decimal degrees
eoc2$Latitude <- gsub("d", " ", eoc2$Latitude)
eoc2$Latitude[grep("S", eoc2$Latitude)] <- paste("-", eoc2$Latitude[grep("S", eoc2$Latitude)], sep = "")
eoc2$Latitude <- gsub("m|N|S", "", eoc2$Latitude)
eoc2$Longitude <- gsub("d", " ", eoc2$Longitude)
eoc2$Longitude[grep("W", eoc2$Longitude)] <- paste("-", eoc2$Longitude[grep("W", eoc2$Longitude)], sep = "")
eoc2$Longitude <- gsub("m|W|E", "", eoc2$Longitude)

eoc2$DecLat <- suppressWarnings(as.numeric(eoc2$Latitude))
eoc2$DecLong <- suppressWarnings(as.numeric(eoc2$Longitude))
eoc2$DecLat[grepl(" ", eoc2$Latitude)] <- as.numeric(conv_unit(eoc2$Latitude[grepl(" ", eoc2$Latitude)], from = "deg_dec_min", to = "dec_deg"))
eoc2$DecLong[grepl(" ", eoc2$Longitude)] <- as.numeric(conv_unit(eoc2$Longitude[grepl(" ", eoc2$Longitude)], from = "deg_dec_min", to = "dec_deg"))
# there are 16 NAs which were samples in the original data without precise coordinates

# extract water depth from "year"
eoc2$water.depth <- eoc2$Year
eoc2$water.depth[!(eoc2$Author %in% c("Pacific", "Atlantic", "Indian", "Antarctic"))] <- NA
eoc2$water.depth <- abs(eoc2$water.depth) # remove the minus signs (as all water depths have to be positive)
eoc2$Year[(eoc2$Author %in% c("Pacific", "Atlantic", "Indian", "Antarctic"))] <- NA

# extract sample depth from title
eoc2$sample.depth <- eoc2$Title
eoc2$sample.depth[!(eoc2$Author %in% c("Pacific", "Atlantic", "Indian", "Antarctic"))] <- NA
grep("^\\D*$", unique(eoc2$sample.depth), value = TRUE) # non-numeric characters
eoc2$sample.depth <- suppressWarnings(as.numeric(eoc2$sample.depth))
eoc2$Title[(eoc2$Author %in% c("Pacific", "Atlantic", "Indian", "Antarctic"))] <- eoc2$Location[(eoc2$Author %in% c("Pacific", "Atlantic", "Indian", "Antarctic"))]

# tidy the dataset
eoc2$abundance <- gsub(" ", "", eoc2$abundance)
eoc2$num.age <- suppressWarnings(as.numeric(as.character(eoc2$Age)))
eoc2$zones <- eoc2$Age
eoc2$zones[grepl("^[0-9]", eoc2$zones)] <- NA
eoc2$zones[grep("[A-Z][0-9]", eoc2$zones)] <- grep("[0-9]", unlist(strsplit(eoc2$zones[grep("[A-Z][0-9]", eoc2$zones)], " ")), value = TRUE)
eoc2$zones <- gsub("^\\(|\\)$", "", eoc2$zones)
eoc2$orig.species[grep("^\\S*$", eoc2$orig.species)] <- gsub("\\.", " ", eoc2$orig.species[grep("^\\S*$", eoc2$orig.species)])
eoc2$orig.species <- gsub(" $", "", eoc2$orig.species)
eoc2$orig.species <- gsub("  ", " ", eoc2$orig.species)

# run the PNstructure function
# first time through
# eoc3 <- PNstructure(eoc2, input.init = "IF", database.source = "Fentonetal2016", ODP = "N", pal.lat.full = FALSE, multiple.abun = TRUE)
# if re-running
eoc3 <- PNstructure(eoc2, input.init = "IF", database.source = "Fentonetal2016", ODP = "N", pal.lat.full = FALSE, choices = eoc3$choices, multiple.abun = TRUE)

# add leg / site info
eoc3$data$leg[grepl("_", eoc3$data$holeID)] <- unlist(lapply(strsplit(grep("_", eoc3$data$holeID, value = TRUE), "_"), function(x) x[[1]]))
eoc3$data$site[grepl("_", eoc3$data$holeID)] <- gsub("[A-z]$", "", unlist(lapply(strsplit(grep("_", eoc3$data$holeID, value = TRUE), "_"), function(x) x[[2]])))
eoc3$data$hole[grepl("_", eoc3$data$holeID)] <- unlist(lapply(strsplit(grep("_", eoc3$data$holeID, value = TRUE), "_"), function(x) x[[2]]))

# add sample type
eoc3$data$sample.type[!is.na(eoc3$data$leg)] <- "ODP"
eoc3$data$sample.type[is.na(eoc3$data$leg)] <- "Terrestrial"

# update sources
eoc3$data$source[grepl("Pacific|Atlantic|Indian|Antarctic", eoc3$data$source)] <- eoc3$data$holeID[grepl("Pacific|Atlantic|Indian|Antarctic", eoc3$data$source)]

save(eoc, eoc2, eoc3, file = "Outputs/FentonEtAl2016.RData")


# 4. Extra datasets -------------------------------------------------------
# if re-running
load("Outputs/GIK179557_2.RData")
tmp.dat <- read_excel("Data/DB_GIK17957-2.xlsx")
tmp.dat$row.num  <- paste("R", 1:nrow(tmp.dat), sep = "")
tmp.dat <- pivot_longer(tmp.dat, 3:27, "species")
head(tmp.dat)
tmp.dat$latitude <- 10.898333
tmp.dat$longitude <- 115.305
tmp.dat$Drillcore <- "GIK17957-2"
tmp.dat$species <- gsub("\\s\\[%\\]$", "", tmp.dat$species)
names(tmp.dat) <- gsub("\\[|\\]", "", names(tmp.dat))

# GIK17957.2 <- PNstructure(tmp.dat, input.init = "IF", pal.lat.full = FALSE, database.source = "Pangaea", ODP = "N", choices = tmp.choices)
# rerun
GIK17957.2 <- PNstructure(tmp.dat, input.init = "IF", pal.lat.full = FALSE, database.source = "Pangaea", ODP = "N", choices = GIK17957.2$choices)
GIK17957.2$data$age <- GIK17957.2$data$age / 1000 # as the age is in ka

save(GIK17957.2, file = "Outputs/GIK179557_2.RData")
rm(tmp.dat)

# 5. Combine datasets ----------------------------------------------------
res.sp1 <- neptune$data
res.sp1 <- rbind(res.sp1, forcens$data)
res.sp1 <- rbind(res.sp1, eoc3$data)
res.sp1 <- rbind(res.sp1, GIK17957.2$data)
rownames(res.sp1) <- 1:nrow(res.sp1)

# remove NAs in age
dim(res.sp1)
sum(is.na(res.sp1$age))
res.sp1 <- res.sp1[!is.na(res.sp1$age), ]

summary(res.sp1)

tapply(res.sp1$abundance, res.sp1$abun.units, summary)
tapply(res.sp1$age, res.sp1$age.calc, summary)

# 6. Tidy up --------------------------------------------------------
save(res.sp1, file = "Outputs/Neptune_plus.RData")
rm(forcens, forcens.long, forcens.orig, neptune, neptune.orig, eoc, eoc2, eoc3, GIK17957.2)

