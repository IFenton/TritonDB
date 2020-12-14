# Speciation / extinction convert these to GTS 2012
#
# Previous file: N/A
# Next file: Triton_2Neptune_plus.R
   
# Source files / libraries ------------------------------------------------
library(openxlsx)
library(readxl) # read_excel
library(tidyverse)

# 1. Convert the foram ages to GTS 2020 --------------------------------------------
# load in the speciation / extinction ages
foram.ages <- read.xlsx("Data/PF ages.xlsx", sheet = "Final") 
head(foram.ages)
summary(foram.ages)

# load in the foram zones
PFzones.ts <- read.xlsx("Data/Timescale conversion.xlsx", sheet = "WadeZones")
head(PFzones.ts)

# load in the magneto chrons
magneto.ts <- read.xlsx("Data/Timescale conversion.xlsx", sheet = "Chrons")
head(magneto.ts)

# create a function for conversion to GTS2020
ts.conv <- function(age, orig.ts = "CK95", new.ts = "GTS2020", scheme = "Magneto") {
  # create subsets of the data for the new / original ages
  if (scheme == "Magneto") {
    orig.ages <- magneto.ts[, c("Chron", grep(orig.ts, names(magneto.ts), value = TRUE))]
    new.ages <- magneto.ts[, c("Chron", grep(new.ts, names(magneto.ts), value = TRUE))]
  }
  if (scheme == "Forams") {
    orig.ages <- PFzones.ts[, c("Zone", grep(orig.ts, names(PFzones.ts), value = TRUE))]
    new.ages <- PFzones.ts[, c("Zone", grep(new.ts, names(PFzones.ts), value = TRUE))]
  }
  names(orig.ages) <- names(new.ages) <- c("Chron", "Start", "End")
  
  # calculate chron
  chron <- orig.ages$Chron[orig.ages$Start >= age & orig.ages$End < age]
  if (age == 0 & scheme == "Forams")
    chron <- "PT1b"
  if (age == 0 & scheme == "Magneto")
    chron <- "C1n"
  # rescale based on the zone assignments
  new.age <- new.ages$Start[new.ages$Chron == chron] - (orig.ages$Start[new.ages$Chron == chron] - age) / (orig.ages$Start[new.ages$Chron == chron] - orig.ages$End[new.ages$Chron == chron]) * (new.ages$Start[new.ages$Chron == chron] - new.ages$End[new.ages$Chron == chron])
  
  # return the rescaled values
  return(round(new.age, 2))
}


# convert the missing foram zone ages
PFzones.ts$Start.GTS2020[is.na(PFzones.ts$Start.GTS2020)] <- unlist(sapply(PFzones.ts$Start.GTS2012[is.na(PFzones.ts$Start.GTS2020)], ts.conv, orig.ts = "GTS2012"))
PFzones.ts$End.GTS2020[is.na(PFzones.ts$End.GTS2020)] <- unlist(sapply(PFzones.ts$End.GTS2012[is.na(PFzones.ts$End.GTS2020)], ts.conv, orig.ts = "GTS2012"))

names(foram.ages)[names(foram.ages) == "Start"] <- "orig.st"
names(foram.ages)[names(foram.ages) == "End"] <- "orig.en"
names(foram.ages)[names(foram.ages) == "Timescale"] <- "orig.ts"

foram.ages$Start <- NA
foram.ages$End <- NA
foram.ages$Start[foram.ages$orig.ts == "CK95"] <- unlist(sapply(foram.ages$orig.st[foram.ages$orig.ts == "CK95"], ts.conv, scheme = "Forams"))
foram.ages$End[foram.ages$orig.ts == "CK95"] <- unlist(sapply(foram.ages$orig.en[foram.ages$orig.ts == "CK95"], ts.conv, scheme = "Forams"))
foram.ages$Start[foram.ages$orig.ts == "GTS2012"] <- unlist(sapply(foram.ages$orig.st[foram.ages$orig.ts == "GTS2012"], ts.conv, scheme = "Forams", orig.ts = "GTS2012"))
foram.ages$End[foram.ages$orig.ts == "GTS2012"] <- unlist(sapply(foram.ages$orig.en[foram.ages$orig.ts == "GTS2012"], ts.conv, scheme = "Forams", orig.ts = "GTS2012"))


# 3. Update the chrons -------------------------------------
# load in the updated stratigraphic markers
all.chrons <- read_xlsx("Data/BiostratAges.xlsx", sheet = "Final", na = "NA")
names(all.chrons) <- gsub(" ", ".", names(all.chrons))
all.chrons$corr.chron <- paste(all.chrons$Corrected.Event.type, all.chrons$Corrected.event)

# add in foram ages
foram.events <- tibble("Corrected.Type" = "Foram", "Corrected.Event.type" = rep(c("B", "T"), each = nrow(foram.ages)), "Corrected.event" = rep(foram.ages$Species.name, 2), Age = c(foram.ages$Start, foram.ages$End), "Corrected.Source" = "Aze.2020", Timescale = "Post2020", "Age.notes" = NA, corr.chron = c(paste("B", foram.ages$Species.name), paste("T", foram.ages$Species.name)))
foram.events <- foram.events[!(foram.events$corr.chron %in% intersect(foram.events$corr.chron, all.chrons$corr.chron)), ]
all.chrons <- rbind(all.chrons, foram.events)

# convert these to GTS 2020
names(all.chrons)[names(all.chrons) == "Age"] <- "orig" 
names(all.chrons)[names(all.chrons) == "Timescale"] <- "orig.ts"
all.chrons$Age <- NA
all.chrons$Age[all.chrons$orig.ts == "Post2020" & !is.na(all.chrons$orig.ts)] <- all.chrons$orig[all.chrons$orig.ts == "Post2020" & !is.na(all.chrons$orig.ts)]
all.chrons$Age[all.chrons$orig.ts == "Post2012" & !is.na(all.chrons$orig.ts)] <- unlist(sapply(all.chrons$orig[all.chrons$orig.ts == "Post2012" & !is.na(all.chrons$orig.ts)], ts.conv, orig.ts = "GTS2012"))
all.chrons$Age[all.chrons$orig.ts == "Pre2012" & !is.na(all.chrons$orig.ts)] <- unlist(sapply(all.chrons$orig[all.chrons$orig.ts == "Pre2012" & !is.na(all.chrons$orig.ts)], ts.conv))
all.chrons$Age[all.chrons$orig == 0 & !is.na(all.chrons$orig)] <- 0


# 4. Compile the synonymy list --------------------------------------------
chrons.syn <- read_xlsx("Data/Biostrat synonymy.xlsx", sheet = "Final", na = "NA", col_types = "text")
names(chrons.syn) <- gsub(" ", ".", names(chrons.syn))
chrons.syn$Original.age[suppressWarnings(!is.na(as.numeric(chrons.syn$Original.age)))] <- suppressWarnings(as.numeric(chrons.syn$Original.age)[!is.na(as.numeric(chrons.syn$Original.age))]) 
chrons.syn <- chrons.syn[, c("Corrected.Type", "orig.chron", "Corrected.event", "Corrected.Event.type", "Original.age")]

# add in the complete chron list
chrons.new.syn <- tibble(Corrected.Type = all.chrons$Corrected.Type, orig.chron = all.chrons$corr.chron, Corrected.event = all.chrons$Corrected.event, Corrected.Event.type = all.chrons$Corrected.Event.type, Original.age = all.chrons$orig)
all.chrons <- all.chrons[, !(names(all.chrons) %in% c("orig", "Corrected.Source", "orig.ts"))]

# add in the magneto chrons
chrons.mag.syn <- tibble(Corrected.Type = "Magneto", orig.chron = c(paste("T", magneto.ts$Chron), paste("T", magneto.ts$Chron), paste("B", magneto.ts$Chron)), Corrected.event = NA, Corrected.Event.type = "B", Original.age = c(magneto.ts$End.GTS2020, magneto.ts$End.CK95, magneto.ts$Start.CK95))
chrons.mag.syn$Corrected.event <- magneto.ts$Chron[match(chrons.mag.syn$Original.age, magneto.ts$Start.GTS2020)]
chrons.mag.syn$Corrected.event[is.na(chrons.mag.syn$Corrected.event)] <- magneto.ts$Chron[match(chrons.mag.syn$Original.age[is.na(chrons.mag.syn$Corrected.event)], magneto.ts$Start.CK95)]
chrons.mag.syn$Corrected.Event.type[is.na(chrons.mag.syn$Corrected.event)] <- "T"
chrons.mag.syn$Corrected.event[is.na(chrons.mag.syn$Corrected.event)] <- "C1n"
chrons.mag.syn$Corrected.event[chrons.mag.syn$Corrected.event == "C5r.2r"] <- "C5r.2r:2r"

chrons.syn <- rbind(chrons.syn, chrons.new.syn, chrons.mag.syn)
chrons.syn <- unique(chrons.syn)

# match chrons.syn to chrons.full
all.chrons <- merge(all.chrons, chrons.syn, all = TRUE)
head(all.chrons)


# 5. Update zones ---------------------------------------------------------
# load in zones
all.zones <- read_excel("Data/BiostratSchemes.xlsx", sheet = "Zones", na = "NA", col_types = c(rep("text", 2), "numeric", "text", rep("numeric", 2), rep("text", 4)))
head(all.zones)

# add in columns for the ocean
all.zones$OceanS <- NA
all.zones$OceanS[grepl(" Atl", all.zones$EventS)] <- "Atl"
all.zones$OceanS[grepl(" IndoPac", all.zones$EventS)] <- "IndoPac"
all.zones$EventS <- gsub(" Atl| IndoPac", "", all.zones$EventS)

all.zones$OceanE <- NA
all.zones$OceanE[grepl(" Atl", all.zones$EventE)] <- "Atl"
all.zones$OceanE[grepl(" IndoPac", all.zones$EventE)] <- "IndoPac"
all.zones$EventE <- gsub(" Atl| IndoPac", "", all.zones$EventE)

# list of events in all.zones
all.events <- data.frame(orig.chron = c(all.zones$EventS, all.zones$EventE), Original.age = c(all.zones$Start, all.zones$End), scheme = c(all.zones$Scheme, all.zones$Scheme), Corrected.Type = c(all.zones$Type, all.zones$Type), Age.notes = c(all.zones$OceanS, all.zones$OceanE), stringsAsFactors = FALSE)
all.events <- unique(all.events)
head(all.events)
all.events$Corrected.Type[all.events$orig.chron == "Truncorotalia truncatulinoides (d:s)" & !is.na(all.events$orig.chron)] <- "Coiling"

# add missing events to list
all.events <- merge(all.chrons, all.events[, !(names(all.events) %in% c("Age.notes", "scheme"))], all = TRUE)
all.events <- unique(all.events)

all.events$Age.notes[grep("Trop|Temp", all.events$Age.notes)] <- NA

# use biostrat updates to get events on GTS 2012
all.zones$orig.st <- all.zones$Start
all.zones$orig.en <- all.zones$End
all.zones$Start <- all.zones$End <- NA

all.zones$Start <- all.events$Age[match(paste(all.zones$EventS, all.zones$orig.st, all.zones$OceanS), paste(all.events$orig.chron, all.events$Original.age, all.events$Age.notes))]
all.zones$End <- all.events$Age[match(paste(all.zones$EventE, all.zones$orig.en, all.zones$OceanE), paste(all.events$orig.chron, all.events$Original.age, all.events$Age.notes))]

# get GTS estimates for those without marker events (i.e. Ericson)
all.zones$Start[is.na(all.zones$Start)] <- unlist(sapply(all.zones$orig.st[is.na(all.zones$Start)], ts.conv))
all.zones$End[is.na(all.zones$End)] <- unlist(sapply(all.zones$orig.en[is.na(all.zones$End)], ts.conv))

all.zones$Mean <- rowMeans(all.zones[, c("Start", "End")])

# create a dataset to add these to all.chrons
zone.chrons <- tibble(Corrected.Type = rep(paste(all.zones$Type, "zone"), 2), Corrected.Event.type = rep(c("B", "T"), each = nrow(all.zones)), Corrected.event = rep(paste(all.zones$Zone, all.zones$Scheme), 2), Age.notes = c(all.zones$OceanS, all.zones$OceanE), Age = c(all.zones$Start, all.zones$End), Original.age = c(all.zones$orig.st, all.zones$orig.en))
zone.chrons$orig.chron <- zone.chrons$corr.chron <- paste(zone.chrons$Corrected.Event.type, zone.chrons$Corrected.event)
zone.chrons <- zone.chrons[zone.chrons$Corrected.Type != "Magneto zone", ]
all.chrons <- rbind(all.chrons, zone.chrons)

# update those of all.chrons where the value is NA
na.chrons <- which(is.na(all.chrons$corr.chron))
all.chrons$corr.chron[na.chrons] <- paste(all.chrons$Corrected.Event.type[na.chrons], all.chrons$Corrected.event[na.chrons])
all.chrons$Age[na.chrons] <- all.chrons$Age[-na.chrons][match(all.chrons$corr.chron[na.chrons], all.chrons$corr.chron[-na.chrons])]

rm(all.events, chrons.mag.syn, chrons.new.syn, chrons.syn, foram.events, na.chrons, magneto.ts, PFzones.ts, zone.chrons, ts.conv)
