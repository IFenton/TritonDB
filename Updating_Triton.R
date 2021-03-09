# Updating Triton
library(readxl)
library(plyr)

# if you want to update any of the marker events or the geological timescale, then do so directly in the Ages.xlsx file 
# however it is easier to update the taxonomy in UpdatedTaxonomy.xlsx to ensure that all the relevant details are changed. 

# also you need to decide if you want the new ages to be taken as exact, or only if they extend the current range. 
# The default is only to change the ages if they extend the current range: Choose either "Extend" or "Exact"
age.rng <- "Extend"


# 1. Source the age models / PF data ------------------------------------------------
source("Code/Triton_1AgeModels.R")


# 2. Update the taxonomy / ages -------------------------------------------

# 2a. Load in the new dataset ----------------------------------------------
# the new synonymies
updated.syns <- read_xlsx("Data/UpdatedTaxonomy.xlsx", sheet = "New Synonyms")

# the list of newly recognised macroperforate species
updated.macro <- read_xlsx("Data/UpdatedTaxonomy.xlsx", sheet = "New Macroperforates")

# the list of newly recognised microperforate species
updated.micro <- read_xlsx("Data/UpdatedTaxonomy.xlsx", sheet = "New Microperforates")

# the list of new speciation / extinction ages
updated.ages <- read_xlsx("Data/UpdatedTaxonomy.xlsx", sheet = "New Ages")

# 2b. Update the synonym list ---------------------------------------------
# update the accepted names
foram.syns$AcceptedName <- mapvalues(foram.syns$AcceptedName, updated.syns$Synonym[updated.syns$Synonym %in% foram.syns$AcceptedName], updated.syns$AcceptedName[updated.syns$Synonym %in% foram.syns$AcceptedName])

# update the synonyms 
foram.syns$AcceptedName[match(updated.syns$Synonym, foram.syns$Synonym)[!is.na(match(updated.syns$Synonym, foram.syns$Synonym))]] <- updated.syns$AcceptedName[!is.na(match(updated.syns$Synonym, foram.syns$Synonym))]

# add the new synonyms
foram.syns <- rbind(foram.syns, updated.syns)
foram.syns <- foram.syns[!duplicated(foram.syns[1:3]), ]

# add any new species
if (nrow(updated.macro) > 0) {
  tmp.macro <- data.frame(Synonym = updated.macro$Species, AcceptedName = updated.macro$Species, Micro = "No", Source = updated.macro$Source, Date = format(Sys.Date(),  "%Y"), Details = NA)
  foram.syns <- rbind(foram.syns, tmp.macro)
  rm(tmp.macro)
}
  
if (nrow(updated.micro) > 0){
  tmp.micro <- data.frame(Synonym = updated.micro$Species, AcceptedName = updated.micro$Species, Micro = "No", Source = updated.micro$Source, Date = format(Sys.Date(),  "%Y"), Details = NA)
  foram.syns <- rbind(foram.syns, tmp.micro)
  rm(tmp.micro)
}

forams.macro <- grep(" ", sort(unique(foram.syns$AcceptedName[foram.syns$Micro == "No"])), value = TRUE)
forams.micro <- grep(" ", sort(unique(foram.syns$AcceptedName[foram.syns$Micro == "Yes"])), value = TRUE)

# 2c. Update the ages -----------------------------------------------------
names(updated.ages)[names(updated.ages) == "Timescale"] <- "orig.ts"
# update to GTS 2020 if necessary
if (any(updated.ages$orig.ts == "CK95")) {
  updated.ages$Start[updated.ages$orig.ts == "CK95"] <- unlist(sapply(updated.ages$Start[updated.ages$orig.ts == "CK95"], ts.conv, scheme = "Forams"))
  updated.ages$End[updated.ages$orig.ts == "CK95"] <- unlist(sapply(updated.ages$End[updated.ages$orig.ts == "CK95"], ts.conv, scheme = "Forams"))
}
  
if (any(updated.ages$orig.ts == "GTS2012")) {
  foram.ages$Start[foram.ages$orig.ts == "GTS2012"] <- unlist(sapply(foram.ages$orig.st[foram.ages$orig.ts == "GTS2012"], ts.conv, scheme = "Forams", orig.ts = "GTS2012"))
  foram.ages$End[foram.ages$orig.ts == "GTS2012"] <- unlist(sapply(foram.ages$orig.en[foram.ages$orig.ts == "GTS2012"], ts.conv, scheme = "Forams", orig.ts = "GTS2012"))
}

# update the ages species list
foram.ages$Species.name <- compare(foram.ages$Species.name, micro = TRUE)

# remove any that are no longer considered species level
foram.ages <- foram.ages[grepl(" ", foram.ages$Species.name), ]

# merge ages for species that are now synonymised
for (i in unique(foram.ages$Species.name[duplicated(foram.ages$Species.name)])) {
  foram.ages$Start[foram.ages$Species.name == i] <- max(foram.ages$Start[foram.ages$Species.name == i])
  foram.ages$End[foram.ages$Species.name == i] <- min(foram.ages$End[foram.ages$Species.name == i])
}
foram.ages <- foram.ages[!duplicated(foram.ages$Species.name), ]

# add new species
if (suppressWarnings(any(sort(foram.ages$Species.name) != sort(c(forams.macro, forams.micro))))) {
  foram.ages <- merge(foram.ages, data.frame(Species.name = c(forams.macro, forams.micro)), all = TRUE)
}

# add macro / micro to this file
foram.ages$`Macro/micro`[match(forams.macro, foram.ages$Species.name)] <- "Macroperforate"
foram.ages$`Macro/micro`[match(forams.micro, foram.ages$Species.name)] <- "Microperforate"

# update the names for the ages file
if (any(!grepl(" ", compare(updated.ages$Species.name, micro = TRUE)))) {
  print(updated.ages$Species.name[!grepl(" ", compare(updated.ages$Species.name, micro = TRUE))])
  stop("Update the synonymy list in 'UpdatedTaxonomy.xlsx' with these unrecognised names, then re-run the code")
}

updated.ages$Species.name <- compare(updated.ages$Species.name, micro = TRUE)

# replace the ages
if (age.rng == "Extend") {
  foram.ages$Start[match(updated.ages$Species.name, foram.ages$Species.name)] <- apply(cbind(updated.ages$Start, foram.ages$Start[match(updated.ages$Species.name, foram.ages$Species.name)]), 1, max, na.rm = TRUE)
  foram.ages$End[match(updated.ages$Species.name, foram.ages$Species.name)] <- apply(cbind(updated.ages$End, foram.ages$End[match(updated.ages$Species.name, foram.ages$Species.name)]), 1, min, na.rm = TRUE)
} else {
  foram.ages$Start[match(updated.ages$Species.name, foram.ages$Species.name)] <- updated.ages$Start
  foram.ages$End[match(updated.ages$Species.name, foram.ages$Species.name)] <- updated.ages$End
}

# run the rest of the Triton database
source("Code/Triton_2Neptune_plus.R")
source("Code/Triton_3Pangaea.R")
source("Code/Triton_4MoveDB.R")
source("Code/Triton_5IODPextra.R")
source("Code/Triton_6CombinedData.R")
source("Code/Triton_7Figures.R")


