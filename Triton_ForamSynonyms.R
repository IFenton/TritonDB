## function to determine names of BFD based on lookup tables provided by Tracy Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare <- function (forams.sample, micro = FALSE, st.age = NA, en.age = NA, age.check = FALSE, PF.ages = foram.ages) {
  # st.age - sample age(s), or oldest age
  # en.age - youngest age (where applicable)
  
  # load lookup tables
  library("openxlsx")
  path <- gsub("/[^/]*(Box|Dropbox).*/.*$", "", getwd())
  forams.lookup <- read.xlsx("Data/ForamSynonyms.xlsx", sheet = "foramslookup")
  forams.lookup <- forams.lookup[,1:3]
  
  lookup.table <- function(lookup) {
    # create a version of the lookup table which only has the species names
    species.nm <- lookup
    tmp <- species.nm[!duplicated(species.nm$AcceptedName),]
    tmp$Synonym <- tmp$AcceptedName
    species.nm <- rbind(species.nm, tmp)
    # remove the first name (genus)
    species.nm$Synonym <- gsub("^[^ ]* ", "", species.nm$Synonym)
    # remove duplicates from mispelled genus 
    species.nm <- species.nm[!duplicated(species.nm[,1:2]),]
    # where there are still duplicates, set these as "Unsure"
    spp.dup <- species.nm$Synonym[duplicated(species.nm$Synonym) & species.nm$AcceptedName != "Unsure"]
    species.nm$AcceptedName[species.nm$Synonym %in% unique(spp.dup)] <- "Unsure"
    
    # create versions of the lookup table / species list with abbreviated genus names
    abb.nm <- lookup
    abb.nm <- rbind(abb.nm, tmp)
    # remove the first name (genus)
    abb.nm$Synonym <- gsub("^(.)[^ ]* ", "\\1\\. ", abb.nm$Synonym)
    # remove duplicates from mispelled genus, where the accepted name is the same
    abb.nm <- abb.nm[!duplicated(abb.nm[,1:2]),]
    # where there are still duplicates, set these as "Unsure"
    abb.dup <- abb.nm$Synonym[duplicated(abb.nm$Synonym) & abb.nm$AcceptedName != "Unsure"]
    abb.nm$AcceptedName[abb.nm$Synonym %in% unique(abb.dup)] <- "Unsure"
    
    # merge these three dataframes
    return(rbind(lookup, species.nm, abb.nm))
  }
  
  full.lookup <- lookup.table(forams.lookup)
  
  # if not micro
  if (!micro) {
    full.lookup$AcceptedName[full.lookup$Micro == "Yes"] <- "Micro"
  }
  
  # create the compare function
  comp.func <- function(forams.sample, lookup) {
    forams.sample <- as.character(forams.sample) ## make sure it is not a factor
    species <- lookup$AcceptedName[match(forams.sample, lookup$AcceptedName)]
    if (is.na(species)) {
      species <- lookup$AcceptedName[match(forams.sample, lookup$Synonym)]
      if (is.na(species))
        species <- "unknown"
    }
    return(species)
  }
  
  forams.sample <- gsub("\\s", " ", forams.sample)
  species.list <- sapply(forams.sample, comp.func, full.lookup, USE.NAMES = FALSE) 
  
  # add in a check for "Unsure" based on ages of everything else
  if ((!is.na(st.age[1]) | age.check) & any(species.list == "Unsure")) {
    # calculate max / min ages
    if (is.na(st.age[1])) {
      # if these aren't already specified, then base it on the species list
      st.age <- max(PF.ages$Start[PF.ages$Species.name %in% species.list])
      en.age <- min(PF.ages$End[PF.ages$Species.name %in% species.list])
    }
    beg.age <- max(st.age, en.age, na.rm = TRUE)
    end.age <- min(st.age, en.age, na.rm = TRUE)
    age.spp <- PF.ages$Species.name[PF.ages$End <= beg.age & PF.ages$Start >= end.age]
    age.forams.lookup <- forams.lookup[forams.lookup$AcceptedName %in% age.spp,]
    
    # recalulate the lookup table, so that Unsure is only specified within the ages
    age.full.lookup <- lookup.table(age.forams.lookup)
    # if not micro
    if (!micro) {
      age.full.lookup$AcceptedName[age.full.lookup$Micro == "Yes"] <- "Micro"
    }
    
    species.list[species.list == "Unsure"] <- ifelse(sapply(forams.sample[species.list == "Unsure"], comp.func, age.full.lookup, USE.NAMES = FALSE) == "unknown", "Unsure", sapply(forams.sample[species.list == "Unsure"], comp.func, age.full.lookup, USE.NAMES = FALSE))
  }
  return(species.list)
}

