# MoveDB data
# Adding the data from Lloyd et al (2011) palaeobiology

# Previous file: Triton_3Pangaea.R
# Next file: Triton_5IODPextra.R

# libraries ---------------------------------------------------------------
library(RODBC)
library(openxlsx) 

# if re-running skip to section 3c
load("Outputs/MoveDBworking.RData")
load("Outputs/MoveDB.RData")

# # 1. Load in the data -----------------------------------------------------
# # access the database
# channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=Data/MoveDB.accdb")
# 
# moveDB.tables <- sqlTables(channel)$TABLE_NAME[sqlTables(channel)$TABLE_TYPE == "TABLE"]
# 
# for (i in moveDB.tables)
#   assign(i, sqlQuery(channel, paste(paste("Select * from ", i))))
# 
# odbcClose(channel)
# rm(channel, `Name AutoCorrect Save Failures`, i)
# 
# # save(list = eval(parse(text = "moveDB.tables")), file = "Data/moveDB_full.RData")
# load("Data/moveDB_full.RData")
# lbloc_country <- read.xlsx("Data/LBloc_Country.xlsx")
# rm(moveDB.tables)
# 
# # 2. Reformat the data ----------------------------------------------------
# # 2a. General -----------------------------------------------------------------
# # authors (inc. authors_type)
# authors$Name <- as.character(authors$Name)
# authors$Type <- as.character(authors_type$Type[match(authors$Type, authors_type$ID)])
# 
# 
# # reference (inc. references_type)
# reference$Shortref <- as.character(reference$Shortref)
# reference$BIBTeXkey <- as.character(reference$BIBTeXkey)
# reference$doi <- as.character(reference$doi)
# reference$url <- as.character(reference$url)
# reference$Type <- as.character(references_type$Type[match(reference$Type, references_type$ID)])
# 
# 
# # 2b. Time periods ----------------------------------------------------------
# # chrono_era, chrono_epoch, chrono_markers, chrono_mtyp, chrono_period, chrono_subepoch, chrono_stage, chrono_zgrp, chrono_zone, chrono_zreg
# # geological timescale
# chrono_era$Name <- as.character(chrono_era$Name)
# chrono_era$RefID <- reference$Shortref[match(chrono_era$RefID, reference$ID)]
# 
# chrono_period$Name <- as.character(chrono_period$Name)
# chrono_period$EraID <- chrono_era$Name[match(chrono_period$EraID, chrono_era$ID)]
# chrono_period$RefID <- reference$Shortref[match(chrono_period$RefID, reference$ID)]
# 
# chrono_epoch$Name <- as.character(chrono_epoch$Name)
# chrono_epoch$PeriodID <- chrono_period$Name[match(chrono_epoch$PeriodID, chrono_period$ID)]
# chrono_epoch$RefID <- reference$Shortref[match(chrono_epoch$RefID, reference$ID)]
# 
# chrono_subepoch$Name <- as.character(chrono_subepoch$Name)
# chrono_subepoch$EpochID <- chrono_epoch$Name[match(chrono_subepoch$EpochID, chrono_epoch$ID)]
# chrono_subepoch$RefID <- reference$Shortref[match(chrono_subepoch$RefID, reference$ID)]
# 
# chrono_stage$Name <- as.character(chrono_stage$Name)
# chrono_stage$EpochID <- chrono_epoch$Name[match(chrono_stage$EpochID, chrono_epoch$ID)]
# chrono_stage$SubepochID <- chrono_subepoch$Name[match(chrono_stage$SubepochID, chrono_subepoch$ID)]
# chrono_stage$RefID <- reference$Shortref[match(chrono_stage$RefID, reference$ID)]
# 
# # create a dataset of the different time periods
# chrono <- chrono_era
# names(chrono)[names(chrono) == "Name"] <- "Era"
# chrono$level <- "Era"
# chrono$Period <- NA
# 
# tmp <- chrono_period
# names(tmp)[names(tmp) == "Name"] <- "Period"
# names(tmp)[names(tmp) == "EraID"] <- "Era"
# tmp$level <- "Period"
# chrono <- rbind(chrono, tmp)
# 
# tmp <- chrono_epoch
# names(tmp)[names(tmp) == "Name"] <- "Epoch"
# names(tmp)[names(tmp) == "PeriodID"] <- "Period"
# tmp$level <- "Epoch"
# tmp$Era <- chrono$Era[match(tmp$Period, chrono$Period)]
# chrono$Epoch <- NA
# chrono <- rbind(chrono, tmp)
# 
# tmp <- chrono_subepoch
# names(tmp)[names(tmp) == "Name"] <- "Subepoch"
# names(tmp)[names(tmp) == "EpochID"] <- "Epoch"
# tmp$level <- "Subepoch"
# tmp$Period <- chrono$Period[match(tmp$Epoch, chrono$Epoch)]
# tmp$Era <- chrono$Era[match(tmp$Period, chrono$Period)]
# chrono$Subepoch <- NA
# chrono <- rbind(chrono, tmp)
# 
# tmp <- chrono_stage
# names(tmp)[names(tmp) == "Name"] <- "Stage"
# names(tmp)[names(tmp) == "EpochID"] <- "Epoch"
# names(tmp)[names(tmp) == "SubepochID"] <- "Subepoch"
# tmp$level <- "Stage"
# tmp$Period <- chrono$Period[match(tmp$Epoch, chrono$Epoch)]
# tmp$Era <- chrono$Era[match(tmp$Period, chrono$Period)]
# chrono$Stage <- NA
# chrono <- rbind(chrono, tmp)
# 
# chrono$interval <- NA
# chrono$interval[chrono$level == "Era"] <- as.character(chrono$Era[chrono$level == "Era"])
# chrono$interval[chrono$level == "Period"] <- as.character(chrono$Period[chrono$level == "Period"])
# chrono$interval[chrono$level == "Epoch"] <- as.character(chrono$Epoch[chrono$level == "Epoch"])
# chrono$interval[chrono$level == "Subepoch"] <- as.character(chrono$Subepoch[chrono$level == "Subepoch"])
# chrono$interval[chrono$level == "Stage"] <- as.character(chrono$Stage[chrono$level == "Stage"])
# head(chrono)
# 
# # create a dataset of zones
# chrono_markers$Name <- as.character(chrono_markers$Name)
# chrono_markers$Type <- chrono_mtyp$Name[match(chrono_markers$Type, chrono_mtyp$ID)]
# chrono_markers$Reference <- reference$Shortref[match(chrono_markers$Reference, reference$ID)]
# 
# chrono_zone$Name <- as.character(chrono_zone$Name)
# chrono_zone$Group <- as.character(chrono_zgrp$Group[match(chrono_zone$Group, chrono_zgrp$ID)])
# chrono_zone$Region <- as.character(chrono_zreg$Region[match(chrono_zone$Region, chrono_zreg$ID)])
# chrono_zone$Reference <- reference$Shortref[match(chrono_zone$Reference, reference$ID)]
# 
# zones <- chrono_zone
# zones$Top <- chrono_markers$Date[match(zones$Top, chrono_markers$ID)]
# names(zones)[names(zones) == "Name"] <- "Zone"
# zones <- merge(zones, chrono_markers, by.x = "Bottom", by.y = "ID")
# names(zones)[names(zones) == "Bottom"] <- "MarkerID"
# names(zones)[names(zones) == "ID"] <- "ZoneID"
# zones <- zones[, !grepl("Reference", names(zones))]
# names(zones)[names(zones) == "Name"] <- "Marker"
# names(zones)[names(zones) == "Date"] <- "Bottom"
# zones <- zones[, c(1:5, 9, 6:8)]
# head(zones)
# write.csv(zones, file = "Outputs/MoveDBzones.csv", row.names = FALSE)
# 
# 
# # 2c. Taxonomy -------------------------------------------------------------------
# # taxon_phylum, taxon_class, taxon_order, taxon_family, taxon_genus, taxon_species, taxon_status, taxon_foramabund, taxon_forampres, taxon_nannoabund, taxon_nannopres, taxon_type , taxon_others, taxon_synonymy
# taxon_phylum$Name <- as.character(taxon_phylum$Name)
# 
# taxon_class$Name <- as.character(taxon_class$Name)
# 
# taxon_order$Name <- as.character(taxon_order$Name)
# taxon_order$RefID <- reference$Shortref[match(taxon_order$RefID, reference$ID)]
# 
# taxon_family$Name <- as.character(taxon_family$Name)
# taxon_family$RefID <- reference$Shortref[match(taxon_family$RefID, reference$ID)]
# 
# taxon_genus$Name <- as.character(taxon_genus$Name)
# taxon_genus$Type <- as.character(taxon_type$Name[match(taxon_genus$Type, taxon_type$ID)])
# taxon_genus$RefID <- reference$Shortref[match(taxon_genus$RefID, reference$ID)]
# 
# taxon_species$Species <- as.character(taxon_species$Species)
# taxon_species$NEPTUNE_taxon_id <- as.character(taxon_species$NEPTUNE_taxon_id)
# taxon_species$Status <- taxon_status$Name[match(taxon_species$Status, taxon_status$ID)]
# taxon_species$Reference <- reference$Shortref[match(taxon_species$Reference, reference$ID)]
# 
# taxon_others$Name <- as.character(taxon_others$Name)
# 
# # abundance units
# taxon_foramabund$Name <- as.character(taxon_foramabund$Name)
# taxon_foramabund$Description <- as.character(taxon_foramabund$Description)
# taxon_foramabund$Reference <- reference$Shortref[match(taxon_foramabund$Reference, reference$ID)]
# 
# # preservation
# taxon_forampres$Name <- as.character(taxon_forampres$Name)
# taxon_forampres$Description <- as.character(taxon_forampres$Description)
# taxon_forampres$Reference <- reference$Shortref[match(taxon_forampres$Reference, reference$ID)]
# 
# taxon_nannoabund$Name <- as.character(taxon_nannoabund$Name)
# taxon_nannoabund$Description <- as.character(taxon_nannoabund$Description)
# taxon_nannoabund$Reference <- reference$Shortref[match(taxon_nannoabund$Reference, reference$ID)]
# 
# taxon_nannopres$Name <- as.character(taxon_nannopres$Name)
# taxon_nannopres$Description <- as.character(taxon_nannopres$Description)
# taxon_nannopres$Reference <- reference$Shortref[match(taxon_nannopres$Reference, reference$ID)]
# 
# taxon_spabund$Name <- as.character(taxon_spabund$Name)
# taxon_spabund$Description <- as.character(taxon_spabund$Description)
# taxon_spabund$Reference <- reference$Shortref[match(taxon_spabund$Reference, reference$ID)]
# 
# # dataset of all the taxa
# taxon <- merge(taxon_species, taxon_genus, by.x = "Genus", by.y = "ID")
# names(taxon)[names(taxon) == "ID"] <- "SpeciesID"
# names(taxon)[names(taxon) == "Genus"] <- "GenusID"
# names(taxon)[names(taxon) == "Name"] <- "Genus"
# names(taxon)[names(taxon) == "Reference"] <- "SpReference"
# names(taxon)[names(taxon) == "RefID"] <- "GenReference"
# head(taxon)
# 
# taxon <- merge(taxon, taxon_family, by.x = "FamilyID", by.y = "ID")
# names(taxon)[names(taxon) == "Name"] <- "Family"
# names(taxon)[names(taxon) == "RefID"] <- "FamReference"
# head(taxon)
# 
# taxon <- merge(taxon, taxon_order, by.x = "OrderID", by.y = "ID")
# names(taxon)[names(taxon) == "Name"] <- "Order"
# names(taxon)[names(taxon) == "RefID"] <- "OrdReference"
# head(taxon)
# 
# taxon <- merge(taxon, taxon_class, by.x = "ClassID", by.y = "ID")
# names(taxon)[names(taxon) == "Name"] <- "Class"
# head(taxon)
# 
# taxon <- merge(taxon, taxon_phylum, by.x = "PhylumID", by.y = "ID")
# names(taxon)[names(taxon) == "Name"] <- "Phylum"
# head(taxon)
# 
# taxon$orig.binomial <- paste(taxon$Genus, taxon$Species)
# head(taxon)
# 
# # dataset of synonyms
# synonyms <- taxon_synonymy
# synonyms$Junior_synonym <- taxon$orig.binomial[match(synonyms$Junior_synonym, taxon$SpeciesID)]
# synonyms$Senior_synonym <- taxon$orig.binomial[match(synonyms$Senior_synonym, taxon$SpeciesID)]
# synonyms$Type <- taxon$Type[match(synonyms$Senior_synonym, taxon$orig.binomial)]
# head(synonyms)
# write.csv(synonyms, file = "Outputs/MoveDBsynonyms.csv", row.names = FALSE)
# 
# taxon$binomial <- synonyms$Senior_synonym[match(taxon$orig.binomial, synonyms$Junior_synonym)]
# taxon$binomial[is.na(taxon$binomial)] <- taxon$orig.binomial[is.na(taxon$binomial)]
# head(taxon)
# 
# uniq.pfsp <- taxon[taxon$Type %in% c("Macroperforate", "Microperforate", "None"), c("binomial", "Type")]
# uniq.pfsp <- unique(uniq.pfsp)
# write.csv(uniq.pfsp, file = "Outputs/MoveDBpfsp.csv", row.names = FALSE)
# 
# 
# # 2d. dsloc (deep sea locations) ---------------------------------------------
# # dsloc_authors, dsloc_unitref, dsloc_unitsp, dsloc_unittax, dsloc_prog, dsloc_ocean, dsloc_leg, dsloc_site, dsloc_hole, dsloc_unitlith, dsloc_unit, missing
# dsloc_authors$Author <- authors$Name[match(dsloc_authors$Author, authors$ID)]
# 
# dsloc_ocean$Ocean <- as.character(dsloc_ocean$Ocean)
# 
# dsloc_site$OceanID <- dsloc_ocean$Ocean[match(dsloc_site$OceanID, dsloc_ocean$ID)]
# names(dsloc_site)[names(dsloc_site) == "OceanID"] <- "Ocean"
# 
# dsloc_leg$Number <- as.character(dsloc_leg$Number)
# dsloc_leg$ProgramID <- as.character(dsloc_prog$Name[match(dsloc_leg$ProgramID, dsloc_prog$ID)])
# 
# dsloc_hole$Hole <- as.character(dsloc_hole$Hole)
# 
# # dslocs - deep sea sample units
# dslocs <- merge(dsloc_hole, dsloc_leg, by.x = "LegID", by.y = "ID")
# names(dslocs)[names(dslocs) == "ID"] <- "HoleID"
# names(dslocs)[names(dslocs) == "Number"] <- "Leg"
# 
# dslocs <- merge(dslocs, dsloc_site, by.x = "SiteID", by.y = "ID")
# names(dslocs)[names(dslocs) == "Number"] <- "Site"
# 
# dslocs <- dslocs[, c(1:3, 11, 14, 4:10, 12:13, 15)]
# 
# dsloc_unit$Notes <- as.character(dsloc_unit$Notes)
# dsloc_unit$Oldest_nanno <- chrono_zone$Name[match(dsloc_unit$Oldest_nanno, chrono_zone$ID)]
# dsloc_unit$Youngest_nanno <- chrono_zone$Name[match(dsloc_unit$Youngest_nanno, chrono_zone$ID)]
# dsloc_unit$Oldest_foram <- chrono_zone$Name[match(dsloc_unit$Oldest_foram, chrono_zone$ID)]
# dsloc_unit$Youngest_foram <- chrono_zone$Name[match(dsloc_unit$Youngest_foram, chrono_zone$ID)]
# dsloc_unit$Foram_abundance <- taxon_foramabund$Name[match(dsloc_unit$Foram_abundance, taxon_foramabund$ID)]
# dsloc_unit$Foram_preservation <- taxon_forampres$Name[match(dsloc_unit$Foram_preservation, taxon_forampres$ID)]
# dsloc_unit$Nanno_abundance <- taxon_nannoabund$Name[match(dsloc_unit$Nanno_abundance, taxon_nannoabund$ID)]
# dsloc_unit$Nanno_preservation <- taxon_nannopres$Name[match(dsloc_unit$Nanno_preservation, taxon_nannopres$ID)]
# dsloc_unit$Missing <- missing$Name[match(dsloc_unit$Missing, missing$ID)]
# dslocs <- merge(dslocs, dsloc_unit, by.x = "HoleID", by.y = "Hole")
# names(dslocs)[names(dslocs) == "ID"] <- "UnitID"
# 
# dsloc_unittax$Taxon <- taxon_others$Name[match(dsloc_unittax$Taxon, taxon_others$ID)]
# dsloc_unittax <- dsloc_unittax[order(dsloc_unittax$Taxon),]
# tmp <- lapply(tapply(dsloc_unittax$Taxon, dsloc_unittax$Unit, as.character), paste, collapse = "; ")
# tmp <- data.frame(UnitID = names(tmp), OtherTaxon = unlist(tmp), stringsAsFactors = FALSE)
# dslocs <- merge(dslocs, tmp, all.x = TRUE)
# 
# dsloc_unitref$Reference <- reference$Shortref[match(dsloc_unitref$Reference, reference$ID)]
# tmp <- tapply(dsloc_unitref$Reference, dsloc_unitref$Unit, paste, collapse = "; ")
# dslocs$Reference <- tmp[match(dslocs$UnitID, names(tmp))]
# 
# head(dslocs)
# 
# # dsloc_sp - species level info data
# dsloc_unitsp$orig.species <- taxon$orig.binomial[match(dsloc_unitsp$Species, taxon$SpeciesID)]
# names(dsloc_unitsp)[names(dsloc_unitsp) == "Species"] <- "SpeciesID"
# dsloc_unitsp$Species <- taxon$binomial[match(dsloc_unitsp$Species, taxon$SpeciesID)]
# dsloc_sp <- merge(dsloc_unitsp, taxon_spabund, by.x = "Abundance", by.y = "ID")
# names(dsloc_sp)[names(dsloc_sp) == "Abundance"] <- "AbundanceID"
# names(dsloc_sp)[names(dsloc_sp) == "ID"] <- "UnitSpID"
# names(dsloc_sp)[names(dsloc_sp) == "Name"] <- "Abundance"
# names(dsloc_sp)[names(dsloc_sp) == "Reference"] <- "SpReference"
# names(dsloc_sp)[names(dsloc_sp) == "Description"] <- "AbundanceDesc"
# 
# dsloc_sp <- merge(dsloc_sp, dslocs, by.x = "Unit", by.y = "UnitID") 
# names(dsloc_sp)[names(dsloc_sp) == "Unit"] <- "UnitID"
# 
# dsloc_sp$SpType <- taxon$Type[match(dsloc_sp$Species, taxon$binomial)]
# 
# head(dsloc_sp)
# table(dsloc_sp$SpType)
# 
# 
# # 2e. lbloc (land based locations) ------------------------------------------
# # lbloc_cont, lbloc_country, lbloc_region, lbloc_section, lbloc_styp, lbloc_unit, lbloc_unitlith, lbloc_unitref, lbloc_unitsp, lbloc_unittax, litho_types
# lbloc_cont$Name <- as.character(lbloc_cont$Name)
# 
# lbloc_country$Continent <- lbloc_cont$Name[match(lbloc_country$Continent, lbloc_cont$ID)]
# 
# lbloc_region$Name <- as.character(lbloc_region$Name)
# 
# # lblocs - land based sample units
# lblocs <- merge(lbloc_region, lbloc_country, by.x = "Country", by.y = "ID")
# names(lblocs)[names(lblocs) == "ID"] <- "RegionID"
# names(lblocs)[names(lblocs) == "Country"] <- "CountryID"
# names(lblocs)[names(lblocs) == "Name.x"] <- "Region"
# names(lblocs)[names(lblocs) == "Name.y"] <- "Country"
# 
# lbloc_section$Name <- as.character(lbloc_section$Name)
# lbloc_section$Type <- lbloc_styp$Name[match(lbloc_section$Type, lbloc_styp$ID)]
# lbloc_section$Reference <- reference$Shortref[match(lbloc_section$Reference, reference$ID)]
# lblocs <- merge(lblocs, lbloc_section, by.x = "RegionID", by.y = "Region")
# names(lblocs)[names(lblocs) == "ID"] <- "SectionID"
# names(lblocs)[names(lblocs) == "Name"] <- "Section"
# lblocs <- lblocs[, names(lblocs) != "Reference"]
# 
# lbloc_unit$Notes <- as.character(lbloc_unit$Notes)
# lbloc_unit$Oldest_nanno <- chrono_zone$Name[match(lbloc_unit$Oldest_nanno, chrono_zone$ID)]
# lbloc_unit$Youngest_nanno <- chrono_zone$Name[match(lbloc_unit$Youngest_nanno, chrono_zone$ID)]
# lbloc_unit$Oldest_foram <- chrono_zone$Name[match(lbloc_unit$Oldest_foram, chrono_zone$ID)]
# lbloc_unit$Youngest_foram <- chrono_zone$Name[match(lbloc_unit$Youngest_foram, chrono_zone$ID)]
# lbloc_unit$Foram_abundance <- taxon_foramabund$Name[match(lbloc_unit$Foram_abundance, taxon_foramabund$ID)]
# lbloc_unit$Foram_preservation <- taxon_forampres$Name[match(lbloc_unit$Foram_preservation, taxon_forampres$ID)]
# lbloc_unit$Nanno_abundance <- taxon_nannoabund$Name[match(lbloc_unit$Nanno_abundance, taxon_nannoabund$ID)]
# lbloc_unit$Nanno_preservation <- taxon_nannopres$Name[match(lbloc_unit$Nanno_preservation, taxon_nannopres$ID)]
# lbloc_unit$Missing <- missing$Name[match(lbloc_unit$Missing, missing$ID)]
# lblocs <- merge(lblocs, lbloc_unit, by.x = "SectionID", by.y = "Section")
# names(lblocs)[names(lblocs) == "ID"] <- "UnitID"
# names(lblocs)[names(lblocs) == "Name"] <- "Section"
# 
# litho_types$Name <- as.character(litho_types$Name)
# 
# lbloc_unitlith$Lith <- litho_types$Name[match(lbloc_unitlith$Lith, litho_types$ID)]
# lbloc_unitlith$LithProp <- paste(lbloc_unitlith$Lith, lbloc_unitlith$Proportion, sep = ": ")
# tmp <- lapply(tapply(lbloc_unitlith$LithProp, lbloc_unitlith$Unit, c), paste, collapse = "; ")
# tmp <- data.frame(UnitID = names(tmp), LithProp = unlist(tmp), stringsAsFactors = FALSE)
# lblocs <- merge(lblocs, tmp, all.x = TRUE)
# 
# lbloc_unitref$Reference <- reference$Shortref[match(lbloc_unitref$Reference, reference$ID)]
# lblocs <- merge(lblocs, lbloc_unitref, by.x = "UnitID", by.y = "Unit")
# names(lblocs)[names(lblocs) == "ID"] <- "ReferenceID"
# 
# lbloc_unittax$Taxon <- taxon_others$Name[match(lbloc_unittax$Taxon, taxon_others$ID)]
# lbloc_unittax <- lbloc_unittax[order(lbloc_unittax$Taxon),]
# tmp <- lapply(tapply(lbloc_unittax$Taxon, lbloc_unittax$Unit, as.character), paste, collapse = "; ")
# tmp <- data.frame(UnitID = names(tmp), OtherTaxon = unlist(tmp), stringsAsFactors = FALSE)
# lblocs <- merge(lblocs, tmp, all.x = TRUE)
# 
# head(lblocs)
# 
# # lbloc_sp - species level info data
# lbloc_unitsp$orig.species <- taxon$orig.binomial[match(lbloc_unitsp$Species, taxon$SpeciesID)]
# names(lbloc_unitsp)[names(lbloc_unitsp) == "Species"] <- "SpeciesID"
# lbloc_unitsp$Species <- taxon$binomial[match(lbloc_unitsp$SpeciesID, taxon$SpeciesID)]
# lbloc_sp <- merge(lbloc_unitsp, taxon_spabund, by.x = "Abundance", by.y = "ID")
# names(lbloc_sp)[names(lbloc_sp) == "Abundance"] <- "AbundanceID"
# names(lbloc_sp)[names(lbloc_sp) == "ID"] <- "UnitSpID"
# names(lbloc_sp)[names(lbloc_sp) == "Name"] <- "Abundance"
# names(lbloc_sp)[names(lbloc_sp) == "Reference"] <- "SpReference"
# names(lbloc_sp)[names(lbloc_sp) == "Description"] <- "AbundanceDesc"
# 
# 
# lbloc_sp <- merge(lbloc_sp, lblocs, by.x = "Unit", by.y = "UnitID") # n.b. this results in the loss of 104 rows of data (some with abundance unknown) that have no unit ID
# names(lbloc_sp)[names(lbloc_sp) == "Unit"] <- "UnitID"
# 
# lbloc_sp$SpType <- taxon$Type[match(lbloc_sp$Species, taxon$binomial)]
# 
# head(lbloc_sp)
# table(lbloc_sp$SpType)
# 
# 
# 
# # 3. Converting to general structure --------------------------------------
# 
# # 3a. Restructuring -------------------------------------------------------
# # lbloc
# # put the zones into one column
# lbloc_sp$NannoZone <- with(lbloc_sp, ifelse(Oldest_nanno == Youngest_nanno, as.character(Oldest_nanno), paste(Oldest_nanno, Youngest_nanno, sep = "_")))
# lbloc_sp$ForamZone <- with(lbloc_sp, ifelse(Oldest_foram == Youngest_foram, as.character(Oldest_foram), paste(Oldest_foram, Youngest_foram, sep = "_")))
# lbloc_sp$NannoZone[lbloc_sp$NannoZone == "NO NANNO ZONE"] <- NA
# lbloc_sp$ForamZone[lbloc_sp$ForamZone == "NO FORAM ZONE"] <- NA
# #give priority to nanno zones over foram zones
# lbloc_sp$comb.zone <- lbloc_sp$NannoZone
# lbloc_sp$comb.zone[is.na(lbloc_sp$comb.zone)] <- lbloc_sp$ForamZone[is.na(lbloc_sp$comb.zone)]
# head(lbloc_sp)
# 
# # remove nannofossils
# lbloc_sp_pf <- lbloc_sp[lbloc_sp$SpType %in% c("Macroperforate", "Microperforate", "None"),]
# 
# lbloc_sp_pf$Core <- paste(lbloc_sp_pf$Continent, lbloc_sp_pf$Country, lbloc_sp_pf$Region, lbloc_sp_pf$Section, sep = "-")
# 
# # dsloc
# # convert zones into one column
# dsloc_sp$NannoZone <- with(dsloc_sp, ifelse(Oldest_nanno == Youngest_nanno, as.character(Oldest_nanno), paste(Oldest_nanno, Youngest_nanno, sep = "_")))
# dsloc_sp$ForamZone <- with(dsloc_sp, ifelse(Oldest_foram == Youngest_foram, as.character(Oldest_foram), paste(Oldest_foram, Youngest_foram, sep = "_")))
# dsloc_sp$NannoZone[dsloc_sp$NannoZone == "NO NANNO ZONE"] <- NA
# dsloc_sp$ForamZone[dsloc_sp$ForamZone == "NO FORAM ZONE"] <- NA
# dsloc_sp$comb.zone <- dsloc_sp$NannoZone
# dsloc_sp$comb.zone[is.na(dsloc_sp$comb.zone)] <- dsloc_sp$ForamZone[is.na(dsloc_sp$comb.zone)]
# head(dsloc_sp)
# 
# #only forams
# dsloc_sp_pf <- dsloc_sp[dsloc_sp$SpType %in% c("Macroperforate", "Microperforate", "None"),]
# 
# # 3b. Sorting abundance ---------------------------------------------------
# # create a summary of abundances
# abun <- unique(rbind(lbloc_sp_pf[, c("Abundance", "AbundanceDesc")], dsloc_sp_pf[, c("Abundance", "AbundanceDesc")]))
# abun <- abun[order(abun$Abundance),]
# abun$type <- c(rep("Custom", 6), rep("DSDP", 9), rep("ODP", 12), NA)
# abun$num <- c(4, 5, 3, 6, 1, 2, 9, 8, 6, 2, 7, 5, 3, 4, 1, 11, 10, 8, 9, 2, 5, 7, 6, 4, 3, 4, 1, 1)
# abun$bin <- c("C", "A", "F", "D", "P", "R", "A", "C", "F", "P", "F/C", "R", "T", "VR", "P", "A", "C", "F", "F/C", "P", "R", "F", "R/F", "VR", "T", "VR", "P", "P")
# 
# #Custom: -,P,R,F,C,D,Fl
# #DSDP: -,P,T,VR,R,F,FC,C,A,
# #ODP: -,P,T,VR,R,R/F,F,F/C,C,A,
# #Combined: -,P,T,VR,R,R/F,F,F/C,C,A,D
# 
# 
# lbloc_sp_pf$bin.abun <- abun$bin[match(lbloc_sp_pf$Abundance, abun$Abundance)]
# dsloc_sp_pf$bin.abun <- abun$bin[match(dsloc_sp_pf$Abundance, abun$Abundance)]
# 
#               
# 3c. PNstructure ---------------------------------------------------------
movedb.model.type <- read_xlsx("Data/age_models.xlsx", sheet = "moveDB")

#View(lbloc_sp_pf)
lb.movedb <- PNstructure(lbloc_sp_pf, "IF", "Lloydetal2012", ODP = "N", pal.lat.full = FALSE, choices = lb.movedb$choices, db.ID = "LBMoveDB")

#View(dsloc_sp_pf)
ds.movedb <- PNstructure(dsloc_sp_pf, "IF", "Lloydetal2012", ODP = "Y", pal.lat.full = FALSE, choices = ds.movedb$choices, db.ID = "DSMoveDB")

for (j in unique(ds.movedb$data$holeID)) {
  if (any(grepl(paste("data.age", j, sep = "_"), names(ds.movedb$choices)))) {
    # add age model plots
    tmp.zon <- ds.movedb$data[ds.movedb$data$holeID == j,]
    tmp.zon <- tmp.zon[!duplicated(tmp.zon$sampleID), ]
    tmp.zon <- tmp.zon[, grep("core|sample|age|zone", names(tmp.zon))]
    tmp.zon <- tmp.zon[order(tmp.zon$sample.depth), ]
    
    if (any(!is.na(tmp.zon$age))) {
      png(paste("Figures/Age models/DSMoveDB", "_", j, ".png", sep = ""), 600, 600)
      plot(tmp.zon$sample.depth ~ tmp.zon$zon.age, pch = 16, xlim = c(min(tmp.zon$age.en, na.rm = TRUE), max(tmp.zon$age.st, na.rm = TRUE)), main = j, ylim = c(min(tmp.zon$sample.depth[!is.na(tmp.zon$zon.age)], na.rm = TRUE), max(tmp.zon$sample.depth[!is.na(tmp.zon$zon.age)], na.rm = TRUE)))
      for (k in 1:nrow(tmp.zon)) {
        lines(c(tmp.zon$age.st[k], tmp.zon$age.en[k]), c(tmp.zon$sample.depth[k], tmp.zon$sample.depth[k]))
      }
      if (any(grepl(paste("data.age", j, sep = "_"), names(ds.movedb$choices))))
        points(ds.movedb$choices[grep(paste("data.age", j, sep = "_"), names(ds.movedb$choices))][[1]]$Depth ~ ds.movedb$choices[grep(paste("data.age", j, sep = "_"), names(ds.movedb$choices))][[1]]$Age, type = "b", col = 4, pch = 16)
      
      points(tmp.zon$sample.depth ~ tmp.zon$int.age, pch = 16, type = "b", col = 2)
      points(tmp.zon$sample.depth ~ tmp.zon$mod.age, pch = 16, col = 3, type = "b")
      points(tmp.zon$sample.depth ~ tmp.zon$age, pch = 16, col = 7, type = "b")
      
      legend("bottomright", pch = 16, legend = c("Zones", "Interp", "Model", "chrono", "age"), col = c(1,2,3,4,7))
      dev.off()
    }
  }
}
rm(j, k)

res.sp3 <- rbind(lb.movedb$data, ds.movedb$data)

# 4. Tidy up ---------------------------------------------------------
# remove ages outside the range
res.sp3 <- res.sp3[res.sp3$age < 66 & !is.na(res.sp3$age), ]

summary(res.sp3)

tapply(res.sp3$age, res.sp3$age.calc, summary)

# check abundances
tapply(res.sp3$abundance, res.sp3$abun.units, summary)
summary(with(res.sp3[res.sp3$abun.units == "Binned",], tapply(abundance, sampleID, sum)))

save(dsloc_sp, dslocs, lbloc_sp, lblocs, taxon, zones, abun, authors, authors_type, chrono, chrono_epoch, chrono_era, chrono_markers, chrono_mtyp, chrono_period, chrono_stage, chrono_subepoch, chrono_zgrp, chrono_zone, chrono_zreg, dsloc_authors, dsloc_hole, dsloc_leg, dsloc_ocean, dsloc_prog, dsloc_site, dsloc_unit, dsloc_unitlith, dsloc_unitref, dsloc_unitsp, dsloc_unittax, lbloc_cont, lbloc_country, lbloc_region, lbloc_section, lbloc_styp, lbloc_unit, lbloc_unitlith, lbloc_unitref, lbloc_unitsp, lbloc_unittax, litho_types, missing, reference, references_type, synonyms, taxon_class, taxon_family, taxon_foramabund, taxon_forampres, taxon_genus, taxon_nannoabund, taxon_nannopres, taxon_order, taxon_others, taxon_phylum, taxon_spabund, taxon_species, taxon_status, taxon_synonymy, taxon_type, uniq.pfsp, file = "Outputs/MoveDBworking.RData")
save(lb.movedb, ds.movedb, lbloc_sp_pf, dsloc_sp_pf, move.db, res.sp3, file = "Outputs/MoveDB.RData")
rm(dsloc_sp, dsloc_sp_pf, dslocs, ds.movedb, lbloc_sp, lbloc_sp_pf, lblocs, lb.movedb, taxon, zones, move.db, movedb.model.type)
rm(abun, authors, authors_type, chrono, chrono_epoch, chrono_era, chrono_markers, chrono_mtyp, chrono_period, chrono_stage, chrono_subepoch, chrono_zgrp, chrono_zone, chrono_zreg, dsloc_authors, dsloc_hole, dsloc_leg, dsloc_ocean, dsloc_prog, dsloc_site, dsloc_unit, dsloc_unitlith, dsloc_unitref, dsloc_unitsp, dsloc_unittax, lbloc_cont, lbloc_country, lbloc_region, lbloc_section, lbloc_styp, lbloc_unit, lbloc_unitlith, lbloc_unitref, lbloc_unitsp, lbloc_unittax, litho_types, missing, reference, references_type, synonyms, taxon_class, taxon_family, taxon_foramabund, taxon_forampres, taxon_genus, taxon_nannoabund, taxon_nannopres, taxon_order, taxon_others, taxon_phylum, taxon_spabund, taxon_species, taxon_status, taxon_synonymy, taxon_type, uniq.pfsp, tmp.zon)

