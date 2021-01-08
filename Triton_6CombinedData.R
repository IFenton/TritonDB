# Combining all the data for Triton
#  
# Previous file: Triton_5IODPextra.R
# Next file: Triton_7Figures.R
#   
# Inputs ------------------------------------------------------------------

# Outputs -----------------------------------------------------------------

# Source files / libraries ------------------------------------------------
library(svDialogs) # e.g. dlg_list
library(openxlsx)

# 1. Combine the different datasets ---------------------------------------------------------
# load("Outputs/Neptune_plus.RData") # for res.sp1
# load("Outputs/res_sp2.RData") # for res.sp2c
# load("Outputs/MoveDB.RData") # for res.sp3
# load("Outputs/res_sp4.RData") # for res.sp4
triton <- rbind(res.sp1, res.sp2c, res.sp3[res.sp3$dbID != "DSMoveDB", ], res.sp4)

str(triton)
summary(triton)

table(triton$db.source)

# 2. Relative abundances -------------------------------------------------
# some core IDs are duplicated, create separate sample IDs for these
tmp.dup <- tapply(triton$dbID, triton$holeID, function(x) length(unique(x)))
tmp.dup2 <- names(tmp.dup)[tmp.dup > 1]

for (i in tmp.dup2) {
  tmp.db <- unique(triton$dbID[triton$holeID == i])
  if (length(tmp.db) > 1) 
    for (j in 2:length(tmp.db)) {
      triton$sampleID[triton$dbID == tmp.db[j] & triton$holeID == i] <- paste(triton$sampleID[triton$dbID == tmp.db[j] & triton$holeID == i], letters[j - 1], sep = ".")
      triton$holeID[triton$dbID == tmp.db[j] & triton$holeID == i] <- paste(triton$holeID[triton$dbID == tmp.db[j] & triton$holeID == i], letters[j - 1], sep = ".")
    }
}
rm(i, j, tmp.db, tmp.dup, tmp.dup2)

# also update the rowIDs
triton$rowID <- paste(triton$dbID, triton$sampleID, gsub("^.*_", "", triton$rowID), sep = "_")

# calculate the numerical total per sample
tmp.num <- tapply(triton$abundance, triton$sampleID, sum)
triton$num.ind <- as.numeric(tmp.num[match(triton$sampleID, names(tmp.num))])
rm(tmp.num)
# check which are relative abundance and have values > 100 
tapply(triton$num.ind, triton$abun.units, summary)
with(triton[triton$abun.units == "Relative abundance",], tapply(num.ind, db.source, summary))
unique(triton$dbID[triton$abun.units == "Relative abundance" & triton$num.ind > 100])
# I've checked all of these individually, and they are fine.

# remove those where the entire sample is 0
triton <- triton[triton$num.ind > 0, ]
# calculate relative abundances
triton$rel.abun <- triton$abundance / triton$num.ind * 100
# set p/a to NA (as these aren't relative abundances)
triton$rel.abun[triton$abun.units == "P/A"] <- NA

summary(triton$rel.abun)

# 3. Update ages / remove those with large errors ---------------------------------------------------
triton$age[triton$age.calc == "Orig" & (triton$year >= 2012 & !is.na(triton$year) | triton$db.source == "Neptune") & triton$age > 0] <- unlist(sapply(triton$age[triton$age.calc == "Orig" & (triton$year >= 2012 & !is.na(triton$year) | triton$db.source == "Neptune") & triton$age > 0], ts.conv, orig.ts = "GTS2012"))
triton$age[triton$age.calc == "Orig" & (triton$year < 2012 & !is.na(triton$year) & triton$db.source != "Neptune") & triton$age > 0] <- unlist(sapply(triton$age[triton$age.calc == "Orig" & (triton$year < 2012 & !is.na(triton$year) & triton$db.source != "Neptune") & triton$age > 0], ts.conv))

# if the zones are > 5Ma set to NA
triton$age[triton$age.err > 5] <- NA
# remove these
triton <- triton[!is.na(triton$age),]

# 4. Remove duplicates ----------------------------------------------------
triton <- triton[order(match(triton$db.source, c("ForCenS", "IODP", "Neptune", "Pangaea", "Lloydetal2012", "Fentonetal2016"))),]
triton$round.coord <- paste(round(triton$latitude, 3), round(triton$longitude, 3))
triton <- triton[which(!duplicated(triton[, c("species", "abundance", "sample.depth", "round.coord")])),]
triton <- triton[, -which(names(triton) == "round.coord")]

# 5. Calculating palaeocoordinates  -----------------
triton$round.age <- round(triton$age, 0)
triton$pal.lat[is.na(triton$pal.lat) & triton$round.age == 0] <- triton$latitude[is.na(triton$pal.lat) & triton$round.age == 0]
triton$pal.long[is.na(triton$pal.long) & triton$round.age == 0] <- triton$longitude[is.na(triton$pal.long) & triton$round.age == 0]

# update using current coordinates where possible
load("Outputs/pal_coord.RData")
tmpID <- paste(triton$round.age, round(triton$latitude, 4), round(triton$longitude, 4))
triton$pal.lat[is.na(triton$pal.lat)] <- pal.coord.dat$pal.lat[match(tmpID[is.na(triton$pal.lat)], pal.coord.dat$ID)]
triton$pal.long[is.na(triton$pal.long)] <- pal.coord.dat$pal.long[match(tmpID[is.na(triton$pal.long)], pal.coord.dat$ID)]

# add the new coordinates to pal.coord
tmp.pal.coord <- triton[is.na(triton$pal.lat), c("round.age", "latitude", "longitude", "pal.lat", "pal.long")]
tmp.pal.coord$ID <- paste(tmp.pal.coord$round.age, round(tmp.pal.coord$latitude, 4), round(tmp.pal.coord$longitude, 4))
tmp.pal.coord <- unique(tmp.pal.coord)
pal.coord.dat <- merge(pal.coord.dat, tmp.pal.coord, all.x = TRUE, all.y = TRUE)
pal.coord.dat$calc[is.na(pal.coord.dat$calc)] <- "No" # have the coordinates been calculated?


for (i in 1:66) {
  print(i)
  tmp.pal.rows <- which(pal.coord.dat$calc == "No" & pal.coord.dat$round.age == i)
  #tmp.pal.rows <- which(is.na(pal.coord.dat$pal.lat) & pal.coord.dat$round.age == i)
  if (length(tmp.pal.rows) > 0) {
    tmp <- pal.coord.round(pal.coord.dat[tmp.pal.rows,], model = "MATTHEWS2016")
    pal.coord.dat$pal.lat[tmp.pal.rows] <- tmp$paleolat
    pal.coord.dat$pal.long[tmp.pal.rows] <- tmp$paleolng
  }
}
rm(tmp.pal.rows, i, tmp.pal.coord)

# merge these into the full dataset
triton$pal.lat[is.na(triton$pal.lat)] <- pal.coord.dat$pal.lat[match(tmpID[is.na(triton$pal.lat)], pal.coord.dat$ID)]
triton$pal.long[is.na(triton$pal.long)] <- pal.coord.dat$pal.long[match(tmpID[is.na(triton$pal.long)], pal.coord.dat$ID)]


# save out the new paleo coord data
pal.coord.dat <- triton[, c("round.age", "latitude", "longitude", "pal.lat", "pal.long")]
pal.coord.dat$ID <- paste(pal.coord.dat$round.age, round(pal.coord.dat$latitude, 4), round(pal.coord.dat$longitude, 4))
pal.coord.dat <- unique(pal.coord.dat)
pal.coord.dat$calc <- "Yes"
#save(pal.coord.dat, file = "Outputs/pal_coord.RData")


triton$pal.lat[is.na(triton$pal.lat) & triton$age < 1] <- triton$latitude[is.na(triton$pal.lat) & triton$age < 1]
triton$pal.long[is.na(triton$pal.long) & triton$age < 1] <- triton$longitude[is.na(triton$pal.long) & triton$age < 1]

triton <- triton[!is.na(triton$pal.lat), ]

triton$round.lat <- round(triton$latitude/5, 0)*5
triton$round.pal.lat <- round(triton$pal.lat/5, 0)*5

rm(tmpID)

# 6. Add speciation/extinction data --------------------------------------------------
# add new ages
# assumes you have run Timescale_conv.R
head(foram.ages)
names(foram.ages)[names(foram.ages) == "Start"] <- "Speciation"
names(foram.ages)[names(foram.ages) == "End"] <- "Extinction"

# compare this species list to the species in triton
triton.sp <- sort(unique(triton$species))
setdiff(triton.sp, foram.ages$Species.name) # species only in triton
setdiff(foram.ages$Species.name, triton.sp) # species only in the foram ages (no records in Triton)

triton <- merge(triton, foram.ages, by.x = "species", by.y = "Species.name", all.x = TRUE)


# 7. Tag species significantly outside their known range -----------------------------
triton$trim <- "inc"
triton$trim[(triton$Speciation - triton$age) < -5 & triton$age > 23] <- "exc"
triton$trim[(triton$Extinction - triton$age) >5 & triton$age > 23] <- "exc"
triton$trim[(triton$Speciation - triton$age) < -2 & triton$age <= 23] <- "exc"
triton$trim[(triton$Extinction - triton$age) >2 & triton$age <= 23] <- "exc"

# 8. Create a subset where abundance is greater than 0 --------------------
triton.pres <- triton[triton$abundance > 0, ]

# 9. Tidy up --------------------------------------------------------------
# save(triton, triton.pres, file = "Outputs/triton.RData")
rm(res.sp1, res.sp2c, res.sp3, res.sp4, pal.coord.dat, triton.sp)



