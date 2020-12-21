# Summaries / figures for Triton
#  
# Previous file: Triton_6CombinedData.R
# Next file: N/A

# Source files / libraries ------------------------------------------------
library(colorRamps)

# 0. choice of dataset ----------------------------------------------
foram.data <- triton.pres

# 1. Summary info ---------------------------------------------------------
# number of species
length(unique(foram.data$species)) # 393
length(unique(foram.data$species[foram.data$`Macro/micro` == "Macroperforate"])) # 339 macroperforates
length(unique(foram.data$species[foram.data$`Macro/micro` == "Microperforate"])) # 54 microperforates

# number of records per species
sort(table(foram.data$species))
rev(sort(table(foram.data$species)))[1:30]

# 2. Database sources ----------------------------------------------------
png("Figures/Data sources.png")
par(mar = c(8, 4.1, 4.1, 2.1))
barplot(rev(sort(table(foram.data$db.source)))/1000, las = 2, xlab = "", col = "navy", ylab = expression(paste("Number of records (", "10"^"3", ")")), mgp = c(2.5, 1, 0))
dev.off()

nrow(foram.data)

# totals
table(foram.data$db.source)

# macro / micro
table(foram.data$`Macro/micro`)
table(foram.data$`Macro/micro`, foram.data$db.source)

# Neogene/Paleogene
table(foram.data$age < 23.03)
table(foram.data$age < 23.03, foram.data$db.source)

# Latitude
table(abs(foram.data$pal.lat) < 30)
table(abs(foram.data$pal.lat) < 60 & abs(foram.data$pal.lat) >= 30)
table(abs(foram.data$pal.lat) >= 60)
table(abs(foram.data$pal.lat) < 30, foram.data$db.source)
table(abs(foram.data$pal.lat) < 60 & abs(foram.data$pal.lat) >= 30 , foram.data$db.source)
table(abs(foram.data$pal.lat) >= 60 , foram.data$db.source)

# Abundance
table(foram.data$abun.units)
table(foram.data$abun.units, foram.data$db.source)

# neptune values
load("Outputs/Neptune.RData")
rm(neptune.orig)
neptune$data <- merge(neptune$data, foram.ages, by.x = "species", by.y = "Species.name")
nrow(neptune$data)
table(neptune$data$`Macro/micro`)

# Neogene/Paleogene
table(neptune$data$age < 23.03)

# Latitude
table(abs(neptune$data$pal.lat) < 30)
table(abs(neptune$data$pal.lat) < 60 & abs(neptune$data$pal.lat) >= 30)
table(abs(neptune$data$pal.lat) >= 60)

# Abundance
table(neptune$data$abun.units)

# number of recognised species
nrow(foram.ages)

# number of species in triton
length(unique(foram.data$species))

# percentage
length(unique(foram.data$species)) / nrow(foram.ages) * 100

# rare / absent species
sum(foram.data$species == "Dentoglobigerina juxtabinaiensis")

# 3. Plots of records by age / lat ----------------------------------------

# 3a. Total records --------------------------------------------------------

# plot number of records by age / pal.latitude
foram.data %>%
  group_by(round.age, round.pal.lat) %>%
  summarise(total.sp = length(species)) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = total.sp)) + 
  scale_fill_gradient(limits = c(1, 16000)) +
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Records")
ggsave("Figures/n_lat_age.png")

# plot number of species by age / pal.latitude
foram.data %>%
  group_by(round.age, round.pal.lat, sampleID) %>%
  summarise(max.sp = length(unique(species))) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = max.sp)) + 
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Species")
ggsave("Figures/SR_lat_age.png")


# 3b. With / without neptune ----------------------------------------------
foram.data[foram.data$db.source != "Neptune",] %>%
  group_by(round.age, round.pal.lat, sampleID) %>%
  summarise(max.sp = length(unique(species))) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = max.sp)) + 
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Species")
ggsave("Figures/SR_lat_age_woNep.png")

# using neptune
neptune$data$round.age <- round(neptune$data$age, 0)
neptune$data$round.pal.lat <- round(neptune$data$pal.lat/5, 0)*5
neptune$data %>%
  group_by(round.age, round.pal.lat, sampleID) %>%
  summarise(max.sp = length(unique(species))) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = max.sp)) + 
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Species")
ggsave("Figures/SR_lat_age_Nep.png")

foram.data[foram.data$db.source != "Neptune",] %>%
  group_by(round.age, round.pal.lat) %>%
  summarise(total.sp = length(species)) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = total.sp)) + 
  scale_fill_gradient(limits = c(1, 16000)) +
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Records")
ggsave("Figures/n_lat_age_woNep.png")


neptune$data %>%
  group_by(round.age, round.pal.lat) %>%
  summarise(total.sp = length(species)) %>%
  ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
  geom_tile(mapping = aes(fill = total.sp)) + 
  scale_fill_gradient(limits = c(1, 16000)) +
  xlab("Age / Ma") + 
  ylab("Paleolatitude") + 
  ylim(c(-90,90)) +
  labs(fill = "Records")
ggsave("Figures/n_lat_age_Nep.png")


# 4. Completeness by age / latitude --------------------------------------
# Paleocene 66-56
table(foram.data$round.pal.lat[foram.data$age > 56])

# Eocene 56-33.9
table(foram.data$round.pal.lat[foram.data$age > 33.9 & foram.data$age <= 56])

# Oligocene 33.9-23.03
table(foram.data$round.pal.lat[foram.data$age > 23.03 & foram.data$age <= 33.9])

# Miocene 23.03-5.333
table(foram.data$round.pal.lat[foram.data$age > 5.333 & foram.data$age <= 23.03])

# Pliocene 5.333-2.58
table(foram.data$round.pal.lat[foram.data$age > 2.58 & foram.data$age <= 5.333])

# Pleistocene 2.58-0.0117
table(foram.data$round.pal.lat[foram.data$age > 0.0117 & foram.data$age <= 2.58])

# Holocene 0.0117-0
table(foram.data$round.pal.lat[foram.data$age <= 0.0017])


# 5. Diversity through time -----------------------------------------------
png("Figures/Diversity.png")
plot(c(0, 66), c(0, 170), type = "n", xlab = "Age / Ma", ylab = "Species richness", bty = "l", main = "Triton", las = 1, cex.lab = 1.5)
abline(v = c(2.59, 5.33, 23.03, 33.9, 56), col = "grey70")
tmp.y <- NULL
for (i in 1:66) {
  tmp.y <- c(tmp.y, length(unique(foram.data$species[foram.data$age >= (i-1) & foram.data$age < i])))
}
points((1:66) - 0.5, tmp.y, type = "b", pch = 16)
dev.off()
rm(i, tmp.y)

png("Figures/Diversity_Nep.png")
plot(c(0, 66), c(0, 130), type = "n", xlab = "Age / Ma", ylab = "Species richness", bty = "l", main = "Neptune", las = 1, cex.lab = 1.5)
abline(v = c(2.59, 5.33, 23.03, 33.9, 56), col = "grey70")
tmp.y <- NULL
for (i in 1:66) {
  tmp.y <- c(tmp.y, length(unique(neptune$data$species[neptune$data$age >= (i-1) & neptune$data$age < i])))
}
points((1:66) - 0.5, tmp.y, type = "b", pch = 16)
dev.off()
rm(i, tmp.y)


trim.5.2 <- foram.data[foram.data$trim == "inc",]

dim(trim.5.2)

png("Figures/Diversity_trim5_2.png")
plot(c(0, 66), c(0, 120), type = "n", xlab = "Age / Ma", ylab = "Species richness", bty = "l", las = 1, cex.lab = 1.5)
abline(v = c(2.59, 5.33, 23.03, 33.9, 56), col = "grey70")
tmp.y <- NULL
for (i in 1:66) {
  tmp.y <- c(tmp.y, length(unique(trim.5.2$species[trim.5.2$age >= (i-1) & trim.5.2$age < i])))
}
points((1:66) - 0.5, tmp.y, type = "b", pch = 16)
dev.off()
rm(i, tmp.y)


# 6. Example age model ----------------------------------------------------
iodp.files <- grep("^(?=.*\\.xls)(?!.*chron)", list.files("Data/IODP extras", recursive = TRUE), value = TRUE, perl = TRUE)
model.type <- read_xlsx("Data/age_models.xlsx", sheet = "iodp")
# if re-running load in the previous data
load("Outputs/IODP_sep.RData")
i <- grep("1499A", iodp.data$file)
# tmp.path <- paste("Data/IODP extras/", i, sep = "")
# tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
# age plots
tmp.dat <- iodp.data$new.dat[[i]][!duplicated(iodp.data$new.dat[[i]]$sampleID), ]
tmp.dat <- tmp.dat[order(tmp.dat$sample.depth),]
tmp.full <- all.chrons[all.chrons$`Age notes` %in% c(NA, unique(as.character(iodp.data$choices[[i]]$zones.tab$ocean)), unique(as.character(iodp.data$choices[[i]]$zones.tab$region))), ]

j <- unique(tmp.dat$hole)
j <- gsub("^U|^C|^M", "", j)

tmp.age <- merge(eval(parse(text = paste0("iodp.data$choices[[i]]$data.age_", j))), tmp.full, all.x = TRUE)
tmp.age <- tmp.age[order(tmp.age[, names(tmp.age) == j], tmp.age$Age), ]

png("Figures/Age1499A.png", 600, 600)
plot(tmp.age[, names(tmp.age) == j] ~ tmp.age$Age, xlab = "Age", ylab = "Depth", pch = 16, type = "b", col = 4, main = j, xlim = c(min(c(unlist(tmp.dat[, c("age", "zon.age", "age.st", "age.en", "int.age", "mod.age")]), tmp.age$Age), na.rm = TRUE), max(c(unlist(tmp.dat[, c("age", "zon.age", "age.st", "age.en", "int.age", "mod.age")]), tmp.age$Age), na.rm = TRUE)), las = 1, bty = "l")
points(tmp.dat$sample.depth ~ tmp.dat$mag.age, pch = 16, type = "b", col = 4)
for (k in 1:nrow(tmp.dat)) {
  lines(c(tmp.dat$mag.age.st[k], tmp.dat$mag.age.en[k]), c(tmp.dat$sample.depth[k], tmp.dat$sample.depth[k]), col = 4)
}
# interpolated mag ages
points(tmp.dat$sample.depth ~ tmp.dat$int.mag.age, pch = 16, type = "b", col = 6)
points(tmp.dat$sample.depth ~ tmp.dat$zon.age, pch = 16, type = "b")

for (k in 1:nrow(tmp.dat)) {
  lines(c(tmp.dat$age.st[k], tmp.dat$age.en[k]), c(tmp.dat$sample.depth[k], tmp.dat$sample.depth[k]))
}
# interpolated ages
points(tmp.dat$sample.depth ~ tmp.dat$int.age, pch = 16, type = "b", col = 2, cex = 1.2)
# modelled ages
points(tmp.dat$sample.depth ~ tmp.dat$mod.age, pch = 16, type = "b", col = 3, cex = 1.4)

legend("bottomright", pch = 16, legend = c("Model", "Zones", "Interp", "mag", "int.mag"), col = c(3,1,2,4,6), bty = "n")
dev.off()



# 7. Species completeness -------------------------------------------------
# Species level data 
# create the species level dataframe
datasp <- foram.ages[, c("Species.name", "Macro/micro", "Start", "End")]
names(datasp)[names(datasp) == "Species.name"] <- "species"
datasp <- datasp[order(datasp$species), ]
sp.ord <- sort(unique(foram.data$species))

# populate it with database info
datasp$n.occur <- 0
datasp$n.occur[match(sp.ord, datasp$species)] <- table(foram.data$species)
datasp$min.age[datasp$species %in% sp.ord] <- tapply(foram.data$age, foram.data$species, min, na.rm = TRUE)
datasp$max.age[datasp$species %in% sp.ord] <- tapply(foram.data$age, foram.data$species, max, na.rm = TRUE)
datasp$age.range <- datasp$max.age - datasp$min.age
datasp$min.pal.lat[datasp$species %in% sp.ord] <- tapply(foram.data$pal.lat, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, min(x, na.rm = TRUE), NA))
datasp$max.pal.lat[datasp$species %in% sp.ord] <- tapply(foram.data$pal.lat, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, max(x, na.rm = TRUE), NA))
datasp$pal.lat.range <- datasp$max.pal.lat - datasp$min.pal.lat
datasp$min.pal.long[datasp$species %in% sp.ord] <- tapply(foram.data$pal.long, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, min(x, na.rm = TRUE), NA))
datasp$max.pal.long[datasp$species %in% sp.ord] <- tapply(foram.data$pal.long, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, max(x, na.rm = TRUE), NA))
datasp$pal.long.range <- datasp$max.pal.long - datasp$min.pal.long
datasp$nMa.rec <- tapply(foram.data$round.age, foram.data$species, function(x) length(unique(x)))[datasp$species]

# consider fraction of data completeness at given bin widths
summary(sapply(datasp$species, age.frac, datasp, foram.data, 1))


# fully trimmed dataset
foram.data.full.trim <- foram.data[foram.data$Speciation - foram.data$age > 0, ]
foram.data.full.trim <- foram.data.full.trim[foram.data.full.trim$Extinction - foram.data.full.trim$age < 0, ]

# calculating species completeness with 1Ma bins
tab.age <- tapply(foram.data.full.trim$round.age, foram.data.full.trim$species, function(x) length(unique(x)))
datasp$nMa.rng <- 0
datasp$nMa.rng[match(names(tab.age), datasp$species)] <- tab.age
datasp$age.rng <- round(datasp$Start) - round(datasp$End) + 1
datasp$frac.rng.1Ma <- datasp$nMa.rng / datasp$age.rng

#at 0.5Ma bins
foram.data.full.trim$round.age.0.5 <- round(foram.data.full.trim$age * 2) / 2
tab.age.0.5 <- tapply(foram.data.full.trim$round.age.0.5, foram.data.full.trim$species, function(x) length(unique(x)))
datasp$nMa.rng.0.5 <- 0
datasp$nMa.rng.0.5[match(names(tab.age.0.5), datasp$species)] <- tab.age.0.5
datasp$age.rng.0.5 <- round(datasp$Start * 2) - round(datasp$End * 2) + 1
datasp$frac.rng.0.5Ma <- datasp$nMa.rng.0.5 / datasp$age.rng.0.5


# number of species with at least one record (within their known time period)
length(datasp$frac.rng.1Ma[datasp$nMa.rng > 0])
# fraction of valid species
length(datasp$frac.rng.1Ma[datasp$nMa.rng > 0]) / nrow(datasp)

# completeness at 1Ma resolution
summary(datasp$frac.rng.1Ma[datasp$nMa.rng > 0])
sum(datasp$frac.rng.1Ma[datasp$nMa.rng > 0] == 1)
sum(datasp$frac.rng.1Ma[datasp$nMa.rng > 0] == 1) / length(datasp$frac.rng.1Ma[datasp$nMa.rng >0])

# completeness at 0.5Ma resolution
summary(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0])
sum(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0] == 1)
sum(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0] == 1) / length(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 >0])



