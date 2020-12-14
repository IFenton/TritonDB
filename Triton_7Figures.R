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

# totals
table(foram.data$db.source)

# macro / micro
table(foram.data$`Macro/micro`)
table(foram.data$`Macro/micro`, foram.data$db.source)

# Neogene/Paleogene
table(foram.data$age < 23.03)
table(foram.data$age < 23.03, foram.data$db.source)

# Latitude
table(foram.data$pal.lat < 30)
table(foram.data$pal.lat < 60 & foram.data$pal.lat >= 30)
table(foram.data$pal.lat >= 60)
table(foram.data$pal.lat < 30, foram.data$db.source)
table(foram.data$pal.lat < 60 & foram.data$pal.lat >= 30 , foram.data$db.source)
table(foram.data$pal.lat >= 60 , foram.data$db.source)

# Abundance
table(foram.data$abun.units)
table(foram.data$abun.units, foram.data$db.source)

# neptune values
load("Outputs/Neptune.RData")
rm(neptune.orig)
neptune$data <- merge(neptune$data, foram.ages, by.x = "species", by.y = "Species.name")
table(neptune$data$`Macro/micro`)

# Neogene/Paleogene
table(neptune$data$age < 23.03)

# Latitude
table(neptune$data$pal.lat < 30)
table(neptune$data$pal.lat < 60 & neptune$data$pal.lat >= 30)
table(neptune$data$pal.lat >= 60)

# Abundance
table(neptune$data$abun.units)


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
plot(c(0, 66), c(0, 120), type = "n", xlab = "Age / Ma", ylab = "Species richness", bty = "l", main = "Triton", las = 1, cex.lab = 1.5)
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
model.type <- read_xlsx("Data/iodp_age_models.xlsx")
# if re-running load in the previous data
load("Outputs/IODP_separate.RData")
i <- grep("1499A", iodp.files, value = TRUE)
tmp.path <- paste("Data/IODP extras/", i, sep = "")
tmp.file <- paste("iodp", gsub("/|-| |\\+", "_", i), sep = "")
assign("choices_iodp", eval(parse(text = paste(tmp.file, "$choices"))))
# age plots
tmp.dat <- eval(parse(text = paste0(tmp.file, "$data[!duplicated(", tmp.file, "$data$sampleID), ]")))
tmp.dat <- tmp.dat[order(tmp.dat$sample.depth),]
tmp.full <- all.chrons[all.chrons$`Age notes` %in% c(NA, unique(as.character(eval(parse(text = paste0(tmp.file, "$choices$zones.tab$ocean"))))), unique(as.character(eval(parse(text = paste0(tmp.file, "$choices$zones.tab$region")))))), ]

j <- unique(tmp.dat$hole)
j <- gsub("^U|^C|^M", "", j)

tmp.age <- merge(eval(parse(text = paste0(tmp.file, "$choices$data.age_", j))), tmp.full, all.x = TRUE)
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





