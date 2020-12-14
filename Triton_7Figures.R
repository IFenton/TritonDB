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


# # 5. Species level data --------------------------------------------------------
# 
# # 5a. Species plots -------------------------------------------------------
# foram.data[foram.data$species == "Orbulina universa",] %>%
#   group_by(round.age, round.pal.lat) %>%
#   summarise(total.sp = length(species)) %>%
#   ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
#   geom_tile(mapping = aes(fill = total.sp)) + 
#   ggtitle("Orbulina universa")
# 
# foram.data[foram.data$species == "Globigerinoidesella fistulosa",] %>%
#   group_by(round.age, round.pal.lat) %>%
#   summarise(total.sp = length(species)) %>%
#   ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
#   geom_tile(mapping = aes(fill = total.sp)) + 
#   ggtitle("Globigerinoidesella fistulosa")
# 
# 
# foram.data[foram.data$species == "Globoconella conoidea",] %>%
#   group_by(round.age, round.pal.lat) %>%
#   summarise(total.sp = length(species)) %>%
#   ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
#   geom_tile(mapping = aes(fill = total.sp)) + 
#   ggtitle("Globoconella conoidea")
# 
# foram.data[foram.data$species == "Acarinina bullbrooki",] %>%
#   group_by(round.age, round.pal.lat) %>%
#   summarise(total.sp = length(species)) %>%
#   ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
#   geom_tile(mapping = aes(fill = total.sp)) + 
#   ggtitle("Acarinina bullbrooki")
# 
# 
# for (i in unique(foram.data$species)) {
#   png(paste("Figures/6_Summaries/SpeciesRanges/", i, ".png", sep = ""))
#   print(foram.data[foram.data$species == i,] %>%
#           group_by(round.age, round.pal.lat) %>%
#           summarise(total.sp = length(species)) %>%
#           ggplot(mapping = aes(x = round.age, y = round.pal.lat)) +
#           geom_tile(mapping = aes(fill = total.sp)) +
#           xlab("Age / Ma") + 
#           ylab("Paleolatitude") + 
#           ylim(c(-90,90)) +
#           labs(fill = "Records"))
#   dev.off()
# }
# rm(i)
# 
# # 5b. Species level summaries ----------------------------------------------
# table(foram.data$species, foram.data$round.age)
# 
# # Species level data 
# # create the species level dataframe
# datasp <- foram.func[, c("specName", "Macro/micro", "Extinction", "Speciation")]
# names(datasp)[names(datasp) == "specName"] <- "species"
# datasp <- datasp[order(datasp$species), ]
# sp.ord <- sort(unique(foram.data$species))
# 
# # populate it with database info
# datasp$n.occur <- 0
# datasp$n.occur[match(sp.ord, datasp$species)] <- table(foram.data$species)
# datasp$min.age[datasp$species %in% sp.ord] <- tapply(foram.data$age, foram.data$species, min, na.rm = TRUE)
# datasp$max.age[datasp$species %in% sp.ord] <- tapply(foram.data$age, foram.data$species, max, na.rm = TRUE)
# datasp$age.range <- datasp$max.age - datasp$min.age
# datasp$min.pal.lat[datasp$species %in% sp.ord] <- tapply(foram.data$pal.lat, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, min(x, na.rm = TRUE), NA))
# datasp$max.pal.lat[datasp$species %in% sp.ord] <- tapply(foram.data$pal.lat, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, max(x, na.rm = TRUE), NA))
# datasp$pal.lat.range <- datasp$max.pal.lat - datasp$min.pal.lat
# datasp$min.pal.long[datasp$species %in% sp.ord] <- tapply(foram.data$pal.long, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, min(x, na.rm = TRUE), NA))
# datasp$max.pal.long[datasp$species %in% sp.ord] <- tapply(foram.data$pal.long, foram.data$species, function(x) ifelse(sum(!is.na(x)) > 0, max(x, na.rm = TRUE), NA))
# datasp$pal.long.range <- datasp$max.pal.long - datasp$min.pal.long
# datasp$nMa.rec <- tapply(foram.data$round.age, foram.data$species, function(x) length(unique(x)))[datasp$species]
# 
# # datasp$age.range.frac <- datasp$age.range / (datasp$Speciation - datasp$Extinction)
# 
# # how many of the occurrences fall outside the expected age range?
# datasp$before <- datasp$after <- NA
# 
# datasp$before <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > foram.data$Speciation[foram.data$species == datasp$species[x]]))
# datasp$after <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < foram.data$Extinction[foram.data$species == datasp$species[x]]))
# datasp$outside <- (datasp$before + datasp$after) / datasp$n.occur
# 
# hist(datasp$outside)
# 
# # age range plus/minus 1Ma
# datasp$before.1 <- datasp$after.1 <- NA
# datasp$before.1 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > (foram.data$Speciation[foram.data$species == datasp$species[x]] + 1)))
# datasp$after.1 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < (foram.data$Extinction[foram.data$species == datasp$species[x]] - 1)))
# datasp$outside.1 <- (datasp$before.1 + datasp$after.1) / datasp$n.occur
# 
# # 2Ma
# datasp$before.2 <- datasp$after.2 <- NA
# datasp$before.2 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > (foram.data$Speciation[foram.data$species == datasp$species[x]] + 2)))
# datasp$after.2 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < (foram.data$Extinction[foram.data$species == datasp$species[x]] - 2)))
# datasp$outside.2 <- (datasp$before.2 + datasp$after.2) / datasp$n.occur
# 
# # 3Ma
# datasp$before.3 <- datasp$after.3 <- NA
# datasp$before.3 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > (foram.data$Speciation[foram.data$species == datasp$species[x]] + 3)))
# datasp$after.3 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < (foram.data$Extinction[foram.data$species == datasp$species[x]] - 3)))
# datasp$outside.3 <- (datasp$before.3 + datasp$after.3) / datasp$n.occur
# 
# # 4Ma
# datasp$before.4 <- datasp$after.4 <- NA
# datasp$before.4 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > (foram.data$Speciation[foram.data$species == datasp$species[x]] + 4)))
# datasp$after.4 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < (foram.data$Extinction[foram.data$species == datasp$species[x]] - 4)))
# datasp$outside.4 <- (datasp$before.4 + datasp$after.4) / datasp$n.occur
# 
# datasp$before.5 <- datasp$after.5 <- NA
# datasp$before.5 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] > (foram.data$Speciation[foram.data$species == datasp$species[x]] + 5)))
# datasp$after.5 <- sapply(1:nrow(datasp), function (x) sum(foram.data$age[foram.data$species == datasp$species[x]] < (foram.data$Extinction[foram.data$species == datasp$species[x]] - 5)))
# datasp$outside.5 <- (datasp$before.5 + datasp$after.5) / datasp$n.occur
# 
# pdf("Figures/6_Summaries/6DS5_Exclude by age.pdf")
# hist(datasp$outside, xlab = "Fraction outside", ylab = "Number of species", main = "No margin")
# hist(datasp$outside.1, xlab = "Fraction outside", ylab = "Number of species", main = "1Ma")
# hist(datasp$outside.2, xlab = "Fraction outside", ylab = "Number of species", main = "2Ma")
# hist(datasp$outside.3, xlab = "Fraction outside", ylab = "Number of species", main = "3Ma")
# hist(datasp$outside.4, xlab = "Fraction outside", ylab = "Number of species", main = "4Ma")
# hist(datasp$outside.5, xlab = "Fraction outside", ylab = "Number of species", main = "5Ma")
# dev.off()
# 
# # exclude based on hiatus
# diff.hiatus <- function(sp, gap) {
#   # identify the differences between successive records
#   tmp.sp <- sort(foram.data$int.age[foram.data$species == sp])
#   tmp.diff <- diff(tmp.sp)
#   # find the halfway point in occurrences
#   hwp <- sum(foram.data$species == sp) / 2
#   # for the first half, identify differences > x and remove all those before
#   gap.diff <- which(tmp.diff > gap)
#   fh.diff <- ifelse(length(gap.diff[gap.diff < hwp]) > 0, max(gap.diff[gap.diff < hwp]), 0)
#   # repeat for the second half removing all those after 
#   sh.diff <- ifelse(length(gap.diff[gap.diff > hwp]) > 0, min(gap.diff[gap.diff > hwp]), length(tmp.sp))
#   # return the number of occurrences
#   tmp.occur <- length(tmp.sp) - fh.diff + (sh.diff - length(tmp.sp))
#   
#   # calculate which fall outside the reported age range
#   tmp.sp.trim <- tmp.sp[(fh.diff):(sh.diff)]
#   frac.wo <- sum(tmp.sp.trim > datasp$Speciation[datasp$species == sp] | tmp.sp.trim < datasp$Extinction[datasp$species == sp])
#   
#   return(list(hiatus = (1 - tmp.occur / length(tmp.sp)), age.range = frac.wo / length(tmp.sp)))
# }
# 
# datasp$hiatus.1 <- unlist(sapply(datasp$species, diff.hiatus, gap = 1)[1,])
# datasp$hiatus.2 <- unlist(sapply(datasp$species, diff.hiatus, gap = 2)[1,])
# datasp$hiatus.3 <- unlist(sapply(datasp$species, diff.hiatus, gap = 3)[1,])
# datasp$hiatus.4 <- unlist(sapply(datasp$species, diff.hiatus, gap = 4)[1,])
# datasp$hiatus.5 <- unlist(sapply(datasp$species, diff.hiatus, gap = 5)[1,])
# 
# datasp$hiatus.age.1 <- unlist(sapply(datasp$species, diff.hiatus, gap = 1)[2,])
# datasp$hiatus.age.2 <- unlist(sapply(datasp$species, diff.hiatus, gap = 2)[2,])
# datasp$hiatus.age.3 <- unlist(sapply(datasp$species, diff.hiatus, gap = 3)[2,])
# datasp$hiatus.age.4 <- unlist(sapply(datasp$species, diff.hiatus, gap = 4)[2,])
# datasp$hiatus.age.5 <- unlist(sapply(datasp$species, diff.hiatus, gap = 5)[2,])
# 
# 
# pdf("Figures/6_Summaries/6DS5_Exclude by hiatus.pdf")
# hist(datasp$hiatus.1, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside hiatus", ylab = "Number of species", main = "1 Ma hiatus")
# hist(datasp$hiatus.2, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside hiatus", ylab = "Number of species", main = "2 Ma hiatus")
# hist(datasp$hiatus.3, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside hiatus", ylab = "Number of species", main = "3 Ma hiatus")
# hist(datasp$hiatus.4, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside hiatus", ylab = "Number of species", main = "4 Ma hiatus")
# hist(datasp$hiatus.5, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside hiatus", ylab = "Number of species", main = "5 Ma hiatus")
# dev.off()
# 
# 
# pdf("Figures/6_Summaries/6DS5_Exclude by hiatus and age.pdf")
# hist(datasp$hiatus.age.5, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside", ylab = "Number of species", main = "5Ma hiatus")
# hist(datasp$hiatus.age.4, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside", ylab = "Number of species", main = "4Ma hiatus")
# hist(datasp$hiatus.age.3, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside", ylab = "Number of species", main = "3Ma hiatus")
# hist(datasp$hiatus.age.2, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside", ylab = "Number of species", main = "2Ma hiatus")
# hist(datasp$hiatus.age.1, breaks = 10, xlim = c(0, 1), xlab = "Fraction outside", ylab = "Number of species", main = "1Ma hiatus")
# dev.off()
# 
# # consider fraction of data completeness at given bin widths
# summary(sapply(datasp$specName, age.frac, datasp, foram.data, 1))
# 
# # add in functional data
# datasp <- merge(datasp, foram.func[, -match(c("Macro/micro", "Extinction", "Speciation"), colnames(foram.func))], by.x = "species", by.y = "specName")
# 
# # latitudinal range by age
# plot(datasp$pal.lat.range, datasp$max.age, pch = 16, col = datasp$eco)
# plot(datasp$pal.lat.range, datasp$min.age, pch = 16, col = datasp$eco)
# plot(datasp$pal.lat.range, datasp$age.range, pch = 16, col = as.factor(datasp$spinose))
# plot(datasp$pal.lat.range, datasp$age.range, pch = 16, col = matlab.like(max(round(datasp$log.area), na.rm = TRUE))[datasp$log.area])
# 
# 
# # fully trimmed dataset
# foram.data.full.trim <- foram.data[foram.data$Speciation - foram.data$age > 0, ]
# foram.data.full.trim <- foram.data.full.trim[foram.data.full.trim$Extinction - foram.data.full.trim$age < 0, ]
# 
# # calculating species completeness with 1Ma bins
# tab.age <- tapply(foram.data.full.trim$round.age, foram.data.full.trim$species, function(x) length(unique(x)))
# datasp$nMa.rng <- 0
# datasp$nMa.rng[match(names(tab.age), datasp$species)] <- tab.age
# datasp$age.rng <- round(datasp$Speciation) - round(datasp$Extinction) + 1
# datasp$frac.rng.1Ma <- datasp$nMa.rng / datasp$age.rng
# 
# #at 0.5Ma bins
# foram.data.full.trim$round.age.0.5 <- round(foram.data.full.trim$age * 2) / 2
# tab.age.0.5 <- tapply(foram.data.full.trim$round.age.0.5, foram.data.full.trim$species, function(x) length(unique(x)))
# datasp$nMa.rng.0.5 <- 0
# datasp$nMa.rng.0.5[match(names(tab.age.0.5), datasp$species)] <- tab.age.0.5
# datasp$age.rng.0.5 <- round(datasp$Speciation * 2) - round(datasp$Extinction * 2) + 1
# datasp$frac.rng.0.5Ma <- datasp$nMa.rng.0.5 / datasp$age.rng.0.5
# 
# 
# # number of species with at least one record (within their known time period)
# length(datasp$frac.rng.1Ma[datasp$nMa.rng > 0])
# # fraction of valid species
# length(datasp$frac.rng.1Ma[datasp$nMa.rng > 0]) / 451
# 
# # completeness at 1Ma resolution
# summary(datasp$frac.rng.1Ma[datasp$nMa.rng > 0])
# sum(datasp$frac.rng.1Ma[datasp$nMa.rng > 0] == 1)
# sum(datasp$frac.rng.1Ma[datasp$nMa.rng > 0] == 1) / length(datasp$frac.rng.1Ma[datasp$nMa.rng >0])
# 
# # completeness at 0.5Ma resolution
# summary(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0])
# sum(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0] == 1)
# sum(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 > 0] == 1) / length(datasp$frac.rng.0.5Ma[datasp$nMa.rng.0.5 >0])
# 
# # # consider fraction of data completeness at given bin widths
# # summary(sapply(datasp$species, age.frac, datasp, foram.data, 1))
# # pdf("Figures/6_Summaries/Bin widths.pdf")
# # hist(sapply(datasp$species[!is.na(datasp$Speciation)], age.frac, datasp, foram.data, 1), main = "Bin width of 1 Ma", xlab = "Fraction of completeness")
# # hist(sapply(datasp$species[!is.na(datasp$Speciation)], age.frac, datasp, foram.data, 2), main = "Bin width of 2 Ma", xlab = "Fraction of completeness")
# # dev.off()
# 
# pdf("Figures/6_Summaries/6DS5_Species ranges.pdf", 10, 10)
# # plot species ranges
# tmp <- datasp[order(datasp$Speciation, datasp$Extinction, decreasing = TRUE), ]
# plot(1, xlim = c(66, 0), ylim = c(1, nrow(datasp)), type = "n", bty = "l", xlab = "Age / Ma", ylab = "Species", yaxt = "n", main = "Species ranges")
# unlist(sapply(1:nrow(tmp), function(x) lines(c(tmp$min.age[x], tmp$max.age[x]), c(x,x), col = "red")))
# unlist(sapply(1:nrow(tmp), function(x) lines(c(tmp$Extinction[x], tmp$Speciation[x]), c(x,x))))
# 
# plot(1, xlim = c(66, 0), ylim = c(1, nrow(datasp)), type = "n", bty = "l", xlab = "Age / Ma", ylab = "Species", yaxt = "n", main = "Species occurrences")
# unlist(sapply(1:nrow(tmp), function(x) lines(c(tmp$Extinction[x], tmp$Speciation[x]), c(x,x))))
# points(foram.data$age, match(foram.data$species, tmp$species), col = "red", pch = ".")
# dev.off()
# rm(tmp)
# 
# # Outside species range 
# # tmp <- table(foram.data$species[foram.data$age <= foram.data$Speciation & foram.data$age >= foram.data$Extinction])
# # datasp$n.occur.in.rng <- 0
# # datasp$n.occur.in.rng[match(names(tmp), datasp$species)] <- tmp
# # datasp$frac.occur.in.rng <- datasp$n.occur.in.rng / datasp$n.occur
# # 
# # tmp.foram <- foram.data[foram.data$rng.age < 3, ]
# # tmp <- table(tmp.foram$species[tmp.foram$age <= tmp.foram$Speciation & tmp.foram$age >= tmp.foram$Extinction])
# # datasp$n.occur3.in.rng <- 0
# # datasp$n.occur3.in.rng[match(names(tmp), datasp$species)] <- tmp
# # datasp$frac.occur3.in.rng <- datasp$n.occur3.in.rng / datasp$n.occur


# # 6. Age summaries --------------------------------------------------------
# 
# # 6a. Calculate ages summaries --------------------------------------------
# age.summary <- data.frame(age = seq(0, 66, by = 2))
# age.summary$num <- table(foram.data$round.age)[as.character(age.summary$age)]
# age.summary$n.spec <- tapply(foram.data$species, foram.data$round.age, function(x)length(unique(x)))[as.character(age.summary$age)]
# 
# age.summary$mn.lat.range <- NA
# age.summary$max.lat.range <- NA
# age.summary$min.lat.range <- NA
# age.summary$sd.lat.range <- NA
# 
# for (i in 1:nrow(age.summary)) {
#   tmp1 <- with(foram.data[foram.data$round.age == age.summary$age[i],], tapply(abs(pal.lat), species, max))
#   tmp2 <- with(foram.data[foram.data$round.age == age.summary$age[i],], tapply(abs(pal.lat), species, min))
#   tmp3 <- with(foram.data[foram.data$round.age == age.summary$age[i],], tapply(abs(pal.lat), species, mean))
#   age.summary$mn.lat.range[i] <- mean(tmp1-tmp2, na.rm = TRUE)
#   age.summary$max.lat.range[i] <- max(tmp1-tmp2, na.rm = TRUE)
#   age.summary$min.lat.range[i] <- min(tmp1-tmp2, na.rm = TRUE)
#   age.summary$sd.lat.range[i] <- sd(tmp1-tmp2, na.rm = TRUE)
#   age.summary$mn.centre.range[i] <- mean(tmp3, na.rm = TRUE)
# }
# rm(i, tmp1, tmp2, tmp3)
# 
# # 6b. Average range through time -------------------------------------------
# png("Figures/6_Summaries/6DS6_rangesize.png", 500, 300)
# plot(age.summary$age, age.summary$mn.centre.range, xlab = "Age / Ma", ylab = "abs(Latitude)", ylim = c(0, 60), type = "n", bty = "l")
# for (i in 1:nrow(age.summary)) {
#   lines(c(age.summary$age[i], age.summary$age[i]), c(age.summary$mn.centre.range[i] + age.summary$mn.lat.range[i]/2, age.summary$mn.centre.range[i] - age.summary$mn.lat.range[i]/2), col = "skyblue3", lwd = 3)
# } 
# rm(i)
# points(age.summary$age, age.summary$mn.centre.range, pch = 16)
# dev.off()

# 7. Diversity through time -----------------------------------------------
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
plot(c(0, 66), c(0, 120), type = "n", xlab = "Age / Ma", ylab = "Species richness", bty = "l", main = paste("Trimmed:Pal 5 MA, Neo 2 Ma; n records:", nrow(trim.5.2)), las = 1, cex.lab = 1.5)
abline(v = c(2.59, 5.33, 23.03, 33.9, 56), col = "grey70")
tmp.y <- NULL
for (i in 1:66) {
  tmp.y <- c(tmp.y, length(unique(trim.5.2$species[trim.5.2$age >= (i-1) & trim.5.2$age < i])))
}
points((1:66) - 0.5, tmp.y, type = "b", pch = 16)
dev.off()
rm(i, tmp.y)



