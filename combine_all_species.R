library(ggplot2)
library(ggthemes)
library(reshape2)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\Disaggregating LPIs\\Habitats")

habitats <- read.table("All species_habitats.csv", sep = ";", header = TRUE)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\Disaggregating LPIs")

hot_cold <- read.table("species_hotcold.txt", sep = ",", header = TRUE)
tax_drag <- read.table("tax_dragonflies.csv", sep = ";", header = TRUE)

species <- merge(habitats, hot_cold, by.x = "Species", by.y = "Species", all  = TRUE)
species$Habitat <- as.character(species$Habitat)
species[species$Habitat %in% NA, "Habitat"] <- "eurytopic"
species$Habitat <- as.factor(species$Habitat)
species[species$Group %in% NA & species$Taxon %in% "Butterflies", "Group"] <- "butterflies"
species[species$Group %in% NA & species$Taxon %in% "Birds", "Group"] <- "birds"
species$select <- 1

species <- merge(species, tax_drag, by.x = "Species",by.y = "tax", all.x = TRUE )
species$Species <- as.character(species$Species)
species$Name <- as.character(species$Name)
species$Species <- ifelse(! species$Name %in% NA, species$Name, species$Species ) 
species$Species <- as.factor(species$Species)
species <- species[,c(1:4,7)]
  
head(species)

summary(species)

write.table(species, "species_habitats_hot_cold.csv", sep = ";", row.names = FALSE, col.names = TRUE)


test_STI <- species[,c(1,3,4,5)]
test_STI[test_STI$Group %in% NA & test_STI$Taxon %in% "Birds", "Group"] <- "birds"
test_STI[test_STI$Group %in% NA & test_STI$Taxon %in% "Butterflies", "Group"] <- "butterflies"
test_STI$STI..oC. <- as.numeric(test_STI$STI..oC.)
test_STI <- test_STI[! test_STI$STI..oC. %in% NA,]
summary(test_STI)

median_group <- as.data.frame(aggregate(test_STI$STI..oC. ~ test_STI$Group, data = test_STI, median))
colnames(median_group) <- c("Group", "median_STI")
median_group_birds <- median_group[1,2]
median_group_butterflies <- median_group[2,2]
median_group_dragonflies <- median_group[3,2]

STI_birds <- test_STI[test_STI$Group %in% "birds",]
STI_butterflies <- test_STI[test_STI$Group %in% "butterflies",]
STI_dragonflies <- test_STI[test_STI$Group %in% "dragonflies",]

quant_birds <- as.data.frame(quantile(STI_birds$STI..oC., probs = seq(0,1,0.25)))
bi25 <- quant_birds[2,1]
bi75 <- quant_birds[4,1]
quant_butterflies <- as.data.frame(quantile(STI_butterflies$STI..oC., probs = seq(0,1,0.25)))
bu25 <- quant_butterflies[2,1]
bu75 <- quant_butterflies[4,1]
quant_dragonflies <- as.data.frame(quantile(STI_dragonflies$STI..oC., probs = seq(0,1,0.25)))
dr25 <- quant_dragonflies[2,1]
dr75 <- quant_dragonflies[4,1]

#test_STI <- merge(test_STI, median_group, by.x = "Group", by.y = "Group", all.x = TRUE)

STI_birds$category <- ifelse(STI_birds$STI..oC. > bi75, "hot", "medium")
STI_birds$category <- ifelse(STI_birds$STI..oC. < bi25, "cold", STI_birds$category)
STI_butterflies$category <- ifelse(STI_butterflies$STI..oC. > bu75, "hot", "medium")
STI_butterflies$category <- ifelse(STI_butterflies$STI..oC. < bu25, "cold", STI_butterflies$category)
STI_dragonflies$category <- ifelse(STI_dragonflies$STI..oC. > dr75, "hot", "medium")
STI_dragonflies$category <- ifelse(STI_dragonflies$STI..oC. < dr25, "cold", STI_dragonflies$category)

test_STI <- rbind(STI_birds, STI_butterflies, STI_dragonflies)
test_STI <- test_STI[,c(1,2,3,5)]
test_STI$category <- as.factor(test_STI$category)
summary(test_STI)
#test_STI$category <- ifelse(test_STI$STI..oC. < test_STI$median_STI, "froid", "chaud")
species <- merge(species, test_STI, by.x = c("Species", "Group", "STI..oC."), by.y = c("Species", "Group", "STI..oC."), all.x = TRUE )
#species$category <- as.character(species$category)
#species[species$category %in% NA, "category"] <- "medium"


summary(median_group)
#species <- merge(species, test_STI, by.x = c("Species", "Group", "STI..oC.", "Taxon"), by.y = c("Species", "Group", "STI..oC.", "Taxon"), all.x = TRUE )

to_remove <- read.table("species_to_remove.csv", sep = ";", header = TRUE)
to_remove$Species <- as.character(to_remove$Species)
to_remove[to_remove$ID %in% 618, "Species"] <- "Hemianax ephippiger Burmeister, 1839"
to_remove[to_remove$ID %in% 636, "Species"] <- "Sympetrum flaveolum Linnaeus, 1758"
to_remove[to_remove$ID %in% 79647, "Species"] <- "Orthetrum albistylum Selys, 1848"
to_remove[to_remove$Species %in% "Gomphus flavipes", "Species"] <- "Gomphus flavipes Selys, 1837"
to_remove[to_remove$Species %in% "Gomphus vulgatissimus", "Species"] <- "Gomphus vulgatissimus Linnaeus, 1758"
to_remove[to_remove$Species %in% "Brown hare", "Species"] <- "Lepus europaeus Pallas 1778"


aa <- unique(to_remove[,1])

species[species$Species %in% aa, "select"] <- 0

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")

#moth <- read.table("Copy of Output details moth species.csv", sep = ";", header = TRUE)
#moth <- moth[moth$Stratum_omschrijving %in% "Belgium",]
#moth <- moth[moth$Recordtype_naam %in% c("indexen", "se_indexen"),]
#moth_trend <- moth[! moth$Multipl_trend_category %in% NA,]
#moth_trend <- moth_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
#colnames(moth_trend) <- c("code", "tax", "Multipl_trend_category")
#moth <- moth[, c(1,2,4,15:31)]
#colnames(moth) <- c("code", "group_tax", "tax", "type", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
#moth <- melt(moth, id.vars = c("code", "group_tax", "tax", "type"))
#moth_se <- moth[moth$type %in% "se_indexen", ]
#moth_se <- moth_se[,c("code", "variable", "value")]
#colnames(moth_se) <- c("code", "year", "se")
#moth_index <- moth[moth$type %in% "indexen", ]
#moth_index <- moth_index[, c("code", "group_tax", "tax", "variable", "value")]
#colnames(moth_index) <- c("code", "group_tax", "tax", "year", "index")
#moth <- merge(moth_index, moth_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )


dragonflies <- read.table("Copy of Output details dragonflies.csv", sep = ";", header = TRUE)
dragonflies <- dragonflies[dragonflies$Stratum_omschrijving %in% "Belgium",]
dragonflies <- dragonflies[dragonflies$Recordtype_naam %in% c("indexen", "se_indexen"),]
dragonflies_trend <- dragonflies[! dragonflies$Multipl_trend_category %in% NA,]
dragonflies_trend <- dragonflies_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(dragonflies_trend) <- c("code", "tax", "Multipl_trend_category")
dragonflies <- dragonflies[, c(1,2,4,15:45,8,9)]
colnames(dragonflies) <- c("code", "group_tax", "tax", "type", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_dr <- dragonflies[,c(1,2,3,4,34,35,36)]
slope_square_dr <- slope_square_dr[slope_square_dr$type %in% "indexen", ]
dragonflies <- melt(dragonflies, id.vars = c("code", "group_tax", "tax", "type"))
dragonflies_se <- dragonflies[dragonflies$type %in% "se_indexen", ]
dragonflies_se <- dragonflies_se[,c("code", "variable", "value")]
colnames(dragonflies_se) <- c("code", "year", "se")
dragonflies_index <- dragonflies[dragonflies$type %in% "indexen", ]
dragonflies_index <- dragonflies_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(dragonflies_index) <- c("code", "group_tax", "tax", "year", "index")
dragonflies <- merge(dragonflies_index, dragonflies_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
dragonflies <- dragonflies[! dragonflies$year %in% "n_square",]
dragonflies <- dragonflies[! dragonflies$year %in% "slope",]
dragonflies <- dragonflies[! dragonflies$year %in% "se_slope",]


mammals <- read.table("Copy of Output details mammals.csv", sep = ";", header = TRUE)
mammals <- mammals[mammals$Stratum_omschrijving %in% "Belgium",]
mammals <- mammals[mammals$Recordtype_naam %in% c("indexen", "se_indexen"),]
mammals_trend <- mammals[! mammals$Multipl_trend_category %in% NA,]
mammals_trend <- mammals_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(mammals_trend) <- c("code", "tax", "Multipl_trend_category")
mammals <- mammals[, c(1,2,4,15:27,8,9)]
colnames(mammals) <- c("code", "group_tax", "tax", "type", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_ma <- mammals[,c(1,2,3,4,16,17,18)]
slope_square_ma <- slope_square_ma[slope_square_ma$type %in% "indexen", ]
mammals <- melt(mammals, id.vars = c("code", "group_tax", "tax", "type"))
mammals_se <- mammals[mammals$type %in% "se_indexen", ]
mammals_se <- mammals_se[,c("code", "variable", "value")]
colnames(mammals_se) <- c("code", "year", "se")
mammals_index <- mammals[mammals$type %in% "indexen", ]
mammals_index <- mammals_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(mammals_index) <- c("code", "group_tax", "tax", "year", "index")
mammals <- merge(mammals_index, mammals_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
mammals <- mammals[! mammals$year %in% "n_square",]
mammals <- mammals[! mammals$year %in% "slope",]
mammals <- mammals[! mammals$year %in% "se_slope",]


amphibians <- read.table("Copy of Output details amphibians.csv", sep = ";", header = TRUE)
amphibians <- amphibians[amphibians$Stratum_omschrijving %in% "Belgium",]
amphibians <- amphibians[amphibians$Recordtype_naam %in% c("indexen", "se_indexen"),]
amphibians_trend <- amphibians[! amphibians$Multipl_trend_category %in% NA,]
amphibians_trend <- amphibians_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(amphibians_trend) <- c("code", "tax", "Multipl_trend_category")
amphibians <- amphibians[, c(1,2,4,15:45,8,9)]
colnames(amphibians) <- c("code", "group_tax", "tax", "type", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_am <- amphibians[,c(1,2,3,4,34,35,36)]
slope_square_am <- slope_square_am[slope_square_am$type %in% "indexen", ]
amphibians <- melt(amphibians, id.vars = c("code", "group_tax", "tax", "type"))
amphibians_se <- amphibians[amphibians$type %in% "se_indexen", ]
amphibians_se <- amphibians_se[,c("code", "variable", "value")]
colnames(amphibians_se) <- c("code", "year", "se")
amphibians_index <- amphibians[amphibians$type %in% "indexen", ]
amphibians_index <- amphibians_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(amphibians_index) <- c("code", "group_tax", "tax", "year", "index")
amphibians <- merge(amphibians_index, amphibians_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
amphibians <- amphibians[! amphibians$year %in% "n_square",]
amphibians <- amphibians[! amphibians$year %in% "slope",]
amphibians <- amphibians[! amphibians$year %in% "se_slope",]


grasshoppers <- read.table("Copy of Output details grasshoppers.csv", sep = ";", header = TRUE)
grasshoppers <- grasshoppers[grasshoppers$Stratum_omschrijving %in% "Belgium",]
grasshoppers <- grasshoppers[grasshoppers$Recordtype_naam %in% c("indexen", "se_indexen"),]
grasshoppers_trend <- grasshoppers[! grasshoppers$Multipl_trend_category %in% NA,]
grasshoppers_trend <- grasshoppers_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(grasshoppers_trend) <- c("code", "tax", "Multipl_trend_category")
grasshoppers <- grasshoppers[, c(1,2,4,15:41,8,9)]
colnames(grasshoppers) <- c("code", "group_tax", "tax", "type", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_gr <- grasshoppers[,c(1,2,3,4,30,31,32)]
slope_square_gr <- slope_square_gr[slope_square_gr$type %in% "indexen", ]
grasshoppers <- melt(grasshoppers, id.vars = c("code", "group_tax", "tax", "type"))
grasshoppers_se <- grasshoppers[grasshoppers$type %in% "se_indexen", ]
grasshoppers_se <- grasshoppers_se[,c("code", "variable", "value")]
colnames(grasshoppers_se) <- c("code", "year", "se")
grasshoppers_index <- grasshoppers[grasshoppers$type %in% "indexen", ]
grasshoppers_index <- grasshoppers_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(grasshoppers_index) <- c("code", "group_tax", "tax", "year", "index")
grasshoppers <- merge(grasshoppers_index, grasshoppers_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
grasshoppers <- grasshoppers[! grasshoppers$year %in% "n_square",]
grasshoppers <- grasshoppers[! grasshoppers$year %in% "slope",]
grasshoppers <- grasshoppers[! grasshoppers$year %in% "se_slope",]



reptiles <- read.table("Copy of Output details reptiles.csv", sep = ";", header = TRUE)
reptiles <- reptiles[reptiles$Stratum_omschrijving %in% "Belgium",]
reptiles <- reptiles[reptiles$Recordtype_naam %in% c("indexen", "se_indexen"),]
reptiles_trend <- reptiles[! reptiles$Multipl_trend_category %in% NA,]
reptiles_trend <- reptiles_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(reptiles_trend) <- c("code", "tax", "Multipl_trend_category")
reptiles <- reptiles[, c(1,2,4,15:39,8,9)]
colnames(reptiles) <- c("code", "group_tax", "tax", "type", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_re <- reptiles[,c(1,2,3,4,28,29,30)]
slope_square_re <- slope_square_re[slope_square_re$type %in% "indexen", ]
reptiles <- melt(reptiles, id.vars = c("code", "group_tax", "tax", "type"))
reptiles_se <- reptiles[reptiles$type %in% "se_indexen", ]
reptiles_se <- reptiles_se[,c("code", "variable", "value")]
colnames(reptiles_se) <- c("code", "year", "se")
reptiles_index <- reptiles[reptiles$type %in% "indexen", ]
reptiles_index <- reptiles_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(reptiles_index) <- c("code", "group_tax", "tax", "year", "index")
reptiles <- merge(reptiles_index, reptiles_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
reptiles <- reptiles[! reptiles$year %in% "n_square",]
reptiles <- reptiles[! reptiles$year %in% "slope",]
reptiles <- reptiles[! reptiles$year %in% "se_slope",]


butterflies <- read.table("Copy of Output details butterflies.csv", sep = ";", header = TRUE)
butterflies1 <- read.table("Copy of Output details butterflies second generation.csv", sep = ";", header = TRUE)

second_gen <- unique(butterflies1[,4])
butterflies <- butterflies[! butterflies$Nednaam %in% second_gen,]

butterflies <- rbind(butterflies, butterflies1)
butterflies <- butterflies[butterflies$Stratum_omschrijving %in% "Belgium",]
summary(butterflies)

butterflies <- butterflies[butterflies$Recordtype_naam %in% c("indexen", "se_indexen"),]
butterflies_trend <- butterflies[! butterflies$Multipl_trend_category %in% NA,]
butterflies_trend <- butterflies_trend[,c("Soortcode","Nednaam","Multipl_trend_category")]
colnames(butterflies_trend) <- c("code", "tax", "Multipl_trend_category")
butterflies <- butterflies[, c(1,2,4,15:45,8,9)]
colnames(butterflies) <- c("code", "group_tax", "tax", "type", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_bu <- butterflies[,c(1,2,3,4,34,35,36)]
slope_square_bu <- slope_square_bu[slope_square_bu$type %in% "indexen",]
butterflies <- melt(butterflies, id.vars = c("code", "group_tax", "tax", "type"))
butterflies_se <- butterflies[butterflies$type %in% "se_indexen", ]
butterflies_se <- butterflies_se[,c("code", "variable", "value")]
colnames(butterflies_se) <- c("code", "year", "se")
butterflies_index <- butterflies[butterflies$type %in% "indexen", ]
butterflies_index <- butterflies_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(butterflies_index) <- c("code", "group_tax", "tax", "year", "index")
butterflies <- merge(butterflies_index, butterflies_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
butterflies <- butterflies[! butterflies$year %in% "n_square",]
butterflies <- butterflies[! butterflies$year %in% "slope",]
butterflies <- butterflies[! butterflies$year %in% "se_slope",]

euring <- read.table("euring.txt", sep = "\t", header = FALSE)
colnames(euring) <- c("code", "Sci_name")

birds <- read.table("Swan_Results_Pools_Belgium_birds.csv", sep = ";", header = TRUE)
birds1 <- read.table("Swan_Results_Pools_Belgium_birds1.csv", sep = ";", header = TRUE)
birds2 <- read.table("Swan_Results_Pools_Belgium_birds2.csv", sep = ";", header = TRUE)


birds <- rbind(birds, birds1, birds2)

birds <- birds[birds$Recordtype_name %in% c("indexen", "se_indexen"),]
birds_trend <- birds[! birds$Multipl_trend_category %in% NA,]
birds_trend <- birds_trend[,c("Species_nr","Multipl_trend_category")]
birds_trend <- merge(birds_trend, euring, by.x = "Species_nr", by.y = "code", all.x = TRUE)
colnames(birds_trend) <- c("code", "Multipl_trend_category", "tax")
birds_trend <- birds_trend[,c("code", "tax", "Multipl_trend_category")]
birds$group_tax <- "BIR"
birds <- birds[, c(4,51,5,10:40, 43,44)]
colnames(birds) <- c("code", "group_tax", "tax", "type", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_bi <- birds[,c(1,2,3,4,34,35,36)]
slope_square_bi <- slope_square_bi[slope_square_bi$type %in% "indexen",]
birds <- melt(birds, id.vars = c("code", "group_tax", "tax", "type"))
birds_se <- birds[birds$type %in% "se_indexen", ]
birds_se <- birds_se[,c("code", "variable", "value")]
colnames(birds_se) <- c("code", "year", "se")
birds_index <- birds[birds$type %in% "indexen", ]
birds_index <- birds_index[, c("code", "group_tax", "tax", "variable", "value")]
colnames(birds_index) <- c("code", "group_tax", "tax", "year", "index")
birds <- merge(birds_index, birds_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )
birds$tax <- as.character(birds$tax)
birds[birds$tax %in% NA, "tax"] <- "Carrion Crow"
birds$tax <- as.factor(birds$tax)
summary(birds)

birds <- merge(birds, euring, by.x = "code", by.y = "code", all.x = TRUE)
birds <- birds[c("code", "year", "group_tax", "Sci_name", "index", "se")]
colnames(birds) <- c("code", "year", "group_tax", "tax", "index", "se")
birds <- birds[! birds$year %in% "n_square",]
birds <- birds[! birds$year %in% "slope",]
birds <- birds[! birds$year %in% "se_slope",]

birds3 <- read.table("all_Indices_All_Trends_fla.csv", sep = ";", header = TRUE)
birds3 <- birds3[birds3$Species_number %in% c("4500", "5320", "5460"),]
birds3 <- birds3[birds3$Recordtype_name %in% c("indices", "se_indices"),]
birds3_trend <- birds3[! birds3$Slope_imputed_classification %in% "",]
birds3_trend <- birds3_trend[,c("Species_number","Slope_imputed_classification")]
birds3_trend <- merge(birds3_trend, euring, by.x = "Species_number", by.y = "code", all.x = TRUE)
birds3_trend <- birds3_trend[,c(1,3,2)]
colnames(birds3_trend) <- c("code", "tax", "Multipl_trend_category")
birds3$group_tax <- "BIR"
birds3 <- birds3[, c(2,27,5,15:26,6,7,8)]
colnames(birds3) <- c("code", "group_tax", "type",  "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_bi3 <- birds3[,c(1,2,3,16,17,18)]
slope_square_bi3 <- slope_square_bi3[slope_square_bi3$type %in% "indices",]
slope_square_bi3 <- merge(slope_square_bi3, euring, by.x = "code", by.y = "code", all.x = TRUE)
slope_square_bi3 <- slope_square_bi3[,c(1,2,7,3,4,5,6)]
colnames(slope_square_bi3) <- c("code", "group_tax", "tax", "type", "n_square", "slope", "se_slope")
birds3 <- melt(birds3, id.vars = c("code", "group_tax", "type","n_square", "slope", "se_slope"))
birds3_se <- birds3[birds3$type %in% "se_indices", ]
birds3_se <- birds3_se[,c("code", "variable", "value")]
colnames(birds3_se) <- c("code", "year", "se")
birds3_index <- birds3[birds3$type %in% "indices", ]
birds3_index <- birds3_index[, c("code", "group_tax",  "variable", "value")]
colnames(birds3_index) <- c("code", "group_tax",  "year", "index")
birds3 <- merge(birds3_index, birds3_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )

birds4 <- read.table("all_Indices_All_Trends_wal.csv", sep = ";", header = TRUE)
birds4 <- birds4[birds4$Species_number %in% c("11980", "12600", "14860", "15150", "16400", "16540"),]
birds4 <- birds4[birds4$Recordtype_name %in% c("indices", "se_indices"),]
birds4_trend <- birds4[! birds4$Slope_imputed_classification %in% "",]
birds4_trend <- birds4_trend[,c("Species_number","Slope_imputed_classification")]
birds4_trend <- merge(birds4_trend, euring, by.x = "Species_number", by.y = "code", all.x = TRUE)
birds4_trend <- birds4_trend[,c(1,3,2)]
colnames(birds4_trend) <- c("code", "tax", "Multipl_trend_category")
birds4$group_tax <- "BIR"
birds4 <- birds4[, c(2,44,5,15:43,6,7,8)]
colnames(birds4) <- c("code", "group_tax", "type", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","n_square","slope","se_slope")
slope_square_bi4 <- birds4[,c(1,2,3,33,34,35)]
slope_square_bi4 <- slope_square_bi4[slope_square_bi4$type %in% "indices",]
slope_square_bi4 <- merge(slope_square_bi4, euring, by.x = "code", by.y = "code", all.x = TRUE)
slope_square_bi4 <- slope_square_bi4[,c(1,2,7,3,4,5,6)]
colnames(slope_square_bi4) <- c("code", "group_tax", "tax", "type", "n_square", "slope", "se_slope")
birds4 <- melt(birds4, id.vars = c("code", "group_tax", "type","n_square", "slope", "se_slope"))
birds4_se <- birds4[birds4$type %in% "se_indices", ]
birds4_se <- birds4_se[,c("code", "variable", "value")]
colnames(birds4_se) <- c("code", "year", "se")
birds4_index <- birds4[birds4$type %in% "indices", ]
birds4_index <- birds4_index[, c("code", "group_tax",  "variable", "value")]
colnames(birds4_index) <- c("code", "group_tax",  "year", "index")
birds4 <- merge(birds4_index, birds4_se, by.x = c("code", "year"), by.y = c("code", "year"), all = TRUE )


birds5 <- rbind(birds3, birds4)
birds5 <- merge(birds5, euring, by.x = "code", by.y = "code", all.x = TRUE)
birds5 <- birds5[c("code", "year", "group_tax", "Sci_name", "index", "se")]
colnames(birds5) <- c("code", "year", "group_tax", "tax", "index", "se")
birds5$se <- chartr(old = ",", new = ".", x = birds5$se)
birds5$index <- chartr(old = ",", new = ".", x = birds5$index)
birds5$se <- as.numeric(as.character(birds5$se))*100

birds <- rbind(birds, birds5)

slope_square_bi <- rbind(slope_square_bi,slope_square_bi3,slope_square_bi4)

#############################################################################################

bats <- read.table("bats_TRIMindices.csv", sep = ";", header = TRUE)
bat_tax <- read.table("tax_chiro.csv", sep = ";", dec = ",", header = FALSE)
colnames(bat_tax) <- c("tax", "code", "species")
bats <- merge(bats, bat_tax, by.x = "species", by.y = "species", all.x = TRUE)
bats <- bats[bats$region %in% "Belgium",]
bats$group_tax <- "ZOG"
bats <- bats[,c(11,2,12,10,3,4)]
colnames(bats) <- c("code", "year", "group_tax", "tax", "index", "se")
bats <- bats[! bats$tax %in% "Chiroptera sp.",]
bats <- bats[! bats$tax %in% "Myotis sp.",]
bats$index <- as.character(bats$index)
bats$index <- chartr(old = ",", new = ".", x = bats$index)
bats$index <- as.numeric(bats$index)
bats$se <- as.character(bats$se)
bats$se <- chartr(old = ",", new = ".", x = bats$se)
bats$se <- as.numeric(bats$se)
bats$trend <- NA

summary(bats)

bats_trend <- bats[c(1,4,7)]
colnames(bats_trend) <- c("code", "tax", "Multipl_trend_category")
bats_trend <- as.data.frame(unique(bats_trend[,c("code", "tax", "Multipl_trend_category")]))
bats <- bats[,c(1:6)]

#############################################################################################

##heterocera


################################################################################################

all_species <- rbind(birds, mammals, dragonflies, butterflies,  grasshoppers, amphibians, reptiles)
all_species$se <- as.numeric(all_species$se)
all_species$index <- as.numeric((all_species$index))
all_species$ET <- all_species$se*1.96
all_species$ICpos <- all_species$index+all_species$ET
all_species$ICneg <- all_species$index-all_species$ET
all_species$year <- as.numeric(as.character(all_species$year))

summary(all_species)

all_species_trend <- rbind(birds3_trend, birds4_trend, mammals_trend, dragonflies_trend, reptiles_trend, amphibians_trend, butterflies_trend, grasshoppers_trend, birds_trend) 

all_species <- merge(all_species, all_species_trend, by.x = c("code", "tax"), by.y = c("code", "tax"), all = TRUE)
all_species <- merge(all_species, species, by.x = "tax", by.y = "Species", all.x = TRUE)
#all_species <- all_species[,c(1:11)]
#all_species <- merge(all_species, hot_cold, by.x = "tax", by.y = "Species", all.x = TRUE)
#all_species <- all_species[,c(1:11,14)]

mode(all_species$tax)
class(all_species$tax)

all_species[all_species$tax %in% "Miliaria calandra", "Habitat"] <- "agriculture"
all_species[all_species$tax %in% "Sylvia communis", "Habitat"] <- "agriculture"
all_species[all_species$tax %in% "Sturnus vulgaris", "Habitat"] <- "agriculture"
all_species[all_species$tax %in% "Saxicola torquatus", "Habitat"] <- "agriculture"
all_species[all_species$tax %in% "Streptopelia turtur", "Habitat"] <- "agriculture"
all_species[all_species$code %in% "389", "Habitat"] <- "woodlands" # squirrel
all_species[all_species$group_tax %in% "LIB", "Habitat"] <- "wetlands" # all dragonflies in wetlands habitats
all_species[all_species$Habitat %in% NA, "Habitat"] <- "eurytopic"

all_species$tax <- as.character(all_species$tax)

#bug <- all_species[all_species$group_tax %in% "VLI" & all_species$group %in% "Cold" & all_species$Habitat %in% "open nature land",]
#bug$tax <- as.factor(bug$tax)
#summary(bug)

out <- to_remove[,c("Species")]
summary(out)

#exit <- unique(bug$tax)

#all_species <- all_species[! all_species$tax %in% exit] ## warning ! to remove when problematic butterflies are found.

summary(all_species)

all_species <- all_species[! all_species$tax %in% out,] #change tax fro factor to character
all_species <- all_species[! all_species$tax %in% " Hemianax ephippiger Burmeister, 1839",]
all_species <- all_species[! all_species$code == "401",]
all_species <- all_species[! all_species$code == "5772714",]
all_species <- all_species[! all_species$code == "4535434",]
all_species <- all_species[! all_species$code == "7809779",]

summary(all_species)

all_species$tax <- as.factor(all_species$tax)

summary(all_species)

all_species[all_species$index %in% NA, "index"] <- 0
all_species[all_species$se %in% NA, "se"] <- 0
all_species[all_species$ET %in% NA, "ET"] <- 0
all_species[all_species$ICpos %in% NA, "ICpos"] <- 0
all_species[all_species$ICneg %in% NA, "ICneg"] <- 0

#all_species <- all_species[! all_species$Multipl_trend_category %in% "Uncertain",]

summary(all_species)

##########################################################################################################

min_year <- as.data.frame(tapply(all_species$year, all_species$tax,  min))
min_year
max_year <- tapply(all_species$year, all_species$tax,  max)

synthese <- as.data.frame(unique(all_species[,c("code", "tax", "group_tax", "Multipl_trend_category", "Habitat", "category")]))
synthese <- synthese[order(synthese$tax),]
synthese <- cbind(synthese, min_year)
synthese <- cbind(synthese, max_year)
colnames(synthese) <- c("code", "tax", "group_tax", "Multipl_trend_category", "Habitat", "category", "first_year", "last_year")
synthese <- synthese[,c("code", "tax", "group_tax", "Multipl_trend_category","first_year", "last_year", "Habitat", "category")]

slopes_squares <- rbind(slope_square_am,slope_square_bi, slope_square_bu, slope_square_dr, slope_square_gr, slope_square_ma, slope_square_re)

synthese <- merge(synthese, slopes_squares, by.x = "code", by.y = "code", all.x = TRUE)
synthese <- synthese[,c(1:8,12,13,14)]
class(synthese$slope)
mode(synthese$slope)

synthese$slope <- as.character(synthese$slope)
synthese$slope <- gsub(",", ".", synthese$slope)
synthese$slope <- as.numeric(synthese$slope)
synthese$slope <- synthese$slope*100
synthese$se_slope <- as.character(synthese$se_slope)
synthese$se_slope <- gsub(",", ".", synthese$se_slope)
synthese$se_slope <- as.numeric(synthese$se_slope)
synthese$se_slope <- synthese$se_slope*100

aaaaa <- synthese[! synthese$n_square %in% NA,]
bbbbb <- aaaaa[! aaaaa$slope %in% NA,]

aaaaa <- aaaaa$n_square
bbbbb <- bbbbb$slope

cor(aaaaa,bbbbb, method = "pearson")
cor.test(aaaaa, bbbbb, method=c("pearson", "kendall", "spearman"))

write.table(synthese, "synthese_species_LPI.csv", sep = ";", col.names = TRUE , row.names = FALSE)

##########################################################################################################

species_list <- as.data.frame(unique(all_species[,c("code", "tax", "group_tax", "Habitat", "category", "STI..oC.")]))
write.table(species_list, "LPI_species_habitat.csv", sep = ";", dec = ",", col.names = TRUE, row.names = FALSE)


liste_belgium <- as.list(unique(all_species$code))
head(liste_belgium)

mingraph <- min(all_species$year)
maxgraph <- max(all_species$year)


for (i in 1 : length(liste_belgium)) {
  
  species <- all_species[all_species$code == liste_belgium[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\GRAPHS")
  
  
  ggsave (paste0("graphe_", liste_belgium[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_belgium[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_belgium[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## ALL SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_all.csv", row.names = FALSE, sep = ";", dec = ",")


ALL <- LL[,c("euring", "imputed", "se_imp", "year")]
ALL$index <- ALL[,c("imputed")]
ALL$se <- ALL[,c("se_imp")]
#ALL$taxprio <- as.character(ALL$taxprio)
#ALL[ALL$taxprio == "Emberiza calandra" & !is.na(ALL$taxprio), "taxprio"] <- "Miliaria calandra"
#ALL <- merge(ALL, g, by.x = "euring", by.y="code_euring")
#unique(ALL$nom_latin)
ALL <- ALL[,c("euring", "year", "index", "se")]
colnames(ALL) <- c("species", "year", "index", "se")
head(ALL)
summary(ALL)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI")

write.table (ALL, file = "ALL_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI.R")


###############################################################################
## agriculture

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


agriculture_species <- all_species[all_species$Habitat %in% "agriculture",]

liste_agriculture <- as.list(unique(agriculture_species$code))
head(liste_agriculture)

mingraph <- min(agriculture_species$year)
maxgraph <- max(agriculture_species$year)


for (i in 1 : length(liste_agriculture)) {
  
  species <- agriculture_species[agriculture_species$code == liste_agriculture[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\AGRICULTURE")
  
  
  ggsave (paste0("graphe_", liste_agriculture[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_agriculture[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_agriculture[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## AGRICULTURE SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_agriculture.csv", row.names = FALSE, sep = ";", dec = ",")


AGRIC <- LL[,c("euring", "imputed", "se_imp", "year")]
AGRIC$index <- AGRIC[,c("imputed")]
AGRIC$se <- AGRIC[,c("se_imp")]
#AGRIC$taxprio <- as.character(AGRIC$taxprio)
#AGRIC[AGRIC$taxprio == "Emberiza calandra" & !is.na(AGRIC$taxprio), "taxprio"] <- "Miliaria calandra"
#AGRIC <- merge(AGRIC, g, by.x = "euring", by.y="code_euring")
#unique(AGRIC$nom_latin)
AGRIC <- AGRIC[,c("euring", "year", "index", "se")]
colnames(AGRIC) <- c("species", "year", "index", "se")
head(AGRIC)
summary(AGRIC)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\AGRICULTURE")

write.table (AGRIC, file = "AGRIC_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_agriculture.R")

#########################################################################
## hot species

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


hot_species <- all_species[all_species$category %in% "hot",]

liste_hot <- as.list(unique(hot_species$code))
head(liste_hot)

mingraph <- min(hot_species$year)
maxgraph <- max(hot_species$year)


for (i in 1 : length(liste_hot)) {
  
  species <- hot_species[hot_species$code == liste_hot[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\HOT")
  
  
  ggsave (paste0("graphe_", liste_hot[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_hot[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_hot[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## HOT SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_hot.csv", row.names = FALSE, sep = ";", dec = ",")


HOT <- LL[,c("euring", "imputed", "se_imp", "year")]
HOT$index <- HOT[,c("imputed")]
HOT$se <- HOT[,c("se_imp")]
#HOT$taxprio <- as.character(HOT$taxprio)
#HOT[HOT$taxprio == "Emberiza calandra" & !is.na(HOT$taxprio), "taxprio"] <- "Miliaria calandra"
#HOT <- merge(HOT, g, by.x = "euring", by.y="code_euring")
#unique(HOT$nom_latin)
HOT <- HOT[,c("euring", "year", "index", "se")]
colnames(HOT) <- c("species", "year", "index", "se")
head(HOT)
summary(HOT)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\HOT")

write.table (HOT, file = "HOT_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_hot.R")


##############################################################################
## cold

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


cold_species <- all_species[all_species$category %in% "cold",]

liste_cold <- as.list(unique(cold_species$code))
head(liste_cold)

mingraph <- min(cold_species$year)
maxgraph <- max(cold_species$year)


for (i in 1 : length(liste_cold)) {
  
  species <- cold_species[cold_species$code == liste_cold[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\COLD")
  
  
  ggsave (paste0("graphe_", liste_cold[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_cold[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_cold[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## COLD SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_cold.csv", row.names = FALSE, sep = ";", dec = ",")


COLD <- LL[,c("euring", "imputed", "se_imp", "year")]
COLD$index <- COLD[,c("imputed")]
COLD$se <- COLD[,c("se_imp")]
#COLD$taxprio <- as.character(COLD$taxprio)
#COLD[COLD$taxprio == "Emberiza calandra" & !is.na(COLD$taxprio), "taxprio"] <- "Miliaria calandra"
#COLD <- merge(COLD, g, by.x = "euring", by.y="code_euring")
#unique(COLD$nom_latin)
COLD <- COLD[,c("euring", "year", "index", "se")]
colnames(COLD) <- c("species", "year", "index", "se")
head(COLD)
summary(COLD)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\COLD")

write.table (COLD, file = "COLD_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_cold.R")

#################################################################################################
## medium

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


medium_species <- all_species[all_species$category %in% "medium",]

liste_medium <- as.list(unique(medium_species$code))
head(liste_medium)

mingraph <- min(medium_species$year)
maxgraph <- max(medium_species$year)


for (i in 1 : length(liste_medium)) {
  
  species <- medium_species[medium_species$code == liste_medium[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MEDIUM")
  
  
  ggsave (paste0("graphe_", liste_medium[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_medium[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_medium[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## MEDIUM SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_medium.csv", row.names = FALSE, sep = ";", dec = ",")


MEDIUM <- LL[,c("euring", "imputed", "se_imp", "year")]
MEDIUM$index <- MEDIUM[,c("imputed")]
MEDIUM$se <- MEDIUM[,c("se_imp")]
#MEDIUM$taxprio <- as.character(MEDIUM$taxprio)
#MEDIUM[MEDIUM$taxprio == "Emberiza calandra" & !is.na(MEDIUM$taxprio), "taxprio"] <- "Miliaria calandra"
#MEDIUM <- merge(MEDIUM, g, by.x = "euring", by.y="code_euring")
#unique(MEDIUM$nom_latin)
MEDIUM <- MEDIUM[,c("euring", "year", "index", "se")]
colnames(MEDIUM) <- c("species", "year", "index", "se")
head(MEDIUM)
summary(MEDIUM)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MEDIUM")

write.table (MEDIUM, file = "MEDIUM_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_medium.R")

#################################################################################################

########################################################################################################
## open_natural

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


open_natural_species <- all_species[all_species$Habitat %in% "open nature land",]

liste_open_natural <- as.list(unique(open_natural_species$code))
head(liste_open_natural)

mingraph <- min(open_natural_species$year)
maxgraph <- max(open_natural_species$year)


for (i in 1 : length(liste_open_natural)) {
  
  species <- open_natural_species[open_natural_species$code == liste_open_natural[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\OPEN_NATURAL")
  
  
  ggsave (paste0("graphe_", liste_open_natural[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_open_natural[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_open_natural[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## OPEN_NATURAL SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_open_natural.csv", row.names = FALSE, sep = ";", dec = ",")


OPEN <- LL[,c("euring", "imputed", "se_imp", "year")]
OPEN$index <- OPEN[,c("imputed")]
OPEN$se <- OPEN[,c("se_imp")]
#OPEN$taxprio <- as.character(OPEN$taxprio)
#OPEN[OPEN$taxprio == "Emberiza calandra" & !is.na(OPEN$taxprio), "taxprio"] <- "Miliaria calandra"
#OPEN <- merge(OPEN, g, by.x = "euring", by.y="code_euring")
#unique(OPEN$nom_latin)
OPEN <- OPEN[,c("euring", "year", "index", "se")]
colnames(OPEN) <- c("species", "year", "index", "se")
head(OPEN)
summary(OPEN)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\OPEN_NATURAL")

write.table (OPEN, file = "OPEN_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_open_natural.R")


########################################################################################
## woodlands

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


woodlands_species <- all_species[all_species$Habitat %in% "woodlands",]

liste_woodlands <- as.list(unique(woodlands_species$code))
head(liste_woodlands)

mingraph <- min(woodlands_species$year)
maxgraph <- max(woodlands_species$year)


for (i in 1 : length(liste_woodlands)) {
  
  species <- woodlands_species[woodlands_species$code == liste_woodlands[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WOODLANDS")
  
  
  ggsave (paste0("graphe_", liste_woodlands[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_woodlands[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_woodlands[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## WOODLANDS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_woodlands.csv", row.names = FALSE, sep = ";", dec = ",")


WOODLANDS <- LL[,c("euring", "imputed", "se_imp", "year")]
WOODLANDS$index <- WOODLANDS[,c("imputed")]
WOODLANDS$se <- WOODLANDS[,c("se_imp")]
#WOODLANDS$taxprio <- as.character(WOODLANDS$taxprio)
#WOODLANDS[WOODLANDS$taxprio == "Emberiza calandra" & !is.na(WOODLANDS$taxprio), "taxprio"] <- "Miliaria calandra"
#WOODLANDS <- merge(WOODLANDS, g, by.x = "euring", by.y="code_euring")
#unique(WOODLANDS$nom_latin)
WOODLANDS <- WOODLANDS[,c("euring", "year", "index", "se")]
colnames(WOODLANDS) <- c("species", "year", "index", "se")
head(WOODLANDS)
summary(WOODLANDS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WOODLANDS")

write.table (WOODLANDS, file = "WOODLANDS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_woodlands.R")


#######################################################################
## wetlands

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


wetlands_species <- all_species[all_species$Habitat %in% "wetlands",]

liste_wetlands <- as.list(unique(wetlands_species$code))
head(liste_wetlands)

mingraph <- min(wetlands_species$year)
maxgraph <- max(wetlands_species$year)


for (i in 1 : length(liste_wetlands)) {
  
  species <- wetlands_species[wetlands_species$code == liste_wetlands[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WETLANDS")
  
  
  ggsave (paste0("graphe_", liste_wetlands[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_wetlands[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_wetlands[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## WETLANDS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_wetlands.csv", row.names = FALSE, sep = ";", dec = ",")


WETLANDS <- LL[,c("euring", "imputed", "se_imp", "year")]
WETLANDS$index <- WETLANDS[,c("imputed")]
WETLANDS$se <- WETLANDS[,c("se_imp")]
#WETLANDS$taxprio <- as.character(WETLANDS$taxprio)
#WETLANDS[WETLANDS$taxprio == "Emberiza calandra" & !is.na(WETLANDS$taxprio), "taxprio"] <- "Miliaria calandra"
#WETLANDS <- merge(WETLANDS, g, by.x = "euring", by.y="code_euring")
#unique(WETLANDS$nom_latin)
WETLANDS <- WETLANDS[,c("euring", "year", "index", "se")]
colnames(WETLANDS) <- c("species", "year", "index", "se")
head(WETLANDS)
summary(WETLANDS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WETLANDS")

write.table (WETLANDS, file = "WETLANDS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_wetlands.R")

#############################################################################################
## urban

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


urban_species <- all_species[all_species$Habitat %in% "urban",]

liste_urban <- as.list(unique(urban_species$code))
head(liste_urban)

mingraph <- min(urban_species$year)
maxgraph <- max(urban_species$year)


for (i in 1 : length(liste_urban)) {
  
  species <- urban_species[urban_species$code == liste_urban[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\URBAN")
  
  
  ggsave (paste0("graphe_", liste_urban[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_urban[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_urban[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## URBAN SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_urban.csv", row.names = FALSE, sep = ";", dec = ",")


URBAN <- LL[,c("euring", "imputed", "se_imp", "year")]
URBAN$index <- URBAN[,c("imputed")]
URBAN$se <- URBAN[,c("se_imp")]
#URBAN$taxprio <- as.character(URBAN$taxprio)
#URBAN[URBAN$taxprio == "Emberiza calandra" & !is.na(URBAN$taxprio), "taxprio"] <- "Miliaria calandra"
#URBAN <- merge(URBAN, g, by.x = "euring", by.y="code_euring")
#unique(URBAN$nom_latin)
URBAN <- URBAN[,c("euring", "year", "index", "se")]
colnames(URBAN) <- c("species", "year", "index", "se")
head(URBAN)
summary(URBAN)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\URBAN")

write.table (URBAN, file = "URBAN_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_urban.R")


#############################################################################################
## eurytopique

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


eurytopique_species <- all_species[all_species$Habitat %in% "eurytopic",]

liste_eurytopique <- as.list(unique(eurytopique_species$code))
head(liste_eurytopique)

mingraph <- min(eurytopique_species$year)
maxgraph <- max(eurytopique_species$year)


for (i in 1 : length(liste_eurytopique)) {
  
  species <- eurytopique_species[eurytopique_species$code == liste_eurytopique[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\EURYTOPIQUE")
  
  
  ggsave (paste0("graphe_", liste_eurytopique[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_eurytopique[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_eurytopique[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## EURYTOPIQUE SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_eurytopique.csv", row.names = FALSE, sep = ";", dec = ",")


EURYTOPIQUE <- LL[,c("euring", "imputed", "se_imp", "year")]
EURYTOPIQUE$index <- EURYTOPIQUE[,c("imputed")]
EURYTOPIQUE$se <- EURYTOPIQUE[,c("se_imp")]
#EURYTOPIQUE$taxprio <- as.character(EURYTOPIQUE$taxprio)
#EURYTOPIQUE[EURYTOPIQUE$taxprio == "Emberiza calandra" & !is.na(EURYTOPIQUE$taxprio), "taxprio"] <- "Miliaria calandra"
#EURYTOPIQUE <- merge(EURYTOPIQUE, g, by.x = "euring", by.y="code_euring")
#unique(EURYTOPIQUE$nom_latin)
EURYTOPIQUE <- EURYTOPIQUE[,c("euring", "year", "index", "se")]
colnames(EURYTOPIQUE) <- c("species", "year", "index", "se")
head(EURYTOPIQUE)
summary(EURYTOPIQUE)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\EURYTOPIQUE")

write.table (EURYTOPIQUE, file = "EURYTOPIQUE_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_eurytopique.R")


###################################################################################


###################################################################################
## birds

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


birds_species <- all_species[all_species$group_tax %in% "BIR",]

liste_birds <- as.list(unique(birds_species$code))
head(liste_birds)

mingraph <- min(birds_species$year)
maxgraph <- max(birds_species$year)


for (i in 1 : length(liste_birds)) {
  
  species <- birds_species[birds_species$code == liste_birds[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\BIRDS")
  
  
  ggsave (paste0("graphe_", liste_birds[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_birds[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_birds[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## BIRDS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_birds.csv", row.names = FALSE, sep = ";", dec = ",")


BIRDS <- LL[,c("euring", "imputed", "se_imp", "year")]
BIRDS$index <- BIRDS[,c("imputed")]
BIRDS$se <- BIRDS[,c("se_imp")]
#BIRDS$taxprio <- as.character(BIRDS$taxprio)
#BIRDS[BIRDS$taxprio == "Emberiza calandra" & !is.na(BIRDS$taxprio), "taxprio"] <- "Miliaria calandra"
#BIRDS <- merge(BIRDS, g, by.x = "euring", by.y="code_euring")
#unique(BIRDS$nom_latin)
BIRDS <- BIRDS[,c("euring", "year", "index", "se")]
colnames(BIRDS) <- c("species", "year", "index", "se")
head(BIRDS)
summary(BIRDS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\BIRDS")

write.table (BIRDS, file = "BIRDS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_birds.R")

#########################################################################################################

## amphibians

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


amphibians_species <- all_species[all_species$group_tax %in% "AMF",]

liste_amphibians <- as.list(unique(amphibians_species$code))
head(liste_amphibians)

mingraph <- min(amphibians_species$year)
maxgraph <- max(amphibians_species$year)


for (i in 1 : length(liste_amphibians)) {
  
  species <- amphibians_species[amphibians_species$code == liste_amphibians[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\AMPHIBIANS")
  
  
  ggsave (paste0("graphe_", liste_amphibians[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_amphibians[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_amphibians[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## AMPHIBIANS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_amphibians.csv", row.names = FALSE, sep = ";", dec = ",")


AMPHIBIANS <- LL[,c("euring", "imputed", "se_imp", "year")]
AMPHIBIANS$index <- AMPHIBIANS[,c("imputed")]
AMPHIBIANS$se <- AMPHIBIANS[,c("se_imp")]
#AMPHIBIANS$taxprio <- as.character(AMPHIBIANS$taxprio)
#AMPHIBIANS[AMPHIBIANS$taxprio == "Emberiza calandra" & !is.na(AMPHIBIANS$taxprio), "taxprio"] <- "Miliaria calandra"
#AMPHIBIANS <- merge(AMPHIBIANS, g, by.x = "euring", by.y="code_euring")
#unique(AMPHIBIANS$nom_latin)
AMPHIBIANS <- AMPHIBIANS[,c("euring", "year", "index", "se")]
colnames(AMPHIBIANS) <- c("species", "year", "index", "se")
head(AMPHIBIANS)
summary(AMPHIBIANS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\AMPHIBIANS")

write.table (AMPHIBIANS, file = "AMPHIBIANS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_amphibians.R")

####################################################################################################
## moths

#setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


#moths_species <- all_species[all_species$group_tax %in% "MOT",]

#liste_moths <- as.list(unique(moths_species$code))
#head(liste_moths)

#mingraph <- min(moths_species$year)
#maxgraph <- max(moths_species$year)


#for (i in 1 : length(liste_moths)) {
  
 # species <- moths_species[moths_species$code == liste_moths[i],]
  
  
#  XYZ <- ggplot(species, aes(x = year, y = index)) +
 #   geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
 #               alpha=0.2, fill = "#B5B276") +
  #  geom_point(size = 4, color = "#6F603D", shape = 18 )+
   # geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
  #  xlab("") + ylab("Annual index") +
   # theme_bw(12) +
    #scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    #ylim(0,max(species$ICpos))+
    #geom_hline(yintercept=100, linetype="dashed")+
    #ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    #theme_classic()+
    #theme(axis.text.x = element_text(angle=90))+
    #theme(legend.position='bottom')
#  print (XYZ)
  
 # setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MOTHS")
  
  
  #ggsave (paste0("graphe_", liste_moths[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  #ggsave (paste0("graphe_", liste_moths[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  #write.table(species, (paste0("index_", liste_moths[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
#}


## MOTHS SPECIES

#listeidx <- list.files(path = ".", pattern = "^index")
#listeidx
#ll <- length(listeidx)
#ll

#LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
#LL

#for (f in 1 : ll){
  
 # LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  #LLL
#  LL <- rbind (LL,LLL) 
  
#}

#colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

#LL <- unique(LL)
#LL

#write.table (LL, file = "Indices_moths.csv", row.names = FALSE, sep = ";", dec = ",")


#MOTHS <- LL[,c("euring", "imputed", "se_imp", "year")]
#MOTHS$index <- MOTHS[,c("imputed")]
#MOTHS$se <- MOTHS[,c("se_imp")]
#MOTHS$taxprio <- as.character(MOTHS$taxprio)
#MOTHS[MOTHS$taxprio == "Emberiza calandra" & !is.na(MOTHS$taxprio), "taxprio"] <- "Miliaria calandra"
#MOTHS <- merge(MOTHS, g, by.x = "euring", by.y="code_euring")
#unique(MOTHS$nom_latin)
#MOTHS <- MOTHS[,c("euring", "year", "index", "se")]
#colnames(MOTHS) <- c("species", "year", "index", "se")
#head(MOTHS)
#summary(MOTHS)

#setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MOTHS")

#write.table (MOTHS, file = "MOTHS_MSI.txt", row.names = FALSE, sep = "\t")

#############################################################################################################

## reptiles

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


reptiles_species <- all_species[all_species$group_tax %in% "REP",]

liste_reptiles <- as.list(unique(reptiles_species$code))
head(liste_reptiles)

mingraph <- min(reptiles_species$year)
maxgraph <- max(reptiles_species$year)


for (i in 1 : length(liste_reptiles)) {
  
  species <- reptiles_species[reptiles_species$code == liste_reptiles[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\REPTILES")
  
  
  ggsave (paste0("graphe_", liste_reptiles[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_reptiles[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_reptiles[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## REPTILES SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_reptiles.csv", row.names = FALSE, sep = ";", dec = ",")


REPTILES <- LL[,c("euring", "imputed", "se_imp", "year")]
REPTILES$index <- REPTILES[,c("imputed")]
REPTILES$se <- REPTILES[,c("se_imp")]
#REPTILES$taxprio <- as.character(REPTILES$taxprio)
#REPTILES[REPTILES$taxprio == "Emberiza calandra" & !is.na(REPTILES$taxprio), "taxprio"] <- "Miliaria calandra"
#REPTILES <- merge(REPTILES, g, by.x = "euring", by.y="code_euring")
#unique(REPTILES$nom_latin)
REPTILES <- REPTILES[,c("euring", "year", "index", "se")]
colnames(REPTILES) <- c("species", "year", "index", "se")
head(REPTILES)
summary(REPTILES)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\REPTILES")

write.table (REPTILES, file = "REPTILES_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_reptiles.R")


##################################################################################################################

## mammals

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


mammals_species <- all_species[all_species$group_tax %in% "ZOG",]

liste_mammals <- as.list(unique(mammals_species$code))
head(liste_mammals)

mingraph <- min(mammals_species$year)
maxgraph <- max(mammals_species$year)


for (i in 1 : length(liste_mammals)) {
  
  species <- mammals_species[mammals_species$code == liste_mammals[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MAMMALS")
  
  
  ggsave (paste0("graphe_", liste_mammals[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_mammals[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_mammals[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## MAMMALS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_mammals.csv", row.names = FALSE, sep = ";", dec = ",")


MAMMALS <- LL[,c("euring", "imputed", "se_imp", "year")]
MAMMALS$index <- MAMMALS[,c("imputed")]
MAMMALS$se <- MAMMALS[,c("se_imp")]
#MAMMALS$taxprio <- as.character(MAMMALS$taxprio)
#MAMMALS[MAMMALS$taxprio == "Emberiza calandra" & !is.na(MAMMALS$taxprio), "taxprio"] <- "Miliaria calandra"
#MAMMALS <- merge(MAMMALS, g, by.x = "euring", by.y="code_euring")
#unique(MAMMALS$nom_latin)
MAMMALS <- MAMMALS[,c("euring", "year", "index", "se")]
colnames(MAMMALS) <- c("species", "year", "index", "se")
head(MAMMALS)
summary(MAMMALS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\MAMMALS")

write.table (MAMMALS, file = "MAMMALS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_mammals.R")


####################################################################################################################

## dragonflies

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


dragonflies_species <- all_species[all_species$group_tax %in% "LIB",]

liste_dragonflies <- as.list(unique(dragonflies_species$code))
head(liste_dragonflies)

mingraph <- min(dragonflies_species$year)
maxgraph <- max(dragonflies_species$year)


for (i in 1 : length(liste_dragonflies)) {
  
  species <- dragonflies_species[dragonflies_species$code == liste_dragonflies[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\DRAGONFLIES")
  
  
  ggsave (paste0("graphe_", liste_dragonflies[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_dragonflies[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_dragonflies[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## DRAGONFLIES SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_dragonflies.csv", row.names = FALSE, sep = ";", dec = ",")


DRAGONFLIES <- LL[,c("euring", "imputed", "se_imp", "year")]
DRAGONFLIES$index <- DRAGONFLIES[,c("imputed")]
DRAGONFLIES$se <- DRAGONFLIES[,c("se_imp")]
#DRAGONFLIES$taxprio <- as.character(DRAGONFLIES$taxprio)
#DRAGONFLIES[DRAGONFLIES$taxprio == "Emberiza calandra" & !is.na(DRAGONFLIES$taxprio), "taxprio"] <- "Miliaria calandra"
#DRAGONFLIES <- merge(DRAGONFLIES, g, by.x = "euring", by.y="code_euring")
#unique(DRAGONFLIES$nom_latin)
DRAGONFLIES <- DRAGONFLIES[,c("euring", "year", "index", "se")]
colnames(DRAGONFLIES) <- c("species", "year", "index", "se")
head(DRAGONFLIES)
summary(DRAGONFLIES)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\DRAGONFLIES")

write.table (DRAGONFLIES, file = "DRAGONFLIES_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_dragonflies.R")


##################################################################################################################

## grasshoppers

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


grasshoppers_species <- all_species[all_species$group_tax %in% "SPR",]

liste_grasshoppers <- as.list(unique(grasshoppers_species$code))
head(liste_grasshoppers)

mingraph <- min(grasshoppers_species$year)
maxgraph <- max(grasshoppers_species$year)


for (i in 1 : length(liste_grasshoppers)) {
  
  species <- grasshoppers_species[grasshoppers_species$code == liste_grasshoppers[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\GRASSHOPPERS")
  
  
  ggsave (paste0("graphe_", liste_grasshoppers[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_grasshoppers[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_grasshoppers[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## GRASSHOPPERS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_grasshoppers.csv", row.names = FALSE, sep = ";", dec = ",")


GRASSHOPPERS <- LL[,c("euring", "imputed", "se_imp", "year")]
GRASSHOPPERS$index <- GRASSHOPPERS[,c("imputed")]
GRASSHOPPERS$se <- GRASSHOPPERS[,c("se_imp")]
#GRASSHOPPERS$taxprio <- as.character(GRASSHOPPERS$taxprio)
#GRASSHOPPERS[GRASSHOPPERS$taxprio == "Emberiza calandra" & !is.na(GRASSHOPPERS$taxprio), "taxprio"] <- "Miliaria calandra"
#GRASSHOPPERS <- merge(GRASSHOPPERS, g, by.x = "euring", by.y="code_euring")
#unique(GRASSHOPPERS$nom_latin)
GRASSHOPPERS <- GRASSHOPPERS[,c("euring", "year", "index", "se")]
colnames(GRASSHOPPERS) <- c("species", "year", "index", "se")
head(GRASSHOPPERS)
summary(GRASSHOPPERS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\GRASSHOPPERS")

write.table (GRASSHOPPERS, file = "GRASSHOPPERS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_grasshoppers.R")


#################################################################################################################

## butterflies

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


butterflies_species <- all_species[all_species$group_tax %in% "VLI",]

liste_butterflies <- as.list(unique(butterflies_species$code))
head(liste_butterflies)

mingraph <- min(butterflies_species$year)
maxgraph <- max(butterflies_species$year)


for (i in 1 : length(liste_butterflies)) {
  
  species <- butterflies_species[butterflies_species$code == liste_butterflies[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\BUTTERFLIES")
  
  
  ggsave (paste0("graphe_", liste_butterflies[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_butterflies[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_butterflies[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## BUTTERFLIES SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_butterflies.csv", row.names = FALSE, sep = ";", dec = ",")


BUTTERFLIES <- LL[,c("euring", "imputed", "se_imp", "year")]
BUTTERFLIES$index <- BUTTERFLIES[,c("imputed")]
BUTTERFLIES$se <- BUTTERFLIES[,c("se_imp")]
#BUTTERFLIES$taxprio <- as.character(BUTTERFLIES$taxprio)
#BUTTERFLIES[BUTTERFLIES$taxprio == "Emberiza calandra" & !is.na(BUTTERFLIES$taxprio), "taxprio"] <- "Miliaria calandra"
#BUTTERFLIES <- merge(BUTTERFLIES, g, by.x = "euring", by.y="code_euring")
#unique(BUTTERFLIES$nom_latin)
BUTTERFLIES <- BUTTERFLIES[,c("euring", "year", "index", "se")]
colnames(BUTTERFLIES) <- c("species", "year", "index", "se")
head(BUTTERFLIES)
summary(BUTTERFLIES)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\BUTTERFLIES")

write.table (BUTTERFLIES, file = "BUTTERFLIES_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_butterflies.R")

##################################################################################################

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


problem_species <- all_species[all_species$group_tax %in% "VLI" & all_species$group %in% "Cold" & all_species$Habitat %in% "open nature land",]

liste_problem <- as.list(unique(problem_species$code))
head(liste_problem)

mingraph <- min(problem_species$year)
maxgraph <- max(problem_species$year)


for (i in 1 : length(liste_problem)) {
  
  species <- problem_species[problem_species$code == liste_problem[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\PROBLEM")
  
  
  ggsave (paste0("graphe_", liste_problem[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_problem[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_problem[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## PROBLEM SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_problem.csv", row.names = FALSE, sep = ";", dec = ",")


PROBLEM <- LL[,c("euring", "imputed", "se_imp", "year")]
PROBLEM$index <- PROBLEM[,c("imputed")]
PROBLEM$se <- PROBLEM[,c("se_imp")]
#PROBLEM$taxprio <- as.character(PROBLEM$taxprio)
#PROBLEM[PROBLEM$taxprio == "Emberiza calandra" & !is.na(PROBLEM$taxprio), "taxprio"] <- "Miliaria calandra"
#PROBLEM <- merge(PROBLEM, g, by.x = "euring", by.y="code_euring")
#unique(PROBLEM$nom_latin)
PROBLEM <- PROBLEM[,c("euring", "year", "index", "se")]
colnames(PROBLEM) <- c("species", "year", "index", "se")
head(PROBLEM)
summary(PROBLEM)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\PROBLEM")

write.table (PROBLEM, file = "PROBLEM_MSI.txt", row.names = FALSE, sep = "\t")

#######################################################################################################################
##Grasslands


setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


grasslands_species <- all_species[all_species$Habitat %in% "grasslands",]

liste_grasslands <- as.list(unique(grasslands_species$code))
head(liste_grasslands)

mingraph <- min(grasslands_species$year)
maxgraph <- max(grasslands_species$year)


for (i in 1 : length(liste_grasslands)) {
  
  species <- grasslands_species[grasslands_species$code == liste_grasslands[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\GRASSLANDS")
  
  
  ggsave (paste0("graphe_", liste_grasslands[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_grasslands[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_grasslands[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## GRASSLANDS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend", "habitat", "group")

LL <- unique(LL)
LL

write.table (LL, file = "Indices_grasslands.csv", row.names = FALSE, sep = ";", dec = ",")


GRASS <- LL[,c("euring", "imputed", "se_imp", "year")]
GRASS$index <- GRASS[,c("imputed")]
GRASS$se <- GRASS[,c("se_imp")]
#GRASS$taxprio <- as.character(GRASS$taxprio)
#GRASS[GRASS$taxprio == "Emberiza calandra" & !is.na(GRASS$taxprio), "taxprio"] <- "Miliaria calandra"
#GRASS <- merge(GRASS, g, by.x = "euring", by.y="code_euring")
#unique(GRASS$nom_latin)
GRASS <- GRASS[,c("euring", "year", "index", "se")]
colnames(GRASS) <- c("species", "year", "index", "se")
head(GRASS)
summary(GRASS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\GRASSLANDS")

write.table (GRASS, file = "GRASS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_grasslands.R")

#########################################################################
## OPEN

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul")


ouverts_species <- all_species[all_species$Habitat %in% "agriculture" | all_species$Habitat %in% "open nature land",]

liste_ouverts <- as.list(unique(ouverts_species$code))
head(liste_ouverts)

mingraph <- min(ouverts_species$year)
maxgraph <- max(ouverts_species$year)


for (i in 1 : length(liste_ouverts)) {
  
  species <- ouverts_species[ouverts_species$code == liste_ouverts[i],]
  
  
  XYZ <- ggplot(species, aes(x = year, y = index)) +
    geom_ribbon(aes(ymin=ICneg, ymax=ICpos),
                alpha=0.2, fill = "#B5B276") +
    geom_point(size = 4, color = "#6F603D", shape = 18 )+
    geom_line(shape = 20, color = "#6F603D", size = 1) +
    #geom_errorbar(aes(ymin=ICneg, ymax=ICpos), width=.3,position=position_dodge(0.05), colour="#6F603D")+
    xlab("") + ylab("Annual index") +
    theme_bw(12) +
    scale_x_continuous("Year", breaks = c(mingraph:maxgraph), limits = c(mingraph,maxgraph), labels = c(mingraph:maxgraph))+
    ylim(0,max(species$ICpos))+
    geom_hline(yintercept=100, linetype="dashed")+
    ggtitle(paste(species$tax, "\t", species$Multipl_trend_category))+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position='bottom')
  print (XYZ)
  
  setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\OUVERTS")
  
  
  ggsave (paste0("graphe_", liste_ouverts[i],"_trends.pdf"), plot = XYZ, width=20/2.54, height = 28/2.54)
  ggsave (paste0("graphe_", liste_ouverts[i],"_trends.png"), plot = XYZ, width=20/2.54, height = 28/2.54)
  write.table(species, (paste0("index_", liste_ouverts[i],".txt")), sep = "\t", col.names = TRUE, row.names = FALSE)
  
}


## OUVERTS SPECIES

listeidx <- list.files(path = ".", pattern = "^index")
listeidx
ll <- length(listeidx)
ll

LL <- read.table(listeidx[1], sep = "\t", header = TRUE)
LL

for (f in 1 : ll){
  
  LLL <- read.table(listeidx[f], sep = "\t", header = TRUE)
  LLL
  LL <- rbind (LL,LLL) 
  
}

colnames(LL) <- c("taxprio", "euring", "year", "group_tax", "imputed", "se_imp" ,"ET", "ICpos", "ICneg", "trend",  "group", "STI","habitat","select", "category" )

LL <- unique(LL)
LL

write.table (LL, file = "Indices_ouverts.csv", row.names = FALSE, sep = ";", dec = ",")


OUVERTS <- LL[,c("euring", "imputed", "se_imp", "year")]
OUVERTS$index <- OUVERTS[,c("imputed")]
OUVERTS$se <- OUVERTS[,c("se_imp")]
#OUVERTS$taxprio <- as.character(GRASS$taxprio)
#OUVERTS[OUVERTS$taxprio == "Emberiza calandra" & !is.na(OUVERTS$taxprio), "taxprio"] <- "Miliaria calandra"
#OUVERTS <- merge(OUVERTS, g, by.x = "euring", by.y="code_euring")
#unique(OUVERTS$nom_latin)
OUVERTS <- OUVERTS[,c("euring", "year", "index", "se")]
colnames(OUVERTS) <- c("species", "year", "index", "se")
head(OUVERTS)
summary(OUVERTS)

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\OUVERTS")

write.table (OUVERTS, file = "OUVERTS_MSI.txt", row.names = FALSE, sep = "\t")
source("LPI_Belgium_MSI_ouverts.R")




#######################################################################################################################


##robustesse


##LPI without amphibians

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_AMPHIBIANS")

amph_free <- rbind(REPTILES, MAMMALS, GRASSHOPPERS, DRAGONFLIES, BUTTERFLIES, BIRDS)
write.table(amph_free, "AMPH_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_amphibians.R")

##LPI without birds

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_BIRDS")

bird_free <- rbind(REPTILES, MAMMALS, GRASSHOPPERS, DRAGONFLIES, BUTTERFLIES, AMPHIBIANS)
write.table(bird_free, "BIRD_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_birds.R")

##LPI without butterflies

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_BUTTERFLIES")

bird_free <- rbind(REPTILES, MAMMALS, GRASSHOPPERS, DRAGONFLIES, BIRDS, AMPHIBIANS)
write.table(bird_free, "BUTTERFLIE_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_butterflies.R")


##LPI without dragonflies

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_DRAGONFLIES")

bird_free <- rbind(REPTILES, MAMMALS, GRASSHOPPERS, BUTTERFLIES, BIRDS, AMPHIBIANS)
write.table(bird_free, "DRAGONFLIE_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_dragonflies.R")

##LPI without grasshoppers

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_GRASSHOPPERS")

bird_free <- rbind(REPTILES, MAMMALS, DRAGONFLIES, BUTTERFLIES, BIRDS, AMPHIBIANS)
write.table(bird_free, "GRASSHOPPER_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_grasshoppers.R")

##LPI without mammals

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_MAMMALS")

bird_free <- rbind(REPTILES, DRAGONFLIES, GRASSHOPPERS, BUTTERFLIES, BIRDS, AMPHIBIANS)
write.table(bird_free, "MAMMAL_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_mammals.R")


##LPI without reptiles

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_REPTILES")

bird_free <- rbind(MAMMALS, DRAGONFLIES, GRASSHOPPERS, BUTTERFLIES, BIRDS, AMPHIBIANS)
write.table(bird_free, "REPTILE_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_reptiles.R")

##LPI without agriculture

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_AGRICULTURE")

agric_free <- rbind(GRASS, WOODLANDS, WETLANDS, EURYTOPIQUE, OPEN, URBAN)
write.table(agric_free, "agriculture_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_agriculture.R")

##LPI without grasslands

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_GRASSLANDS")

agric_free <- rbind(AGRIC, WOODLANDS, WETLANDS, EURYTOPIQUE, OPEN, URBAN)
write.table(agric_free, "grasslands_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_grasslands.R")

##LPI without woodlands

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_WOODLANDS")

agric_free <- rbind(AGRIC, GRASS, WETLANDS, EURYTOPIQUE, OPEN, URBAN)
write.table(agric_free, "woodlands_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_woodlands.R")

##LPI without wetlands

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_WETLANDS")

agric_free <- rbind(AGRIC, GRASS, WOODLANDS, EURYTOPIQUE, OPEN, URBAN)
write.table(agric_free, "wetlands_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_wetlands.R")

##LPI without eurytopique

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_EURYTOPIQUE")

agric_free <- rbind(AGRIC, GRASS, WOODLANDS, WETLANDS, OPEN, URBAN)
write.table(agric_free, "eurytopique_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_eurytopique.R")

##LPI without open_natural

setwd("D:\\COA\\CBM_BELGIUM\\For_LPIsCalcul\\MSI\\WITHOUT_OPEN_NATURAL")

agric_free <- rbind(AGRIC, GRASS, WOODLANDS, WETLANDS, EURYTOPIQUE, URBAN)
write.table(agric_free, "open_natural_FREE.txt", row.names = FALSE, sep = "\t" )
source("LPI_Belgium_MSI_without_open_natural.R")
