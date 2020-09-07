library(RPostgres)


# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
dbcon <- dbConnect(RPostgres::Postgres(), dbname = "species",
                   host = "studies.natagora.be", port = 5432,
                   user = "", password = '')

reqPeriode <- paste ("select * FROM rainne.herpeto_belgium")
# pour info Louis a désormais rassemblé toutes les données dans la base "species", cette base contient plusieurs schémas, dont boa qui m'intéresse ici => d'où les boa. dans la requête ci-dessus
resPeriode <- dbGetQuery(dbcon, reqPeriode) # Faire tourner la requête ; warnings acceptables (unknown (16400)) sur les colonnes 9 (programme) et 78 (geom)
summary(resPeriode)

dbDisconnect(dbcon)
str (resPeriode)


# copie du jeu de données

dHerp2 <- resPeriode
# write.table (dHerp,file="dHerp", quote=FALSE, row.names=FALSE, col.names=TRUE, sep = ";")

# supprimer les données dont la précision est supérieure à 1 km

dHerp2$precision2 <- dHerp2$precision
dHerp2[dHerp2$precision2 %in% NA, "precision2"] <- 0 # il bugge avec les NA; je les remplace par 0
dHerp2 <- dHerp2[dHerp2$precision2 < 1001,]



# ajouter une colonne site_ID issu de la coordonnée de la grille européenne EEA (carré dans lequel la donnée tombe)
###############################################################################


library(maptools)
library(rgdal)

# supprimer les lignes qui n'ont pas de coordonnées géographiques et qui font bugger la suite (657 lignes)

dHerp2[dHerp2$x_obs_org %in% NA, "x_obs_org"] <- 0 #    ça bugge avec les NA
dHerp2[dHerp2$y_obs_org %in% NA, "y_obs_org"] <- 0

dHerp2 <- dHerp2[dHerp2$x_obs_org > 0,]


# préciser les colonnes qui contiennent les coordonnées géographiques

dHerp <- dHerp2 # important de créer un jeu bis car on a besoin de celui de départ ensuite
coordinates(dHerp) <- ~x_obs_org + y_obs_org 
str(dHerp)
summary (dHerp)


# Definir le système de projection 

proj4string(dHerp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  ## WGS84


# Plot observations

plot(dHerp, pch=1)


# Combiner avec la grille européenne

# lire la grille européenne 1x1km
EUgrid <- readOGR("Z:\\Equipe\\AnneW\\2019\\wwf\\grille de référence européenne", "be_1km", verbose = TRUE) # devrait marcher aussi
proj4string(EUgrid)
plot(EUgrid)


# Projecter la grille européenne en WGS84
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
EUgridWGS84 <- spTransform(EUgrid, WGS84)

# superposer les couches
ff <- over(dHerp,EUgridWGS84)
ff$EU1x1 <- as.character(ff$CellCode)
EU1x1 <- subset(ff, select=c(EU1x1))
str(EU1x1)


# Combiner avec les observations
obs.EUgrid <- cbind(dHerp2,EU1x1)
str(obs.EUgrid)

write.table(obs.EUgrid, "Herpeto_EUgrid.csv",sep=";", row.names=F, quote = TRUE)
#write.table(obs.EUgrid, "Herpeto_EUgrid.txt",sep="\t", row.names=F)

# ça bugge je crée une table de survie sans les champ qui contiennent des signes cabalistiques en attendant... 
dHerp3 <- obs.EUgrid
dHerp3 <- dHerp3 [,c(1,2,4,5,6,7,10,11,13,22)]
write.table(dHerp3, "dHerp3.csv",sep=";", row.names=F, quote = TRUE) # ok nombre de lignes correct


dHerp3 <- read.table ("dHerp3.csv", header = TRUE, sep = ";", quote = "\"")


# transformation de la date en trois champs distincts

dHerp3$date <- as.Date (dHerp3$date, format = "%Y-%m-%d") # le contenu de cette colonne correspond à des dates
dHerp3$year <- format(dHerp3$date, "%Y")    # la fonction format permet de soustraire une partie de l'info contenue dans la date, ici l'année
dHerp3$year <-as.numeric (dHerp3$year)   # character => convertion en numérique
mode (dHerp3$year)
dHerp3$month <- format(dHerp3$date, "%m")    # idem avec mois
dHerp3$month <-as.numeric (dHerp3$month)   
mode (dHerp3$month)
dHerp3$day <- format(dHerp3$date, "%d")    
dHerp3$day <-as.numeric (dHerp3$day)   # idem avevc jour
mode (dHerp3$day)

# le code de la grille n'était pas le bon => table de conversion
grill <- read.table ("grid_file_FINAL.csv", header = TRUE, sep = ";", quote = "\"")

dHerp4 <- merge (dHerp3,grill, by.x = "EU1x1", by.y="EEA_Gridcell", all.x = TRUE)  # all.x = TRUE, all.y = TRUE garantit la présence de toutes les lignes
dHerp4 <- dHerp4[, c(2,3,4,8,9,10,11,12,13,14,15)]

# supprimer les lignes qui n'ont pas de coordonénes (175 lignes)
dHerp4[dHerp4$Site_id %in% NA, "Site_id"] <- 0 
dHerp4 <- dHerp4[dHerp4$Site_id >0,]


## ajout d'un champ jour julien
###############################

# ajout d'un champ jour julien

dHerp4$mergeJ <- paste (dHerp4$day, dHerp4$month, sep = "_")
ju <- read.table("Jjulien.csv", sep=";", header=T)

dHerp4 <- merge (dHerp4,ju, by.x = "mergeJ", by.y="mergeP", all.x = TRUE, all.y = FALSE)
dHerp4 <- dHerp4 [, c(2,3,4,5,6,7,11,12,15)]


## ajouter une colonne avec le bon species_ID
#############################################

# créer un table avec les noms existants pour créer un taxon de référence (j'ai choisi le catagalogue of life comme référence)

#Ex3 <- as.data.frame (table(dHerp$tax)) # ATTENTION rechercher les noms de taxon dans tax et dans taxprio
#Ex3b <- as.data.frame (table(dHerp$taxprio))
# Ex3 <- rbind (Ex3, Ex3b)
#Ex3 <-  as.data.frame (Ex3[,1])
#Ex3 <- unique (Ex3)
#colnames(Ex3) <- c("Esp")
#Ex3 <- as.data.frame (sort(Ex3$Esp, decreasing = FALSE))
#write.table (Ex3,file="Ex3.txt", quote=FALSE, row.names=FALSE, col.names=TRUE, sep = ";")

dictio <- read.table("dictio herpeto.csv", sep=";", header=T)
dHerp5 <- dHerp4
dHerp5$ligne <- seq (1,335191, 1)
dHerp5 <- merge (dHerp5,dictio, by.x = "taxprio", by.y="Species", all.x = TRUE, all.y = FALSE)
dHerp5 <- unique (dHerp5)

dictio2 <- dictio  
colnames (dictio2)<-c("Species2", "groupe2", "Species_ID2", "TaxLife2", "remarques2")
dHerp5 <- merge (dHerp5,dictio2, by.x = "tax", by.y="Species2", all.x = TRUE, all.y = FALSE)
dHerp5 <- unique (dHerp5)

dHerp6 <- dHerp5
dHerp6 <- dHerp6 [, c(1,2,3,4,5,7,8,9,10,11,12,13,15,16,17)]


# aller chercher les info dans la colonne tax quand taxprio est vide

dHerp6[dHerp6$Species_ID %in% NA, "Species_ID"] <- 0 
dHerp6a <- dHerp6[dHerp6$Species_ID != 0,]
dHerp6a <- dHerp6a [, c(1,2,3,4,5,6,7,8,9,10,11,12)]

dHerp6b <- dHerp6[dHerp6$Species_ID == 0,]
dHerp6b <- dHerp6b [, c(1,2,3,4,5,6,7,8,9,13,14,15)]
colnames (dHerp6b)<-c("tax", "taxprio", "n", "date","stade","Site_id","Gewest","julien", "ligne","Groupe","Species_ID", "TaxLife")

dHerp7 <- rbind (dHerp6a,dHerp6b)


# supprimer les lignes sans espèce identifiée (larves + pondscape) = 412 lignes; tot=334.791 lignes 

dHerp7[dHerp7$Species_ID %in% NA, "Species_ID"] <- 0 
dHerp7 <- dHerp7[dHerp7$Species_ID != 0,]


dHerp7$date <- as.Date (dHerp7$date, format = "%Y-%m-%d") # le contenu de cette colonne correspond à des dates
dHerp7$year <- format(dHerp7$date, "%Y")    # la fonction format permet de soustraire une partie de l'info contenue dans la date, ici l'année
dHerp7$year <-as.numeric (dHerp7$year)   # character => convertion en numérique
mode (dHerp7$year)
dHerp7$month <- format(dHerp7$date, "%m")    # idem avec mois
dHerp7$month <-as.numeric (dHerp7$month)   
mode (dHerp7$month)
dHerp7$day <- format(dHerp7$date, "%d")    
dHerp7$day <-as.numeric (dHerp7$day)   # idem avevc jour
mode (dHerp7$day)

# faire une champ numérique avec le stade

stades <- as.data.frame (table(dHerp7$stade))
stades$stage_ID <- seq (1,12, 1)
dHerp7 <- merge (dHerp7,stades, by.x = "stade", by.y="Var1", all.x = TRUE, all.y = FALSE)


# oups il manque le champ province

grill <- read.table ("grid_file_FINAL.csv", header = TRUE, sep = ";", quote = "\"")
dHerp7 <- merge (dHerp7,grill, by.x = "Site_id", by.y="Site_id", all.x = TRUE, all.y = FALSE)
dHerp7 <- dHerp7 [, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19)]


# suppression des zéros

dHerp7 <- dHerp7 [, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19)]
dHerp7 <- dHerp7[dHerp7$n > 0,]

##########################

write.table(dHerp7, "dHerp7.csv",sep=";", row.names=F, quote = TRUE) # ok nombre de lignes correct 330.170
dHerp7 <- read.table ("dHerp7.csv", header = TRUE, sep = ";", quote = "\"")

###########################


# Création des datafile

amphibiens <- dHerp7[dHerp7$Groupe == "Amphibien",]  
reptiles <- dHerp7[dHerp7$Groupe == "Reptile",]

Datafile_Amp <- amphibiens[, c(11,13,14,15,1,5,17)]
colnames (Datafile_Amp)<-c("Species_id", "Year", "Month", "Day", "Site_id", "Count", "Stage_id")

Datafile_Rept <- reptiles[, c(11,13,14,15,1,5,17)]
colnames (Datafile_Rept)<-c("Species_id", "Year", "Month", "Day", "Site_id", "Count", "Stage_id")


write.table(Datafile_Amp, "Datafile_Amp.csv",sep=";", row.names=F, quote = TRUE) 
write.table(Datafile_Rept, "Datafile_Rept.csv",sep=";", row.names=F, quote = TRUE) # ok nombre de lignes correct 330.170


# creation de l'extrafile site (un seul pour tous les groupes)

EFsite <- dHerp7[, c(1,7,18)]
EFsite <- unique (EFsite)
write.table(EFsite, "EFsite.csv",sep=";", row.names=F, quote = TRUE) 


# creation des extrafiles stade

EFstadeAmp <- amphibiens[, c(2,17)]
EFstadeAmp <- unique (EFstadeAmp)
EFstadeAmp$Stage_name <- c ("Indetermine","Adulte","Oeuf/Ponte","Subadulte","Larve","Juvenile","En_metamorphose","Immature","Aberrant", NA,"Adulte_nuptial", "Neotene","Mue")
EFstadeAmp <- EFstadeAmp[, c(2,3)]
EFstadeAmp <- EFstadeAmp[-10,]
EFstadeAmp <- EFstadeAmp [order(EFstadeAmp$stage_ID),]
write.table(EFstadeAmp, "EFstadeAmp.csv",sep=";", row.names=F, quote = TRUE) 

EFstadeRept <- reptiles[, c(2,17)]
EFstadeRept <- unique (EFstadeRept)
EFstadeRept <- EFstadeRept[-8,]
EFstadeRept <- EFstadeRept [order(EFstadeRept$stage_ID),]
EFstadeRept$Stage_name <- c ("Aberrant","Adulte","Adulte_nuptial","Immature","Indetermine", "Juvenile","Larve","Mue", "Oeuf/Ponte", "Subadulte")
EFstadeRept <- EFstadeRept[, c(2,3)]
write.table(EFstadeRept, "EFstadeRept.csv",sep=";", row.names=F, quote = TRUE) 


# creation des extrafiles taxons

EFTaxAmp <- amphibiens[, c(11,12)]
EFTaxAmp <- unique (EFTaxAmp)
EFTaxAmp <- EFTaxAmp [order(EFTaxAmp$Species_ID),]

dictio <- read.table("dictio herpeto.csv", sep=";", header=T)
dictioF <- dictio [dictio$Langue == "F",]
dictioF <- dictioF [, c(2,6)]
EFTaxAmp <- merge (EFTaxAmp,dictioF, by.x = "TaxLife", by.y="TaxLife", all.x = TRUE, all.y = FALSE)
dictioN <- dictio [dictio$Langue == "N",]
dictioN <- dictioN [, c(2,6)]
EFTaxAmp <- merge (EFTaxAmp,dictioN, by.x = "TaxLife", by.y="TaxLife", all.x = TRUE, all.y = FALSE)

write.table(EFTaxAmp, "EFTaxAmptravail.csv",sep=";", row.names=F, quote = TRUE) 

greR <- dHerp7[dHerp7$Species_ID == 78,]
quantile(greR$julien, c(.05, .95))


EFTaxRept <- reptiles[, c(11,12)]
EFTaxRept <- unique (EFTaxRept)
EFTaxRept <- EFTaxRept [order(EFTaxRept$Species_ID),]

dictio <- read.table("dictio herpeto.csv", sep=";", header=T)
dictioF <- dictio [dictio$Langue == "F",]
dictioF <- dictioF [, c(2,6)]
EFTaxRept <- merge (EFTaxRept,dictioF, by.x = "TaxLife", by.y="TaxLife", all.x = TRUE, all.y = FALSE)
dictioN <- dictio [dictio$Langue == "N",]
dictioN <- dictioN [, c(2,6)]
EFTaxRept <- merge (EFTaxRept,dictioN, by.x = "TaxLife", by.y="TaxLife", all.x = TRUE, all.y = FALSE)

write.table(EFTaxRept, "EFTaxRepttravail.csv",sep=";", row.names=F, quote = TRUE) 

greR <- dHerp7[dHerp7$Species_ID == 79,]
quantile(greR$julien, c(.05, .95))
