# Auteur : IdéO BFC, jerome boutet, ideo[at]ternum-bfc.fr

# Objectifs -------------------------------------------------------------------------------------------------------------------------------------
# identifier les zones de qualite d adressage moindres a partir de donnees ouvertes

# chargement de librairies ----------------------------------------------------------------------------------------------------------------------
require(readr)
require(forcats)
require(ggplot2)
require(tidyverse)
require(ggpubr)
require(ggrepel)
require(RColorBrewer)
require(sp)
require(sf)
require(rgdal)
require(reshape2)
# require(htmlwidgets)
# require(highcharter)

options(encoding = "UTF-8")


# recuperation BAL --------------------------------------------------------
# BAN chargement de data brutes ------------------------------------------------------------------------------------------------------------------------------------------
# 1 ligne par position et par source
# consulter https://adresse.data.gouv.fr/docs/BAN_Descriptif_Donnees.pdf
ban21<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-21.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# Warning: 6024 parsing failures.
ban25<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-25.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban39<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-39.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban58<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-58.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban70<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-70.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban71<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-71.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban89<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-89.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
ban90<-read_delim("https://adresse.data.gouv.fr/data/ban/export-api-gestion/latest/ban/ban-90.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)

# on compile le tout
ban_bfc<- bind_rows(ban21, ban25, ban39, ban58, ban70, ban71, ban89, ban90)
# on supprime les objets departementaux
rm(ban21)
rm(ban25)
rm(ban39)
rm(ban58)
rm(ban70)
rm(ban71)
rm(ban89)
rm(ban90)

dim(ban_bfc) # 2586482 usr 19 le 04/08 # 2580575 sur 19 colonnes
# 2588856

# Traitement des positions ----------------------------------------------------------------------------------------------------------------------
# TROUVER LES DOUBLONS et filter pour ne garder que la meilleur position
# ban21$double_ad <- duplicated(ban21$id_ban_adresse)
# View(ban21)

# https://joseph.larmarange.net/?Rechercher-et-visualiser-les
# FONCTION IDENTIFIANT TOUT LES DOUBLONS
duplicated2 <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c("data.frame","matrix")) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}

#Si cela est fort utile pour supprimer les doublons 
# (puisque seules les secondes occurrences sont identifi?es), 
# on peut avoir besoin d identifier l ensemble des lignes concernees (y compris donc les premieres occurrences), si l?on souhaite par exemple visualiser les doublons, autrement dit d?avoir une fonction duplicated2() dont le r?sultat serait :
ban_bfc$double_ad <- duplicated2(ban_bfc$id_ban_adresse)

# ggplot(ban_bfc) + geom_bar(aes(x=typ_loc, fill=double_ad))
ban_bfcfilter <- subset(ban_bfc, double_ad=="FALSE")

# on rajoute un ordre pour la precision de position
ban_bfc$order <- 0
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='entrance', 10, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='building', 9, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='staircase', 8, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='unit', 7, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='parcel', 6, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='segment', 5, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='utility', 4, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='postal', 3, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='area', 2, ban_bfc$order)
ban_bfc$order <- ifelse(ban_bfc$typ_loc=='unknown', 1, ban_bfc$order)

# on ne garde que le adresse, avec l ordre (position) la plus importante
result <- subset(ban_bfc, double_ad=="TRUE") %>% 
  group_by(id_ban_adresse) %>%
  filter(order ==max(order)) # %>%

# dataset avec les meilleurs positions
ad_bfc <- bind_rows(ban_bfcfilter, result)

rm(ban_bfcfilter)
rm(result)

typo_loc_df <- as.data.frame(table(ad_bfc$typ_loc))
# names(typo_loc_df) <- "typ_loc" 

# nb_ad_bfc <- as.data.frame(table(ban_bfc$code_insee, ban_bfc$typ_loc)) 
nb_ad_bfc <- as.data.frame(table(ad_bfc$code_insee, ad_bfc$typ_loc)) 

names(nb_ad_bfc) <- c("code_insee","typ_loc", "value")

nb_ad_bfc <- nb_ad_bfc %>% 
  group_by(code_insee) %>%
  mutate(nb_ad_tot = sum(value))

nb_ad_bfc <- nb_ad_bfc %>% arrange(-nb_ad_tot)
nb_ad_bfc <- subset(nb_ad_bfc, value!=0)

length(unique(nb_ad_bfc$code_insee)) # 3702 communes

nb_ad_bfc$order <- 0
# renommage
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='entrance', 10, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='building', 9, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='staircase', 8, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='unit', 7, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='parcel', 6, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='segment', 5, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='utility', 4, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='postal', 3, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='area', 2, nb_ad_bfc$order)
nb_ad_bfc$order <- ifelse(nb_ad_bfc$typ_loc=='unknown', 1, nb_ad_bfc$order)

nb_ad_bfc$dep <- substr(nb_ad_bfc$code_insee, 1, 2)
# nb_ad_bfc$prct <- (nb_ad_bfc$value / nb_ad_bfc$nb_ad_tot)*100
nb_ad_bfc$prct <- round((nb_ad_bfc$value / nb_ad_bfc$nb_ad_tot)*100,2)

# calcul de l indicateur
nb_ad_bfc_melt <- reshape2::dcast(nb_ad_bfc, code_insee~typ_loc, value.var = "value")
View(nb_ad_bfc_melt)

# on passe les valeurs vides a zero
nb_ad_bfc_melt[is.na(nb_ad_bfc_melt)] <-0
# on rajoute une colonne nombre total d adresse nb_tot
nb_ad_bfc_melt$nb_tot <- rowSums(nb_ad_bfc_melt[,2:8])

# pourcentage : nombre adresse a l entree + sur le nombre d adresse total pour la commune
# EDIT : on rajoute le nombre d adresse positionne au segment (troncon de voie)
# nb_ad_bfc_melt$pct_en <- (nb_ad_bfc_melt$entrance / nb_ad_bfc_melt$nb_tot)*100
nb_ad_bfc_melt$pct_en <- round(((nb_ad_bfc_melt$entrance + nb_ad_bfc_melt$segment) / nb_ad_bfc_melt$nb_tot)*100,2)

summary(nb_ad_bfc_melt$pct_en)


# Tables intermediaires -------------------------------------------------------------------------------------------------------------------------

# construire les tables intermediaire de comptages
# https://hackmd.io/4pvVFgQkTrqcGuyF-8zQOA?view
recap_nb_ad_reg <- nb_ad_bfc %>% group_by(typ_loc) %>% summarise( nb = sum(value))
recap_nb_ad_dep <- nb_ad_bfc %>% group_by(code_dep = as.character(dep), typ_loc) %>% summarise( nb = sum(value))


# Valorisation ------------------------------------------------------------

# jointure avec une couche communale et représentation de l'indicateur

