
# Objectifs -------------------------------------------------------------------------------------------------------------------------------------
# identifier les zones de qualite d adressage moindres a partir de donnees ouvertes


# chargement de librairies ----------------------------------------------------------------------------------------------------------------------
library(readr)
library(forcats)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(RColorBrewer)
library(sp)
library(sf)
library(rgdal)
library(reshape2)
library(htmlwidgets)
require(highcharter)

options(encoding = "UTF-8")
# df21<-read_csv('https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-21.csv.gz', sep=";")
# dim(df21)
# # df21<-read_delim("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-21.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# # Warning: 7462 parsing failures.
# head(df21)
# dim(df21)
# df25<-read_delim("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-25.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# df39<-read_delim("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-39.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# df58<-read_delim("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-58.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# df70<-read_delim("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-70.csv.gz", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# rm(df21)
# rm(ad_bdc)
# rm(df25)
# rm(df39)
# rm(df70)
# rm(df58)

# 
# View(df21)
# 
# unique(df21$source_position)
# unique(df21$source_nom_voie)
# 
# as.data.frame(table(df21$source_position))
# 
# 
# ggplot(df21) + geom_bar(aes(x=source_position))
# ggplot(df21) + geom_bar(aes(x=source_nom_voie))
# 
# ggplot(df21) + geom_bar(aes(x=code_insee))
# ggplot(df21) + geom_bar(aes(x=fct_infreq(as.factor(code_insee))))+ coord_flip()
# 
# nb_ad21 <- as.data.frame(table(df21$code_insee))
# summary(nb_ad21$Freq) # medianne a 134 adresses, 3/4 a 250 adresses
# 
# nb_ad21$group <- cut(nb_ad21$Freq, breaks=c(0, 75, 135, 300, max(nb_ad21$Freq)), include.lowest=TRUE, dig.lab=10)
# # df21$group <- cut(length(df21$code_insee), breaks=c(0, 75, 135, 300, max(df21$Freq)), include.lowest=TRUE, dig.lab=10)
# 
# ggplot(nb_ad21) + geom_bar(aes(x=group))+ coord_flip()
# ggplot(nb_ad21) + geom_bar(aes(x=reorder(Var1,-Freq) ,y=Freq, fill=group), width=1, stat="identity")+scale_y_log10() +theme(panel.grid.minor.x = element_blank()) + coord_flip()
# 


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
rm(ban21)
rm(ban25)
rm(ban39)
rm(ban58)
rm(ban70)
rm(ban71)
rm(ban89)
rm(ban90)

dim(ban_bfc) # 2586482 usr 19 le 04/08 # 2580575 sur 19 colonnes

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
# on peut avoir besoin d?identifier l?ensemble des lignes concern?es (y compris donc les premi?res occurrences), si l?on souhaite par exemple visualiser les doublons, autrement dit d?avoir une fonction duplicated2() dont le r?sultat serait :
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
# arrange(A,B,C)
#  View(result)
# unique(result$source)
# table(result$source)
# is.data.frame(result)

# dataset avec les meilleurs positions
ad_bfc <- bind_rows(ban_bfcfilter, result)

rm(ban_bfcfilter)
rm(result)
# nb_pos_bfc <- as.data.frame(table(ban_bfc$double_ad))
# ggplot(nb_pos_bfc) + geom_bar(aes(x=Var1,y=Freq), stat="identity")
# faire le df de priorisation du type de localisation
# rm(typo_loc)

typo_loc_df <- as.data.frame(table(ad_bfc$typ_loc))
# names(typo_loc_df) <- "typ_loc" 

typ_loc_df$order <- 0
# renommage
names(typ_loc_df) <- c("typ_loc", "value", "order")

typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='entrance', 10, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='building', 9, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='staircase', 8, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='unit', 7, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='parcel', 6, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='segment', 5, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='utility', 4, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='postal', 3, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='area', 2, typ_loc_df$order)
typ_loc_df$order <- ifelse(typ_loc_df$typ_loc=='unknown', 1, typ_loc_df$order)




# creation de la table nb_ad_bfc ------------------------------------------
# dataset filtre avec nombre et type d adresse par commune IL FAUT PRENDRE en compte le meilleur placement
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


# on supprime les tables primaires trop grosses
rm(ban_bfcfilter)
rm(ad_bfc)
rm(ban_bfc)


# Tables intermediaires -------------------------------------------------------------------------------------------------------------------------

# construire les tables intermediaire de comptages
# https://hackmd.io/4pvVFgQkTrqcGuyF-8zQOA?view
recap_nb_ad_reg <- nb_ad_bfc %>% group_by(typ_loc) %>% summarise( nb = sum(value))
recap_nb_ad_dep <- nb_ad_bfc %>% group_by(code_dep = as.character(dep), typ_loc) %>% summarise( nb = sum(value))

recap_nb_ad_dep <- recap_nb_ad_dep %>% 
  group_by(code_dep) %>%
  mutate(nb_ad_tot = sum(nb))

recap_nb_ad_dep$pct <- round((recap_nb_ad_dep$nb / recap_nb_ad_dep$nb_ad_tot)*100,1)

recap_nb_ad_epci <- merge(y=nb_ad_bfc , x=couch_com, by.y= "code_insee" , by.x="INSEE_COM")
recap_nb_ad_epci <- as.data.frame(select(recap_nb_ad_epci, - geometry))
recap_nb_ad_epci <- select(recap_nb_ad_epci, - nb_ad_tot)
# merge avec une dataframe code_epci et label epci (voir code_dept)

recap_nb_ad_epci <- recap_nb_ad_epci %>% 
  group_by(CODE_EPCI) %>%
  mutate(nb_ad_tot = sum(value))

recap_nb_ad_epci <- recap_nb_ad_epci %>% group_by(CODE_EPCI , typ_loc) %>% summarise( nb = sum(value))
# typ_loc_df <- as.data.frame(table(ban_bfc$typ_loc)) # comptage d occurence
# typ_loc_df_source <- as.data.frame(table(ban_bfc$typ_loc, ban_bfc$source))
recap_nb_ad_epci <- recap_nb_ad_epci %>% 
  group_by(CODE_EPCI) %>%
  mutate(nb_ad_tot = sum(nb))
recap_nb_ad_epci$prct <- round((recap_nb_ad_epci$nb / recap_nb_ad_epci$nb_ad_tot)*100,2)

# necessite la jointure pour les epci
# recap_nb_ad_reg <- layer_compl %>% group_by(typ_loc) %>% summarise( nb = sum(value))
# dcast a faire
# TO DO :dcast(layer_compl, "INSEE_COM"+"CODE_EPCI"~., value.var = )
# recap_nb_ad_epci <- layer_compl %>% group_by(code_dep = as.character(CODE_EPCI), typ_loc) %>% summarise( nb = sum(value))


# comment pour les EPCI ?
# passer par la table commune, rajouter le code SIRENE de l EPCI a partir de couche IGN ou table INSEE

# on rajoute l ordre
recap_nb_ad_reg$order <- 0
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='entrance', 10, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='building', 9, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='staircase', 8, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='unit', 7, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='parcel', 6, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='segment', 5, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='postal', 3, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='area', 2, recap_nb_ad_reg$order)
recap_nb_ad_reg$order <- ifelse(recap_nb_ad_reg$typ_loc=='unknown', 1, recap_nb_ad_reg$order)

recap_nb_ad_reg$color <- "rien"
# on reordonne
recap_nb_ad_reg <- arrange(recap_nb_ad_reg, order)

# on rajoute une colonne
colramp <- brewer.pal(length(unique(recap_nb_ad_reg$order)), name = 'Spectral')
recap_nb_ad_reg$color <- as.data.frame(colramp)







# on ajoute le prct
recap_nb_ad_reg$pct <- round((recap_nb_ad_reg$nb / (sum(recap_nb_ad_reg$nb)))*100,2)
#sum(recap_nb_ad_reg$pct)

recap_nb_ad_dep$order <- 0
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='entrance', 10, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='building', 9, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='staircase', 8, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='unit', 7, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='parcel', 6, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='segment', 5, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='postal', 3, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='area', 2, recap_nb_ad_dep$order)
recap_nb_ad_dep$order <- ifelse(recap_nb_ad_dep$typ_loc=='unknown', 1, recap_nb_ad_dep$order)

recap_nb_ad_dep$color <- "rien"
# on reordonne
recap_nb_ad_dep <- arrange(recap_nb_ad_dep, order)

# on rajoute une colonne
colramp <- brewer.pal(length(unique(recap_nb_ad_dep$order)), name = 'Spectral')
recap_nb_ad_dep$color <- as.data.frame(colramp)


# https://statisticsglobe.com/r-assign-fixed-colors-to-categorical-variables-in-ggplot2-plot

toto <- subset(nb_ad_bfc, typ_loc=="area" & dep=="21")
sum(toto$value)


# 
# unique(ban_bfc$source)
# source_nb <- as.data.frame(table(ban_bfc$source))

# length(unique(ban_bfc$id_ban_adresse)) #  1463595 (11/05)  1461576
# length(unique(ban_bfc$id_ban_position)) # 2580575
# length(unique(ban21$id_ban_adresse)) # 225119
# length(unique(ban21$id_ban_position)) # 405842
# 225119 adresses et plusieurs positions 405842


# creation de data affinee table traitemetn -----------------------------------------------------------------------------------------------------
# unique(ban_bfc$typ_loc)

# calcul de l indicateur
nb_ad_bfc_melt <- reshape2::dcast(nb_ad_bfc, code_insee~typ_loc, value.var = "value")
View(nb_ad_bfcm_let)

# Calcul indicateur pourcentage adresse placees -------------------------------------------------------------------------------------------------
# on passe les valeurs vides a zero
nb_ad_bfc_melt[is.na(nb_ad_bfc_melt)] <-0

# on rajoute une colonne nombre total d adresse nb_tot
nb_ad_bfc_melt$nb_tot <- rowSums(nb_ad_bfc_melt[,2:8])

# pourcentage : nombre adresse a l entree + sur le nombre d adresse total pour la commune
# EDIT : on rajoute le nombre d adresse positionne au segment (troncon de voie)
# nb_ad_bfc_melt$pct_en <- (nb_ad_bfc_melt$entrance / nb_ad_bfc_melt$nb_tot)*100
nb_ad_bfc_melt$pct_en <- round(((nb_ad_bfc_melt$entrance + nb_ad_bfc_melt$segment) / nb_ad_bfc_melt$nb_tot)*100,2)

summary(nb_ad_bfc_melt$pct_en)

View(nb_ad_bfc)
head(nb_ad_bfc_melt)


# jointureavec couche sig de commune IGN 2020 --------------------------------------------------------------------------------------------------
options = "ENCODING=UTF-8"

couch_com <-sf::st_read("C:/COPY_data_local/IGN/COMMUNE_BFC.shp")
layer_compl <- sp::merge(y=nb_ad_bfc_melt , x=couch_com, by.y= "code_insee" , by.x="INSEE_COM")

setwd(dir = "C:/COPY_data_local/ADRESSE/2020/datas")
# https://gis.stackexchange.com/questions/93441/merging-and-dissolving-shapefile-in-r

class(layer_compl)
class(couch_com)

# sauvegarde de la couche
# writeOGR(dsn='.',obj=layer_compl , layer=layer_compl, driver="ESRI Shapefile") #also you were missing the driver argument
Encoding(layer_compl$INSEE_COM) <- "UTF-8"
library(stringi)
# conv(layer_compl$INSEE_COM)

# for (col in colnames(layer_compl)){
#   Encoding(layer_compl[[col]]) <- "UTF-8"}

setwd("C:/COPY_data_local/ADRESSE/2020/datas/")
options = "ENCODING=UTF-8"
st_write(layer_compl, "adresse_recap_typloc.shp") #, layer_options='ENCODING="UTF-8"')
# Warning message:
# In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  :
# GDAL Message 1: One or several characters couldn't be converted correctly from UTF-8 to ISO-8859-1.  This warning will not be emitted anymore.

# coorddf <-  SpatialPolygonsDataFrame(layer_compl)

# connexion bdd
# library(RPostgreSQL)
# con <- dbConnect(dbDriver("PostgreSQL"), host="localhost", port="5433",dbname="postgis_24_sample", user="postgres", pass="JB221cp")

writeOGR(dsn=con, obj=layer_compl)


class(layer_compl)

typeof(layer_compl)


length(unique(nb_ad_bfc$code_insee)) # 3702



# changement de type loc --------------------------------------------------
# recode(char_vec, a = "Apple", b = "Banana")
nb_ad_bfc$typ_loc <- dplyr::recode(nb_ad_bfc$typ_loc , 
       entrance = "entrée", 
       area = "zone adressage", 
       building = "batiment" , 
       parcel = "parcelle cadastrale", 
       unit = "logement" , 
       segment ="segment de voie",
       postal = "boite à lettre")
       

table(nb_ad_bfc$typ_loc)

# Visualisation ---------------------------------------------------------------------------------------------------------------------------------
options(scipen=999)

# G1 : graphe pour la region
g1 <- ggplot(recap_nb_ad_reg) + geom_bar(aes(x=1, y=nb, fill=typ_loc), stat="identity") +
  scale_fill_manual("Type de localisation",values=c('area'='#D53E4F',
                                'postal'='#FC8D59',
                                'segment'='#FEE08B',
                                'parcel'='#FFFFBF',
                                'unit'='#E6F598',
                                'building'='#99D594',
                                'entrance'='#3288BD'),
                    breaks=c('area','postal','segment','parcel','unit','building','entrance')) +theme_bw() +
  geom_text(aes(x=1, y=nb, label=paste(round(pct,0), "%", " "), alpha=ifelse(pct>2,1,0)), position = position_stack(vjust = 0.5), show.legend = FALSE)

g1 <- g1 + coord_flip() + theme(axis.title.y = element_blank())+ labs(y="Nombre Adresse pour la r?gion", title = "Nombre d'adresses et type de localisation pour la r?gion BFC", caption = "BAN, 05/2020") 

# G2 : graphe par departemetn
g2 <- ggplot(recap_nb_ad_dep) + geom_bar(aes(x=code_dep, y=nb, fill=typ_loc), stat="identity") +
  # geom_text(aes(x=code_dep, y=nb, label=paste(round(pct,0), "%", " "), alpha=ifelse(pct>2,1,0)), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_fill_manual("Type de localisation",values=c('area'='#D53E4F',
                                'postal'='#FC8D59',
                                'segment'='#FEE08B',
                                'parcel'='#FFFFBF',
                                'unit'='#E6F598',
                                'building'='#99D594',
                                'entrance'='#3288BD'),
                    breaks=c('area','postal','segment','parcel','unit','building','entrance')) +theme_bw()


g2 <- g2 + theme(axis.title.x = element_blank())+ labs(y="Nombre Adresse", title = "Nombre d'adresse selon type par d?partements", caption = "BAN, 05/2020")

pag1<- ggpubr::ggarrange(g1, g2, nrow = 2, align = "v", legend= "bottom", common.legend = TRUE, widths = c(2, 1))

# Graphe trois par commune
g3 <- ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(code_insee, -nb_ad_tot) ,y=value, fill=typ_loc, group=code_insee), width=1, stat="identity")+
  theme_minimal() + theme(panel.grid.minor.x = element_blank(),
                          panel.grid.major.x = element_blank(), axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          legend.position = c(0.75,0.75))  + 
  scale_fill_manual("Type de localisation",values=c('area'='#D53E4F',
                                                    'postal'='#FC8D59',
                                                    'segment'='#FEE08B',
                                                    'parcel'='#FFFFBF',
                                                    'unit'='#E6F598',
                                                    'building'='#99D594',
                                                    'entrance'='#3288BD'),
                    breaks=c('area','postal','segment','parcel','unit','building','entrance'))

g3 <- g3 + scale_y_log10()+ labs(y="Nombre Adresse", x="" ,title = "Nombre d'adresse selon type", caption = "BAN, 05/2020")

# epci
g4 <- ggplot(recap_nb_ad_epci) + geom_bar(aes(x=fct_reorder(CODE_EPCI, -nb_ad_tot) ,y=nb, fill=typ_loc, group=CODE_EPCI), color="gray60",width=1, stat="identity")+
  theme_minimal() + theme(panel.grid.minor.x = element_blank(),
                          panel.grid.major.x = element_blank(), axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          legend.position = c(0.90,0.90))  + 
  scale_fill_manual("Type de localisation",values=c('area'='#D53E4F',
                                                    'postal'='#FC8D59',
                                                    'segment'='#FEE08B',
                                                    'parcel'='#FFFFBF',
                                                    'unit'='#E6F598',
                                                    'building'='#99D594',
                                                    'entrance'='#3288BD'),
                    breaks=c('area','postal','segment','parcel','unit','building','entrance'))
# scale_fill_manual("Type de localisation",values=c('area'='#D53E4F',
#                                                   'postal'='#FC8D59',
#                                                   'segment'='#FEE08B',
#                                                   'parcel'='#FFFFBF',
#                                                   'unit'='#E6F598',
#                                                   'building'='#99D594',
#                                                   'entrance'='#3288BD'),
#                   breaks=c('area','postal','segment','parcel','unit','building','entrance'))
g4 <- g4 + labs(y="Nombre Adresse", x="" ,title = "Nombre d'adresse par EPCI selon type", caption = "BAN, 05/2020") #` + scale_y_log10()


# GX : ratio du type adresse
ggplot(subset(nb_ad_bfc, typ_loc=="entrance")) + geom_boxplot(aes(x=dep, y=prct, fill=typ_loc))
ggplot(subset(nb_ad_bfc, typ_loc=="entrance")) + geom_violin(aes(x=dep, y=prct, fill=typ_loc))
ggplot(nb_ad_bfc) + geom_jitter(aes(x=dep, y=prct, color=typ_loc), alpha=0.7) +
  scale_color_manual("",values=c('area'='#D53E4F',
                                'postal'='#FC8D59',
                                'segment'='#FEE08B',
                                'parcel'='#FFFFBF',
                                'unit'='#E6F598',
                                'building'='#99D594',
                                'entrance'='#3288BD'),
                    breaks=c('area','postal','segment','parcel','unit','building','entrance')) +
  theme_bw()
 
# definition des couleurs de type d adresse
unique(typ_loc_df$typ_loc)
cols <- c("unknown" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
# p + scale_colour_manual(values = cols)

# recap de nombre d adresse pour la region
# ggplot(recap_nb_ad_reg) + geom_bar(aes(x=1, y=nb, fill=typ_loc), stat="identity") + 
#   scale_fill_manual("", values=colramp)

unique(recap_nb_ad_reg$typ_loc)


                     # labels=c("Plaque adresse","Batiment","parcelle","Voie","unit?", "Zone d'adressage","Commune"))

ggplot(recap_nb_ad_reg) + geom_bar(aes(x=1, y=nb, fill=typ_loc), stat="identity") +
  scale_fill_manual("",values=colors_pal,
                    breaks=c('entrance','building','parcel','segment','unit','postal','area'),
                    labels=c("Plaque adresse","Batiment","parcelle","Voie","unit?", "Zone d'adressage","Commune"))


RColorBrewer::display.brewer.all()
display.brewer.pal(n = length(unique(recap_nb_ad_reg$typ_loc)), name = 'Spectral')
display.brewer.pal(n = 10, name = 'Spectral')
colors_pal <- brewer.pal(10, name = 'Spectral')


g4 <- ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(nb_ad_bfc$typ_loc, -nb_ad_bfc$value), y=value), stat = "identity") +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank()) +
  geom_text(aes(x=fct_reorder(nb_ad_bfc$typ_loc, -nb_ad_bfc$value), y=value, label=value), color="white", position = position_stack(vjust = .5), angle = 90) + # alpha=ifelse(typo_loc_df$Freq>500000,1,0
  labs(x="Type de localisation", y="Nombre Adresse (en log)", title="Nombre d'adresse par type de localisation en BFC", caption = "BAN, 05/2020")

g1col <- ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(nb_ad_bfc$typ_loc, -nb_ad_bfc$value), y=value, fill=as.factor(order)), stat = "identity", color="gray60") +
  theme_minimal() + scale_y_log10()+ scale_fill_brewer(palette = "Spectral") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(0.75,0.75)) +
  geom_text(aes(x=fct_reorder(nb_ad_bfc$typ_loc, -nb_ad_bfc$value), y=value, label=value), color="gray20", position = position_stack(vjust = .5), angle = 90) + # alpha=ifelse(typ_loc_nb$Freq>500000,1,0
  labs(x="Type de localisation", y="Nombre Adresse (en log)", title="Nombre d'adresse par type de localisation en BFC", caption = "BAN, 05/2020")

g1col  + annotate("text", x = 7, y = 1500, label = "Attention, \n Une m?me adresse \n a plusieurs positions", color="red")

g3 <- ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(typ_loc,-value), y=value), stat="identity") +
  theme_minimal() + theme(panel.grid.major.x = element_blank()) +
  labs(x="Type de localisation", y="Nombre Adresse (en log)", title="Nombre d'adresse par type de localisation en BFC", caption = "BAN, 05/2020")

g3col <- ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(typ_loc, -value), y=value, fill=as.factor(nb_ad_bfc$order)), stat="identity") + 
  scale_fill_brewer(palette = "Spectral") + 
  theme_minimal()  +labs(x="Type de localisation", y="Nombre Adresse filtr?es", title="Nombre d'adresse par type de localisation en BFC", fill="type de localisation", caption = "BAN, 05/2020")

g3col + facet_wrap(nb_ad_bfc$dep~. , ncol=4)+ theme(legend.position = "bottom")

# g3 <- ggplot(nb_ad_bfc) + geom_bar(aes(x=1, y=value, fill=as.factor(nb_ad_bfc$order)), stat="identity") + 
#   scale_fill_brewer(palette = "Spectral") + coord_polar()+
#   theme_minimal()


ggarrange(g1, g3)

g2 <- ggplot(typ_loc_nb_2) + geom_bar(aes(x=fct_reorder(typ_loc_nb_2$Var1, -typ_loc_nb_2$Freq), y=Freq, fill=Var2), stat = "identity") +
  scale_y_log10() + theme_minimal() +
  theme(panel.grid.major.x = element_blank())+ theme(legend.position = "bottom") + labs(x="Type de localisation", y="Nombre", fill="Contributeur")

ggpubr::ggarrange(g1, g2, ncol = 2, align = "h")


# Visualisation interactive -----------------------------------------------

# iripg <-hchart(iris_med, "column", hcaes(group = Species, y = n, x = Sepal.Length), pointWidth = 10)
chart_dep1 <-hchart(recap_nb_ad_dep, "column", hcaes(group = code_dep , y = nb_ad_tot, x = typ_loc), pointWidth = 10)

chart_dep2 <-hchart(recap_nb_ad_dep, "column", hcaes(group = typ_loc , y = nb_ad_tot, x = code_dep), stacking = "normal")

# hc <- df2 %>% 
# hchart(
#  'column', hcaes(x = 'dose', y = 'len', group = 'supp'),
  #stacking = "normal"
#) %>%
#   hc_colors(c("#0073C2FF", "#EFC000FF"))

chart_dep2 <- recap_nb_ad_dep %>%
  hchart(
    "column", hcaes(group = typ_loc , y = nb_ad_tot, x = code_dep),
    stacking = "normal"
  ) %>%
  hc_colors(c("#D53E4F", "#FC8D59", '#FEE08B','#FFFFBF','#E6F598','#99D594', '#3288BD'))

# sauvegarde le html pour tester de taille
saveWidget(chart_dep2, file="highchart_dep2.html") # LA BONNE SOLUTION !!


# ('area'='#D53E4F',
#   'postal'='#FC8D59',
#   'segment'='#FEE08B',
#   'parcel'='#FFFFBF',
#   'unit'='#E6F598',
#   'building'='#99D594',
#   'entrance'='#3288BD')


# Automatisation des graphes par commune ----------------------------------
# faire une loop
# https://statisticsglobe.com/print-ggplot2-plot-within-for-loop-in-r
# https://stackoverflow.com/questions/20892266/multiple-plots-using-loops-in-r
# set.seed(159159)                                      # Create example data
# data <- data.frame(x = 1:100,
#                    y1 = rnorm(100),
#                    y2 = rnorm(100),
#                    y3 = rnorm(100))
# 
# for(i in 2:ncol(data)) {print(i)}
# 
# for(i in 2:ncol(data)) {                              # Printing ggplot within for-loop
#   print(ggplot(data, aes(x = x, y = data[ , i])) +
#           geom_point())
#   Sys.sleep(2)
# }
# test unitaire
subset(nb_ad_bfc, code_insee==21231)

sample_commune <- nb_ad_bfc %>% subset(code_insee==21241) %>%
  hchart(
    "column", hcaes(group = typ_loc , y = value, x = typ_loc  ),
    stacking = "normal"
  ) %>%
  hc_colors(c("#3288BD", "#99D594", '#E6F598','#FFFFBF','#FEE08B','#FC8D59', '#D53E4F'))%>%
  # hc_colors(c("#D53E4F", "#FC8D59", '#FEE08B','#FFFFBF','#E6F598','#99D594', '#3288BD'))
  hc_xAxis(title = list(text = "par type de localisation")) %>%
  hc_yAxis(title = list(text = "Nombre d'adresses")) %>%
  hc_title(text = "<b>Précision de l'adresse dans la commune</b>",align = "center")%>%
  hc_caption(text = "Données Adresse, BAN, 08/2020", align = "right")
# saveWidget(sample_commune, file="sample_example.html")

saveWidget(sample_commune, file='sample_commune.html', selfcontained = FALSE)


# graphe avec EPCI
chart-ecpi <- recap_nb_ad_epci %>% hchart(
  "column", hcaes(group = typ_loc , y = prct, x = CODE_EPCI),
  stacking = "normal"
) %>%
  hc_colors(c("#3288BD", "#99D594", '#E6F598','#FFFFBF','#FEE08B','#FC8D59', '#D53E4F'))

# https://stackoverflow.com/questions/2375587/reorder-levels-of-a-factor-without-changing-order-of-values/2375877#2375877
# df$letters = factor(df$letters, labels=c("d", "c", "b", "a"))

# https://stackoverflow.com/questions/19953898/r-define-levels-of-a-factor-in-data-frame
levels(recap_nb_ad_epci$typ_loc)

# recode levels of EPCI ---------------------------------------------------
# entrance = "entrée", 
# area = "zone adressage", 
# building = "batiment" , 
# parcel = "parcelle cadastrale", 
# unit = "logement" , 
# segment ="segment de voie",
# postal = "boite à lettre")


recap_nb_ad_epci$typ_loc = factor(recap_nb_ad_epci$typ_loc, labels=c("entrée", "zone adressage", "batiment",
                                                                     "parcelle cadastrale",
                                                                     "logement",
                                                                     "segment de voie",
                                                                     "boite à lettre"))

# for(i in  :ncol(data)) {print(i)}

list_com <- unique(nb_ad_bfc$code_insee)
length(list_com)

getwd()
setwd("C:/COPY_data_local/ADRESSE/2020/R_project/chart_noselfcontained")


for (i in 1:length(list_com)){ 
  temp <- nb_ad_bfc[nb_ad_bfc$code_insee==list_com[i],]
  #more things to do with temp
  chart_temp <- temp %>%
    hchart(
      "column", hcaes(group = typ_loc , y = value, x = typ_loc  ),
      stacking = "normal"
    ) %>%
    hc_colors(c("#3288BD", "#99D594", '#E6F598','#FFFFBF','#FEE08B','#FC8D59', '#D53E4F'))%>%
    # hc_colors(c("#D53E4F", "#FC8D59", '#FEE08B','#FFFFBF','#E6F598','#99D594', '#3288BD'))
    hc_xAxis(title = list(text = "par type de localisation")) %>%
    hc_yAxis(title = list(text = "Nombre d'adresses")) %>%
    hc_title(text = "<b>Précision de l'adresse dans la commune</b>",align = "center")%>%
    hc_caption(text = "Données Adresse, BAN, 08/2020", align = "right")
    # hchart(
    #   "column", hcaes(group = typ_loc , y = value, x = typ_loc),
    #   stacking = "normal"
    # ) %>%
    # hc_colors(c("#D53E4F", "#FC8D59", '#FEE08B','#FFFFBF','#E6F598','#99D594', '#3288BD'))
  saveWidget(chart_temp, file=paste(unique(temp$code_insee), '.html', sep=""), selfcontained = FALSE)
  }




# sauvegarde le html pour tester de taille
saveWidget(chart_dep2, file="highchart_dep2.html")



list_com[1]
list_com[12]
last(list_com)


nb_ad_bfc$num_insee <- as.integer(nb_ad_bfc$code_insee)
select(nb_ad_bfc) <- drop(nb_ad_bfc$num_insee)

min(nb_ad_bfc$num_insee)

# Traitement sur la table de recap par commune -----------------------------------------------------------------------------------------------
# on calcul le nombre de adresse par commune et leur type de localisation

levels(nb_ad_bfc$typ_loc)
unique(nb_ad_bfc$typ_loc)

nb_ad_bfc$order <- 0

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

# rajout de la colonne dep
nb_ad_bfc$dep <- substr(nb_ad_bfc$code_insee, 1, 2)
nb_ad_bfc$prct <- nb_ad_bfc$value / nb_ad_bfc$nb_ad_tot
  
# nb_ad21$value_log <- log10(nb_ad21$value)


View(nb_ad21)
View(nb_ad_bfc)
ggarrange(g1, g3)

ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_infreq(nb_ad_bfc$typ_loc), fill=as.factor(order))) +
  scale_fill_brewer(palette = "Spectral") +
  theme_pubr()+ facet_wrap(dep~., ncol = 4)# + gghighlight::gghighlight()

# ggplot(nb_ad_bfc) + geom_bar(aes(x=1, y=as.factor(order)), stat="identity") +
#   scale_fill_brewer(palette = "Spectral") +
#   theme_minimal()+ coord_polar()

# ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_infreq(nb_ad_bfc$typ_loc), fill=as.factor(order))) +
#   scale_fill_brewer(palette = "Spectral") +
#   theme_classic() 


# affichage en pourcentage => A ORDONNER
ggplot(nb_ad_bfc) + geom_bar(aes(x=fct_reorder(code_insee, -prct-nb_ad_tot) ,y=value, fill=as.factor(order)), width=1, stat="identity", position = "fill")+
  theme_minimal() + theme(panel.grid.minor.x = element_blank(),
                          panel.grid.major.x = element_blank(), axis.text.x = element_blank(),
                          axis.ticks.x = element_blank())  +
  scale_fill_brewer(palette = "Spectral")



ggplot(subset(nb_ad21, value!=0)) + geom_bar(aes(x=reorder(code_insee,-nb_ad_tot) ,y=value_log, fill=order), width=1, stat="identity")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), axis.text.x = element_blank())


#1: Transformation introduced infinite values in continuous y-axis 
# 2: Removed 1262 rows containing missing values (geom_bar). 
View(nb_ad21)


# FIBRE et représentation -------------------------------------------------
fibre_ad_complete <-sf::st_read("C:/COPY_data_local/ADRESSE/2020/datas/communes-bfc_zones_fibres.shp")
# fibre_ad_complete <- sp::merge(y=fibre_com , x=layer_compl, by.y= "INSEE_COM" , by.x="INSEE_COM")
# la fusion fibre, com+adresse est faite dans FME
# layer_compl$INSEE_COM
# fibre_com$INSEE_COM
# https://r-spatial.github.io/sf/articles/sf1.html
# ON NE GARDE que les attributs
fibre_ad_complete <- as.data.frame(fibre_ad_complete)
class(fibre_ad_complete)
## [1] "data.frame"

fibre_ad_complete$type_cor <- ifelse(fibre_ad_complete$type=='RIP PHASE 1'|fibre_ad_complete$type=='RIP PHASE 2', 'RIP', fibre_ad_complete$type)

# recap_nb_ad_epci <- recap_nb_ad_epci %>% group_by(CODE_EPCI , typ_loc) %>% summarise( nb = sum(value))

recap_fibre_ad <- fibre_ad_complete %>% group_by(INSEE_DEP, type_cor) %>% summarise( nb_com = length(INSEE_COM))
recap_fibre_pop <- fibre_ad_complete %>% group_by(type_cor) %>% summarise( nb_hab = sum(POPULATION))
recap_fibre_pop_dep <- fibre_ad_complete %>% group_by(INSEE_DEP,type_cor) %>% summarise( nb_hab = sum(POPULATION))


fibre_ad_complete$POPULATION

head(recap_fibre_ad)

chart_com_fibre <- recap_fibre_ad %>%
  hchart(
    "column", hcaes(group = type_cor , y = nb_com, x = INSEE_DEP),
    stacking = "normal"
  ) %>%hc_colors(c("#fdaa00","#33a02c","#1f78b4"))

camem_pop <- recap_fibre_pop %>%
  hchart("pie", hcaes(x = type_cor, y = nb_hab)) %>%hc_colors(c("#fdaa00","#33a02c","#1f78b4"))

# saveWidget(widget=carte_rond_proportionnel,file="Taux de décès des mères à la naissance.html")
saveWidget(widget=camem_pop,file="Test export direct hgicharter.html")

camem_pop_dep <- recap_fibre_pop_dep %>%
  hchart("column", hcaes(group=type_cor, x = INSEE_DEP, y = nb_hab)) %>%hc_colors(c("#fdaa00","#33a02c","#1f78b4"))

chart_ad_densite <- hchart(
    density(round(fibre_ad_complete$pct_en),2), 
    type = "area", name = "Pourcentage Adressage")  

# hchart(density(fibre_ad_complete$pct_en), type = "area", group= "INSEE_dep")
saveWidget(partial_bundle(camem_pop), file="nimp.html")

head(fibre_ad_complete)
d21 <- fibre_ad_complete %>% filter(INSEE_DEP == 21)
d25 <- fibre_ad_complete %>% filter(INSEE_DEP == 25)
d39 <- fibre_ad_complete %>% filter(INSEE_DEP == 39)

unique(fibre_ad_complete$type_cor)
zone_amel <- fibre_ad_complete %>% filter(type_cor == "AMEL")
zone_amii <- fibre_ad_complete %>% filter(type_cor == "AMII")
zone_rip <- fibre_ad_complete %>% filter(type_cor == "RIP")


options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

chart_ratio_zone <- hchart(
  density(zone_amel$pct_en), type = "area",
  color = "#fdaa00", name = "AMEL"
) %>%
  hc_add_series(
    density(zone_amii$pct_en), type = "area",
    color = "#33a02c",
    name = "AMII"
  ) %>%
  hc_add_series(
    density(zone_rip$pct_en), type = "area",
    color = "#1f78b4",
    name = "RIP"
  )%>%
  hc_xAxis(title = list(text = "Pourcentage d'adresse précise par type de zone")) %>%
  hc_yAxis(title = list(text = "Densité"))%>%
  hc_title(text = "<b>Priorisation des communes à expérimenter</b>",align = "center")%>%
  hc_caption(text = "Données Adresse, BAN, 08/2020, Données Fibre, Région BFC 08/2020", align = "right")

saveWidget(chart_ratio_zone, "chart_ratio_zone.html")
hc_exporting(chart_ratio_zone, filename="chart_ratio_zone.html")
# export_hc(chart_ratio_zone, "text_export_highcharter.svg")
getwd()

export_hc(chart_ratio_zone, "text_export_highcharter.svg")
hc_exporting(chart_ratio_zone, filename="text_export_hc_exporting")



# graphe Ratio adresse par departement
charte_ratio_dep <- hchart(
    density(d21$pct_en), type = "area",
    color = "#4a148c", name = "Cote d'Or"
  ) %>%
    hc_add_series(
      density(d25$pct_en), type = "area",
      color = "#1a237e",
      name = "Doubs"
    ) %>%
  hc_add_series(
    density(d39$pct_en), type = "area",
    color = "#0d47a1",
    name = "Jura"
  ) %>%
    hc_xAxis(title = list(text = "Pourcentage d'adresse précise")) %>%
   hc_yAxis(title = list(text = "Densité"))

# f <- wdata %>% filter(sex == "F")
# m <- wdata %>% filter(sex == "M")
# hc <- hchart(
#   density(m$weight), type = "area", 
#   color = "steelblue", name = "Male"
# ) %>%
#   hc_add_series(
#     density(f$weight), type = "area",
#     color = "#B71C1C", 
#     name = "Female"
#   )
  
# https://www.infoworld.com/article/3569330/how-to-create-drill-down-graphs-with-highcharter-in-r.html


# Variante avec plotly ----------------------------------------------------
library(plotly)  

# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)

fig <- plot_ly(recap_fibre_ad, x = ~INSEE_DEP, y = ~nb_com, color=~type_cor , type = 'bar', name = ~type_cor)
saveWidget(fig, "test_plotly.html")

fig <- plot_ly(sample_commune, x = ~typ_loc, y = ~value, color=~typ_loc , type = 'bar', name = ~typ_loc)
htmlwidgets::saveWidget(partial_bundle(fig), "test_plotly_partialbundle.html" , selfcontained = FALSE)

# https://plotly-r.com/saving.html
# ON PEUT OPTIMISER L EXPORT PAR DU PARTIAL BUNDLE et commande selfcontained FALSE

htmlwidgets::saveWidget(partial_bundle(fig), "test_plotly_partialbundle.html" , selfcontained = FALSE)
?partial_bundle()
htmlwidgets::saveWidget(partial_bundle(fig, type='auto', local = TRUE, minified = TRUE), "test_plotly_partialbundle.html")
# fig
?saveWidget





# Bilan rapide de surface des zones deploiement fibres-------------------------------------------------

zonage_num <-sf::st_read("T:/Géobourgogne/DONNEES/PLATEFORME/ENTRANTE/region_bfc/2020/zonage_num/zonage_numerique/zonage_numerique.shp")
head(zonage_num)
recap_surface <- zonage_num %>% group_by(type) %>% summarise( surf_km2 = sum(surface))

first_column <- c("value_1", "value_2", ...)
second_column <- c("value_1", "value_2", ...)

df <- data.frame(first_column, second_column)

zone <- c("RIP PHASE 2", "AMII", "AMEL", "RIP PHASE 1")
surf_km2 <- c(16201.0, 3281.0,11953.0,16624.0)
recap_zone_df <-data.frame(zone,surf_km2)

camemb_zone <- recap_zone_df %>%
  hchart(
    "pie", hcaes( y = surf_km2, x = zone),
    name = "Surface en km²",
    stacking = "normal"
  ) %>%
  hc_colors(c("#a6cee3", "#33a02c", '#fdaa00', '#1f78b4')) %>%
hc_title(
  text = "Surface en km² par type de Zones")

saveWidget(camemb_zone, 'recap_camemb_zone.html')


chart_ratio_zone <- hchart(
  density(zone_amel$pct_en), type = "area",
  color = "#4a148c", name = "AMEL"
) %>%
  hc_add_series(
    density(zone_amii$pct_en), type = "area",
    color = "#1a237e",
    name = "AMII"
  ) %>%
  hc_add_series(
    density(zone_rip$pct_en), type = "area",
    color = "#0d47a1",
    name = "RIP"
  )



# Priorisation FTTH RESTREINT ---------------------------------------------
options(encoding = "UTF-8")

final_prio <-sf::st_read("T:/Géobourgogne/DONNEES/PLATEFORME/ENTRANTE/region_bfc/2020/RESTREINTES/communes_ratio_adresse_fibre_ftth.shp")
final_prio <- as.data.frame(select(final_prio, - geometry))

df_final <- final_prio %>% select(INSEE_COM, POPULATION,INSEE_DEP, CODE_EPCI, nb_tot,pct_en,X_overlaps,code_zone,type_zone,date)

# on met les NA a zero
df_final[is.na(df_final)] <- 0

unique(df_final$date)
an_0 <- df_final %>% filter(date == "0")
an_2020 <- df_final %>% filter(date == "2020")
an_2021 <- df_final %>% filter(date == "2021")
an_2022 <- df_final %>% filter(date == "2022")
an_2019 <- df_final %>% filter(date == "2019")

hchart(
  density(df_final$pct_en), date = "area")

# densite par annee
# FAIRE un recap nombre de commune par annee

recap_an_com <- df_final %>% group_by(date, INSEE_DEP) %>% summarise( nb_com = length(INSEE_COM))



# hcaes(x = manufacturer, y = n, group = year),
hchart(df_final, type = "scatter",
       hcaes(x=date, y=pct_en, color=INSEE_DEP, group=INSEE_DEP))

hchart(recap_an_com, type = "bar", stacking="normal",
       hcaes(x=date, y=nb_com, color=as.factor(INSEE_DEP), group=as.factor(INSEE_DEP)))

hchart(density(an_2020$pct_en), type = "area", name = "Année 2020") %>%
  hc_add_series(
    density(an_2019$pct_en), type = "area",
    name = "Année 2019") %>%
  hc_add_series(
    density(an_2021$pct_en), type = "area",
    name = "Année 2021") %>%
  hc_add_series(
    density(an_2022$pct_en), type = "area",
    name = "Année 2022")


recap_an_com

# Test avec chartjs (optimisaton taille sorite) ---------------------------
library(devtools)
devtools::install_github("tutuchan/chartjs", ref = "dev")
# http://tutuchan.github.io/chartjs/
library(chartjs)
data("mtcars")
# documentation 
# https://rdrr.io/github/Tutuchan/chartjs/man/chartjs.html


chartjs(sample_commune$typ_loc, sample_commune$value)
?chartjs()

attach(mtcars)
# chartjs(mtcars, mtcars$mpg, qsec, labels = row.names(mtcars)) %>% cjsBar
chartjs(mtcars$mpg, mtcars$qsec , labels = row.names(mtcars))%>% cjsBar


library(chartjs)
bouboule <- chartjs(height = "500px") %>% 
  cjsPie(labels = mtcars[1:6,]) %>%
  cjsSeries(data = c(1:6))

typeof(bouboule)
class(bouboule)

saveWidget(bouboule, "bouboule.html", selfcontained = TRUE)
row.names(sample_commune) <- sample_commune$typ_loc

?saveWidget

sample_commune <- nb_ad_bfc %>% subset(code_insee==21241)
