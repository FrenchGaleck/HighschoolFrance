library(readr)
library(dplyr)
library(ggplot2)
library(graphics)
library(maps)
library(sf)
library(rnaturalearth)
dataFranck <- read.csv2("~/etudiant_data_mining.csv")
#je supprime des colonnes qui me servent à rien
dataFranck <- subset(dataFranck, select = -c(a_des_effectifs_form_ens, effectif_form_ens, diffusable, donnees_diffusables, effectif_ing, a_des_effectifs_ing, secret, donnees_soumises_au_secret_stat, aca_id, dep_id, uucr_id, rentree, annee))
#data2001 <- subset(dataTrier, annee_universitaire == "2001-02")
# je prends la data juste du pays car la plus intéressante
dataPays <- subset(dataFranck,niveau_geographique == "Pays")
dataPays$annee <- substr(dataPays$annee_universitaire, 1, 4)
#We will take only data with "TOTAL" 
dataPaysTotal <- subset(dataPays,regroupement == "TOTAL")
#create some groupe for do sum after
data_total2 <- group_by(dataPaysTotal, annee_universitaire)
#we create our data such like we will have only the total number of student 
data_total_annee <- summarise(data_total2, nb_etudiants = sum(effectif))
data_total_annee$annee <- substr(data_total_annee$annee_universitaire, 1, 4)
#we want the variation in percentage
min <- as.numeric(min(data_total_annee$nb_etudiants))
max <- as.numeric(max(data_total_annee$nb_etudiants))
pourcent <- ((max - min) / min) * 100
# Ajouter les années manquantes
nouvelles_annees <- data_total_annee[, c("annee", "nb_etudiants")]

# Ajout des années futures
nouvelles_annees <- rbind(nouvelles_annees, 
                          data.frame(annee = factor(2021:2080), 
                                     nb_etudiants = rep(NA, 60)))
nouvelles_annees$annee <- factor(nouvelles_annees$annee)
nouvelles_annees$annee <- as.numeric(as.character(nouvelles_annees$annee))

modele <- lm(nb_etudiants ~ annee, data = nouvelles_annees)
prediction<-predict(modele,newdata = nouvelles_annees, na.action = na.exclude)

summary(modele)
nouvelles_annees$prediction <- prediction

ggplot(data_total_annee, aes(x = annee, y = nb_etudiants, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = min, color="red") +
  geom_hline(yintercept = max, color="green") +
  annotate("text", x = 10, y = min, label = paste0("Min: ", min)) +
  annotate("text", x = 10, y = max, label = paste0("Max: ", max))+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggtitle(paste0("Number of student (since 2002-03)\nVariation in percent : ", round(pourcent, 2), "%"))

minP <- as.numeric(min(nouvelles_annees$prediction))
maxP <- as.numeric(max(nouvelles_annees$prediction))

# Création de la figure pour les prévisions
ggplot() +
  geom_line(data = data.frame(annee = nouvelles_annees$annee,
                              nb_etudiants = prediction),
            aes(x = annee, y = nb_etudiants), color = "red") +
  geom_hline(yintercept = minP, color="red") +
  geom_hline(yintercept = maxP, color="green") +
  annotate("text", x = 2008, y = 2800000, label = paste0("Min: ", minP)) +
  annotate("text", x = 2080, y = 5000057, label = paste0("Max: ", maxP))+
  ggtitle("Prévisions nombre d'étudiant jusqu'en 2080")



#now we will compute the percent of male and female each year
data_sexe <- dataPaysTotal %>% 
  group_by(annee_universitaire, sexe_de_l_etudiant) %>%
  summarise(nb_etudiants = sum(effectif)) %>%
  mutate(pourcentage= nb_etudiants/sum(nb_etudiants) * 100)
data_sexe$annee <- substr(data_sexe$annee_universitaire, 1, 4)
data_sexe <- data_sexe %>% rename(sexe = sexe_de_l_etudiant)
#ON VA garder que le min et le max des femmes
minF <- min(filter(data_sexe, sexe == "Feminin")$pourcentage)
maxF <- max(filter(data_sexe, sexe == "Feminin")$pourcentage)
ggplot(data_sexe, aes(x = annee, y = pourcentage, color = sexe, group = sexe)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = minF, color="red") +
  geom_hline(yintercept = maxF, color="green") +
  annotate("text", x = 10, y = 54, label = paste0("Min: ", minF)) +
  annotate("text", x = 10, y = 57, label = paste0("Max: ", maxF))+
  labs(title = "Répartition des étudiants par sexe et par année",
       x = "Année",
       y = "Pourcentage") +
  theme_minimal()


#pour crée la database ou il y aura secteur, nb etudiant par secteur et %
data_secteur <- dataPaysTotal %>% 
  group_by(annee_universitaire, secteur) %>%
  summarise(nb_etudiants = sum(effectif)) %>%
  mutate(pourcentage= nb_etudiants/sum(nb_etudiants) * 100)
data_secteur$annee <- substr(data_secteur$annee_universitaire, 1, 4)

maxPu <- as.numeric(max(data_secteur$pourcentage))
maxPr<- 20.82641
# trace courbe public et prive
ggplot(data_secteur, aes(x=annee, y=pourcentage, color=secteur,group=secteur)) +
  geom_line(linewidth=1) +
  ggtitle("Evolution des effectifs d'etudiants dans le public et le privé") +
  labs(x="Année universitaire", y="Pourcentage d'étudiants") +
  scale_color_manual(values = c("blue", "pink")) +
  annotate("text", x = 17, y = 14, label = paste0("MaxPR: ", maxPr)) +
  annotate("text", x = 5, y = 80, label = paste0("MaxPU: ", maxPu))+
  geom_point() +
  geom_hline(yintercept = maxPr, color="red") +
  geom_hline(yintercept = maxPu, color="green") +
  theme_minimal()

#on va voir l'evolution des autres fomrations
data_somme_regroupement <- dataPays %>% 
  group_by(annee, regroupement) %>%
  filter(regroupement != "TOTAL") %>%
  summarise(nb_etudiants = sum(effectif)) %>%
  mutate(pourcentage= nb_etudiants/sum(nb_etudiants) * 100) 

ggplot(data_somme_regroupement, aes(x=annee, y=pourcentage, color=regroupement,group=regroupement)) +
  geom_line(linewidth=1) +
  ggtitle("Autres enseignement autre que l'université") +
  labs(x="Année universitaire", y="Pourcentage d'étudiants") +
  #annotate("text", x = 17, y = 14, label = paste0("MaxPR: ", maxPr)) +
  #annotate("text", x = 5, y = 80, label = paste0("MaxPU: ", maxPu))+
  geom_point() +
  #geom_hline(yintercept = maxPr, color="red") +
  #geom_hline(yintercept = maxPu, color="green") +
  theme_minimal()
#on supprime les formations pour rendre le graph lisible
data_somme_regroupement <- data_somme_regroupement %>%
  filter(regroupement != "EC_JUR") %>%
  filter(regroupement != "CPGE")%>%
  filter(regroupement != "EPEU") %>%
  filter(regroupement != "TOTAL") %>%
  filter(regroupement != "UNIV")%>%
  filter(regroupement != "ENS")%>%
  filter(regroupement != "UT") %>%
  filter(regroupement != "INP") %>%
  filter(regroupement != "EC_PARAM")
ggplot(data_somme_regroupement, aes(x=annee, y=pourcentage, color=regroupement,group=regroupement)) +
  geom_line(linewidth=1) +
  ggtitle("Autres enseignement autre que l'université") +
  labs(x="Année universitaire", y="Pourcentage d'étudiants") +
  #annotate("text", x = 17, y = 14, label = paste0("MaxPR: ", maxPr)) +
  #annotate("text", x = 5, y = 80, label = paste0("MaxPU: ", maxPu))+
  geom_point() +
  #geom_hline(yintercept = maxPr, color="red") +
  #geom_hline(yintercept = maxPu, color="green") +
  theme_minimal()  
data_sexe_formation <- dataPays %>%
  filter(regroupement != "TOTAL") %>%
  filter(annee == "2020") %>%
  group_by(annee, rgp_formations_ou_etablissements) %>%
  summarise(total_etudiants = sum(effectif),
            femmes = sum(ifelse(sexe == 2, effectif, 0))) %>%
  mutate(pourcentage_femmes = femmes / total_etudiants * 100)

ggplot(data_sexe_formation, aes(x = pourcentage_femmes,y = reorder(rgp_formations_ou_etablissements, pourcentage_femmes))) +
  geom_col() +
  labs(x = "Pourcentage de femmes", y = "Formation") +
  geom_text(aes(label = scales::percent(pourcentage_femmes/100)), size = 3) +
  ggtitle("pourcentage de femme par formation en 2020") +
  theme_minimal()
# on garde donnée par région
data_region <- dataFranck %>%
  group_by(geo_nom) %>%
  filter(niveau_geographique == "Région") %>%
  filter(annee_universitaire=="2020-21") %>%
  filter(regroupement=="TOTAL") %>%
  summarise(total_etudiants = sum(effectif))


departement <- dataFranck %>%
  group_by(geo_nom) %>%
  filter(niveau_geographique == "Département") %>%
  filter(annee_universitaire=="2020-21") %>%
  filter(regroupement=="TOTAL") %>%
  summarise(total_etudiants = sum(effectif))

france_regions <- ne_states(country = "france", returnclass = "sf")
map_data <- merge(departement, france_regions, by.x = "geo_nom", by.y = "name")
map_data_sf <- st_as_sf(map_data)
ggplot() +
  geom_sf(data = map_data_sf, aes(fill = total_etudiants)) +
  scale_fill_viridis_c(option = "magma", trans = "log10") +
  coord_sf(xlim = c(-5, 10), ylim = c(41, 52))+
  theme_void()
