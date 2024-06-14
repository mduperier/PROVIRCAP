data <- read.csv("data_assoquest_recoded_fmt.csv")

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}


#Afficher combien il y a de réponse manquante dans rse_miss
non_missing_count2 <- sum(is.na(data$Rse_miss))
print(non_missing_count2)


#Afficher combien il y a de réponses "autres" sur la question des missions
non_missing_count <- sum(!is.na(data$Rse_miss_other))
print(non_missing_count)



#Parmis les 116 réponses manquantes sur la question des missions, 85 ont répondu 
#dans la catégorie "autre"
print(non_missing_count2-non_missing_count)
#Il y a donc en réalité 31 valeurs manquantes sur la question globalement

#est ce que c'est possible de répondre une catégorie, et autre? 
filtered_data <- data %>%
  filter(!is.na(Rse_miss) & !is.na(Rse_miss_other)) %>%
print(filtered_data)
#Non, aucune ligne


#Mission RSE
data <- data %>%
  mutate(Rse_miss_other = ifelse(is.na(Rse_miss_other), "NA", "Autre"))

data <- data %>%  
    mutate(Rse_miss =ifelse(is.na(Rse_miss), "NA",Rse_miss))



data$Rse_miss_rec <- ifelse(
  data$Rse_miss != "NA",  # Condition : Rse_miss n'est pas vide
  data$Rse_miss,                             # Valeur si vrai
  data$Rse_miss_other                        # Valeur si faux
)

#on vérifie qu'on a bien 85 autres et 114 NA:
nombre_autre <- sum(data$Rse_miss_rec == "Autre", na.rm = TRUE)
print(nombre_autre)

nombre_autre <- sum(data$Rse_miss_rec == "NA", na.rm = TRUE)
print(nombre_autre)

valeurs_uniques <- unique(data$Rse_miss)
print(valeurs_uniques)


p<- ggplot(data, aes(x = Rse_miss_rec, fill=Rse_miss_rec)) +
  geom_bar() +
  labs(title = "Types de missions",
       x = "",
       y = "Nombre d'observations")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none")+
  coord_flip()

print(p)

ggsave(filename = "type_mission.png", plot = p, width = 8, height = 6)





#Tracer le même graphe, mais en fonction du niveau hérarchique

distribution <- data %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(Rse_miss_rec!="NA") %>%
  group_by(Emp_statut_rec3, Rse_miss_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Rse_miss_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des missions par Statut d'Emploi",
       x = "Statut d'Emploi",
       y = "Pourcentage",
       fill = "Type de mission") +
  theme_minimal()

print(p)
ggsave(filename = "mission_hiérarchie.png", plot = p, width = 8, height = 6)



#Ok, maitenant on va s'intéresser à ce qu'il y a dans la variable Rse_miss_other
data <- read.csv("data_assoquest_recoded_fmt.csv")

other_data <- data %>%  
  filter(!is.na(Rse_miss_other))

print(other_data$Rse_miss_other[1:50])


# Charger les packages nécessaires
library(stringr)
library(stringi)

# Création d'une fonction pour nettoyer les chaînes de caractères
clean_text <- function(text) {
  text %>%
    stri_trans_general("Latin-ASCII") %>%  # Enlever les accents
    str_to_lower()  # Convertir en minuscules
}

# Création d'une fonction pour mettre à jour la colonne Rse_miss
update_rse_miss <- function(df) {
  df <- df %>%
    mutate(Rse_miss_other_cleaned = clean_text(Rse_miss_other)) %>%  # Nettoyer les chaînes de caractères
    mutate(Rse_miss = case_when(
      str_detect(Rse_miss_other_cleaned, "strategi") ~ "Stratégie",
      str_detect(Rse_miss_other_cleaned, "transformation") ~ "Transformation",
      str_detect(Rse_miss_other_cleaned, "environnement|durable|energ")~"Environnement",
      TRUE ~ Rse_miss
    )) %>%
    select(-Rse_miss_other_cleaned)  # Supprimer la colonne nettoyée temporaire
  return(df)
}

# Appliquer la fonction à votre base de données
data <- update_rse_miss(data)

unique_values <- unique(data$Rse_miss)
print(unique_values)

p<- ggplot(data, aes(x = Rse_miss, fill=Rse_miss)) +
  geom_bar() +
  labs(title = "Types de missions",
       x = "",
       y = "Nombre d'observations")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none")+
  coord_flip()

print(p)
