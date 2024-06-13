data <- read.csv("data_assoquest_recoded_fmt.csv")

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

#Afficher combien il y a de réponses "autres" sur la question des missions
non_missing_count <- sum(!is.na(data$Rse_miss_other))
print(non_missing_count)

#Afficher combien il y a de valeurs manquantes dans la question des missions
non_missing_count2 <- sum(!is.na(data$Rse_miss))
print(non_missing_count2)

#Parmis les 199 réponses manquantes sur la question des missions, 85 ont répondu 
#dans la catégorie "autre"
print(non_missing_count2-non_missing_count)
#Il y a donc en réalité 114 valeurs manquantes sur la question globalement

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
  !is.na(data$Rse_miss) & data$Rse_miss != "",  # Condition : Rse_miss n'est pas vide
  data$Rse_miss,                             # Valeur si vrai
  data$Rse_miss_other                        # Valeur si faux
)

#on vérifie qu'on a bien 85 autres et 114 NA:
nombre_autre <- sum(data$Rse_miss_other == "Autre", na.rm = TRUE)
print(nombre_autre)

nombre_autre <- sum(data$Rse_miss == "NA", na.rm = TRUE)
print(nombre_autre)

nombre_autre <- sum(is.na(data$Rse_miss), na.rm = TRUE)
print(nombre_autre)


valeurs_uniques <- unique(data$Rse_miss)
print(valeurs_uniques)


p<- ggplot(data, aes(x = Rse_miss_rec, fill=Rse_miss_rec)) +
  geom_bar() +
  labs(title = "Types de missions",
       x = "",
       y = "Nombre d'observations")+
  theme_minimal()+
  scale_fill_brewer(palette = "Pastel1")+
  theme(legend.position = "none")+
  coord_flip()

print(p)



print(data$Rse_miss[1:20])





