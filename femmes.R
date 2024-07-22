setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")
library(dplyr)
library(ggplot2)

data <- data %>%
  mutate(Statut_soc_passe= case_when(
    Statut_soc_passe == "Bas 1"~1,
    Statut_soc_passe == "2"~ 2,
    Statut_soc_passe == "3"~ 3,
    Statut_soc_passe == "4"~ 4,
    Statut_soc_passe == "5"~ 5,
    Statut_soc_passe == "6"~ 6,
    Statut_soc_passe == "7"~ 7,
    Statut_soc_passe == "8"~ 8,
    Statut_soc_passe == "9"~ 9,
    Statut_soc_passe == "Sommet 10"~10,
    TRUE ~ NA_real_
  )) 


data$diff_statut <- data$Statut_soc_pres - data$Statut_soc_passe


gender_distribution <- data %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Genre)) %>%
  group_by(Emp_statut_rec3, Genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(gender_distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition du Genre par Statut d'Emploi",
       x = "Statut d'Emploi",
       y = "Pourcentage",
       fill = "Genre") +
  theme_minimal()

print(p)
ggsave(filename = "répartition_hf.png", plot = p, width = 8, height = 6)



#Suite de l'étude hommes/femmes

# Installation et chargement du package ggplot2 (si nécessaire)
install.packages("ggplot2")
library(ggplot2)

# Création des box plots avec le point de la moyenne
p<-ggplot(data, aes(x = Genre, y = Rem_annuel, fill = Genre)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "red") +
  labs(title = "Box plots de la rémunération annuelle par genre avec la moyenne",
       x = "Genre",
       y = "Rémunération annuelle") +
  scale_fill_manual(values = c("Femme" = "pink", "Homme" = "blue")) +
  theme_minimal()

print(p)
ggsave(filename = "boxplot.png", plot = p, width = 10, height = 18)




# Supposons que votre dataframe s'appelle data
# Filtrer les données pour obtenir les individus de genre 'Femme'
femme_data <- subset(data, Genre == "Une femme")

# Calculer le maximum de la variable Rem_annuel pour ces individus
max_rem_annuel_femme <- max(femme_data$Rem_annuel, na.rm = TRUE)

# Afficher le résultat
max_rem_annuel_femme

# Supposons que votre dataframe s'appelle data
# Filtrer les données pour obtenir les individus de genre 'Femme'
homme_data <- subset(data, Genre == "Un homme")

# Calculer le maximum de la variable Rem_annuel pour ces individus
max_rem_annuel_homme <- max(homme_data$Rem_annuel, na.rm = TRUE)

# Afficher le résultat
max_rem_annuel_homme





# Charger le package dplyr
library(dplyr)

# Calculer la moyenne de Rem_annuel pour chaque paire de variables Genre et Emp_statut_rec3
tableau <- data %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(moyenne_Rem_annuel = mean(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

# Afficher le tableau
print(tableau)


data_filtered <- data %>%
  filter(!is.na(Genre) & !is.na(Emp_statut_rec3) & !is.na(Rem_annuel))

# Calculer la moyenne de Rem_annuel pour chaque paire de variables Genre et Emp_statut_rec3
tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(moyenne_Rem_annuel = mean(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

# Créer un graphique à barres
p<- ggplot(tableau, aes(x = Emp_statut_rec3, y = moyenne_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Moyenne de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Moyenne de Rem_annuel",
       fill = "Genre") +
  theme_minimal()+
  coord_flip()
print(p)
ggsave(filename = "rem_poste.png", plot = p, width = 8, height = 6)
