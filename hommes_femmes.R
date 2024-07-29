setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")
library(dplyr)
library(ggplot2)

#Suite de l'étude hommes/femmes

#Trouver les valeurs extrêmes pour la rémunération annuelle
femme_data <- subset(data, Genre == "Une femme")
max(femme_data$Rem_annuel, na.rm = TRUE)

homme_data <- subset(data, Genre == "Un homme")
max(homme_data$Rem_annuel, na.rm = TRUE)



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


#Pour chaque type de poste, calculons la moyenne du salaire pour les femmes/hommes
data_filtered <- data %>%
  filter(!is.na(Genre) & !is.na(Emp_statut_rec3) & !is.na(Rem_annuel))

tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(moyenne_Rem_annuel = mean(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

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



tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(mediane_Rem_annuel = median(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(tableau, aes(x = Emp_statut_rec3, y = mediane_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Médiane de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Médiane de Rem_annuel",
       fill = "Genre") +
  theme_minimal() +
  coord_flip()
print(p)
ggsave(filename = "mediane.png", plot = p, width = 8, height = 6)


tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(ecart_type_Rem_annuel = sd(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(tableau, aes(x = Emp_statut_rec3, y = ecart_type_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Écart type de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Écart type de Rem_annuel",
       fill = "Genre") +
  theme_minimal() +
  coord_flip()

print(p)
ggsave(filename = "sd.png", plot = p, width = 8, height = 6)


#Test de significativité sur les différences de salaire

data$Rem_annuel_cat <- cut(data$Rem_annuel, 
                           breaks = quantile(data$Rem_annuel, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                           include.lowest = TRUE, 
                           labels = c("Q1", "Q2", "Q3", "Q4"))

data <- na.omit(data[c("Genre", "Rem_annuel_cat")])

tab <- table(data$Genre, data$Rem_annuel_cat)

chi2_test <- chisq.test(tab)

print(chi2_test)

#Maintenant, on fait la même chose en supprimant les valeurs abérantes
femme_data <- subset(data, Genre == "Une femme")
max_index <- which(data$Rem_annuel == max(femme_data$Rem_annuel, na.rm = TRUE) & data$Genre == "Une femme")
data <- data[-max_index, ]

homme_data <- subset(data, Genre == "Un homme")
max_index <- which(data$Rem_annuel == max(homme_data$Rem_annuel, na.rm = TRUE) & data$Genre == "Un homme")
data <- data[-max_index, ]


#On regarde quels sont les valeurs abérantes maintenant
femme_data <- subset(data, Genre == "Une femme")
max(femme_data$Rem_annuel, na.rm = TRUE)


homme_data <- subset(data, Genre == "Un homme")
max(homme_data$Rem_annuel, na.rm = TRUE)


#On refait les stat sans ces valeurs abérantes 

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
ggsave(filename = "boxplot_rec.png", plot = p, width = 10, height = 18)



#Pour chaque type de poste, calculons la moyenne du salaire pour les femmes/hommes
data_filtered <- data %>%
  filter(!is.na(Genre) & !is.na(Emp_statut_rec3) & !is.na(Rem_annuel))

tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(moyenne_Rem_annuel = mean(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

p<- ggplot(tableau, aes(x = Emp_statut_rec3, y = moyenne_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Moyenne de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Moyenne de Rem_annuel",
       fill = "Genre") +
  theme_minimal()+
  coord_flip()
print(p)
ggsave(filename = "rem_poste_rec.png", plot = p, width = 8, height = 6)



tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(mediane_Rem_annuel = median(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(tableau, aes(x = Emp_statut_rec3, y = mediane_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Médiane de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Médiane de Rem_annuel",
       fill = "Genre") +
  theme_minimal() +
  coord_flip()
print(p)
ggsave(filename = "mediane_rec.png", plot = p, width = 8, height = 6)



tableau <- data_filtered %>%
  group_by(Genre, Emp_statut_rec3) %>%
  summarise(ecart_type_Rem_annuel = sd(Rem_annuel, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(tableau, aes(x = Emp_statut_rec3, y = ecart_type_Rem_annuel, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Écart type de Rem_annuel par Emp_statut_rec3 et Genre",
       x = "Emp_statut_rec3",
       y = "Écart type de Rem_annuel",
       fill = "Genre") +
  theme_minimal() +
  coord_flip()

print(p)
ggsave(filename = "sd_rec.png", plot = p, width = 8, height = 6)


#On refait le tableau des moyennes

descriptives <- data %>%
  group_by(Genre) %>%
  summarise(
    mean_rem = mean(Rem_annuel, na.rm = TRUE),
    min_rem = min(Rem_annuel, na.rm = TRUE),
    max_rem = max(Rem_annuel, na.rm = TRUE),
    sd_rem = sd(Rem_annuel, na.rm = TRUE)
  )

print(descriptives)



#Test de significativité sur les différences de salaire

data$Rem_annuel_cat <- cut(data$Rem_annuel, 
                           breaks = quantile(data$Rem_annuel, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                           include.lowest = TRUE, 
                           labels = c("Q1", "Q2", "Q3", "Q4"))

data <- na.omit(data[c("Genre", "Rem_annuel_cat")])

tab <- table(data$Genre, data$Rem_annuel_cat)

chi2_test <- chisq.test(tab)

print(chi2_test)
