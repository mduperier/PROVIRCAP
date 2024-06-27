data <- read.csv("data_assoquest_recoded_fmt.csv")

#SATISFACTION SALAIRE



# Discrétisation de la variable rem_annuel en intervalles de revenus
data$rem_interval <- cut(data$Rem_annuel, breaks = seq(20000, 100000, by = 10000), include.lowest = TRUE)

# Filtrer les données pour obtenir uniquement les 'Tout à fait satisfait'
data_satisfait <- data[data$Rem_satisf == "Pas du tout satisfaisant", ]

# Compter le nombre de 'Tout à fait satisfait' par intervalle de revenus
count_data <- as.data.frame(table(data_satisfait$rem_interval))
names(count_data) <- c("rem_interval", "count")

# Tracer le graphique
p<-ggplot(count_data, aes(x = rem_interval, y = count)) +
  geom_bar(stat = "identity", fill = "#ADD8E6") +
  labs(title = "Pas du tout satisfaisant' par intervalle de revenus",
       x = "Intervalle de revenus",
       y = "Nombre de 'Tout à fait satisfait'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
ggsave(filename = "rem_insatisf.png", plot = p, width = 8, height = 6)

unique_values <- unique(data$Acteurs_trans_ens_1)
print(unique_values)




#STATUT SOCIAL

data <- data %>%
  mutate(Statut_soc_pres= case_when(
    Statut_soc_pres == "Bas 1"~1,
    Statut_soc_pres == "2"~ 2,
    Statut_soc_pres == "3"~ 3,
    Statut_soc_pres == "4"~ 4,
    Statut_soc_pres == "5"~ 5,
    Statut_soc_pres == "6"~ 6,
    Statut_soc_pres == "7"~ 7,
    Statut_soc_pres == "8"~ 8,
    Statut_soc_pres == "9"~ 9,
    Statut_soc_pres == "Sommet 10"~10,
    TRUE ~ NA_real_
  )) 

# Discrétisation de la variable rem_annuel en intervalles de revenus
data$rem_interval <- cut(data$Rem_annuel, breaks = seq(20000, 100000, by = 10000), include.lowest = TRUE)

# Calculer la moyenne de Statut_soc_pres par intervalle de revenus
mean_data <- aggregate(Statut_soc_pres ~ rem_interval, data, mean, na.rm = TRUE)

# Assurer que tous les intervalles sont présents même s'ils n'ont pas de valeurs
all_intervals <- data.frame(rem_interval = levels(data$rem_interval))
mean_data <- merge(all_intervals, mean_data, by = "rem_interval", all.x = TRUE)

# Remplacer les valeurs NA par 0 ou une autre valeur indicative
mean_data$Statut_soc_pres[is.na(mean_data$Statut_soc_pres)] <- 0

# Tracer le graphique
p<- ggplot(mean_data, aes(x = rem_interval, y = Statut_soc_pres)) +
  geom_bar(stat = "identity", fill = "#98FB98") + # Couleur pastel (vert clair)
  labs(title = "Moyenne de 'Statut_soc_pres' par intervalle de revenus",
       x = "Intervalle de revenus",
       y = "Moyenne de 'Statut_soc_pres'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(data$Statut_soc_pres[1:20])
print(p)
ggsave(filename = "rev_statut.png", plot = p, width = 8, height = 6)


#POSITION POLITIQUE EN FONCTION DU STATUT SOCIAL

distribution <- data %>%
  filter(!is.na(Pol_pos_SQ001))%>%
  filter(!is.na(Statut_soc_pres)) %>%
  group_by(Pol_pos_SQ001, Statut_soc_pres) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Pol_pos_SQ001, y = percentage, fill = Statut_soc_pres)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Statut social et politisation",
       x = "Position politique",
       y = "",
       fill = "Position sur l'échelle soliale") +
  theme_minimal()

print(p)
ggsave(filename = "statut_pol.png", plot = p, width = 8, height = 6)


#DIFFERENCE DE STATUT SOCIAL
#le présent moins le passé : si c'est positif on a augmenté dans le temps 

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

data <- data %>%
  mutate(diff_statut = ifelse(is.na(diff_statut), "NA", diff_statut))

p <- ggplot(data, aes(x = diff_statut, fill=diff_statut)) +
  geom_bar() +
  labs(title = "Evolution du statut social des répondant.es",
       x = "Evolution",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

print(p)
ggsave(filename = "diff_statut.png", plot = p, width = 8, height = 6)


#IMPORTANCE INEGALITE SELON POSITION POLITIQUE (changer le chiffre pour autres types d'ineg)

distribution <- data %>%
  filter(!is.na(Pol_pos_SQ001))%>%
  filter(!is.na(Ineg_imp_2)) %>%
  group_by(Pol_pos_SQ001, Ineg_imp_2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Pol_pos_SQ001, y = percentage, fill = Ineg_imp_2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Statut social et importance de l'inégalité de patrimoine",
       x = "Position politique",
       y = "",
       fill = "Importance des ineg de patrimoine") +
  theme_minimal()

print(p)
ggsave(filename = "ineg_pat_pol.png", plot = p, width = 8, height = 6)



#ACTEURS CLES SELON TYPE ORGANISME

library(tidyverse)

# Supposons que 'data' est votre DataFrame

# Calculer la moyenne des colonnes 'Acteurs_trans_ens_1' à 'Acteurs_trans_ens_12' pour chaque catégorie de 'Org_type'
data_means <- data %>%
  group_by(Org_type) %>%
  summarise(across(starts_with("Acteurs_trans_ens_"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Acteurs_trans_ens_"), names_to = "Acteurs_trans_ens", values_to = "mean_value")

# Créer le graphique avec ggplot2
new_labels <- c(
  "Acteurs_trans_ens_1" = "Entreprises",
  "Acteurs_trans_ens_2" = "Asso pro",
  "Acteurs_trans_ens_3" = "Recherche publique",
  "Acteurs_trans_ens_4" = "ONG militantes",
  "Acteurs_trans_ens_5" = "ONG non militantes",
  "Acteurs_trans_ens_6" = "Pouvoirs publics",
  "Acteurs_trans_ens_7" = "Consommateurs",
  "Acteurs_trans_ens_8" = "Lanceurs d'alerte",
  "Acteurs_trans_ens_9" = "Médias",
  "Acteurs_trans_ens_10" = "La justice",
  "Acteurs_trans_ens_11" = "Institutions religieuses",
  "Acteurs_trans_ens_12" = "Syndicats"
)

# Créer le graphique avec ggplot2
p <- ggplot(data_means, aes(x = Acteurs_trans_ens, y = mean_value, fill = Org_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Importance moyenne des acteurs dans la transformation durable des entreprises",
       x = "Acteurs",
       y = "Moyenne") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(labels = new_labels)+
  coord_flip()

print(p)
ggsave(filename = "acteurs_clés_type.png", plot = p, width = 8, height = 6)



#FAVORIS FACE EMPLOI SELON TYPE D ORGA

distribution <- data %>%
  filter(!is.na(Org_type))%>%
  filter(!is.na(Ineg_fav_3)) %>%
  group_by(Org_type, Ineg_fav_3) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Org_type, y = percentage, fill = Ineg_fav_3)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Position favorisée face à l'emploi et le chômage",
       x = "Type d'organisation",
       y = "",
       fill = "Position face aux inégalités d'emploi") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "fav_org.png", plot = p, width = 8, height = 6)


#POSITION FAVORISEE SELON STATUT SOCIAL

distribution <- data %>%
  filter(!is.na(Statut_soc_pres))%>%
  filter(!is.na(Ineg_fav_3)) %>%
  group_by(Statut_soc_pres, Ineg_fav_3) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Statut_soc_pres, y = percentage, fill = Ineg_fav_3)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Position favorisée face à l'emploi et le chômage",
       x = "Statut social",
       y = "",
       fill = "Position face aux inégalités d'emploi") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "fav_statut.png", plot = p, width = 8, height = 6)




distribution <- data %>%
  filter(!is.na(Statut_soc_pres))%>%
  filter(!is.na(Ineg_fav_2)) %>%
  group_by(Statut_soc_pres, Ineg_fav_2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Statut_soc_pres, y = percentage, fill = Ineg_fav_2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Position favorisée face aux inégaités de patrioine",
       x = "Statut social",
       y = "",
       fill = "") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "fav_statut_patri.png", plot = p, width = 8, height = 6)



#AVIS SELON LES LOIS ET LES ONG SELON TYPE D ORGA


distribution <- data %>%
  filter(!is.na(Org_type))%>%
  filter(!is.na(Rse_lois)) %>%
  group_by(Org_type, Rse_lois) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Org_type, y = percentage, fill = Rse_lois)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Avis sur les lois RSE",
       x = "Type d'organisation",
       y = "",
       fill = "") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "lois_org.png", plot = p, width = 8, height = 6)


#AVIS SELON LES LOIS ET LES ONG SELON TYPE D ORGA


distribution <- data %>%
  filter(!is.na(Org_type))%>%
  filter(!is.na(Rse_ong_milit_rec)) %>%
  group_by(Org_type, Rse_ong_milit_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Org_type, y = percentage, fill = Rse_ong_milit_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Avis sur les critiques des ONG",
       x = "Type d'organisation",
       y = "",
       fill = "") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "ong_org.png", plot = p, width = 8, height = 6)


# Charger les bibliothèques nécessaires
# Exemple de dataframe
# data <- data.frame(Rem_annuel = ..., Rem_satisf = ...)

# Vérifier et nettoyer les données
data <- data %>%
  filter(!is.na(Rem_annuel)) %>% 
  mutate(Rem_annuel = as.numeric(Rem_annuel))
  
  # Vérifier les valeurs minimales et maximales de Rem_annuel
min_salary <- min(data$Rem_annuel, na.rm = TRUE)
max_salary <- max(data$Rem_annuel, na.rm = TRUE)

# Créer des intervalles de salaire
data <- data %>%
  mutate(Salary_interval = cut(Rem_annuel, breaks = seq(min_salary, max_salary, by = 10000), include.lowest = TRUE))

print(data$Salary_interval[1:20])

distribution <- data %>%
  filter(!is.na(Salary_interval)) %>%
  filter(!is.na(Rem_satisf)) %>%
  group_by(Salary_interval, Rem_satisf) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Salary_interval) %>%
  mutate(total_count = sum(count)) %>%
  filter(total_count >= 10) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  select(-total_count)

p<-ggplot(distribution, aes(x = Salary_interval, y = percentage, fill = Rem_satisf)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Satisfaction selon la tranche de salaire",
       x = "Salaire",
       y = "Pourcentage",
       fill = "Satisfaction") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "final_obs.png", plot = p, width = 8, height = 6)
