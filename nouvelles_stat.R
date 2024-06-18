data <- read.csv("data_assoquest_recoded_fmt.csv")

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

#rse_tps_rec
data <- data %>%
  mutate(Rse_tps_rec = ifelse(is.na(Rse_tps_rec), "NA", Rse_tps_rec))

filtered_data <- data %>%
  filter(Rse_tps_rec != "NA") %>%
  group_by(Rse_tps_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Rse_tps_rec, y = percentage, fill=Rse_tps_rec)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Temps de travail associé à la RSE",
       x = "",
       y = "") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() 

print(p)
ggsave(filename = "temps_rse.png", plot = p, width = 8, height = 6)



#emp_sec sans les sans emploi

filtered_data <- data %>%
  filter(!is.na(Emp_sec)) %>%
  group_by(Emp_sec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Emp_sec, y = percentage, fill=Emp_sec)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "right") + 
  labs(x = NULL, y = "Valeur", title = "Secteur d'activié")+

  scale_fill_brewer(palette = "Accent")

print(p)
ggsave(filename = "secteur.png", plot = p, width = 8, height = 6)



#trav_tps

filtered_data <- data %>%
  filter(!is.na(Trav_tps)) %>%
  group_by(Trav_tps) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Trav_tps, y = percentage, fill=Trav_tps)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Temps de travail associé à la RSE",
       x = "",
       y = "") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() 

print(p)
ggsave(filename = "temps_plein.png", plot = p, width = 8, height = 6)

#multinationale : org_multi

data <- data %>%
  mutate(Org_multi= ifelse(is.na(Org_multi), "NA", Org_multi))
p <- ggplot(data, aes(x = Org_multi, fill=Org_multi)) +
  geom_bar() +
  labs(title = "Multinationale?",
       x = "",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

print(p)
ggsave(filename = "multinationale.png", plot = p, width = 8, height = 6)



#org_N

filtered_data <- data %>%
  filter(!is.na(Org_N)) %>%
  group_by(Org_N) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Org_N, y = percentage, fill=Org_N)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "salariés sur le territoire français",
       x = "",
       y = "") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() 

print(p)
ggsave(filename = "salaries_fr.png", plot = p, width = 8, height = 6)

#org_n_int
data <- data %>%
  filter(Org_multi=="Oui" | Cab_cons_taille=="D'un grand cabinet international")%>%
  mutate(Org_N_int = ifelse(is.na(Org_N_int), "NA", Org_N_int))
  
p <- ggplot(data, aes(x = Org_N_int, fill=Org_N_int)) +
  geom_bar() +
  labs(title = "salariés à l'international",
       x = "",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

print(p)

ggsave(filename = "salaries_int.png", plot = p, width = 8, height = 6)


#Secteur d'activité: org_sec


# Chargement des packages nécessaires

library(forcats)


# Filtrage et calcul des pourcentages
filtered_data <- data %>%
  filter(!is.na(Org_sec)) %>%
  group_by(Org_sec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) # Tri décroissant par pourcentage

# Réordonner les facteurs selon le pourcentage
filtered_data <- filtered_data %>%
  mutate(Org_sec = fct_reorder(Org_sec, percentage, .desc = TRUE))

# Création du diagramme en bâtons
p <- ggplot(filtered_data, aes(x = Org_sec, y = percentage, fill = Org_sec)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) + # Ajuste la position verticale des étiquettes
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "right") +
  labs(x = NULL, y = "Pourcentage", title = "Secteur d'activité")

# Affichage du graphique
print(p)
ggsave(filename = "activ_sec.png", plot = p, width = 8, height = 6)

#












