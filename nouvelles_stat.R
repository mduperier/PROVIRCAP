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
data2 <- data %>%
  filter(Org_multi=="Oui" | Cab_cons_taille=="D'un grand cabinet international")%>%
  mutate(Org_N_int = ifelse(is.na(Org_N_int), "NA", Org_N_int))
  
p <- ggplot(data2, aes(x = Org_N_int, fill=Org_N_int)) +
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

#département rse? dep_rse

distribution <- data %>%
  filter(!is.na(Org_type))%>%
  filter(!is.na(Dep_rse)) %>%
  group_by(Org_type, Dep_rse) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Org_type, y = percentage, fill = Dep_rse)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Présence d'un département RSE",
       x = "type d'organisation",
       y = "",
       fill = "type d'organisation") +
  theme_minimal()

print(p)
ggsave(filename = "dep_rse.png", plot = p, width = 8, height = 6)



#niveau de dep RSE (dep_rse_multi) fonction du nb salariés français (org_N)

data <- data %>%
  mutate(Dep_rse_multi = case_when(
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate" ~ "niveau international/corporate",
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate et d'un·e autre au niveau France" ~ "niveau international/corporate et niveau France",
    Dep_rse_multi == "d'un·e département/équipe au niveau France" ~ "niveau France",
    TRUE ~ Dep_rse_multi  # Pour garder les autres valeurs inchangées
  ))

distribution <- data %>%
  filter(!is.na(Org_N))%>%
  filter(!is.na(Dep_rse_multi)) %>%
  group_by(Org_N, Dep_rse_multi) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


p<-ggplot(distribution, aes(x = Org_N, y = percentage, fill = Dep_rse_multi)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Niveau du département RSE",
       x = "nombre de salariés en france",
       y = "",
       fill = "") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "dep_rse_niv.png", plot = p, width = 8, height = 6)

#Thématiques traitées par département (en pourcentage): dep_rse2_1 ... dep_rse3_7

counts <- sapply(data[, paste0("Dep_rse3_", 1:7)], function(x) sum(x == "Oui"))
result_df <- data.frame(Variable = paste0("rse_", 1:7), Count = counts)
print(result_df)

new_rownames <- c("Environnement", "Qulaité de vie au travail", "Diversité et inclusion", "Investissement responsable", "Mécénat", "Droits humains", "Ethique et conformité")
rownames(result_df) <- new_rownames

total_oui <- sum(result_df$Count)
result_df$Percentage <- (result_df$Count / total_oui) * 100

p<- ggplot(result_df, aes(x = rownames(result_df), y = Percentage, fill=rownames(result_df))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Pourcentage de 'Oui' par Variable",
       x = "Variable",
       y = "Pourcentage de 'Oui' (%)",
       fill = "") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() +
  coord_flip()

print(p)
ggsave(filename = "theme_rse.png", plot = p, width = 8, height = 6)


#terme RSE
data2 <- data %>%
  filter(!is.na(Rse_mot))
p <- ggplot(data2, aes(x = Rse_mot, fill=Rse_mot)) +
  geom_bar() +
  labs(title = "Terme RSE",
       x = "terme",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

print(p)
ggsave(filename = "mot_rse.png", plot = p, width = 8, height = 6)

#terme RSE selon niveau hiérarchique

distribution <- data %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Rse_mot)) %>%
  group_by(Emp_statut_rec3, Rse_mot) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Rse_mot)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Mot RSE",
       x = "Niveau hiérarchique",
       y = "",
       fill = "emploi") +
  theme_minimal()

print(p)
ggsave(filename = "mot_hiérarchie.png", plot = p, width = 8, height = 6)



#vocation rse (rse_vocation_rec) en fonction de hiérarchie

distribution <- data %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Rse_vocation_rec)) %>%
  group_by(Emp_statut_rec3, Rse_vocation_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Rse_vocation_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Niveau hiérarchique",
       y = "",
       fill = "vocation de la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "vocation_h.png", plot = p, width = 8, height = 6)


#reglementation en fonction hiérarchie (rse_lois) sans les Sans opinion
data2<- data %>%
  filter(Rse_lois != "Sans opinion")
distribution <- data2 %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Rse_lois)) %>%
  group_by(Emp_statut_rec3, Rse_lois) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Rse_lois)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Niveau hiérarchique",
       y = "",
       fill = "lois de la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "lois_h.png", plot = p, width = 8, height = 6)


#les critiques (rse_on_milit_rec)
data2<- data %>%
  filter(!is.na(Rse_ong_milit_rec))
distribution <- data2 %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Rse_ong_milit_rec)) %>%
  group_by(Emp_statut_rec3, Rse_ong_milit_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Rse_ong_milit_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Niveau hiérarchique",
       y = "",
       fill = "critiques des ong") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "critiques_h.png", plot = p, width = 8, height = 6)


#position droite gauche 
data2<- data %>%
  filter(!is.na(Pol_pos_SQ001))
distribution <- data2 %>%
  filter(!is.na(Emp_statut_rec3))%>%
  filter(!is.na(Pol_pos_SQ001)) %>%
  group_by(Emp_statut_rec3, Pol_pos_SQ001) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = Pol_pos_SQ001)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Niveau hiérarchique",
       y = "",
       fill = "position politique") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "politique_h.png", plot = p, width = 8, height = 6)


#rse_vocation selon le genre

distribution <- data %>%
  filter(!is.na(Genre))%>%
  filter(!is.na(Rse_vocation_rec)) %>%
  group_by(Genre, Rse_vocation_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Genre, y = percentage, fill = Rse_vocation_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Genre",
       y = "",
       fill = "vocation de la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "vocation_g.png", plot = p, width = 8, height = 6)

#vocation selon cadre dirigeante/non dirigeante (emp_statut_bin_rec)

distribution <- data %>%
  filter(!is.na(Emp_statut_bin_rec))%>%
  filter(!is.na(Rse_vocation_rec)) %>%
  group_by(Emp_statut_bin_rec, Rse_vocation_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_bin_rec, y = percentage, fill = Rse_vocation_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Cadre",
       y = "",
       fill = "vocation de la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "vocation_cadre.png", plot = p, width = 8, height = 6)

#lois selon cadre 

distribution <- data %>%
  filter(!is.na(Emp_statut_bin_rec))%>%
  filter(!is.na(Rse_lois)) %>%
  group_by(Emp_statut_bin_rec, Rse_lois) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_bin_rec, y = percentage, fill = Rse_lois)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Cadre",
       y = "",
       fill = "lois sur la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "lois_cadre.png", plot = p, width = 8, height = 6)

#critiques selon cadre

distribution <- data %>%
  filter(!is.na(Emp_statut_bin_rec))%>%
  filter(!is.na(Rse_ong_milit_rec)) %>%
  group_by(Emp_statut_bin_rec, Rse_ong_milit_rec) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(distribution, aes(x = Emp_statut_bin_rec, y = percentage, fill = Rse_ong_milit_rec)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "",
       x = "Cadre",
       y = "",
       fill = "critiques des ong sur la RSE") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "critiques_cadre.png", plot = p, width = 8, height = 6)


#thèmes abordés selon le secteur d'activité

library(tidyverse)


# Reshape les données pour créer la variable "mission"
data_long <- data %>%
  pivot_longer(cols = starts_with("Dep_rse3_"), 
               names_to = "mission", 
               values_to = "value") %>%
  filter(value == "Oui") %>%
  select(-value)

# Ajouter une colonne "mission" avec les valeurs "miss_1", "miss_2", ..., "miss_7"
data_long$mission <- sub("Dep_rse3_", "", data_long$mission)

# Calculer la répartition des missions par secteur d'activité
mission_counts <- data_long %>%
  group_by(Org_sec, mission) %>%
  summarise(count = n()) %>%
  ungroup()

mission_labels <- c("1" = "Environnement", 
                    "2" = "Qualité de vie au travail", 
                    "3" = "Diversité et inclusion", 
                    "4" = "Investissement responsable", 
                    "5" = "Mécénat", 
                    "6" = "Droits humains", 
                    "7" = "Ethique et conformité")

data_long$mission <- factor(data_long$mission, levels = names(mission_labels), labels = mission_labels)

# Calculer la répartition des missions par secteur d'activité
mission_counts <- data_long %>%
  group_by(Org_sec, mission) %>%
  summarise(count = n()) %>%
  ungroup()

# Tracer le diagramme en bâtons empilés avec des pourcentages
p<-ggplot(mission_counts, aes(x = Org_sec, y = count, fill = mission)) +
  geom_bar(stat = "identity", position = "fill") +  # Utilise 'fill' pour des pourcentages
  scale_y_continuous(labels = scales::percent_format()) +  # Affiche les étiquettes de l'axe y en pourcentages
  scale_fill_manual(values = rainbow(7)) +  # Choisissez les couleurs que vous préférez
  labs(title = "Répartition des missions par secteur d'activité en pourcentage",
       x = "Secteur d'activité",
       y = "Pourcentage",
       fill = "Mission") +
  theme_minimal()+
  coord_flip()
print(p)

ggsave(filename = "test.png", plot = p, width = 15, height = 10)

