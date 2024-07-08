setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")

library(dplyr)
library(ggplot2)

#on créé le dataframe investissement responsable
data_inv <- data %>%
  filter(Dep_rse3_4=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_1, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_4)%>%
  mutate(mission = case_when(
    mission =='Oui'~'investissement responsable',
    TRUE ~ mission))
print(data_inv$mission[1:20])

#Afficher combien il y a d'observations
nombre_observations <- nrow(data_inv)
print(nombre_observations)
#il y a 68 observations au total
#(68 personnes qui ont déclaré traiter de cette thématique dans leur mission)


create_bar_plot <- function(data, variable, file_name) {
  data <- data %>%
    mutate(!!variable := ifelse(is.na(!!sym(variable)), "NA", !!sym(variable)))
  
  p <- ggplot(data, aes_string(x = variable, fill = variable)) +
    geom_bar() +
    labs(title = paste("Distribution de la variable", variable),
         x = variable,
         y = "Nombre d'observations") +
    scale_fill_brewer(palette = "Accent") +
    theme_minimal() +
    geom_text(stat = "count", aes_string(label = "..count.."), vjust = -0.5)+
    theme(legend.position = "none")+
    coord_flip()
  
  print(p)
  ggsave(filename = file_name, plot = p, width = 8, height = 6)
}

create_percentage_bar_plot <- function(data, variable, file_name) {
  filtered_data <- data %>%
    filter(!!sym(variable) != "NA") %>%
    group_by(!!sym(variable)) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  p <- ggplot(filtered_data, aes_string(x = variable, y = "percentage", fill = variable)) +
    geom_bar(stat = "identity") +
    geom_text(aes_string(label = "paste0(round(percentage, 1), '%')"), 
              vjust = -0.5) +
    labs(title = paste("Distribution de", variable, "en pourcentages"),
         x = variable,
         y = "Pourcentage") +
    scale_fill_brewer(palette = "Accent") +
    theme_minimal()+
    coord_flip()
  print(p)
  ggsave(filename = file_name, plot = p, width = 8, height = 6)
}


# Genre
create_bar_plot(data_inv, "Genre", "genre_inv.png")
create_percentage_bar_plot(data_inv, "Genre", "genre_inv_pourcentage.png")

# Variable d'âge
p<-ggplot(data_inv, aes(x = age, fill=age)) +
  geom_bar() +
  labs(title = "Distribution de la variable age",
       x = "Age",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()
print(p)
ggsave(filename = "age_inv.png", plot = p, width = 8, height = 6)

# Âge mais version tranches
data_inv$age_group <- cut(data_inv$age, breaks = seq(0, max(data_inv$age, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_inv, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Distribution de la variable âge",
       x = "Tranches d'âge",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

print(p)
ggsave(filename = "age_inv_tranches.png", plot = p, width = 8, height = 6)

#Situation matrimoniale
create_bar_plot(data_inv, "Situ_matri", "situ_matri_inv.png")
create_percentage_bar_plot(data_inv, "Situ_matri", "situ_matri_inv_pourcentage.png")

#Lieu de vie 
create_bar_plot(data_inv, "Lieu_vie", "lieu_inv.png")
create_percentage_bar_plot(data_inv, "Lieu_vie", "lieu_inv_pourcentage.png")

#Diplome

data_inv <- data_inv %>%
  mutate(dipl_niv_short = case_when(
    Dipl_niv == "Un diplôme universitaire correspondant à au moins 4 années d’études supérieures après le baccalauréat (maîtris" ~ "Bac+4 ou plus",
    Dipl_niv == "Un diplôme universitaire correspondant à moins de 4 années d’études supérieures après le baccalauréat (par exem" ~ "jusqu'a Bac+4",
    Dipl_niv == "Le baccalauréat ou un certificat d’études ou un diplôme de l’enseignement secondaire autre que le baccalauréat (" ~ "Bac",
    TRUE ~ Dipl_niv  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_inv, "dipl_niv_short", "diplome_inv.png")
create_percentage_bar_plot(data_inv, "dipl_niv_short", "diplome_inv_pourcentage.png")


#Etablissement d'études sup
create_bar_plot(data_inv, "Type_etudsup", "etude_inv.png")
create_percentage_bar_plot(data_inv, "Type_etudsup", "etude_inv_pourcentage.png")

#Discipline du cursus

data_inv <- data_inv %>%
  mutate(Spe_etudsup= case_when(
    Spe_etudsup == "Sciences humaines et sociales (histoire, sociologie, philosophie, psychologie, sciences politiques, ...)" ~ "Sciences humaines et sociales",
    TRUE ~ Spe_etudsup
  ))

create_bar_plot(data_inv, "Spe_etudsup", "spe_inv.png")
create_percentage_bar_plot(data_inv, "Spe_etudsup", "spe_inv_pourcentage.png")

#Statut d'emploi
data_inv <- data_inv %>%
  mutate(Emp_cont_short = case_when(
    Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)" ~ "Indépendant.e",
    TRUE ~ Emp_cont  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_inv, "Emp_cont_short", "statut_inv.png")
create_percentage_bar_plot(data_inv, "Emp_cont_short", "statut_inv_pourcentage.png")

#Type de cadre
create_bar_plot(data_inv, "cadre_inout", "cadre_inv.png")
create_percentage_bar_plot(data_inv, "cadre_inout", "cadre_inv_pourcentage.png")



#Type d'emploi des consultant.es

df_filtered <- data_inv %>% 
  filter(Emp_cons == "Oui")

df_filtered <- df_filtered %>%
  mutate(emp_cont_grouped = ifelse(Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)", "freelance", 
                                   ifelse(Emp_cont %in% c("CDD", "CDI"), "CDD/CDI", NA)))

create_bar_plot(df_filtered, "emp_cont_grouped", "emploi_cons_inv.png")
create_percentage_bar_plot(df_filtered, "emp_cont_grouped", "emploi_cons_inv_pourcentage.png")



#Niveau hiérarchique
create_bar_plot(data_inv, "Emp_statut", "hiérarchie_inv.png")
create_percentage_bar_plot(data_inv, "Emp_statut", "hiérarchie_inv_pourcentage.png")

#Tableau descriptif de l'âge selon les différents postes
descriptives <- data_inv %>%
  group_by(Emp_statut) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

print(descriptives)
write.table(descriptives, "tableau_inv.txt", sep = "\t", row.names = FALSE)


#Hiérarchie simplifiée
create_bar_plot(data_inv, "Emp_statut_rec2", "hiérarchie_s_inv.png")
create_percentage_bar_plot(data_inv, "Emp_statut_rec2", "hiérarchie_s_inv_pourcentage.png")


#Un escalator de verre?
gender_distribution <- data_inv %>%
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
ggsave(filename = "répartition_hf_inv.png", plot = p, width = 8, height = 6)


#Type d'organisation employeuse
create_bar_plot(data_inv, "Org_type", "org_inv.png")
create_percentage_bar_plot(data_inv, "Org_type", "org_inv_pourcentage.png")

# Niveau hiérarchique hors consultant.es

filtered_data <- data_inv %>%
  filter(Emp_cons == "Non") 
create_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_inv.png")
create_percentage_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_inv_pourcentage.png")

#Niveau hiérarchique consultant.es
filtered_data <- data_inv %>%
  filter(Emp_cons == "Oui") 
create_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_cons_inv.png")
create_percentage_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_cons_inv_pourcentage.png")

#Année du premier CDD ou CDI de la carrière
p<-ggplot(data_inv, aes(x = Prem_contrat_annee)) +
  geom_bar() +
  labs(title = "Année du premier CDD ou CDI de la carrière",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_emploi_inv.png", plot = p, width = 8, height = 6)

#version tranches

data_inv$age_group <- cut(data_inv$Prem_contrat_annee, breaks = seq(0, max(data_inv$Prem_contrat_annee, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_inv, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Premier CDD ou CDI",
       x = "Tranches d'années",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "prem_emploi_inv_tranches.png", plot = p, width = 8, height = 6)




#Année du premier poste en RSE
p<- ggplot(data_inv, aes(x = Prem_rse_annee_rec)) +
  geom_bar() +
  labs(title = "Année du premier poste en RSE",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_rse_inv.png", plot = p, width = 8, height = 6)

#Temps consacré à la rse
create_bar_plot(data_inv, "Rse_tps_rec", "temps_rse_inv.png")

#Emp sec
create_bar_plot(data_inv, "Emp_sec", "secteur_inv.png")

#Temps plein
create_bar_plot(data_inv, "Trav_tps", "temps_plein_inv.png")

#Multinationale
create_bar_plot(data_inv, "Org_multi", "multinationale_inv.png")

#Salariés français
create_bar_plot(data_inv, "Org_N", "salaries_fr_inv.png")

#Salariés international
create_bar_plot(data_inv, "Org_N_int", "salaries_int_inv.png")

#Secteur d'activité
library(forcats)
install.packages("viridis")
library(viridis)


# Filtrage et calcul des pourcentages
filtered_data <- data_inv %>%
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
ggsave(filename = "activ_sec_inv.png", plot = p, width = 8, height = 6)

#Département RSE?
create_bar_plot(data_inv, "Dep_rse", "dep_rse_inv.png")

#Niveau du département RSE
data_inv <- data_inv %>%
  mutate(Dep_rse_multi = case_when(
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate" ~ "niveau international/corporate",
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate et d'un·e autre au niveau France" ~ "niveau international/corporate et niveau France",
    Dep_rse_multi == "d'un·e département/équipe au niveau France" ~ "niveau France",
    TRUE ~ Dep_rse_multi  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_inv, "Dep_rse_multi", "dep_rse_niv_inv.png")

#Terme RSE
create_bar_plot(data_inv, "Rse_mot", "mot_rse_inv.png")

#Lois RSE
create_bar_plot(data_inv, "Rse_lois", "lois_inv.png")

#Vocation
create_bar_plot(data_inv, "Rse_vocation_rec", "vocation_inv.png")

#Critiques ONG
create_bar_plot(data_inv, "Rse_ong_milit_rec", "critiques_inv.png")

#Position politique 
create_bar_plot(data_inv, "Pol_pos_SQ001", "politique_inv.png")






