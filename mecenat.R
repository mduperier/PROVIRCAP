setwd("PROVIRCAP")
library(dplyr)
library(ggplot2)

data <- read.csv("data_assoquest_recoded_fmt.csv")



data_mec <- data %>%
  filter(Dep_rse3_5=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_1, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_5)%>%
  mutate(mission = case_when(
    mission =='Oui'~'mécénat',
    TRUE ~ mission))
print(data_mec$mission[1:20])



#Afficher combien il y a d'observations
nombre_observations <- nrow(data_mec)
print(nombre_observations)
#il y a 55 observations au total
#(55 personnes qui ont déclaré traiter de cette thématique dans leur mission)



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
create_bar_plot(data_mec, "Genre", "genre_mec.png")
create_percentage_bar_plot(data_mec, "Genre", "genre_mec_pourcentage.png")

# Variable d'âge
p<-ggplot(data_mec, aes(x = age, fill=age)) +
  geom_bar() +
  labs(title = "Distribution de la variable age",
       x = "Age",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()
print(p)
ggsave(filename = "age_mec.png", plot = p, width = 8, height = 6)

# Âge mais version tranches
data_mec$age_group <- cut(data_mec$age, breaks = seq(0, max(data_mec$age, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_mec, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Distribution de la variable âge",
       x = "Tranches d'âge",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

print(p)
ggsave(filename = "age_mec_tranches.png", plot = p, width = 8, height = 6)

#Situation matrimoniale
create_bar_plot(data_mec, "Situ_matri", "situ_matri_mec.png")
create_percentage_bar_plot(data_mec, "Situ_matri", "situ_matri_mec_pourcentage.png")

#Lieu de vie 
create_bar_plot(data_mec, "Lieu_vie", "lieu_mec.png")
create_percentage_bar_plot(data_mec, "Lieu_vie", "lieu_mec_pourcentage.png")

#Diplome

data_mec <- data_mec %>%
  mutate(dipl_niv_short = case_when(
    Dipl_niv == "Un diplôme universitaire correspondant à au moins 4 années d’études supérieures après le baccalauréat (maîtris" ~ "Bac+4 ou plus",
    Dipl_niv == "Un diplôme universitaire correspondant à moins de 4 années d’études supérieures après le baccalauréat (par exem" ~ "jusqu'a Bac+4",
    Dipl_niv == "Le baccalauréat ou un certificat d’études ou un diplôme de l’enseignement secondaire autre que le baccalauréat (" ~ "Bac",
    TRUE ~ Dipl_niv  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_mec, "dipl_niv_short", "diplome_mec.png")
create_percentage_bar_plot(data_mec, "dipl_niv_short", "diplome_mec_pourcentage.png")


#Etablissement d'études sup
create_bar_plot(data_mec, "Type_etudsup", "etude_mec.png")
create_percentage_bar_plot(data_mec, "Type_etudsup", "etude_mec_pourcentage.png")

#Discipline du cursus

data_mec <- data_mec %>%
  mutate(Spe_etudsup= case_when(
    Spe_etudsup == "Sciences humaines et sociales (histoire, sociologie, philosophie, psychologie, sciences politiques, ...)" ~ "Sciences humaines et sociales",
    TRUE ~ Spe_etudsup
  ))

create_bar_plot(data_mec, "Spe_etudsup", "spe_mec.png")
create_percentage_bar_plot(data_mec, "Spe_etudsup", "spe_mec_pourcentage.png")

#Statut d'emploi
data_mec <- data_mec %>%
  mutate(Emp_cont_short = case_when(
    Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)" ~ "Indépendant.e",
    TRUE ~ Emp_cont  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_mec, "Emp_cont_short", "statut_mec.png")
create_percentage_bar_plot(data_mec, "Emp_cont_short", "statut_mec_pourcentage.png")

#Type de cadre
create_bar_plot(data_mec, "cadre_inout", "cadre_mec.png")
create_percentage_bar_plot(data_mec, "cadre_inout", "cadre_mec_pourcentage.png")



#Type d'emploi des consultant.es

df_filtered <- data_mec %>% 
  filter(Emp_cons == "Oui")

df_filtered <- df_filtered %>%
  mutate(emp_cont_grouped = ifelse(Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)", "freelance", 
                                   ifelse(Emp_cont %in% c("CDD", "CDI"), "CDD/CDI", NA)))

create_bar_plot(df_filtered, "emp_cont_grouped", "emploi_cons_mec.png")
create_percentage_bar_plot(df_filtered, "emp_cont_grouped", "emploi_cons_mec_pourcentage.png")



#Niveau hiérarchique
create_bar_plot(data_mec, "Emp_statut", "hiérarchie_mec.png")
create_percentage_bar_plot(data_mec, "Emp_statut", "hiérarchie_mec_pourcentage.png")

#Tableau descriptif de l'âge selon les différents postes
descriptives <- data_mec %>%
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
create_bar_plot(data_mec, "Emp_statut_rec2", "hiérarchie_s_mec.png")
create_percentage_bar_plot(data_mec, "Emp_statut_rec2", "hiérarchie_s_mec_pourcentage.png")


#Un escalator de verre?
gender_distribution <- data_mec %>%
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
ggsave(filename = "répartition_hf_mec.png", plot = p, width = 8, height = 6)


#Type d'organisation employeuse
create_bar_plot(data_mec, "Org_type", "org_mec.png")
create_percentage_bar_plot(data_mec, "Org_type", "org_mec_pourcentage.png")

# Niveau hiérarchique hors consultant.es

filtered_data <- data_mec %>%
  filter(Emp_cons == "Non") 
create_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_mec.png")
create_percentage_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_mec_pourcentage.png")

#Niveau hiérarchique consultant.es
filtered_data <- data_mec %>%
  filter(Emp_cons == "Oui") 
create_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_cons_mec.png")
create_percentage_bar_plot(filtered_data, "Emp_statut", "hiérarchie2_cons_mec_pourcentage.png")

#Année du premier CDD ou CDI de la carrière
p<-ggplot(data_mec, aes(x = Prem_contrat_annee)) +
  geom_bar() +
  labs(title = "Année du premier CDD ou CDI de la carrière",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_emploi_mec.png", plot = p, width = 8, height = 6)

#version tranches

data_mec$age_group <- cut(data_mec$Prem_contrat_annee, breaks = seq(0, max(data_mec$Prem_contrat_annee, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_mec, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Premier CDD ou CDI",
       x = "Tranches d'années",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "prem_emploi_mec_tranches.png", plot = p, width = 8, height = 6)




#Année du premier poste en RSE
p<- ggplot(data_mec, aes(x = Prem_rse_annee_rec)) +
  geom_bar() +
  labs(title = "Année du premier poste en RSE",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_rse_mec.png", plot = p, width = 8, height = 6)

#Temps consacré à la rse
create_bar_plot(data_mec, "Rse_tps_rec", "temps_rse_mec.png")

#Emp sec
create_bar_plot(data_mec, "Emp_sec", "secteur_mec.png")

#Temps plein
create_bar_plot(data_mec, "Trav_tps", "temps_plein_mec.png")

#Multinationale
create_bar_plot(data_mec, "Org_multi", "multinationale_mec.png")

#Salariés français
create_bar_plot(data_mec, "Org_N", "salaries_fr_mec.png")

#Salariés international
create_bar_plot(data_mec, "Org_N_int", "salaries_int_mec.png")

#Secteur d'activité
library(forcats)
install.packages("viridis")
library(viridis)


# Filtrage et calcul des pourcentages
filtered_data <- data_mec %>%
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
ggsave(filename = "activ_sec_mec.png", plot = p, width = 8, height = 6)

#Département RSE?
create_bar_plot(data_mec, "Dep_rse", "dep_rse_mec.png")

#Niveau du département RSE
data_mec <- data_mec %>%
  mutate(Dep_rse_multi = case_when(
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate" ~ "niveau international/corporate",
    Dep_rse_multi == "d'un·e département/équipe au niveau international/corporate et d'un·e autre au niveau France" ~ "niveau international/corporate et niveau France",
    Dep_rse_multi == "d'un·e département/équipe au niveau France" ~ "niveau France",
    TRUE ~ Dep_rse_multi  # Pour garder les autres valeurs inchangées
  ))

create_bar_plot(data_mec, "Dep_rse_multi", "dep_rse_niv_mec.png")

#Terme RSE
create_bar_plot(data_mec, "Rse_mot", "mot_rse_mec.png")

#Lois RSE
create_bar_plot(data_mec, "Rse_lois", "lois_mec.png")

#Vocation
create_bar_plot(data_mec, "Rse_vocation_rec", "vocation_mec.png")

#Critiques ONG
create_bar_plot(data_mec, "Rse_ong_milit_rec", "critiques_mec.png")

#Position politique 
create_bar_plot(data_mec, "Pol_pos_SQ001", "politique_mec.png")







