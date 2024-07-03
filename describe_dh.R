setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")

library(dplyr)
library(ggplot2)

#on créé le dataframe droits humains
data_dh <- data %>%
  filter(Dep_rse3_6=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_1, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_6)%>%
  mutate(mission = case_when(
    mission =='Oui'~'droits humains',
    TRUE ~ mission))
print(data_dh$mission[1:20])

#Afficher combien il y a d'observations
nombre_observations <- nrow(data_dh)
print(nombre_observations)
#il y a 62 observations au total
#(62 personnes qui ont déclaré traiter de cette thématique dans leur mission)

## STATISTIQUES DECRIPTIVES

# Variable de genre
data_dh <- data_dh %>%
  mutate(Genre = ifelse(is.na(Genre), "NA", Genre))
p <- ggplot(data_dh, aes(x = Genre, fill=Genre)) +
  geom_bar() +
  labs(title = "Distribution de la variable genre",
       x = "Genre",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)
  
print(p)
ggsave(filename = "genre_dh.png", plot = p, width = 8, height = 6)

filtered_data <- data_dh %>%
  filter(Genre != "NA") %>%
  group_by(Genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Genre, y = percentage, fill=Genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Distribution de Genre en pourcentages",
       x = "Genre",
       y = "Pourcentage") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() 

print(p)
ggsave(filename = "genre_dh_pourcentage.png", plot = p, width = 8, height = 6)






# Variable d'âge
p<-ggplot(data_dh, aes(x = age, fill=age)) +
  geom_bar() +
  labs(title = "Distribution de la variable age",
       x = "Age",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()
print(p)
ggsave(filename = "age_dh.png", plot = p, width = 8, height = 6)

# Âge mais version tranches
data_dh$age_group <- cut(data_dh$age, breaks = seq(0, max(data_dh$age, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_dh, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Distribution de la variable âge",
       x = "Tranches d'âge",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

print(p)
ggsave(filename = "age_dh_tranches.png", plot = p, width = 8, height = 6)






# Situation matrimoniale
data_dh <- data_dh %>%
  mutate(Situ_matri = ifelse(is.na(Situ_matri), "NA", Situ_matri))
p<- ggplot(data_dh, aes(x = Situ_matri, fill=Situ_matri)) +
  geom_bar() +
  labs(title = "Situation matrimoniale des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)
print(p)
ggsave(filename = "situ_matri_dh.png", plot = p, width = 8, height = 6)



#Lieu de vie
data_dh <- data_dh %>%
  mutate(Lieu_vie = ifelse(is.na(Lieu_vie), "NA", Lieu_vie))
p<- ggplot(data_dh, aes(x = Lieu_vie, fill=Lieu_vie)) +
  geom_bar() +
  labs(title = "Lieu de vie des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "lieu_vie_dh.png", plot = p, width = 8, height = 6)





#Diplome le plus élevé
data_dh <- data_dh %>%
  mutate(dipl_niv_short = case_when(
    Dipl_niv == "Un diplôme universitaire correspondant à au moins 4 années d’études supérieures après le baccalauréat (maîtris" ~ "Bac+4 ou plus",
    Dipl_niv == "Un diplôme universitaire correspondant à moins de 4 années d’études supérieures après le baccalauréat (par exem" ~ "jusqu'a Bac+4",
    Dipl_niv == "Le baccalauréat ou un certificat d’études ou un diplôme de l’enseignement secondaire autre que le baccalauréat (" ~ "Bac",
    TRUE ~ Dipl_niv  # Pour garder les autres valeurs inchangées
  ))

p<- ggplot(data_dh, aes(x = dipl_niv_short, fill=dipl_niv_short)) +
  geom_bar() +
  labs(title = "Distribution de la variable niveau de diplôme",
       x = "Niveau de diplôme",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()
print(p)
ggsave(filename = "diplome_dh.png", plot = p, width = 8, height = 6)






#établissement d'études supérieures 
data_dh <- data_dh %>%
  mutate(Type_etudsup = ifelse(is.na(Type_etudsup), "NA", Type_etudsup))
p<- ggplot(data_dh, aes(x = Type_etudsup, fill=Type_etudsup)) +
  geom_bar() +
  labs(title = "Etablissement d'études supérieures",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "etude_sup_dh.png", plot = p, width = 8, height = 6)

filtered_data <- data_dh %>%
  filter(Type_etudsup != "NA") %>%
  group_by(Type_etudsup) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Type_etudsup, y = percentage, fill=Type_etudsup)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Distribution des établissements en pourcentages",
       x = "établissement",
       y = "Pourcentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()
  

print(p)
ggsave(filename = "etude_sup_pourcentage_dh.png", plot = p, width = 8, height = 6)





#Discipline du cursus:
data_dh <- data_dh %>%
  mutate(Spe_etudsup= case_when(
    Spe_etudsup == "Sciences humaines et sociales (histoire, sociologie, philosophie, psychologie, sciences politiques, ...)" ~ "Sciences humaines et sociales",
    TRUE ~ Spe_etudsup
  ))
p<- ggplot(data_dh, aes(x = Spe_etudsup, fill=Spe_etudsup)) +
  geom_bar() +
  labs(title = "Domaine d'études supérieures",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "discipline_dh_eff.png", plot = p, width = 8, height = 6)



filtered_data <- data_dh %>%
  mutate(Spe_etudsup= case_when(
    Spe_etudsup == "Sciences humaines et sociales (histoire, sociologie, philosophie, psychologie, sciences politiques, ...)" ~ "Sciences humaines et sociales",
    TRUE ~ Spe_etudsup
  )) %>%
  filter(!is.na(Spe_etudsup)) %>%
  group_by(Spe_etudsup) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = Spe_etudsup, y = percentage, fill=Spe_etudsup)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  
  labs(title = "Discipline du cursus",
       x = "spécialité",
       y = "Pourcentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()

print(p)
ggsave(filename = "ediscipline_cursus_dh.png", plot = p, width = 8, height = 6)






#Statut d'emploi
data_dh <- data_dh %>%
  mutate(Emp_cont_short = case_when(
    Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)" ~ "Indépendant.e",
    TRUE ~ Emp_cont  # Pour garder les autres valeurs inchangées
  ))


p<-ggplot(data_dh, aes(x = Emp_cont_short, fill=Emp_cont_short)) +
  geom_bar() +
  labs(title = "Statut d'emploi",
       x = "",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "statut_emploi_dh.png", plot = p, width = 8, height = 6)





#Type de cadre
data_dh <- data_dh %>%
  mutate(cadre_inout = ifelse(is.na(cadre_inout), "NA", cadre_inout))
p<- ggplot(data_dh, aes(x = cadre_inout, fill=cadre_inout)) +
  geom_bar() +
  labs(title = "Type de cadre",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "type_cadre_eff_dh.png", plot = p, width = 8, height = 6)




filtered_data <- data_dh %>%
  filter(!is.na(cadre_inout)) %>%
  group_by(cadre_inout) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p<- ggplot(filtered_data, aes(x = cadre_inout, y = percentage, fill=cadre_inout)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "type de cadre",
       x = "",
       y = "Pourcentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")

print(p)
ggsave(filename = "type_cadre_dh.png", plot = p, width = 8, height = 6)





#type d'emploi des consultant.es

df_filtered <- data_dh %>% 
  filter(Emp_cons == "Oui")

df_filtered <- df_filtered %>%
  mutate(emp_cont_grouped = ifelse(Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)", "freelance", 
                                   ifelse(Emp_cont %in% c("CDD", "CDI"), "CDD/CDI", NA)))

p<- ggplot(df_filtered, aes(x = emp_cont_grouped, fill=emp_cont_grouped)) +
  geom_bar() +
  labs(title = "Répartition des types de contrats",
       x = "Type de contrat",
       y = "Nombre d'observations")+
  theme_minimal()+
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")

print(p)
ggsave(filename = "emploi_cons_dh.png", plot = p, width = 8, height = 6)


filtered_data <- df_filtered %>%
  filter(!is.na(emp_cont_grouped)) %>%
  group_by(emp_cont_grouped) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
p<- ggplot(filtered_data, aes(x = emp_cont_grouped, y = percentage, fill=emp_cont_grouped)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "type de cadre",
       x = "",
       y = "Pourcentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")

print(p)
ggsave(filename = "emploi_cons_purcentage_dh.png", plot = p, width = 8, height = 6)




#Niveau hiérarchique 
data_dh <- data_dh %>%
  mutate(Emp_statut = ifelse(is.na(Emp_statut), "NA", Emp_statut))
p<- ggplot(data_dh, aes(x = Emp_statut, fill=Emp_statut)) +
  geom_bar() +
  labs(title = "Niveau hiérarchique",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "niveau_hiérarchique_eff_dh.png", plot = p, width = 8, height = 6)



filtered_data <- data_dh %>%
  filter(!is.na(Emp_statut)) %>%
  group_by(Emp_statut) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
p<- ggplot(filtered_data, aes(x = Emp_statut, y = percentage, fill=Emp_statut)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "type de cadre",
       x = "",
       y = "Pourcentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()

print(p)
ggsave(filename = "niveau_hiérarchique_dh.png", plot = p, width = 8, height = 6)





#Tableau descriptif de l'âge selon les différents postes
descriptives <- data_dh %>%
  group_by(Emp_statut) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

print(descriptives)
write.table(descriptives, "tableau_dh.txt", sep = "\t", row.names = FALSE)



#Niveau hiérarchique simplifié :
filtered_data <- data_dh %>%
  filter(!is.na(Emp_statut_rec2)) %>%
  group_by(Emp_statut_rec2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
p<-ggplot(filtered_data, aes(x = Emp_statut_rec2, y = percentage, fill=Emp_statut_rec2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "type de cadre",
       x = "",
       y = "Pourcentage") +
  theme_minimal()+
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")


print(p)
ggsave(filename = "hiérarchie_simple_dh.png", plot = p, width = 8, height = 6)







#Un escalator de verre? Emp_statut_rec3

gender_distribution <- data_dh %>%
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
ggsave(filename = "répartition_hf_dh.png", plot = p, width = 8, height = 6)









#Type d'organisation employeuse 

data_dh <- data_dh %>%
  mutate(Org_type = ifelse(is.na(Org_type), "NA", Org_type))
p<- ggplot(data_dh, aes(x = Org_type, fill=Org_type)) +
  geom_bar() +
  labs(title = "Type d'organisation employeuse",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()
  
print(p)
ggsave(filename = "organisation_dh.png", plot = p, width = 8, height = 6)




#Niveau hérarchique (hors consultant.es)

filtered_data <- data_dh %>%
  filter(Emp_cons == "Non") %>%
  mutate(Emp_statut = ifelse(is.na(Emp_statut), "NA", Emp_statut))

p<- ggplot(filtered_data, aes(x = Emp_statut, fill=Emp_statut)) +
  geom_bar() +
  labs(title = "Niveau hiérarchique (hors consultant.es)",
       x = "Niveau hiérarchique",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "hiérarchie_dh.png", plot = p, width = 8, height = 6)






#Niveau hiérarchique (consultant.es seulement)
filtered_data <- data_dh %>%
  filter(Emp_cons == "Oui") %>%
  mutate(Emp_statut_cons = ifelse(is.na(Emp_statut_cons), "NA", Emp_statut_cons))

p<-ggplot(filtered_data, aes(x = Emp_statut_cons, fill=Emp_statut_cons)) +
  geom_bar() +
  labs(title = "Niveau hiérarchique (consultant.es seulement)",
       x = "Niveau hiérarchique",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "hiérarchie_cons_dh.png", plot = p, width = 8, height = 6)






#Année du premier CDD ou CDI de la carrière
p<-ggplot(data_dh, aes(x = Prem_contrat_annee)) +
  geom_bar() +
  labs(title = "Année du premier CDD ou CDI de la carrière",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_emploi_dh.png", plot = p, width = 8, height = 6)

#version tranches

data_dh$age_group <- cut(data_dh$Prem_contrat_annee, breaks = seq(0, max(data_dh$Prem_contrat_annee, na.rm = TRUE), by = 5), right = FALSE)

p <- ggplot(data_dh, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Premier CDD ou CDI",
       x = "Tranches d'années",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)+
  coord_flip()

print(p)
ggsave(filename = "prem_emploi_dh_tranches.png", plot = p, width = 8, height = 6)




#Année du premier poste en RSE
p<- ggplot(data_dh, aes(x = Prem_rse_annee_rec)) +
  geom_bar() +
  labs(title = "Année du premier poste en RSE",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_rse_dh.png", plot = p, width = 8, height = 6)



unique_values <- unique(data$Emp_cont)
print(unique_values)



