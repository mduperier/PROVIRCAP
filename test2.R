setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}



#Afficher les noms des colonnes
colonnes <- names(data)
print(colonnes)

# Afficher le nombre total de colonnes
nombre_colonnes <- ncol(data)
print(nombre_colonnes)
#il y a 363 variables différentes

#Afficher combien il y a d'observations
nombre_observations <- nrow(data)
print(nombre_observations)
#il y a 315 observations au total

# Afficher les premières valeurs de la colonne qu'on veut
head(data$token)
print(data$submitdate[1:20])
print(data$Emp_sec[1:20])
print(data$Dep_rse3_1[1:20])


#Comprendre les variables recodées: Prem_rse_annee_rec

filtered_data <- data %>%
  filter((Prem_rse_annee != Prem_rse_annee_rec) | (is.na(Prem_rse_annee) & !is.na(Prem_rse_annee_rec))) %>%
  select(Prem_rse_annee, Prem_rse_annee_rec, Prem_rse_bin, Prem_contrat_annee)
print(filtered_data)

#On a modifié prem_rse_annee pour contenir l'année du premier contrat rse, y comprit
#lorsque ça correspond à la première expérience pro
#Il reste des valeurs manquantes dans prem_rse_annee_rec, pourquoi?

filtered_data <- data %>%
  filter(is.na(Prem_rse_annee) & is.na(Prem_rse_annee_rec)) %>%
  select(Prem_rse_annee, Prem_rse_annee_rec, Prem_rse_bin, Prem_contrat_annee)
print(filtered_data)

#si ma première expérience n'était pas en rse (ou valeur manquante) et que je n'ai
#pas renseigné de première année rse, on laisse une valeur manquane


#Comprendre les variables recodées: Rse_lois_rec
filtered_data <- data %>%
  filter((Rse_lois_rec != Rse_lois)|(is.na(Rse_lois_rec)& !is.na(Rse_lois))| (is.na(Rse_lois)& !is.na(Rse_lois_rec))) %>%
  select(Rse_lois_rec, Rse_lois)
print(filtered_data)
#on a aucune ligne différente? 


#Comprendre les variables recodées: Rse_lois_rec
filtered_data <- data %>%
  filter(is.na(cadre_inout)) %>%
  select(Emp_sec, Emp_cont, Emp_cons)
print(filtered_data)

filtered_data <- data %>%
  filter((is.na(cadre_inout)& !is.na(Emp_cont))| (is.na(cadre_inout)& !is.na(Emp_cons))) %>%
  select(cadre_inout, Emp_sec, Emp_cont, Emp_cons)
print(filtered_data)
#si la variable de emp_cons ou de emp_cont est renseignée, alors on peut remplir cadre_inout

#Emp_statut_bin_rec
filtered_data <- data %>%
  filter(Emp_statut_bin != Emp_statut_bin_rec) %>%
  select(Emp_statut_bin, Emp_statut_bin_rec)
print(filtered_data)

#Emp_statut_rec
filtered_data <- data %>%
  select(Emp_cons,Emp_statut_cons, Emp_statut_rec)
print(filtered_data)


## STATISTIQUES DECRIPTIVES

# Variable de genre
data <- data %>%
  mutate(Genre = ifelse(is.na(Genre), "NA", Genre))
p <- ggplot(data, aes(x = Genre, fill=Genre)) +
  geom_bar() +
  labs(title = "Distribution de la variable genre",
       x = "Genre",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()
  
print(p)
ggsave(filename = "genre.png", plot = p, width = 8, height = 6)

filtered_data <- data %>%
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
ggsave(filename = "genre_pourcentage.png", plot = p, width = 8, height = 6)






# Variable d'âge
p<-ggplot(data, aes(x = age, fill=age)) +
  geom_bar() +
  labs(title = "Distribution de la variable age",
       x = "Age",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()
print(p)
ggsave(filename = "age.png", plot = p, width = 8, height = 6)





# Situation matrimoniale
data <- data %>%
  mutate(Situ_matri = ifelse(is.na(Situ_matri), "NA", Situ_matri))
p<- ggplot(data, aes(x = Situ_matri, fill=Situ_matri)) +
  geom_bar() +
  labs(title = "Situation matrimoniale des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
ggsave(filename = "situ_matri.png", plot = p, width = 8, height = 6)



#Lieu de vie
data <- data %>%
  mutate(Lieu_vie = ifelse(is.na(Lieu_vie), "NA", Lieu_vie))
p<- ggplot(data, aes(x = Lieu_vie, fill=Lieu_vie)) +
  geom_bar() +
  labs(title = "Lieu de vie des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() +
  theme(legend.position = "none")+
  coord_flip()

print(p)
ggsave(filename = "lieu_vie.png", plot = p, width = 8, height = 6)





#Diplome le plus élevé
data <- data %>%
  mutate(dipl_niv_short = case_when(
    Dipl_niv == "Un diplôme universitaire correspondant à au moins 4 années d’études supérieures après le baccalauréat (maîtris" ~ "Bac+4 ou plus",
    Dipl_niv == "Un diplôme universitaire correspondant à moins de 4 années d’études supérieures après le baccalauréat (par exem" ~ "jusqu'a Bac+4",
    Dipl_niv == "Le baccalauréat ou un certificat d’études ou un diplôme de l’enseignement secondaire autre que le baccalauréat (" ~ "Bac",
    TRUE ~ Dipl_niv  # Pour garder les autres valeurs inchangées
  ))

p<- ggplot(data, aes(x = dipl_niv_short, fill=dipl_niv_short)) +
  geom_bar() +
  labs(title = "Distribution de la variable niveau de diplôme",
       x = "Niveau de diplôme",
       y = "Nombre d'observations") +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal() +
  theme(legend.position = "none")+
  coord_flip()
print(p)
ggsave(filename = "diplome.png", plot = p, width = 8, height = 6)






#établissement d'études supérieures 
data <- data %>%
  mutate(Type_etudsup = ifelse(is.na(Type_etudsup), "NA", Type_etudsup))
p<- ggplot(data, aes(x = Type_etudsup, fill=Type_etudsup)) +
  geom_bar() +
  labs(title = "Etablissement d'études supérieures",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()

print(p)
ggsave(filename = "etude_sup.png", plot = p, width = 8, height = 6)

filtered_data <- data %>%
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
ggsave(filename = "etude_sup_pourcentage.png", plot = p, width = 8, height = 6)





#Discipline du cursus:

filtered_data <- data %>%
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
ggsave(filename = "ediscipline_cursus.png", plot = p, width = 8, height = 6)






#Statut d'emploi
data <- data %>%
  mutate(Emp_cont_short = case_when(
    Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)" ~ "Indépendant.e",
    TRUE ~ Emp_cont  # Pour garder les autres valeurs inchangées
  ))


p<-ggplot(data, aes(x = Emp_cont_short, fill=Emp_cont_short)) +
  geom_bar() +
  labs(title = "Statut d'emploi",
       x = "",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()

print(p)
ggsave(filename = "statut_emploi.png", plot = p, width = 8, height = 6)





#Type de cadre

filtered_data <- data %>%
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
ggsave(filename = "type_cadre.png", plot = p, width = 8, height = 6)





#type d'emploi des consultant.es

df_filtered <- data %>% 
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
ggsave(filename = "emploi_cons.png", plot = p, width = 8, height = 6)


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
ggsave(filename = "emploi_cons_purcentage.png", plot = p, width = 8, height = 6)




#Niveau hiérarchique 
filtered_data <- data %>%
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
ggsave(filename = "niveau_hiérarchique.png", plot = p, width = 8, height = 6)





#Tableau descriptif de l'âge selon les différents postes
descriptives <- data %>%
  group_by(Emp_statut) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

print(descriptives)
write.table(descriptives, "tableau.txt", sep = "\t", row.names = FALSE)





#Niveau hiérarchique simplifié :
filtered_data <- data %>%
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
ggsave(filename = "hiérarchie_simple.png", plot = p, width = 8, height = 6)







#Un escalator de verre? Emp_statut_rec3

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









#Type d'organisation employeuse 

data <- data %>%
  mutate(Org_type = ifelse(is.na(Org_type), "NA", Org_type))
p<- ggplot(data, aes(x = Org_type, fill=Org_type)) +
  geom_bar() +
  labs(title = "Type d'organisation employeuse",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_flip()
  
print(p)
ggsave(filename = "organisation.png", plot = p, width = 8, height = 6)




#Niveau hérarchique (hors consultant.es)

filtered_data <- data %>%
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
  coord_flip()

print(p)
ggsave(filename = "hiérarchie.png", plot = p, width = 8, height = 6)






#Niveau hiérarchique (consultant.es seulement)
filtered_data <- data %>%
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
  coord_flip()

print(p)
ggsave(filename = "hiérarchie_cons.png", plot = p, width = 8, height = 6)






#Année du premier CDD ou CDI de la carrière
p<-ggplot(data, aes(x = Prem_contrat_annee)) +
  geom_bar() +
  labs(title = "Année du premier CDD ou CDI de la carrière",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_emploi.png", plot = p, width = 8, height = 6)




#Année du premier poste en RSE
p<- ggplot(data, aes(x = Prem_rse_annee_rec)) +
  geom_bar() +
  labs(title = "Année du premier poste en RSE",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

print(p)
ggsave(filename = "premier_rse.png", plot = p, width = 8, height = 6)

unique_values <- unique(data$Emp_cont)
print(unique_values)



