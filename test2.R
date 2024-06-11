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
print(data$Emp_statut_rec[1:20])


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
ggplot(data, aes(x = Genre)) +
  geom_bar() +
  labs(title = "Distribution de la variable genre",
       x = "Genre",
       y = "Nombre d'observations") +
  theme_minimal()

filtered_data <- data %>%
  filter(Genre != "NA") %>%
  group_by(Genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(filtered_data, aes(x = Genre, y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Distribution de Genre en pourcentages",
       x = "Genre",
       y = "Pourcentage") +
  theme_minimal() 

# Variable d'âge
ggplot(data, aes(x = age)) +
  geom_bar() +
  labs(title = "Distribution de la variable age",
       x = "Age",
       y = "Nombre d'observations") +
  theme_minimal()

# Situation matrimoniale
data <- data %>%
  mutate(Situ_matri = ifelse(is.na(Situ_matri), "NA", Situ_matri))
ggplot(data, aes(x = Situ_matri)) +
  geom_bar() +
  labs(title = "Situation matrimoniale des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Lieu de vie
data <- data %>%
  mutate(Lieu_vie = ifelse(is.na(Lieu_vie), "NA", Lieu_vie))
ggplot(data, aes(x = Lieu_vie)) +
  geom_bar() +
  labs(title = "Lieu de vie des répondants",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

#Diplome le plus élevé
data <- data %>%
  mutate(dipl_niv_short = case_when(
    Dipl_niv == "Un diplôme universitaire correspondant à au moins 4 années d’études supérieures après le baccalauréat (maîtris" ~ "Bac+4 ou plus",
    Dipl_niv == "Un diplôme universitaire correspondant à moins de 4 années d’études supérieures après le baccalauréat (par exem" ~ "jusqu'a Bac+4",
    Dipl_niv == "Le baccalauréat ou un certificat d’études ou un diplôme de l’enseignement secondaire autre que le baccalauréat (" ~ "Bac",
    TRUE ~ Dipl_niv  # Pour garder les autres valeurs inchangées
  ))

ggplot(data, aes(x = dipl_niv_short)) +
  geom_bar() +
  labs(title = "Distribution de la variable niveau de diplôme",
       x = "Niveau de diplôme",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

#établissement d'études supérieures 
data <- data %>%
  mutate(Type_etudsup = ifelse(is.na(Type_etudsup), "NA", Type_etudsup))
ggplot(data, aes(x = Type_etudsup)) +
  geom_bar() +
  labs(title = "Etablissement d'études supérieures",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

filtered_data <- data %>%
  filter(Type_etudsup != "NA") %>%
  group_by(Type_etudsup) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(filtered_data, aes(x = Type_etudsup, y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5) +  # Ajuste la position verticale des étiquettes
  labs(title = "Distribution des établissements en pourcentages",
       x = "établissement",
       y = "Pourcentage") +
  theme_minimal() 

#Discipline du cursus:



#Statut d'emploi
data <- data %>%
  mutate(Emp_cont_short = case_when(
    Emp_cont == "Statut d'indépendant·e (Auto-entrepreneur·se, portage salarial, société unipersonnelle...)" ~ "Indépendant.e",
    TRUE ~ Emp_cont  # Pour garder les autres valeurs inchangées
  ))


ggplot(data, aes(x = Emp_cont_short)) +
  geom_bar() +
  labs(title = "Statut d'emploi",
       x = "",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()


#Type d'organisation employeuse 

data <- data %>%
  mutate(Org_type = ifelse(is.na(Org_type), "NA", Org_type))
ggplot(data, aes(x = Org_type)) +
  geom_bar() +
  labs(title = "Type d'organisation employeuse",
       x = "catégorie",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

#Niveau hérarchique (hors consultant.es)

filtered_data <- data %>%
  filter(Emp_cons == "Non") %>%
  mutate(Emp_statut = ifelse(is.na(Emp_statut), "NA", Emp_statut))

ggplot(filtered_data, aes(x = Emp_statut)) +
  geom_bar() +
  labs(title = "Niveau hiérarchique (hors consultant.es)",
       x = "Niveau hiérarchique",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

#Niveau hiérarchique (consultant.es seulement)
filtered_data <- data %>%
  filter(Emp_cons == "Oui") %>%
  mutate(Emp_statut_cons = ifelse(is.na(Emp_statut_cons), "NA", Emp_statut_cons))

ggplot(filtered_data, aes(x = Emp_statut_cons)) +
  geom_bar() +
  labs(title = "Niveau hiérarchique (consultant.es seulement)",
       x = "Niveau hiérarchique",
       y = "Nombre d'observations") +
  theme_minimal() +
  coord_flip()

#Année du premier CDD ou CDI de la carrière
ggplot(data, aes(x = Prem_contrat_annee)) +
  geom_bar() +
  labs(title = "Année du premier CDD ou CDI de la carrière",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()

#Année du premier poste en RSE
ggplot(data, aes(x = Prem_rse_annee_rec)) +
  geom_bar() +
  labs(title = "Année du premier poste en RSE",
       x = "année",
       y = "Nombre d'observations") +
  theme_minimal()





