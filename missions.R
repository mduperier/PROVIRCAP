data <- read.csv("data_assoquest_recoded_fmt.csv")

#on créé le dataframe environnement
data_env <- data %>%
  filter(Dep_rse3_1=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_1)%>%
  mutate(mission = case_when(
    mission =='Oui'~'environnement',
    TRUE ~ mission))
print(data_env$mission[1:20])

#on créé le dataframe qvt
data_qvt <- data %>%
  filter(Dep_rse3_2=='Oui')%>%
  select(-Dep_rse3_1, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_2)%>%
  mutate(mission = case_when(
    mission =='Oui'~'qualité vie travail',
    TRUE ~ mission))
print(data_qvt$mission[1:20])

#on créé le dataframe diversité et inclusion
data_div <- data %>%
  filter(Dep_rse3_3=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_1, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_3)%>%
  mutate(mission = case_when(
    mission =='Oui'~'diversité et inclusion',
    TRUE ~ mission))
print(data_div$mission[1:20])

#on créé le dataframe investissement responsable
data_inv <- data %>%
  filter(Dep_rse3_4=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_1, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_4)%>%
  mutate(mission = case_when(
    mission =='Oui'~'investissement responsable',
    TRUE ~ mission))
print(data_inv$mission[1:20])

#on créé le dataframe mécénat
data_mec <- data %>%
  filter(Dep_rse3_5=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_1, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_5)%>%
  mutate(mission = case_when(
    mission =='Oui'~'mécénat',
    TRUE ~ mission))
print(data_mec$mission[1:20])

#on créé le dataframe droits humains
data_dh <- data %>%
  filter(Dep_rse3_6=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_1, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_6)%>%
  mutate(mission = case_when(
    mission =='Oui'~'droits humains',
    TRUE ~ mission))
print(data_dh$mission[1:20])

#on créé le dataframe éthique et conformité
data_eth <- data %>%
  filter(Dep_rse3_7=='Oui')%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_1)%>%
  rename(mission = Dep_rse3_7)%>%
  mutate(mission = case_when(
    mission =='Oui'~'éthique et conformité',
    TRUE ~ mission))
print(data_eth$mission[1:20])

#on créé le dataframe des NA:
data_na <- data %>%
  filter((Dep_rse3_1!='Oui')&(Dep_rse3_2!='Oui')&(Dep_rse3_3!='Oui')&(Dep_rse3_4!='Oui')&(Dep_rse3_5!='Oui')&(Dep_rse3_6!='Oui')&(Dep_rse3_7!='Oui'))%>%
  select(-Dep_rse3_2, -Dep_rse3_3, -Dep_rse3_4, -Dep_rse3_5, -Dep_rse3_6, -Dep_rse3_7)%>%
  rename(mission = Dep_rse3_1)%>%
  mutate(mission = case_when(
    mission =='Oui'~ mission,
    TRUE ~ 'NA'))
print(data_na$mission[1:20])






#on combine toutes les observations:
combined_data <- bind_rows(data_env, data_qvt, data_div, data_inv, data_mec, data_dh, data_eth, data_na)

unique_values <- unique(combined_data$mission)
print(unique_values)

#on affiche le nombre d'observations:
nombre_observations <- nrow(combined_data)
print(nombre_observations)
#on a 685 observations, contre 315 dans le dataframe de base, c'est normal parce
#qu'on s'autorise à répéter une observation si celle ci à renseigner
#plusieurs missions, alors l'observation apparaitra autant de fois que de mission
#séléctionnée, avec à chaque fois la valeur "mission" correspondant à une 
#des mission cochées.




# STATISTIQUES DESCRIPTIVES AVEC CE NOUVEAU DATAFRAME

gender_distribution <- combined_data %>%
  filter(mission!='NA')%>%
  filter(!is.na(Genre)) %>%
  group_by(mission, Genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(gender_distribution, aes(x = mission, y = percentage, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition du Genre par type de mission",
       x = "type de mission",
       y = "Pourcentage",
       fill = "Genre") +
  theme_minimal()+
  coord_flip()

print(p)
ggsave(filename = "genre_miss.png", plot = p, width = 8, height = 6)


#On créé une fonction qui fait ce graphique automatiquement pour être plus efficace

library(rlang)

generate_distribution_plot <- function(data, column_name, output_filename) {
  # Convertir le nom de la colonne en un symbole
  column_sym <- sym(column_name)
  
  # Créer le DataFrame de distribution
  distribution <- data %>%
    filter(mission != 'NA') %>%
    filter(!is.na(!!column_sym)) %>%
    group_by(mission, !!column_sym) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Générer le plot
  p <- ggplot(distribution, aes(x = mission, y = percentage, fill = !!column_sym)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
    labs(title = paste("Répartition par", column_name, "et type de mission"),
         x = "Type de mission",
         y = "Pourcentage",
         fill = column_name) +
    theme_minimal() +
    coord_flip()
  
  # Afficher le plot
  print(p)
  
  # Sauvegarder le plot
  ggsave(filename = output_filename, plot = p, width = 8, height = 6)
}


#le secteur
generate_distribution_plot(combined_data, "Emp_sec", "secteur_miss.png")

#Emploi entièrement dédié à la RSE?
generate_distribution_plot(combined_data, "Emp_rse", "entier_rse_miss.png")

#Le type de mission
generate_distribution_plot(combined_data, "Rse_miss", "miss_miss.png")

#Le type d'organisation
generate_distribution_plot(combined_data, "Org_type", "org_type_miss.png")

#temps à travailler en dehors de l'entreprise
generate_distribution_plot(combined_data, "Trav_ext", "trav_ext_miss.png")

#outil le plus présent
generate_distribution_plot(combined_data, "Outil_rse", "outil_miss.png")

#type d'étude
generate_distribution_plot(combined_data, "Type_etudsup", "etude_miss.png")

#spécialité
combined_data <- combined_data %>%
  mutate(Spe_etudsup= case_when(
    Spe_etudsup == "Sciences humaines et sociales (histoire, sociologie, philosophie, psychologie, sciences politiques, ...)" ~ "Sciences humaines et sociales",
    TRUE ~ Spe_etudsup
  )) 
generate_distribution_plot(combined_data, "Spe_etudsup", "spe_etude_miss.png")

#Motivation pour le poste
generate_distribution_plot(combined_data, "Poste_motiv", "poste_motiv_miss.png")

#futur imaginé par les répondant.es
generate_distribution_plot(combined_data, "Rse_tps_futur", "futur_miss.png")

#Satisafait.e de votre emploi?
generate_distribution_plot(combined_data, "Satisf_emp_actu", "satisfait_miss.png")

#mot utilisé
generate_distribution_plot(combined_data, "Rse_mot", "mot_miss.png")

#la vocation de la rse
generate_distribution_plot(combined_data, "Rse_vocation_rec", "vocation_miss.png")

#avis sur les lois
generate_distribution_plot(combined_data, "Rse_lois", "lois_miss.png")

#avis sur les ong
generate_distribution_plot(combined_data, "Rse_ong_milit_rec", "ong_miss.png")

#Acteurs clés
generate_distribution_plot(combined_data, "Acteurs_clés_rse", "acteurs_miss.png")

#satisfait rémuniération 
generate_distribution_plot(combined_data, "Rem_satisf", "rem_satisf_miss.png")

#inégalités importantes, à tester avec d'autres inégalités par exemple
generate_distribution_plot(combined_data, "Ineg_imp_1", "ineg_rev_miss.png")

#se sentetn-ils favorisés?
generate_distribution_plot(combined_data, "Ineg_fav_2", "fav_patrimoine_miss.png")
generate_distribution_plot(combined_data, "Ineg_fav_3", "fav_emploi_miss.png")

#lieu de vie
generate_distribution_plot(combined_data, "Lieu_vie", "lieu_vie_miss.png")

#statut_soc_pres: statut social
generate_distribution_plot(combined_data, "Statut_soc_pres", "statut_miss.png")
generate_distribution_plot(combined_data, "Statut_soc_passe", "statut_passe_miss.png")

#religion
generate_distribution_plot(combined_data, "Relig", "relig_miss.png")

#position politique
generate_distribution_plot(combined_data, "Pol_pos_SQ001", "politique_miss.png")


