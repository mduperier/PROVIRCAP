# Charger les bibliothèques nécessaires
install.packages("dendextend") # Pour visualiser les dendrogrammes de manière avancée
library(dendextend)
library(dplyr)
install.packages("fastDummies")
install.packages("cluster")
library(fastDummies)
library(cluster)

# Étape 1 : Charger les données
setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")

data <- data %>%
  select(age, Genre, Rem_annuel)

print(data$Genre[1:20])

# Encodage One-Hot pour les variables catégorielles
data_encoded <- dummy_cols(data, select_columns = c("Genre"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Identifier les colonnes binaires
binary_cols = c("Genre_Une femme", "Genre_NA")

# Calcul de la distance de Gower avec daisy du package cluster en spécifiant les types de variables
distance_matrix <- daisy(data_encoded, metric = "gower", type = list(binary = binary_cols))

# Effectuer la CAH
hc <- hclust(distance_matrix, method = "complete")

# Tracer le dendrogramme
plot(hc, main = "Dendrogramme de la Classification Ascendante Hiérarchique", xlab = "", sub = "", cex = 0.9)


# Découper le dendrogramme pour obtenir les groupes
# Par exemple, on veut 6 groupes
groups <- cutree(hc, k = 6)

# Ajouter les groupes aux données d'origine
data$group <- groups

# Afficher les données avec les groupes
print(data)


table(data$group)


dend <- as.dendrogram(hc)
dend_colored <- color_branches(dend, k = 6)
plot(dend_colored, main = "Dendrogramme coloré par groupes", labels=FALSE)
