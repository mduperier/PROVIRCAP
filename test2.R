setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")

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

# Afficher les premières valeurs de la colonne "nom_colonne"
head(data$token)
print(data$submitdate[1:20])
print(data$Emp_sec[1:20])
print(data$age[1:20])
