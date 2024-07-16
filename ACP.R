# Exemple avec la fonction model.matrix
data <- read.csv("data_assoquest_recoded_fmt.csv")

data <- data %>%
  select(age, Genre, Rem_annuel)

# Encodage des variables catégorielles (par exemple, 'Genre')
encoded_data <- model.matrix(~ Genre - 1, data = data)

# Appliquer l'ACP sur les données transformées
acp <- prcomp(encoded_data, center = TRUE, scale = TRUE)

# Résumé de l'ACP
summary(acp)

# Biplot des deux premières composantes principales
biplot(acp, scale = 0)
