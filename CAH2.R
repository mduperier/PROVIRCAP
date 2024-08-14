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

data2 <- data %>%
  select(Rse_miss, Emp_statut_rec3)

data_encoded <- dummy_cols(data2, select_columns = c("Rse_miss"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
data_encoded <- dummy_cols(data_encoded, select_columns = c("Emp_statut_rec3"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

distance_matrix <- daisy(data_encoded, metric = "gower")

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
plot(dend_colored, main = "Dendrogramme coloré par groupes", leaflab = "none")



t_age <- data %>%
  group_by(group) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

print(t_age)



t_rem <- data %>%
  group_by(group) %>%
  summarise(
    mean_rem = mean(Rem_annuel, na.rm = TRUE),
    med_rem = median(Rem_annuel, na.rm = TRUE),
    min_rem = min(Rem_annuel, na.rm = TRUE),
    max_rem = max(Rem_annuel, na.rm = TRUE),
    sd_rem = sd(Rem_annuel, na.rm = TRUE)
  )

print(t_rem)



gender_distribution <- data %>%
  filter(!is.na(group))%>%
  filter(!is.na(Genre)) %>%
  group_by(group, Genre) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(gender_distribution, aes(x = group, y = percentage, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition du Genre par Statut d'Emploi",
       x = "Groupe",
       y = "Pourcentage",
       fill = "Genre") +
  theme_minimal()

print(p)
ggsave(filename = "genre_dendo.png", plot = p, width = 8, height = 6)




gender_distribution <- data %>%
  filter(!is.na(group))%>%
  group_by(group, Emp_statut_rec3) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(gender_distribution, aes(x = group, y = percentage, fill = Emp_statut_rec3)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition du statut d'emploi par groupe",
       x = "Groupe",
       y = "Pourcentage",
       fill = "Statut") +
  theme_minimal()

print(p)
ggsave(filename = "statut_dendo6.png", plot = p, width = 8, height = 6)

groupe3 <- data %>%
  filter(group == 3)

groupe3$Emp_statut_rec3

gender_distribution <- data %>%
  filter(!is.na(group))%>%
  group_by(group, Rse_miss) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

p<-ggplot(gender_distribution, aes(x = group, y = percentage, fill = Rse_miss)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Répartition des missions par groupe",
       x = "Groupe",
       y = "Pourcentage",
       fill = "Missions") +
  theme_minimal()
print(p)
ggsave(filename = "dendo6.png", plot = p, width = 8, height = 6)
