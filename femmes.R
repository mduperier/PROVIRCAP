setwd("PROVIRCAP")
data <- read.csv("data_assoquest_recoded_fmt.csv")
library(dplyr)
library(ggplot2)

data <- data %>%
  mutate(Statut_soc_passe= case_when(
    Statut_soc_passe == "Bas 1"~1,
    Statut_soc_passe == "2"~ 2,
    Statut_soc_passe == "3"~ 3,
    Statut_soc_passe == "4"~ 4,
    Statut_soc_passe == "5"~ 5,
    Statut_soc_passe == "6"~ 6,
    Statut_soc_passe == "7"~ 7,
    Statut_soc_passe == "8"~ 8,
    Statut_soc_passe == "9"~ 9,
    Statut_soc_passe == "Sommet 10"~10,
    TRUE ~ NA_real_
  )) 


data$diff_statut <- data$Statut_soc_pres - data$Statut_soc_passe


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


