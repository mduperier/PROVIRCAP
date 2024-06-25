data <- read.csv("data_assoquest_recoded_fmt.csv")

# Discrétisation de la variable rem_annuel en intervalles de revenus
data$rem_interval <- cut(data$Rem_annuel, breaks = seq(20000, 100000, by = 10000), include.lowest = TRUE)

# Filtrer les données pour obtenir uniquement les 'Tout à fait satisfait'
data_satisfait <- data[data$Rem_satisf == "Pas du tout satisfaisant", ]

# Compter le nombre de 'Tout à fait satisfait' par intervalle de revenus
count_data <- as.data.frame(table(data_satisfait$rem_interval))
names(count_data) <- c("rem_interval", "count")

# Tracer le graphique
p<-ggplot(count_data, aes(x = rem_interval, y = count)) +
  geom_bar(stat = "identity", fill = "#ADD8E6") +
  labs(title = "Pas du tout satisfaisant' par intervalle de revenus",
       x = "Intervalle de revenus",
       y = "Nombre de 'Tout à fait satisfait'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
ggsave(filename = "rem_insatisf.png", plot = p, width = 8, height = 6)

unique_values <- unique(data$Rem_satisf)
print(unique_values)
