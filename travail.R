data <- read.csv("data_assoquest_recoded_fmt.csv")

library(rlang)
library(dplyr)
library(ggplot2)

generate_distribution_plot <- function(data, column_name, output_filename) {
  # Convertir le nom de la colonne en un symbole
  column_sym <- sym(column_name)
  
  # Créer le DataFrame de distribution
  distribution <- data %>%
    filter(Emp_statut_rec3 != 'NA') %>%
    filter(!is.na(!!column_sym)) %>%
    group_by(Emp_statut_rec3, !!column_sym) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Générer le plot
  p <- ggplot(distribution, aes(x = Emp_statut_rec3, y = percentage, fill = !!column_sym)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
    labs(title = paste("Répartition par", column_name, "et niveau hiérarchique"),
         x = "Niveau hiérarchique",
         y = "Pourcentage",
         fill = column_name) +
    theme_minimal() +
    coord_flip()
  
  # Afficher le plot
  print(p)
  
  # Sauvegarder le plot
  ggsave(filename = output_filename, plot = p, width = 8, height = 6)
}


generate_distribution_plot(data, "Trav_avis_1", "trav_1.png")
generate_distribution_plot(data, "Trav_avis_2", "trav_2.png")
generate_distribution_plot(data, "Trav_avis_3", "trav_3.png")
generate_distribution_plot(data, "Trav_avis_4", "trav_4.png")
generate_distribution_plot(data, "Trav_avis_5", "trav_5.png")
generate_distribution_plot(data, "Trav_avis_6", "trav_6.png")
generate_distribution_plot(data, "Trav_avis_7", "trav_7.png")
generate_distribution_plot(data, "Trav_avis_8", "trav_8.png")
generate_distribution_plot(data, "Trav_avis_9", "trav_9.png")
generate_distribution_plot(data, "Trav_avis_10", "trav_10.png")
generate_distribution_plot(data, "Trav_avis_11", "trav_11.png")
