
# Création du dataframe
df_result_param <- data.frame(
  labels = c("Record Nour", "record Salon", "Votre record"),
  valeur = c(32, 98, 16)
)


create_stacked_bar_plot <- function(df_result_param, moyenne = NULL) {

  # aa <- data.frame(Values = c(df_result_param$valeur[1],df_result_param$valeur[2],df_result_param$valeur[3]))
  
  # Trie des valeurs
  # Obtenir l'index des valeurs triées
  index <- order(df_result_param$valeur)
  
  # Réorganiser le dataframe en fonction de l'index
  df_result_param <- df_result_param[index, ]
  
  
  #Permet d'e trier d'affciher les valeurs dans l'ordre alphabetiques qui correspond à l'ordre décroissant des valeurs
  rownames(df_result_param) <- c("C","B","A")
  
  labels_inside_bars <- df_result_param$valeur
  
  #Reformatage des valeurs pour le diagramme empilé
  df_result_param$valeur[3] <- df_result_param$valeur[3] - df_result_param$valeur[2]
  df_result_param$valeur[2] <- df_result_param$valeur[2] - df_result_param$valeur[1]
  
  df_result_param$Ordre <- rownames(df_result_param)
  
  # Définition des étiquettes de la légende
  labels <- c(df_result_param$labels[3], df_result_param$labels[2], df_result_param$labels[1])
  #Associé au label son dataframe
  
  # Ajout d'une colonne x à df_result_param
  df_result_param$x <- 1
  
  # Tracé du graphique
  p <- ggplot(df_result_param, aes(fill = Ordre, y = valeur, x = x)) +
    geom_bar(position = "stack", stat = "identity") +
    geom_text(aes(label = labels_inside_bars), position = position_stack(vjust = 0.5), size = 3) +
    labs(y = "Valeur",
         fill = "Résultats",  # Changement du nom de la légende
         title = "Résultats test CMJ",
         subtitle = "En centimètres") +
    scale_fill_manual(values = c("A" = "#DCC283", "B" = "#67AF5E", "C" = "#81BFE0"),
                      labels = labels) +  # Modification des étiquettes de la légende
    scale_x_continuous(breaks = NULL) + # Suppression de l'échelle sur l'axe x
    scale_y_continuous(breaks = seq(0, sum(df_result_param$valeur), by = 5)) + # Modifie l'axe des y
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Ajout de la ligne de moyenne en pointillé si le paramètre moyenne est fourni
  if (!is.null(moyenne)) {
    p <- p + geom_hline(aes(yintercept = moyenne, linetype = "Moyenne"), size = 1) +
      scale_linetype_manual(name = "Légende", values = "longdash", labels = "Moyenne",
                            guide = guide_legend(override.aes = list(fill = NA, color = "black")))
  }
  
  
  return(p)
  
}

create_stacked_bar_plot(df_result_param, mean(df_result_param$valeur))
