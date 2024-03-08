library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble) 

parse_xml_file <- function(xml_file, columns_to_extract) {
  # Lire le fichier XML ligne par ligne
  xml_lines <- read_lines(xml_file)

  # Initialiser une liste pour stocker les lignes entre les balises Row
  rows_list <- list()
  
  # Initialiser un vecteur pour stocker les lignes entre les balises Row
  current_row <- c()
  
  # Boucle pour lire ligne par ligne
  for (line in xml_lines) {
    # Vérifier si la ligne contient "<Row>"
    if (grepl("<Row>", line)) {
      # Si la ligne contient "<Row>", initialiser un nouveau vecteur pour stocker les lignes entre les balises Row
      current_row <- c()
    } else if (grepl("</Row>", line)) {
      # Si la ligne contient "</Row>", ajouter le vecteur courant à la liste et réinitialiser le vecteur courant
      rows_list <- c(rows_list, list(current_row))
      current_row <- c()
    } else {
      # Si la ligne ne contient ni "<Row>" ni "</Row>", ajouter la ligne au vecteur courant
      current_row <- c(current_row, line)
    }
  }
  
  # Filtrer les lignes de chaque liste de liste
  filtered_rows_list <- lapply(rows_list[1], function(row) {
    row[!grepl("</Cell>|<Cell>", row)]
  })
  
  # Fonction pour trouver les numéros de lignes correspondant à chaque mot clé dans la première liste de la liste de liste
  find_row_numbers <- function(keywords, filt_rows_list) {
    # Initialiser une liste pour stocker les numéros de lignes correspondant à chaque mot clé
    row_numbers <- list()
    
    # Parcourir chaque mot clé
    for (keyword in keywords) {
      # Initialiser un vecteur pour stocker les numéros de lignes correspondant au mot clé courant
      current_row_numbers <- c()
      
      # Parcourir chaque ligne de la liste
      for (i in 1:length(filt_rows_list[[1]])) {
        # Vérifier si le mot clé courant est un mot entier dans la ligne
        if (any(str_detect(filt_rows_list[[1]][i], fixed(keyword)))) {
          valeur <- gsub("<Data ss:Type=\"String\">|<Data ss:Type=\"Number\">|</Data>", "", filt_rows_list[[1]][i])
          valeur <- gsub("\\s+", "", valeur) #Supprime les espaces
          keyword <- gsub("\\s+", "", keyword) #Supprime les espaces
          
          
          #Verifie si le mot est vraiment identique
          if (nchar(valeur) == nchar(keyword)) {
            
            current_row_numbers <- c(current_row_numbers, i)
          }
        }
      }
      
      # Ajouter les numéros de lignes correspondant au mot clé courant à la liste principale
      row_numbers <- c(row_numbers, current_row_numbers)
    }
    
    # Retourner une liste de listes contenant les numéros de lignes correspondant à chaque mot clé
    return(row_numbers)
  }
  
  # Exemple d'utilisation de la fonction
  row_numbers <- find_row_numbers(columns_to_extract, filtered_rows_list)
  print(row_numbers)
  
  get_values_at_indexes <- function(index_list, rows_list_complete) {
    dataframe <- data.frame()
    # Parcours de la liste de listes
    for (i in 2:length(rows_list_complete)) {
      # Initialisation du compteur
      compteur <- 0
      
      # Parcours de chaque élément de la liste de listes
      for (j in 1:length(rows_list_complete[[i]])) {
        # Vérification si l'élément contient "Data ss:Type"
        if (grepl("Data ss:Type", rows_list_complete[[i]][j])) {
          # Incrémentation du compteur
          compteur <- compteur + 1
          # Si la ligne contient le motif, extraire la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression régulière pour extraire les valeurs entre les balises
          valeur <- gsub("<Data ss:Type=\"String\">|<Data ss:Type=\"Number\">|</Data>", "", ligne)
          dataframe <- rbind(dataframe, data.frame(Numéro = compteur, Valeur = valeur, liste = (i - 1)))
        } else if(grepl("ss:Index", rows_list_complete[[i]][j])) {
          # Si la ligne contient le motif, extraire la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression régulière pour extraire les nombres
          numbers <- gsub("[^0-9]", "", ligne)
          compteur <- (as.numeric(numbers) - 1)
        }
      }
      # Filtrer les lignes pour ne conserver que celles où le numéro est dans index_list
      dataframe <- subset(dataframe, Numéro %in% index_list)
    }
    
    # Retourne le compteur
    return(dataframe)
  }
  
  df_global <- get_values_at_indexes(unlist(row_numbers), rows_list)
  
  relation_mot_num <- data.frame(num_ligne = integer(), mot_cle = character())
  
  # Parcourir chaque mot clé
  for (i in 1:length(columns_to_extract)) {
    # Extraire les numéros de ligne correspondant au mot clé courant
    current_row_numbers <- unlist(row_numbers[i])
    
    # Ajouter les numéros de ligne et le mot clé courant au dataframe principal
    relation_mot_num <- rbind(relation_mot_num, data.frame(num_ligne = current_row_numbers, mot_cle = columns_to_extract[i]))
  }
  
  
  df_global <- merge(relation_mot_num, df_global, by.x = "num_ligne", by.y = "Numéro", all.x = TRUE)
  df_global <- df_global %>% arrange(liste)
  
  df_wide <- df_global %>%
    pivot_wider(
      names_from = liste,
      values_from = Valeur
    ) %>%
    mutate(across(everything(), trimws))
  
  # Renommer les colonnes si nécessaire
  names(df_wide) <- c("mot_cle", seq_along(names(df_wide))[-1])
  
  # Décalage du contenu des colonnes
  for (i in 1:(ncol(df_wide)-1)) {
    df_wide[, i] <- df_wide[, i+1]
  }
  
  # Suppression de la dernière colonne
  df_wide <- df_wide[, -ncol(df_wide)]
  
  return(df_wide)
}

# # Exemple d'utilisation
# xml_file <- "AK_Nour_Tapping.xml"
# columns_to_extract <- c("Test", "Date", "Heure", "G/D", "Temps de vol", "Temps de contact", "Rythme[p/s]", "Rythme[pas/m]", "Point d'équilibreX", "Décalage latéralX", "Largeur de pas")
# 
# #Resultats en colonnes
# result <- parse_xml_file(xml_file, columns_to_extract)
# 
# 
# # Si préférence résultats en ligne
# 
# ###############################################
# result_transpose <- as.data.frame(t(result))
# # Définir les noms de colonnes avec la première ligne
# names(result_transpose) <- unlist(result_transpose[1, ])
# # Supprimer la première ligne car elle est maintenant utilisée comme noms de colonnes
# result_transpose <- result_transpose[-1, ]
# print(result_transpose)
# ##############################################


#Utilisée pour le CMJ
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



remove_special_characters <- function(input_file, output_file) {
  # Lire le fichier CSV avec la bibliothèque readr
  df <- readr::read_csv(input_file, show_col_types = FALSE)
  
  # Supprimer les caractères spéciaux avec une expression régulière
  for (col in names(df)) {
    df[[col]] <- gsub("[^[:alnum:] ]", "", df[[col]])
  }
  
  # Sauvegarder le fichier nettoyé en CSV
  write.csv(df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
}


################################################################################
#Suppression des NA dans certaines colonnes d'intérêt seulement
################################################################################

supprimer_lignes_na <- function(df) {
  # Identifier les lignes contenant des valeurs manquantes dans l'une des colonnes spécifiées
  lignes_a_supprimer <- which(rowSums(is.na(df[, c(29, 30, 64)])) > 0)
  
  # Supprimer les lignes identifiées
  df <- df[-lignes_a_supprimer, ]
  
  # Retourner le dataframe modifié
  return(df)
}


################################################################################
# Remplacer les NAs par la moyenne de la colonne 
################################################################################

remplacer_na_par_moyenne <- function(df, liste_de_colonnes) {
  for (colonne in liste_de_colonnes) {
    if (is.numeric(df[,colonne])) {
      moyenne <- mean(df[,colonne], na.rm = TRUE)
      df[,colonne] <- replace(df[,colonne], is.na(df[,colonne]), moyenne)
      #showNotification(paste("Les valeurs manquantes dans la colonne", colonne, "ont été remplacées par la moyenne."), type = "message")
    } else {
      # Convertir la colonne en numérique
      df[,colonne] <- as.numeric(df[,colonne])
      # Vérifier si la conversion a réussi
      if (!all(is.na(df[,colonne]))) {
        moyenne <- mean(df[,colonne], na.rm = TRUE)
        df[,colonne] <- replace(df[,colonne], is.na(df[,colonne]), moyenne)
        showNotification(paste("La colonne", colonne, "a été convertie en numérique et les valeurs manquantes ont été remplacées par la moyenne."), type = "message")
      } else {
        showNotification(paste("La colonne", colonne, "ne peut pas être convertie en numérique. Elle sera ignorée."), type = "warning")
      }
    }
  }
  return(df)
}

################################################################################
# Créer les dataframes gauche et droit
################################################################################

# Df_lateraux <- function(DF) {
#   DF_gauche <- subset(DF, DF[, 5] == "G")
#   DF_droit <- subset(DF, DF[, 5] == "D")
#   
#   return( list(DF_gauche = DF_gauche, DF_droit = DF_droit))
# }

Df_lateraux <- function(DF) {
  # Création d'un index pour les valeurs "G" et "D"
  index_G <- DF[, 5] == "G"
  index_D <- DF[, 5] == "D"
  
  # Filtrer le dataframe en utilisant l'index
  DF_gauche <- DF[index_G, ]
  DF_droit <- DF[index_D, ]
  
  # Retourner les dataframes filtrés dans une liste
  return(list(DF_gauche = DF_gauche, DF_droit = DF_droit))
}

################################################################################
# Graphes taping
################################################################################

#3D
Graphtapping_3D <- function( DF_Gauche, DF_Droit){
  # Créer une palette de couleurs pour DF_gauche
  colors_gauche <-
    colorRampPalette(c("#E9C645", "#AE5853"))(nrow(DF_gauche))
  
  # Créer une palette de couleurs pour DF_droit
  colors_droite <-
    colorRampPalette(c("#A1CCCD", "#6559A7"))(nrow(DF_droit))
  
  # Créer le graphique pour les données du dataframe DF_gauche
  fig_gauche <- plot_ly(
    DF_gauche,
    x = ~ ifelse(DF_gauche[, 5] == "G",-DF_gauche[, 4] / 2, DF_gauche[, 4] / 2),
    y = ~ DF_gauche[, 3],
    z = ~ DF_gauche[, 2],
    color = ~ DF_gauche[, 2],
    # Utiliser le temps de vol comme couleur
    colors = colors_gauche,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(symbol = 'circle', sizemode = 'diameter'),
    sizes = c(5, 150),
    text = ~ paste(
      'DécalageDFlatéralX:',
      ifelse(DF_gauche[, 5] == "G",-DF_gauche[, 4] / 2, DF_gauche[, 4] / 2),
      '<br>Temps de contact:',
      DF_gauche[, 3],
      '<br>Temps de vol:',
      DF_gauche[, 2],
      '<br>Largeur de pas:',
      DF_gauche[, 4],
      '<br>GDFD:',
      DF_gauche[, 5]
    )
  )
  
  # Créer le graphique pour les données du dataframe DF_droit
  fig_droite <- plot_ly(
    DF_droit,
    x = ~ ifelse(DF_droit[, 5] == "G",-DF_droit[, 4] / 2, DF_droit[, 4] / 2),
    y = ~ DF_droit[, 3],
    z = ~ DF_droit[, 2],
    color = ~ DF_droit[, 2],
    # Utiliser le temps de vol comme couleur
    colors = colors_droite,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(symbol = 'circle', sizemode = 'diameter'),
    sizes = c(5, 150),
    text = ~ paste(
      'DécalageDFlatéralX:',
      ifelse(DF_droit[, 5] == "G",-DF_droit[, 4] / 2, DF_droit[, 4] / 2),
      '<br>Temps de contact:',
      DF_droit[, 3],
      '<br>Temps de vol:',
      DF_droit[, 2],
      '<br>Largeur de pas:',
      DF_droit[, 4],
      '<br>GDFD:',
      DF_droit[, 5]
    )
  )
  
  fig <- subplot(fig_gauche, fig_droite, nrows = 1)
  
  fig <- fig %>% layout(
    title = list(text = 'Relation entre Décalage latéral, Temps de vol et Temps de contact',
                 font = list(color = 'white')),
    # Couleur du titre en blanc
    scene = list(
      xaxis = list(
        title = 'Décalage latéral',
        gridcolor = 'rgba(0,0,0,0)',
        # Supprimer le quadrillage pour l'axe x
        titlefont = list(color = 'white'),
        # Couleur du titre de l'axe x en blanc
        tickfont = list(color = 'white')
      ),
      # Couleur des graduations de l'axe x en blanc
      yaxis = list(
        title = 'Temps de contact',
        gridcolor = 'rgba(0,0,0,0)',
        # Supprimer le quadrillage pour l'axe y
        titlefont = list(color = 'white'),
        # Couleur du titre de l'axe y en blanc
        tickfont = list(color = 'white')
      ),
      # Couleur des graduations de l'axe y en blanc
      zaxis = list(
        title = 'Temps de vol',
        gridcolor = 'rgba(0,0,0,0)',
        # Supprimer le quadrillage pour l'axe z
        titlefont = list(color = 'white'),
        # Couleur du titre de l'axe z en blanc
        tickfont = list(color = 'white')
      ) # Couleur des graduations de l'axe z en blanc
    ),
    paper_bgcolor = 'rgb(22, 22, 29)',
    # Couleur de fond type eigengrau
    plot_bgcolor = 'rgb(22, 22, 29)',
    # Couleur de fond type eigengrau
    scene_bgcolor = 'rgb(22, 22, 29)',
    # Couleur de fond type eigengrau
    colorbar = list(
      title = 'Temps de vol',
      titlefont = list(color = 'white'),
      tickfont = list(color = 'white')
    ) # Ajouter une légende de couleur
  )
  return(fig)
}


creer_barplot <- function(DF_gauche, DF_droit, colonne) {
  moyenne_gauche <- mean(DF_gauche[, colonne])
  moyenne_droit <- mean(DF_droit[, colonne])
  ecart_type_gauche <- sd(DF_gauche[, colonne])
  ecart_type_droit <- sd(DF_droit[, colonne])
  
  # Calcul des limites
  limite_inferieure_gauche <- moyenne_gauche - ecart_type_gauche
  limite_superieure_gauche <- moyenne_gauche + ecart_type_gauche
  limite_inferieure_droit <- moyenne_droit - ecart_type_droit
  limite_superieure_droit <- moyenne_droit + ecart_type_droit
  
  p <- plot_ly() %>%
    add_bars(
      x = c("DF_gauche", "DF_droit"),
      y = c(moyenne_gauche, moyenne_droit),
      name = "Moyenne",
      marker = list(color = c('#1f77b4', '#ff7f0e'))
    ) %>%
    add_segments(
      x = c("DF_gauche", "DF_gauche", "DF_droit", "DF_droit"),
      xend = c("DF_gauche", "DF_gauche", "DF_droit", "DF_droit"),
      y = c(
        limite_inferieure_gauche,
        limite_superieure_gauche,
        limite_inferieure_droit,
        limite_superieure_droit
      ),
      yend = c(
        limite_inferieure_gauche,
        limite_superieure_gauche,
        limite_inferieure_droit,
        limite_superieure_droit
      ),
      name = "Limites",
      type = 'scatter',
      mode = 'lines',
      line = list(dash = 'dash', color = '#FFFFFF')
    ) %>%
    add_trace(
      x = c("DF_gauche", "DF_droit"),
      y = c(
        moyenne_gauche + ecart_type_gauche,
        moyenne_droit + ecart_type_droit
      ),
      name = "Moyenne + écart-type",
      type = "scatter",
      mode = "markers",
      marker = list(color = 'white', symbol = "triangle-up"),
      text = c(
        round(moyenne_gauche + ecart_type_gauche, 2),
        round(moyenne_droit + ecart_type_droit, 2)
      ),
      textposition = "top",
      textfont = list(color = "white")
    ) %>%
    add_trace(
      x = c("DF_gauche", "DF_droit"),
      y = c(
        moyenne_gauche - ecart_type_gauche,
        moyenne_droit - ecart_type_droit
      ),
      name = "Moyenne - écart-type",
      type = "scatter",
      mode = "markers",
      marker = list(color = 'white', symbol = "triangle-down"),
      text = c(
        round(moyenne_gauche - ecart_type_gauche, 2),
        round(moyenne_droit - ecart_type_droit, 2)
      ),
      textposition = "bottom",
      textfont = list(color = "white")
    ) %>%
    layout(
      xaxis = list(title = "DataFrame", color = '#FFFFFF'),
      yaxis = list(
        title = paste("Valeurs de la colonne", "Temps de contact"),
        color = '#FFFFFF'
      ),
      title = list(
        text = paste(
          "Comparaison de la moyenne et de l'écart type de la colonne",
          colonne,
          "entre DF_gauche et DF_droit"
        ),
        color = '#FFFFFF'
      ),
      showlegend = TRUE,
      plot_bgcolor = '#16161d',
      # Couleur de fond "eigengrau"
      paper_bgcolor = '#16161d',
      # Couleur de fond du papier "eigengrau"
      legend = list(font = list(color = '#FFFFFF'))
    )
  return(p)
}


creer_grapheline <- function(DF_gauche, DF_droit, indice_colonne) {
  p <- plot_ly() %>%
    add_lines(
      x = ~ seq_along(DF_gauche[, indice_colonne]),
      y = ~ DF_gauche[, indice_colonne],
      name = "DF_gauche",
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
    add_lines(
      x = ~ seq_along(DF_droit[, indice_colonne]),
      y = ~ DF_droit[, indice_colonne],
      name = "DF_droit",
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
    layout(
      xaxis = list(title = "Nombre de pas", color = '#FFFFFF'),
      yaxis = list(
        title = paste("Valeurs de la colonne", " temps de contact"),
        color = '#FFFFFF'
      ),
      title = list(
        text = paste(
          "Comparaison des valeurs de la colonne",
          indice_colonne,
          "entre DF_gauche et DF_droit"
        ),
        color = '#FFFFFF'
      ),
      showlegend = TRUE,
      plot_bgcolor = '#16161d',
      # Couleur de fond "eigengrau"
      paper_bgcolor = '#16161d',
      # Couleur de fond du papier "eigengrau"
      legend = list(font = list(color = '#FFFFFF'))
    )
  return(p)
}



creer_camembert <- function(DF, indice_de_colonne) {
  # Filtrer les données pour les lignes où la colonne "G/D" vaut "G" ou "D"
  DF_gauche <- DF[DF$`G.D` == "G", ]
  DF_droit <- DF[DF$`G.D` == "D", ]
  
  # Calculer la moyenne pour chaque DataFrame
  moyenne_gauche <- mean(DF_gauche[, indice_de_colonne], na.rm = TRUE)
  moyenne_droit <- mean(DF_droit[, indice_de_colonne], na.rm = TRUE)
  
  # Créer les étiquettes et les valeurs pour le graphique camembert
  labels <- c('Gauche', 'Droit')
  valeurs <- c(moyenne_gauche, moyenne_droit)
  
  # Créer le graphique camembert avec Plotly
  camembert <- plot_ly(labels = labels, values = valeurs, type = 'pie',
                       text = paste("Côté: ", labels, "<br>", "Moyenne: ", round(valeurs, 2)),
                       hoverinfo = "text", marker = list(colors = c('#C5243D', '#2C2F65'))) %>%
    layout(title = "Moyenne des données gauche et droite", showlegend = FALSE)
  
  # Renvoyer la figure du graphique camembert
  return(camembert)
}





test_graphique <- function(athlete_data, index_max) {
  # Vérifier si athlete_data contient les bonnes colonnes
  print(names(athlete_data))
  
  # Vérifier si index_max est correct
  print(index_max)
  
  # Vérifier si les données pour le graphique sont correctement spécifiées
  print(athlete_data$Nom)
  print(athlete_data$Hauteur)
  
  # Vérifier les dimensions des données pour le graphique
  print(dim(athlete_data))
}


