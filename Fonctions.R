library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble) 
library(flexdashboard)
library(plotly)
library(DT)
library(ggplot2)
library(gridExtra)

library(ggrepel)


parse_xml_file <- function(xml_file, columns_to_extract) {
  # Lire le fichier XML ligne par ligne
  xml_lines <- read_lines(xml_file)
  
  # Initialiser une liste pour stocker les lignes entre les balises Row
  rows_list <- list()
  
  # Initialiser un vecteur pour stocker les lignes entre les balises Row
  current_row <- c()
  
  # Boucle pour lire ligne par ligne
  for (line in xml_lines) {
    # Verifier si la ligne contient "<Row>"
    if (grepl("<Row>", line)) {
      # Si la ligne contient "<Row>", initialiser un nouveau vecteur pour stocker les lignes entre les balises Row
      current_row <- c()
    } else if (grepl("</Row>", line)) {
      # Si la ligne contient "</Row>", ajouter le vecteur courant à la liste et reinitialiser le vecteur courant
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
  
  # Fonction pour trouver les numeros de lignes correspondant à chaque mot cle dans la première liste de la liste de liste
  find_row_numbers <- function(keywords, filt_rows_list) {
    # Initialiser une liste pour stocker les numeros de lignes correspondant à chaque mot cle
    row_numbers <- list()
    
    # Parcourir chaque mot cle
    for (keyword in keywords) {
      # Initialiser un vecteur pour stocker les numeros de lignes correspondant au mot cle courant
      current_row_numbers <- c()
      
      # Parcourir chaque ligne de la liste
      for (i in 1:length(filt_rows_list[[1]])) {
        # Verifier si le mot cle courant est un mot entier dans la ligne
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
      
      # Ajouter les numeros de lignes correspondant au mot cle courant à la liste principale
      row_numbers <- c(row_numbers, current_row_numbers)
    }
    
    # Retourner une liste de listes contenant les numeros de lignes correspondant à chaque mot cle
    return(row_numbers)
  }
  
  # Exemple d'utilisation de la fonction
  row_numbers <- find_row_numbers(columns_to_extract, filtered_rows_list)
  get_values_at_indexes <- function(index_list, rows_list_complete) {
    dataframe <- data.frame()
    
    # Parcours de la liste de listes
    
    str(rows_list_complete)
    for (i in 2:length(rows_list_complete)) {
      # Initialisation du compteur
      compteur <- 0
      # Parcours de chaque element de la liste de listes
      for (j in 1:length(rows_list_complete[[i]])) {
        # Verification si l'element contient "Data ss:Type"
        if (grepl("Data ss:Type", rows_list_complete[[i]][j])) {
          # Incrementation du compteur
          compteur <- compteur + 1
          # Si la ligne contient le motif, extraire la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression regulière pour extraire les valeurs entre les balises
          valeur <- gsub("<Data ss:Type=\"String\">|<Data ss:Type=\"Number\">|</Data>", "", ligne)
          dataframe <- rbind(dataframe, data.frame(Numero = compteur, Valeur = valeur, liste = (i - 1)))
        } else if(grepl("ss:Index", rows_list_complete[[i]][j])) {
          # Si la ligne contient le motif, extraire la valeur entre les balises
          ligne <- rows_list_complete[[i]][j]
          # Utilisation de l'expression regulière pour extraire les nombres
          numbers <- gsub("[^0-9]", "", ligne)
          compteur <- (as.numeric(numbers) - 1)
        }
      }
      # Filtrer les lignes pour ne conserver que celles où le numero est dans index_list
      dataframe <- subset(dataframe, Numero %in% index_list)
    }
    
    # Retourne le compteur
    return(dataframe)
  }
  
  df_global <- get_values_at_indexes(unlist(row_numbers), rows_list)
  
  relation_mot_num <- data.frame(num_ligne = integer(), mot_cle = character())
  
  # Parcourir chaque mot cle
  for (i in 1:length(columns_to_extract)) {
    # Extraire les numeros de ligne correspondant au mot cle courant
    current_row_numbers <- unlist(row_numbers[i])
    
    # Ajouter les numeros de ligne et le mot cle courant au dataframe principal
    relation_mot_num <- rbind(relation_mot_num, data.frame(num_ligne = current_row_numbers, mot_cle = columns_to_extract[i]))
  }
  
  
  df_global <- merge(relation_mot_num, df_global, by.x = "num_ligne", by.y = "Numero", all.x = TRUE)
  df_global <- df_global %>% arrange(liste)
  
  df_wide <- df_global %>%
    pivot_wider(
      names_from = liste,
      values_from = Valeur
    ) %>%
    mutate(across(everything(), trimws))
  
  # Renommer les colonnes si necessaire
  names(df_wide) <- c("mot_cle", seq_along(names(df_wide))[-1])
  
  # Decalage du contenu des colonnes
  for (i in 1:(ncol(df_wide)-1)) {
    df_wide[, i] <- df_wide[, i+1]
  }
  
  # Suppression de la dernière colonne
  df_wide <- df_wide[, -ncol(df_wide)]
  
  return(df_wide)
}


# # Exemple d'utilisation
# xml_file <- "AK_Nour_Tapping.xml"
# columns_to_extract <- c("Test", "Date", "Heure", "G/D", "Temps de vol", "Temps de contact", "Rythme[p/s]", "Rythme[pas/m]", "Point d'equilibreX", "Decalage lateralX", "Largeur de pas")
# 
# #Resultats en colonnes
# result <- parse_xml_file(xml_file, columns_to_extract)
# 
# 
# # Si preference resultats en ligne
# 
# ###############################################
# result_transpose <- as.data.frame(t(result))
# # Definir les noms de colonnes avec la première ligne
# names(result_transpose) <- unlist(result_transpose[1, ])
# # Supprimer la première ligne car elle est maintenant utilisee comme noms de colonnes
# result_transpose <- result_transpose[-1, ]
# print(result_transpose)
# ##############################################


#Utilisee pour le CMJ
create_stacked_bar_plot <- function(df_result_param, moyenne = NULL) {
  
  # aa <- data.frame(Values = c(df_result_param$valeur[1],df_result_param$valeur[2],df_result_param$valeur[3]))
  
  # Trie des valeurs
  # Obtenir l'index des valeurs triees
  index <- order(df_result_param$valeur)
  
  # Reorganiser le dataframe en fonction de l'index
  df_result_param <- df_result_param[index, ]
  
  
  #Permet d'e trier d'affciher les valeurs dans l'ordre alphabetiques qui correspond à l'ordre decroissant des valeurs
  rownames(df_result_param) <- c("C","B","A")
  
  labels_inside_bars <- df_result_param$valeur
  
  #Reformatage des valeurs pour le diagramme empile
  df_result_param$valeur[3] <- df_result_param$valeur[3] - df_result_param$valeur[2]
  df_result_param$valeur[2] <- df_result_param$valeur[2] - df_result_param$valeur[1]
  
  df_result_param$Ordre <- rownames(df_result_param)
  
  # Definition des etiquettes de la legende
  labels <- c(df_result_param$labels[3], df_result_param$labels[2], df_result_param$labels[1])
  #Associe au label son dataframe
  
  # Ajout d'une colonne x à df_result_param
  df_result_param$x <- 1
  
  # Trace du graphique
  p <- ggplot(df_result_param, aes(fill = Ordre, y = valeur, x = x)) +
    geom_bar(position = "stack", stat = "identity") +
    geom_text(aes(label = labels_inside_bars), position = position_stack(vjust = 0.5), size = 3) +
    labs(y = "Valeur",
         fill = "Resultats",  # Changement du nom de la legende
         title = "Resultats test CMJ",
         subtitle = "En centimètres") +
    scale_fill_manual(values = c("A" = "#DCC283", "B" = "#67AF5E", "C" = "#81BFE0"),
                      labels = labels) +  # Modification des etiquettes de la legende
    scale_x_continuous(breaks = NULL) + # Suppression de l'echelle sur l'axe x
    scale_y_continuous(breaks = seq(0, sum(df_result_param$valeur), by = 5)) + # Modifie l'axe des y
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Ajout de la ligne de moyenne en pointille si le paramètre moyenne est fourni
  if (!is.null(moyenne)) {
    p <- p + geom_hline(aes(yintercept = moyenne, linetype = "Moyenne"), size = 1) +
      scale_linetype_manual(name = "Legende", values = "longdash", labels = "Moyenne",
                            guide = guide_legend(override.aes = list(fill = NA, color = "black")))
  }
  
  
  return(p)
  
}



remove_special_characters <- function(input_file, output_file) {
  # Lire le fichier CSV avec la bibliothèque readr
  df <- readr::read_csv(input_file, show_col_types = FALSE)
  
  # Supprimer les caractères speciaux avec une expression regulière
  for (col in names(df)) {
    df[[col]] <- gsub("[^[:alnum:] ]", "", df[[col]])
  }
  
  # Sauvegarder le fichier nettoye en CSV
  write.csv(df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
}


################################################################################
#Suppression des NA dans certaines colonnes d'interêt seulement
################################################################################

supprimer_lignes_na <- function(df) {
  # Identifier les lignes contenant des valeurs manquantes dans l'une des colonnes specifiees
  lignes_a_supprimer <- which(rowSums(is.na(df[, c(29, 30, 64)])) > 0)
  
  # Supprimer les lignes identifiees
  df <- df[-lignes_a_supprimer, ]
  
  # Retourner le dataframe modifie
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
      #showNotification(paste("Les valeurs manquantes dans la colonne", colonne, "ont ete remplacees par la moyenne."), type = "message")
    } else {
      # Convertir la colonne en numerique
      df[,colonne] <- as.numeric(df[,colonne])
      # Verifier si la conversion a reussi
      if (!all(is.na(df[,colonne]))) {
        moyenne <- mean(df[,colonne], na.rm = TRUE)
        df[,colonne] <- replace(df[,colonne], is.na(df[,colonne]), moyenne)
        showNotification(paste("La colonne", colonne, "a ete convertie en numerique et les valeurs manquantes ont ete remplacees par la moyenne."), type = "message")
      } else {
        showNotification(paste("La colonne", colonne, "ne peut pas être convertie en numerique. Elle sera ignoree."), type = "warning")
      }
    }
  }
  return(df)
}

################################################################################
# Creer les dataframes gauche et droit
################################################################################

# Df_lateraux <- function(DF) {
#   DF_gauche <- subset(DF, DF[, 5] == "G")
#   DF_droit <- subset(DF, DF[, 5] == "D")
#   
#   return( list(DF_gauche = DF_gauche, DF_droit = DF_droit))
# }

Df_lateraux <- function(DF) {
  # Creation d'un index pour les valeurs "G" et "D"
  index_G <- DF[, 5] == "G"
  index_D <- DF[, 5] == "D"
  
  # Filtrer le dataframe en utilisant l'index
  DF_gauche <- DF[index_G, ]
  DF_droit <- DF[index_D, ]
  
  # Retourner les dataframes filtres dans une liste
  return(list(DF_gauche = DF_gauche, DF_droit = DF_droit))
}

################################################################################
# Graphes taping
################################################################################

#3D
Graphtapping_3D <- function( DF_Gauche, DF_Droit){
  # Creer une palette de couleurs pour DF_gauche
  colors_gauche <-
    colorRampPalette(c("#E9C645", "#AE5853"))(nrow(DF_gauche))
  
  # Creer une palette de couleurs pour DF_droit
  colors_droite <-
    colorRampPalette(c("#A1CCCD", "#6559A7"))(nrow(DF_droit))
  
  # Creer le graphique pour les donnees du dataframe DF_gauche
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
      'DecalageDFlateralX:',
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
  
  # Creer le graphique pour les donnees du dataframe DF_droit
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
      'DecalageDFlateralX:',
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
    title = list(text = 'Relation entre Decalage lateral, Temps de vol et Temps de contact',
                 font = list(color = 'white')),
    # Couleur du titre en blanc
    scene = list(
      xaxis = list(
        title = 'Decalage lateral',
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
    ) # Ajouter une legende de couleur
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
      name = "Moyenne + ecart-type",
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
      name = "Moyenne - ecart-type",
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
          "Comparaison de la moyenne et de l'ecart type de la colonne",
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
    # Filtrer les donnees pour les lignes où la colonne "G/D" vaut "G" ou "D"
    DF_gauche <- DF[DF$`G.D` == "G", ]
    DF_droit <- DF[DF$`G.D` == "D", ]
    
    # Calculer la moyenne pour chaque DataFrame
    moyenne_gauche <- mean(DF_gauche[, indice_de_colonne], na.rm = TRUE)
    moyenne_droit <- mean(DF_droit[, indice_de_colonne], na.rm = TRUE)
    
    # Creer les etiquettes et les valeurs pour le graphique camembert
    labels <- c('Gauche', 'Droit')
    valeurs <- c(moyenne_gauche, moyenne_droit)
    
    # Creer le graphique camembert avec Plotly
    camembert <- plot_ly(labels = labels, values = valeurs, type = 'pie',
                         text = paste("Côte: ", labels, "<br>", "Moyenne: ", round(valeurs, 2)),
                         hoverinfo = "text", marker = list(colors = c('#C5243D', '#2C2F65'))) %>%
      layout(title = "Moyenne des donnees gauche et droite", showlegend = FALSE, paper_bgcolor = '#FFFFFF', plot_bgcolor = '#FFFFFF',
             font = list(color = 'white'))
    
    # Renvoyer la figure du graphique camembert
    return(camembert)
  }
  
  creer_gauge_chart <- function(DF, indice_de_colonne) {
    
    # Filtrer les donnees pour les lignes où la colonne "G/D" vaut "G" ou "D"
    DF_gauche <- DF[DF$`G.D` == "G", ]
    DF_droit <- DF[DF$`G.D` == "D", ]
    
    # Calculer la moyenne pour chaque DataFrame
    moyenne_gauche <- mean(DF_gauche[, indice_de_colonne], na.rm = TRUE)
    moyenne_droit <- mean(DF_droit[, indice_de_colonne], na.rm = TRUE)
    
    # Calculer les pourcentages pour chaque côté
    pourcentage_gauche <- moyenne_gauche / (moyenne_gauche + moyenne_droit) * 100
    pourcentage_droit <- moyenne_droit / (moyenne_gauche + moyenne_droit) * 100
    
    # Créer les valeurs pour le graphique gauge
    valeurs <- list(
      list(domain = list(x = c(0, 0.45), y = c(0, 1)), value = pourcentage_gauche, title = list(text = "Gauche")),
      list(domain = list(x = c(0.55, 1), y = c(0, 1)), value = pourcentage_droit, title = list(text = "Droit"))
    )
    
    # Créer le graphique gauge avec Plotly
    gauge_chart <- plot_ly(type = "indicator", mode = "gauge+number",
                           value = valeurs,
                           gauge = list(axis = list(range = list(NULL, 100)),
                                        bar = list(color = "#C5243D"),
                                        steps = list(
                                          list(range = c(0, 50), color = "#2C2F65"),
                                          list(range = c(50, 100), color = "#C5243D")
                                        )),
                           number = list(font = list(color = "white")),
                           domain = list(x = c(0, 1), y = c(0, 1))) %>%
      layout(title = "Pourcentage des données gauche et droite",
             paper_bgcolor = '#FFFFFF', plot_bgcolor = '#FFFFFF',
             font = list(color = 'white'))
    
    # Renvoyer la figure du graphique gauge
    return(gauge_chart)
  }
  

creer_barplotgd <- function(DF, colonne_indice) {
  DF_nettoye <- na.omit(DF)
  
  moyenne_gauche <- mean(DF[DF$G.D == "G", colonne_indice], na.rm = TRUE)
  moyenne_droit <- mean(DF[DF$G.D == "D", colonne_indice], na.rm = TRUE)
  
  # Creer les etiquettes et les valeurs pour le graphique à barres
  labels <- c("Gauche", "Droit")
  valeurs <- c(moyenne_gauche, moyenne_droit)
  textes <- paste(round(valeurs, 2), "Pas/seconde") # Ajouter du texte aux barres
  
  # Creer le graphique à barres avec Plotly
  barplot <- plot_ly(x = labels, y = valeurs, type = 'bar', text = textes, # Ajouter l'argument text
                     marker = list(color = c('#C5243D', '#81BFE0'))) %>%
    layout(title = "Frequence de pas par pied",
           xaxis = list(title = "Côte"),
           yaxis = list(title = paste(colonne_indice, "pas / s")),
           paper_bgcolor = '#2C2F65', plot_bgcolor = '#2C2F65',
           font = list(color = 'white'))
  
  # Renvoyer la figure du graphique à barres
  return(barplot)
}

# creer_barplot <- function(DF, colonne_indice) {
#   # Supprimer les lignes contenant des valeurs manquantes
#   DF_nettoye <- na.omit(DF)
#   
#   # Obtenir le nom de la dernière ligne
#   dernier_nom <- tail(DF_nettoye$Nom, 1)
#   
#   # Filtrer le data frame pour n'inclure que les lignes ayant le même nom que la dernière ligne
#   DF_filtre <- DF_nettoye[DF_nettoye$Nom == dernier_nom, ]
#   
#   # Calculer les moyennes pour chaque côte et la moyenne globale en fonction des noms identiques au dernier nom
#   moyenne_gauche <- mean(DF_filtre[DF_filtre$G.D == "G", colonne_indice], na.rm = TRUE)
#   moyenne_droit <- mean(DF_filtre[DF_filtre$G.D == "D", colonne_indice], na.rm = TRUE)
#   moyenne_globale <- mean(DF_filtre[, colonne_indice], na.rm = TRUE)
#   
#   # Creer les etiquettes et les valeurs pour le graphique à barres
#   labels <- c("Gauche", "Droit", "Moyenne globale")
#   valeurs <- c(moyenne_gauche, moyenne_droit, moyenne_globale)
#   textes <- paste(round(valeurs, 2), "Pas/seconde")
#   
#   # Creer le graphique à barres avec Plotly
#   barplot <- plot_ly(x = labels, y = valeurs, type = 'bar', text = textes,
#                      marker = list(color = c('#C5243D', '#81BFE0', 'black'))) %>%
#     layout(title = paste("Frequences moyennes"),
#            xaxis = list(title = "Côte"),
#            yaxis = list(title = paste(colonne_indice, "pas / s")),
#            paper_bgcolor = '#FFFFFF', plot_bgcolor = '#FFFFFF',
#            font = list(color = 'white'))
#   
#   # Renvoyer la figure du graphique à barres
#   return(barplot)
# }



creer_barplot <- function(DF, colonne_indice) {
  # Supprimer les lignes contenant des valeurs manquantes
  DF_nettoye <- na.omit(DF)
  
  # Obtenir le nom de la dernière ligne
  dernier_nom <- tail(DF_nettoye$Nom, 1)
  
  # Filtrer le data frame pour n'inclure que les lignes ayant le même nom que la dernière ligne
  DF_filtre <- DF_nettoye[DF_nettoye$Nom == dernier_nom, ]
  
  # Calculer les moyennes pour chaque côte et la moyenne globale en fonction des noms identiques au dernier nom
  moyenne_gauche <- mean(DF_filtre[DF_filtre$G.D == "G", colonne_indice], na.rm = TRUE)
  moyenne_droit <- mean(DF_filtre[DF_filtre$G.D == "D", colonne_indice], na.rm = TRUE)
  moyenne_globale <- mean(DF_filtre[, colonne_indice], na.rm = TRUE)
  
  # Creer les etiquettes et les valeurs pour le graphique à barres
  labels <- c("Gauche", "Droit", "Moyenne globale")
  valeurs <- c(moyenne_gauche, moyenne_droit, moyenne_globale)
  textes <- paste(round(valeurs, 2), "Pas/seconde")
  
  # Mettre à jour le titre du graphique avec le nombre total de pas
  titre <- paste("Nombre total de pas :", nrow(DF_filtre))
  
  # Creer le graphique à barres avec Plotly
  barplot <- plot_ly(x = labels, y = valeurs, type = 'bar', text = textes,
                     marker = list(color = c('#C5243D', '#81BFE0', 'black'))) %>%
    layout(title = titre, # Mettre à jour le titre avec le nombre total de pas
           xaxis = list(title = "Côte/Moyenne"),
           yaxis = list(title = paste(colonne_indice, "pas / s")),
           paper_bgcolor = '#2C2F65', plot_bgcolor = '#2C2F65',
           font = list(color = 'white'))
  
  # Renvoyer la figure du graphique à barres
  return(barplot)
}



creer_gauge_chart_plotly <- function(DF, colonne_indice) {
  # Supprimer les lignes contenant des valeurs manquantes
  DF_nettoye <- na.omit(DF)
  
  # Obtenir le nom de la dernière ligne
  dernier_nom <- tail(DF_nettoye$Nom, 1)
  
  # Filtrer le data frame pour n'inclure que les lignes ayant le même nom que la dernière ligne
  DF_filtre <- DF_nettoye[DF_nettoye$Nom == dernier_nom, ]
  
  # Calculer la moyenne
  moyenne <- mean(DF_filtre[, colonne_indice], na.rm = TRUE)
  
  # Calculer la valeur maximale de la colonne
  max_valeur <- max(DF_filtre[, colonne_indice], na.rm = TRUE)
  
  # Creer le gauge chart avec plotly
  gauge <- plot_ly(
    type = 'pie',
    values = c(moyenne / max_valeur * 100, 100 - moyenne / max_valeur * 100),
    labels = c("Valeur", ""),
    hole = 0.5,
    rotation = 90,
    textposition = 'inside',
    textinfo = 'label',
    marker = list(colors = c('#C5243D', 'rgba(255, 255, 255, 0)'))
  ) %>%
    layout(
      title = paste(colonne_indice, "pas / s"),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      paper_bgcolor = '#2C2F65', plot_bgcolor = '#2C2F65',
      font = list(color = 'white')
    )
  
  # Renvoyer le gauge chart
  return(gauge)
}

graphe_radar <- function(df, nom_colonne) {
  # Vérifiez que le nom de colonne existe dans le data frame
  if(!nom_colonne %in% colnames(df)) {
    stop("Le nom de colonne fourni n'existe pas dans le data frame.")
  }
  
  # Calculez les fréquences des valeurs dans la colonne
  frequences <- table(df[[nom_colonne]])
  
  # Préparez les données pour le graphe radar
  radar_data <- as.data.frame(frequences)
  colnames(radar_data) <- c("Valeurs", "Frequences")
  
  # Créez le graphe radar avec plotly
  fig <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  )
  
  fig <- fig %>%
    add_trace(
      r = radar_data$Frequences,
      theta = radar_data$Valeurs,
      name = nom_colonne
    )
  
  fig <- fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max(radar_data$Frequences))
        )
      ),
      title = paste("Graphe radar de la colonne", nom_colonne)
    )
  
  fig
  
}


radar_chart <- function(df, col2, col3, col4, col5) {
  
  # Calculer les moyennes centrées réduites pour chaque colonne et chaque groupe
  
  # tcG <- scale(df %>% dplyr::filter(G.D == "G") %>% pull(col2))
  # tcD <- scale(df %>% dplyr::filter(G.D == "D") %>% pull(col2))
  # tvG <- scale(df %>% dplyr::filter(G.D == "G") %>% pull(col3))
  # tvD <- scale(df %>% dplyr::filter(G.D == "D") %>% pull(col3))
  # RthG <- scale(df %>% dplyr::filter(G.D == "G") %>% pull(col4))
  # RthD <- scale(df %>% dplyr::filter(G.D == "D") %>% pull(col4))
  # DlG <- scale(df %>% dplyr::filter(G.D == "G") %>% pull(col5))
  # DlD <- scale(df %>% dplyr::filter(G.D == "D") %>% pull(col5))
  
  tcG <- mean(df %>% dplyr::filter(G.D == "G") %>% pull(col2))
  tcD <- mean(df %>% dplyr::filter(G.D == "D") %>% pull(col2))
  tvG <- mean(df %>% dplyr::filter(G.D == "G") %>% pull(col3))
  tvD <- mean(df %>% dplyr::filter(G.D == "D") %>% pull(col3))
  RthG <- mean(df %>% dplyr::filter(G.D == "G") %>% pull(col4))
  RthD <- mean(df %>% dplyr::filter(G.D == "D") %>% pull(col4))
  DlG <- mean(df %>% dplyr::filter(G.D == "G") %>% pull(col5))
  DlD <- mean(df %>% dplyr::filter(G.D == "D") %>% pull(col5))
  
  # Créer la liste des valeurs pour le graphe radar
  values <- list(
    list(r = c(mean(tcG), mean(tvG), mean(RthG), mean(DlG)), theta = c("Contact", "Vol", "Rythme", "Décalage latéral"), name = "G", fill = "toself"),
    list(r = c(mean(tcD), mean(tvD), mean(RthD), mean(DlD)), theta = c("Contact", "Vol", "Rythme", "Décalage latéral"), name = "D", fill = "toself")
  )
  
  # Créer le graphe radar
  fig <- plot_ly()
  for (i in seq_along(values)) {
    fig <- fig %>% add_trace(
      type = "scatterpolar",
      r = values[[i]]$r,
      theta = values[[i]]$theta,
      name = values[[i]]$name,
      fill = values[[i]]$fill
    )
  }
  
  # Définir les options de mise en forme du graphe
  fig <- fig %>%
    layout(
      title = "Radar Chart",
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(-3, 7) # Mettre à jour la plage de l'axe radial
        )
      ),
      showlegend = TRUE
    )
  
  # Renvoie le graphe radar
  fig
}


plot_line_chart <- function(df, col_name) {
  
  # Vérifier si la colonne index existe dans le data frame
  if (!"index" %in% colnames(df)) {
    # Créer une colonne index à partir d'une séquence numérique
    df$index <- 1:nrow(df)
  }
  
  # Créer un objet plotly à partir du data frame
  p <- plot_ly(df, x = ~index, y = ~df[,col_name], type = 'scatter', mode = 'lines')
  # p <- plot_ly(df, x = ~index, y = ~df[,col_name], type = 'scatter', mode = 'lines') %>%
  #   add_lines(y = ~lowess(y ~ x)$y, line = list(color = 'red', width = 2))
  
  # Définir les options de mise en forme du graphique
  p <- p %>%
    layout(
      title = paste("Evolution de", col_name),
      xaxis = list(title = "Index"),
      yaxis = list(title = col_name)
    )
  
  # Renvoie le graphique
  p
}

# plot_smooth_line_ggplot <- function(df, col_name, line_color = "#C550AA", line_alpha = 0.2) {
# 
#   # Vérifier si la colonne index existe dans le data frame
#   if (!"index" %in% colnames(df)) {
#     # Créer une colonne index à partir d'une séquence numérique
#     df$index <- 1:nrow(df)
#   }
# 
#   # Calculer les valeurs minimale et maximale de la colonne
#   min_val <- min(df[, col_name])
#   max_val <- max(df[, col_name])
#   mean_val <- mean(df[, col_name])
# 
#   # Convertir la couleur de la ligne en notation RGBA
#   line_color_rgba <- rgb(t(col2rgb(line_color) / 255), alpha = line_alpha)
# 
#   # Créer un objet ggplot à partir du data frame
#   p <- ggplot(data = df, aes(x = index, y = !!rlang::sym(col_name))) +
#     geom_line(color = line_color_rgba) +
#     geom_smooth(se = FALSE, color = "#C5243D", linetype = "solid") +
#     # geom_hline(yintercept = min_val, linetype = "dashed", color = "#2C2F65", alpha = 0.5) +
#     # geom_hline(yintercept = max_val, linetype = "dashed", color = "#2C2F65", alpha = 0.5) +
#     geom_hline(yintercept = mean_val, linetype = "dotted", color = "black", alpha = 0.5) +
#     labs(title = paste("Evolution de", col_name),
#          x = "Index",
#          y = col_name) +
#     annotate("text", x = Inf, y = min_val, label = paste("Min :", round(min_val, 2)), hjust = 1.1, vjust = 0) +
#     annotate("text", x = Inf, y = max_val, label = paste("Max :", round(max_val, 2)), hjust = 1.1, vjust = 0) +
#     theme_void() + # Supprimer les éléments du thème par défaut
#     theme(
#       panel.background = element_rect(fill = "#2C2F65", color = NA), # Définir la couleur de fond du panneau
#       plot.title = element_text(color = "white"), # Définir la couleur du titre du graphique
#       axis.title = element_text(color = "white"), # Définir la couleur des titres des axes
#       axis.text = element_text(color = "white"), # Définir la couleur des étiquettes des axes
#       axis.ticks = element_line(color = "white"), # Définir la couleur des traits des axes
#       plot.margin = unit(c(0, 0, 0, 0), "mm") # Supprimer les marges du graphique
#     )
# 
#   # Renvoie le graphique
#   p
# }

# 
# plot_smooth_line_ggplot <- function(df, col_name, line_color = "#FFFFFF", line_alpha = 0.1) {
#   
#   # Vérifier si la colonne index existe dans le data frame
#   if (!"index" %in% colnames(df)) {
#     # Créer une colonne index à partir d'une séquence numérique
#     df$index <- 1:nrow(df)
#   }
#   
#   # Filtrer les donnees pour les lignes où la colonne "G/D" vaut "G" ou "D"
#   DF_gauche <- df[df$`G.D` == "G", ]
#   DF_droit <- df[df$`G.D` == "D", ]
#   
#   # Calculer la moyenne pour chaque DataFrame
#   moyenne_gauche <- mean(DF_gauche[, col_name], na.rm = TRUE)
#   moyenne_droit <- mean(DF_droit[, col_name], na.rm = TRUE)
#   
#   # Calculer les valeurs minimale et maximale de la colonne
#   min_val <- min(df[, col_name])
#   max_val <- max(df[, col_name])
#   mean_val <- mean(df[, col_name])
#   
#   # Convertir la couleur de la ligne en notation RGBA
#   line_color_rgba <- rgb(t(col2rgb(line_color) / 255), alpha = line_alpha)
#   
#   # Créer un objet ggplot à partir du data frame
#   p <- ggplot(data = df, aes(x = index, y = !!rlang::sym(col_name))) +
#     geom_line(aes(color = `G.D`), alpha = line_alpha) +
#     geom_smooth(aes(color = `G.D`), se = FALSE, linetype = "solid") +
#     geom_hline(aes(yintercept = mean_val, color = "Mean"), linetype = "dotted", show.legend = TRUE) +
#     scale_color_manual(values = c("G" = "#FF00F0", "D" = "#00F7FF", "Mean" = "#FF2c6c"),
#                        labels = c("Pied Gauche", "Pied Droit", "Moyenne")) +
#     labs(title = paste("Evolution de", col_name),
#          x = "Index",
#          y = col_name,
#          color = "Evolution par pied") +
#     theme_void() + # Supprimer les éléments du thème par défaut
#     theme(
#       panel.background = element_rect(fill = "#2C2F65", color = NA), # Définir la couleur de fond du panneau
#       plot.background = element_rect(fill = "#2C2F65", color = NA), # Définir la couleur de fond du graphique
#       plot.title = element_text(color = "white"), # Définir la couleur du titre du graphique
#       axis.title = element_text(color = "white"), # Définir la couleur des titres des axes
#       axis.text = element_text(color = "white"), # Définir la couleur des étiquettes des axes
#       axis.ticks = element_line(color = "white"), # Définir la couleur des traits des axes
#       plot.margin = unit(c(0, 0, 0, 0), "mm"), # Supprimer les marges du graphique
#       legend.text = element_text(color = "white", size = 14),
#       legend.title = element_text(color = "white", size = 16),
#       legend.position = "top", # Déplacer la légende en haut du graphique
#       legend.box.background = element_rect(color = NA), # Supprimer la couleur de fond de la légende
#       legend.key = element_blank() # Supprimer les carrés de couleur dans la légende
#     )
#   
#   # Renvoie le graphique
#   p
# }


plot_smooth_line_ggplot <- function(df, col_name, line_color = "#FFFFFF", line_alpha = 0.1) {
  
  # Vérifier si la colonne index existe dans le data frame
  if (!"index" %in% colnames(df)) {
    # Créer une colonne index à partir d'une séquence numérique
    df$index <- 1:nrow(df)
  }
  
  # Filtrer les donnees pour les lignes où la colonne "G/D" vaut "G" ou "D"
  DF_gauche <- df[df$`G.D` == "G", ]
  DF_droit <- df[df$`G.D` == "D", ]
  
  # Calculer la moyenne pour chaque DataFrame
  moyenne_gauche <- mean(DF_gauche[, col_name], na.rm = TRUE)
  moyenne_droit <- mean(DF_droit[, col_name], na.rm = TRUE)
  
  # Calculer les valeurs minimale et maximale de la colonne
  min_val <- min(df[, col_name])
  max_val <- max(df[, col_name])
  mean_val <- mean(df[, col_name])
  
  # Convertir la couleur de la ligne en notation RGBA
  line_color_rgba <- rgb(t(col2rgb(line_color) / 255), alpha = line_alpha)
  
  # Créer un objet ggplot à partir du data frame
  p <- ggplot(data = df, aes(x = index, y = !!rlang::sym(col_name))) +
    geom_smooth(data = DF_gauche, aes(color = "G"), se = FALSE, linetype = "solid") +
    geom_smooth(data = DF_droit, aes(color = "D"), se = FALSE, linetype = "solid") +
    geom_hline(aes(yintercept = mean_val, color = "Mean"), linetype = "dotted", show.legend = TRUE) +
    scale_color_manual(values = c("G" = "#C5243D", "D" = "#2C2F65", "Mean" = "#FF2c6c"),
                       labels = c("Pied Gauche", "Pied Droit", "Moyenne")) +
    labs(title = paste("Evolution de la fréquence de pas"),
         x = "Nombre de pas",
         y = "Fréquence en pas/secondes",
         color = "Evolution par pied") +
    theme_void() + # Supprimer les éléments du thème par défaut
    theme(
      panel.background = element_rect(fill = "#FFFFFF", color = NA), # Définir la couleur de fond du panneau
      plot.background = element_rect(fill = "#FFFFFF", color = NA), # Définir la couleur de fond du graphique
      plot.title = element_text(color = "black"), # Définir la couleur du titre du graphique
      axis.title = element_text(color = "black"), # Définir la couleur des titres des axes
      axis.text = element_text(color = "black"), # Définir la couleur des étiquettes des axes
      axis.ticks = element_line(color = "black"), # Définir la couleur des traits des axes
      plot.margin = unit(c(0, 0, 0, 0), "mm"), # Supprimer les marges du graphique
      legend.text = element_text(color = "black", size = 14),
      legend.title = element_text(color = "black", size = 16),
      legend.position = "top", # Déplacer la légende en haut du graphique
      legend.box.background = element_rect(color = NA), # Supprimer la couleur de fond de la légende
      legend.key = element_blank() # Supprimer les carrés de couleur dans la légende
    )
  
  # Renvoie le graphique
  p
}


























cone_plot <- function(df, col_x, col_y, col_z) {
  
  df_g <- filter(.data = df, "G.D" == "G")
  df_d <- filter(.data = df, "G.D" == "D")
  
  fig <- plot_ly()
  
  fig <- add_cones(fig,
                   x = df_g[[col_x]],
                   y = df_g[[col_y]],
                   z = df_g[[col_z]],
                   u = rep(0, nrow(df_g)),
                   v = rep(0, nrow(df_g)),
                   sizemode = "diameter",
                   sizeref = 0.1,
                   color = "pink",
                   showscale = FALSE)
  
  fig <- add_cones(fig,
                   x = df_d[[col_x]],
                   y = df_d[[col_y]],
                   z = df_d[[col_z]],
                   u = rep(0, nrow(df_d)),
                   v = rep(0, nrow(df_d)),
                   sizemode = "diameter",
                   sizeref = 0.1,
                   color = "lightblue",
                   showscale = FALSE)
  
  fig <- layout(fig,
                scene = list(xaxis = list(title = col_x),
                             yaxis = list(title = col_y),
                             zaxis = list(title = col_z)),
                showlegend = FALSE)
  
  return(fig)
}



swarm_plot_colored <- function(dataframe, nom, colonne) {

  # Regrouper les données par Nom et garder la valeur maximum de la colonne et le sexe
  dataframe <- dataframe %>%
    group_by(Nom, Sexe) %>%
    summarise(across(all_of(colonne), max))

  # Créer une nouvelle colonne pour les couleurs
  dataframe$couleur <- ifelse(dataframe$Sexe == "Homme", "Homme", "Femme")
  dataframe$couleur[dataframe$Nom == nom] <- "Votre performance"

  # Arrondir les valeurs de hauteur à la première décimale
  dataframe[[colonne]] <- round(dataframe[[colonne]], 1)

  # Filtrer les valeurs où le Sexe est égal à "Femme"
  filtre_femme <- dataframe[dataframe$Sexe == "Femme", ]
  # Calculer la médiane de la colonne "Hauteur"
  moy_hauteur_femme <- round(mean(filtre_femme$Hauteur), 1)

  if(moy_hauteur_femme == "NaN") {
    moy_hauteur_femme = 0
  }
  print(moy_hauteur_femme)

  # Filtrer les valeurs où le Sexe est égal à "Homme"
  filtre_homme <- dataframe[dataframe$Sexe == "Homme", ]
  # Calculer la médiane de la colonne "Hauteur"
  moy_hauteur_homme <- round(mean(filtre_homme$Hauteur), 1)

  # Filtrer les données pour "Votre performance"
  votre_performance <- dataframe[dataframe$couleur == "Votre performance", ]


  # Créer le swarm plot avec ggplot
  p <- ggplot() +
    geom_point(data = dataframe[dataframe$couleur != "Votre performance", ],
               aes(x = Sexe, y = !!rlang::sym(colonne), fill = couleur, text = paste(colonne, ": ", round(!!rlang::sym(colonne), 1))),
               alpha = 0.8, size = 3, shape = 21, color = "black", position = position_jitter(height = 0)) +
    geom_point(data = votre_performance,
               aes(x = Sexe, y = !!rlang::sym(colonne), fill = couleur, text = paste(colonne, ": ", round(!!rlang::sym(colonne), 1))),
               size = 4, shape = 21, color = "black") +
    scale_fill_manual(values = c("Homme" = "#2C2F65", "Femme" = "#C5243D", "Votre performance" = "gold"),
                      labels = c("Homme" = "Hommes", "Femme" = "Femmes", "Votre performance" = "Votre performance")) +

    geom_hline(aes(yintercept = moy_hauteur_femme, linetype = "Moyenne Femme", text = paste("Moyenne Femme: ", moy_hauteur_femme)),
               color = "#C5243D") + # ajoute la ligne de médiane femme

    geom_hline(aes(yintercept = moy_hauteur_homme, linetype = "Moyenne Homme", text = paste("Moyenne Homme: ", moy_hauteur_homme)),
               color = "#2C2F65") + # ajoute la ligne de médiane homme

    scale_linetype_manual(values = c("Moyenne Femme" = "dashed", "Moyenne Homme" = "dashed")) +


    geom_text_repel(data = dataframe, aes(x = Sexe, y = !!rlang::sym(colonne), label = round(!!rlang::sym(colonne), 1)), vjust = -1.5) +  # Ajouter un texte interactif pour les points

    labs(x = "Sexe", y = "Hauteur du saut", title = "Résultats sauts CMJ",
         caption = "Survolez la ligne pour voir la hauteur") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Convertir le graphique ggplot en plotly pour l'interactivité
  p <- ggplotly(p, tooltip = c("text"))

  # Modifier les étiquettes de la légende manuellement


  for(i in seq_along(p$x$data)) {
    print(p$x$data[[i]]$name)
    if(p$x$data[[i]]$name == "(Homme,1)") {
      p$x$data[[i]]$name <- "Score Hommes"
    } else if(p$x$data[[i]]$name == "(Femme,1)") {
      p$x$data[[i]]$name <- "Score Femmes"
    } else if(p$x$data[[i]]$name == "(Votre performance,1)") {
      p$x$data[[i]]$name <- "Votre performance"
    } else if(p$x$data[[i]]$name == "(Moyenne Femme,1)") {
      p$x$data[[i]]$name <- "Moyenne Femme"
    } else if(p$x$data[[i]]$name == "(Moyenne Homme,1)") {
      p$x$data[[i]]$name <- "Moyenne Homme"
    }
  }

  # Appliquer la mise en page pour modifier le titre de la légende et centrer le titre du plot
  p <- p %>%
    layout(
      legend = list(
        title = list(
          text = "<b>Légende</b>",
          font = list(size = 14)
        ),
        y = 0.5,
        yanchor = "middle"
      ),
      title = list(
        text = "Résultats sauts CMJ",
        x = 0.5,
        xanchor = "center"
      )
    )


  # Afficher le graphique
  print(p)
}


library(plotly)

plot_column_evolution <- function(df, column_name) {
  # Ajouter une colonne index au dataframe
  df <- df %>% mutate(index = 1:n())
  # Diviser le dataframe en deux sous-ensembles
  df_gauche <- df[df$G.D == "G", ]
  df_droit <- df[df$G.D == "D", ]
  
  # Créer une liste de courbes pour chaque sous-ensemble
  plots <- list(
    # Courbe pour le sous-ensemble gauche
    plot_ly(df_gauche, x = ~index, y = column_name, type = "scatter", mode = "lines",
            name = "Sous-ensemble gauche", line = list(color = "blue")),
    # Courbe pour le sous-ensemble droit
    plot_ly(df_droit, x = ~index, y = column_name, type = "scatter", mode = "lines",
            name = "Sous-ensemble droit", line = list(color = "red"))
  )
  
  # Ajouter les courbes au graphique
  plot <- subplot(plots, nrows = 2, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(title = "Évolution de la colonne",
           xaxis = list(title = "Index"),
           yaxis = list(title = column_name))
  
  # Retourner le graphique
  return(plot)
}
