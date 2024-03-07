library(shiny)
library(dplyr)

# UI module
CMJ_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, align = "center",
             br(),
             h4("TEST CMJ"),
             fileInput(ns("xml_file"), "Uploader un fichier XML"),
             br(),
             textInput(ns("athlete_name"), label = "Nom de l'athlète"),
             selectInput(ns("gender_select"), label = "Genre", choices = c("Homme", "Femme", "Non binaire")),
             actionButton(ns("submit_button"), label = "Soumettre")
      ),
      column(width = 6, align = "center",
             br(),
             div(
               style = "position:relative;padding-bottom:56.25%;height:0;overflow:hidden;",
               tags$video(
                 src = "reel_nour.mp4",
                 type = "video/mp4",
                 autoplay = TRUE,
                 loop = TRUE,
                 controls = FALSE,
                 style = "position:absolute;top:0;left:0;width:100%;height:100%;"
               )
             )
      )
    ),
    fluidRow(
      column(width = 6, align = "center",
             htmlOutput(ns("results_text"))  # Modification ici
      ),
      column(width = 6, align = "center",
             plotlyOutput(ns("bar_chart"))
      )
    )
  )
}

# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  record_visiteur <- reactiveVal(NULL)
  observeEvent(input$submit_button, {
    req(input$xml_file, input$athlete_name, input$gender_select)
    
    # Exemple d'utilisation
    xml_file <- input$xml_file$datapath
    columns_to_extract <- c("Elevation")
    
    #Resultats en colonnes
    result <- parse_xml_file(xml_file, columns_to_extract)
    
    # Si préférence résultats en ligne
    ###############################################
    
    if (!is.null(result) && length(result) > 0) {
      result <- as.data.frame(t(result))
      # Définir les noms de colonnes avec la première ligne
      names(result) <- unlist(result[1, ])
      # Supprimer la première ligne car elle est maintenant utilisée comme noms de colonnes
      result <- result[-1, ]
      result <- as.numeric(result)
      
      
      Test = "CMJ"
      Nom <- input$athlete_name
      Sexe <- input$gender_select
      
      # Trier les valeurs de result en ordre décroissant
      sorted_result <- sort(result, decreasing = TRUE)
      
      # Obtenir les 3 plus grandes valeurs si il y a 3 valeurs ou plus,
      # les 2 plus grandes valeurs si il y a 2 valeurs,
      # et la valeur maximale si il y a une seule valeur
      top_values <- head(sorted_result, min(length(sorted_result), 3))
      
      # Créer un dataframe avec les données
      nouvelle_ligne <- data.frame(Test = rep(Test, length(top_values)),
                                   Nom = rep(Nom, length(top_values)),
                                   Hauteur = top_values,
                                   Sexe = rep(Sexe, length(top_values)),
                                   isAthlete = rep(FALSE, length(top_values)))
      
    }
    
    # Vérification si le fichier CSV existe
    if (!file.exists("results_globaux_CMJ.csv")) {
      # Si le fichier n'existe pas, écrire le dataframe complet
      write.csv(nouvelle_ligne, file = "results_globaux_CMJ.csv", row.names = FALSE)
    } else {
      # Si le fichier existe, lire les données actuelles
      donnees_existantes <- read.csv("results_globaux_CMJ.csv")
      # Ajouter la nouvelle ligne aux données existantes
      donnees_combinees <- rbind(donnees_existantes, nouvelle_ligne)
      # Réécrire le fichier CSV avec toutes les données
      write.csv(donnees_combinees, file = "results_globaux_CMJ.csv", row.names = FALSE)
      isDataProcessed(TRUE)
    }
    
    ##############################################
  })
  
  output$bar_chart <- renderPlotly({
    req(input$xml_file, input$athlete_name, input$gender_select, isDataProcessed())
    if (file.exists("results_globaux_CMJ.csv")) {
      donnees <- read.csv("results_globaux_CMJ.csv")
      
      if (!is.null(donnees) && nrow(donnees) > 0) {
        hauteur_record <- max(donnees$Hauteur)
        
        # Récupérer les données du visiteur en fonction du nombre de sauts effectués
        data_standeur <- donnees[0, ] # Initialiser data_standeur à une ligne vide
        current_name <- input$athlete_name # Nom de la personne actuelle
        n <- nrow(donnees) # Nombre de lignes dans donnees
        for (i in seq(n, 1, -1)) { # Parcourir les lignes de donnees en partant de la fin
          if (donnees[i, "Nom"] == current_name) { # Si le nom de la personne est le même
            data_standeur <- rbind(data_standeur, donnees[i, ]) # Ajouter la ligne à data_standeur
          } else { # Si le nom de la personne est différent
            break # Arrêter la boucle
          }
        }
        
        # Extraire les valeurs de la colonne Hauteur de data_standeur
        hauteurs <- data_standeur$Hauteur
        
        # Obtenir les 3 plus grandes valeurs si il y a 3 valeurs ou plus,
        # les 2 plus grandes valeurs si il y a 2 valeurs,
        # et la valeur maximale si il y a une seule valeur
        top_values <- head(sort(hauteurs, decreasing = TRUE), min(length(hauteurs), 3))
        
        record_visiteur(top_values[1])
        
        record_nour <- subset(donnees, isAthlete == TRUE)$Hauteur
        
        fig <- plot_ly()
        
        fig <- fig %>% add_trace(x = c(1), y = c(record_nour), name = 'Record de Nour', type = 'bar', marker = list(color = '#DCC283', line = list(color = 'rgb(8,48,107)', width = 1.5)))
        fig <- fig %>% add_trace(x = c(2), y = c(hauteur_record), name = 'Record du salon', type = 'bar', marker = list(color = '#C5243D', line = list(color = 'rgb(8,48,107)', width = 1.5)))
        fig <- fig %>% add_trace(x = c(3), y = c(top_values[1]), name = paste('Votre record : ', round(top_values[1], 2), "cm"), type = 'bar', marker = list(color = '#2C2F65', line = list(color = 'rgb(8,48,107)', width = 1.5)))
        
        # Ajouter les lignes représentant les deux autres sauts du visiteur
        if (length(top_values) > 1) {
          fig <- fig %>% add_segments(x = c(3, 3),
                                      y = c(top_values[2], top_values[2]),
                                      xend = c(2.5, 3.5),
                                      yend = c(top_values[2], top_values[2]),
                                      line = list(color = "orange", width = 2, dash = "dash"),
                                      name = paste('2ème saut : ', round(top_values[2], 2), "cm"),
                                      showlegend = TRUE)
        }
        if (length(top_values) > 2) {
          fig <- fig %>% add_segments(x = c(3, 3),
                                      y = c(top_values[3], top_values[3]),
                                      xend = c(2.5, 3.5),
                                      yend = c(top_values[3], top_values[3]),
                                      line = list(color = "green", width = 2, dash = "dash"),
                                      name = paste('3ème saut : ', round(top_values[3], 2), "cm"),
                                      showlegend = TRUE)
        }
        
        # Ajouter le texte au milieu de chaque barre
        fig <- fig %>% add_text(x = c(1, 2, 3),
                                y = c(record_nour/2, hauteur_record/2, top_values[1]/2), # Diviser par 2 pour centrer verticalement
                                text = paste0(c(record_nour, hauteur_record, top_values[1]), " cm"),
                                textposition = "auto", # Laisser plotly décider de l'alignement horizontal
                                showlegend = FALSE,
                                textfont = list(color = "white", size = 14))
        
        fig <- fig %>% layout(xaxis = list(type = 'linear', title = ''), # Convertir l'axe des x en axe continu
                              yaxis = list(title = 'Hauteur'),
                              barmode = 'group', # Pour afficher les barres côte à côte
                              margin = list(t = 50, b = 50),
                              showlegend = TRUE, # Afficher la légende
                              legend = list(orientation = "h", yanchor = "bottom", y = -0.2, xanchor = "right", x = 1),
                              bargap = 0.1, # Espace entre les groupes de barres
                              bargroupgap = 0.1 # Espace entre les barres d'un même groupe
        )
        
        return(fig)
      } else {
        print("Le fichier CSV est vide.")
      }
    } else {
      print("Le fichier CSV n'existe pas.")
    }
  })
  
  
  
  
  
  output$results_text <- renderUI({
    req(input$xml_file, input$athlete_name, input$gender_select, isDataProcessed())
    if (file.exists("results_globaux_CMJ.csv")) {
      donnees <- read.csv("results_globaux_CMJ.csv")
      
      if (!is.null(donnees) && nrow(donnees) > 0) {
        hauteur_record <- max(donnees$Hauteur)
        data_standeur <- donnees[nrow(donnees), ]
        moyenne <- mean(donnees$Hauteur)
        
        # Générer le texte des résultats
        result_text <- tags$div(
          tags$br(),
          tags$strong(style="font-size:2.2em;","Résultats"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Hauteur record Nour : "), tags$strong(style="font-size:2.2em;", round(hauteur_record, 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Hauteur visiteur : "), tags$strong(style="font-size:2.2em;", round(record_visiteur(), 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Moyenne du salon : "), tags$strong(style="font-size:2.2em;", round(moyenne, 2)), tags$strong(style="font-size:2.2em;"," cm")
        )
        
        return(result_text)
      } else {
        return("Le fichier CSV est vide.")
      }
    } else {
      return("Le fichier CSV n'existe pas.")
    }
  })
}
