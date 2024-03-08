library(shiny)
library(dplyr)
library(plotly)

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
        
        print("data_standeur")
        print(data_standeur)
        
        # Extraire les valeurs de la colonne Hauteur de data_standeur
        hauteurs <- data_standeur$Hauteur
        
        record_visiteur(max(data_standeur$Hauteur))
        
        fig <- plot_ly()
        
        # Créer un vecteur de couleurs pour chaque essai
        colors <- c('#DCC283', '#C5243D', '#2C2F65')
        
        # Ajouter les barres pour chaque essai du visiteur
        for (i in 1:length(hauteurs)) {
          fig <- fig %>% add_trace(x = c(i), y = c(hauteurs[i]), name = paste("Essai", i), type = 'bar', 
                                   marker = list(color = colors[i], line = list(color = 'rgb(8,48,107)', width = 1.5)),
                                   width = 0.5) %>% 
          add_text(x = c(i), y = c(0.5*hauteurs[i]), text = round(hauteurs[i], 2), textposition = "auto", 
                     textfont = list(color = "white", size = 14, family = "Arial", weight = "bold"), showlegend = FALSE) # Ajouter la valeur de la barre au centre de la barre en blanc et centré en hauteur
        
        }
        
        # Supprimer les labels "trace 1", "trace 3", et "trace 5" dans la légende
        fig <- fig %>% layout(
          legend = list(orientation = "v", yanchor = "bottom", y = 0, xanchor = "right", x = 7),
          showlegend = TRUE,
          barmode = "group",
          bargin = 0.05
        )
        
        record_name <- paste("Record du salon : ", round(hauteur_record,2))
        
        nour_record <- max(subset(donnees, isAthlete == TRUE)$Hauteur)
        nour_name <- paste("Record de Nour : ", round(nour_record,2))
        
        moyenne_score <- mean(donnees$Hauteur)
        moyenne_name <- paste("Moyenne du salon : ", round(moyenne_score,2))
        
        # Ajouter les lignes pointillées pour le record de Nour, le record du salon et la moyenne
        fig <- fig %>% add_segments(x = c(0, length(hauteurs) + 1), y = c(hauteur_record, hauteur_record), xend = c(length(hauteurs) + 1, length(hauteurs) + 1), yend = c(hauteur_record, hauteur_record), line = list(color = "orange", width = 2, dash = "dash"), name = record_name, showlegend = TRUE)
        fig <- fig %>% add_segments(x = c(0, length(hauteurs) + 1), y = c(nour_record, nour_record), xend = c(length(hauteurs) + 1, length(hauteurs) + 1), yend = c(nour_record, nour_record), line = list(color = "green", width = 2, dash = "dash"), name = nour_name, showlegend = TRUE)
        fig <- fig %>% add_segments(x = c(0, length(hauteurs) + 1), y = c(moyenne_score, moyenne_score), xend = c(length(hauteurs) + 1, length(hauteurs) + 1), yend = c(moyenne_score, moyenne_score), line = list(color = "blue", width = 2, dash = "dash"), name = moyenne_name, showlegend = TRUE)
        
        
       
        fig <- fig %>% layout(
          xaxis = list(type = 'linear', title = ''), 
          yaxis = list(title = 'Hauteur', tickmode = "linear", dtick = 5), # Utiliser une incrémentation de 5 sur l'axe des ordonnées
          margin = list(t = 50, b = 50),
          showlegend = TRUE,
          legend = list(orientation = "v", yanchor = "bottom", y = 0, xanchor = "right", x = 7), # Déplacer la légende encore plus à droite
          bargap = 0.05 # Réduire l'espace entre les barres
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
        hauteur_nour <- max(subset(donnees, isAthlete == TRUE)$Hauteur)
        data_standeur <- record_visiteur()
        moyenne <- mean(donnees$Hauteur)
        record_salon <- max(subset(donnees, isAthlete == FALSE)$Hauteur)
        
        
        
        # Générer le texte des résultats
        result_text <- tags$div(
          tags$br(),
          tags$strong(style="font-size:2.2em;","Résultats"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Record Nour : "), tags$strong(style="font-size:2.2em;", round(hauteur_nour, 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Votre record : "), tags$strong(style="font-size:2.2em;", round(record_visiteur(), 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Record du salon : "), tags$strong(style="font-size:2.2em;", round(record_salon, 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
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
