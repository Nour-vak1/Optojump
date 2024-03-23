library(shiny)
library(dplyr)
library(plotly)

# UI module
CMJ_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
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
      column(width = 3, align = "center",
             plotlyOutput(ns("bar_chart"), width = "150%", height = "150%")  # Remplacer plotOutput par plotlyOutput et ajouter une hauteur
      
             
      )
    )
  )
}




# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  record_visiteur <- reactiveVal(NULL)
  deux_visiteur <- reactiveVal(NULL)
  trois_visiteur <- reactiveVal(NULL)
  
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
      
      # Vérifier si le nom de l'athlète existe déjà
      if (Nom %in% donnees_existantes$Nom) {
        # Trouver un nom unique en ajoutant un numéro
        num_suffix <- 1
        while (paste0(Nom, num_suffix) %in% donnees_existantes$Nom) {
          num_suffix <- num_suffix + 1
        }
        Nom <- paste0(Nom, num_suffix)
      }
      
      # Ajouter la nouvelle ligne avec le nom mis à jour
      nouvelle_ligne$Nom <- Nom
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
        # Filtrer les données pour l'athlète actuel

        n <- nrow(donnees) # Nombre de lignes dans donnees
        last_name <- donnees[n, "Nom"]
        print(last_name)
        athlete_data <- NULL
        for (i in seq(n, 1, -1)) { # Parcourir les lignes de donnees en partant de la fin
          if (donnees[i, "Nom"] == last_name) { # Si le nom de la personne est le même
            athlete_data <- rbind(athlete_data, donnees[i, ]) # Ajouter la ligne à data_standeur
          } else { # Si le nom de la personne est différent
            break # Arrêter la boucle
          }
        }
        
        print(athlete_data)
        
        if(nrow(athlete_data) > 0) {
          # Trouver la meilleure performance de l'athlète
          
          sorted_heights <- sort(athlete_data$Hauteur, decreasing = TRUE)
          # Trouver la meilleure performance
          record_visiteur(sorted_heights[1])
          
          # Si au moins deux performances sont disponibles
          if (length(sorted_heights) >= 2) {
            # Trouver la deuxième meilleure performance
            deux_visiteur(sorted_heights[2])
          }
          
          # Si au moins trois performances sont disponibles
          if (length(sorted_heights) >= 3) {
            # Trouver la troisième meilleure performance
            trois_visiteur(sorted_heights[3])
          }
          p <- swarm_plot_colored(donnees, input$athlete_name, "Hauteur")
          return(p)
          
        }
          
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
        # moyenne <- mean(donnees$Hauteur)
        # record_salon <- max(subset(donnees, isAthlete == FALSE)$Hauteur)
        
        
        
        # Générer le texte des résultats
        result_text <- tags$div(
          tags$br(),
          tags$strong(style="font-size:2.2em;","Résultats"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Votre record : "), tags$strong(style="font-size:2.2em;", round(record_visiteur(), 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$br(),
          tags$span(style="font-size:2.2em;", "Autres essais : "), 
          tags$br(),
          tags$strong(style="font-size:2.2em;", deux_visiteur()), tags$strong(style="font-size:2.2em;"," cm"), 
          tags$br(), 
          tags$strong(style="font-size:2.2em;", trois_visiteur()), tags$strong(style="font-size:2.2em;"," cm"), 
          tags$br(), 
          tags$br(), 
          
          
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