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
             plotlyOutput(ns("bar_chart"))
      ),
      column(width = 6, align = "center",
             htmlOutput(ns("results_text"))  # Modification ici
      )
    )
  )
}

# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  isDataProcessed <- reactiveVal(NULL)
  
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
      
      
      # Imprimer la plus grande valeur
      max_value <- max(result)
      
      Test = "CMJ"
      Nom <- input$athlete_name
      Sexe <- input$gender_select
      
      # Création d'un dataframe avec les données
      nouvelle_ligne <- data.frame(Test = Test, Nom = Nom, Hauteur = max_value, Sexe = Sexe, isAthlete = FALSE)
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
      
      # Vérifier si le dataframe n'est pas vide
      if (!is.null(donnees) && nrow(donnees) > 0) {
        
        hauteur_record <- max(donnees$Hauteur)
        data_standeur <- donnees[nrow(donnees), ] # Utilisation directe des données de cmj_data()
        
        largeur <- c(0.2, 0.2)
        # Créer le graphique en barres empilées avec Plotly
        fig <- plot_ly(donnees, x = ~Test, type = 'bar', width = largeur)
        
        # Ajouter une trace pour la deuxième barre avec des valeurs de la deuxième colonne Hauteur
        fig <- fig %>% add_trace(y = ~hauteur_record, name = 'Hauteur 2', marker = list(color = '#F9E9CA'), text = paste(round(hauteur_record, 2), "cm"))
        
        # Ajouter une trace pour la première barre avec des valeurs de la première colonne Hauteur
        fig <- fig %>% add_trace(y = ~data_standeur$Hauteur, name = 'Hauteur 1', marker = list(color = '#2C2F65'), text = paste(round(data_standeur$Hauteur, 2), "cm"))
        
        moyenne <- mean(donnees$Hauteur)
        
        # Définir le mode de barres empilées
        fig <- fig %>% layout(yaxis = list(title = 'Hauteur'), barmode = 'overlay',
                              margin = list(t = 50, b = 50))
        
        hline <- function(y = moyenne, color = "#C5243D") {
          list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = moyenne,
            y1 = moyenne,
            line = list(color = color, dash = "dot")
          )
        }
        
        # Définir le mode de barres empilées
        fig <- fig %>% layout(
          shapes = list(hline(0.9)),
          yaxis = list(title = 'Hauteur'),
          barmode = 'overlay',
          margin = list(t = 50, b = 50)
        )
        
        return(fig)
      } else {
        print("Le fichier CSV est vide.")
      }
    } else {
      print("Le fichier CSV n'existe pas.")
    }
    # isDataProcessed <- reactiveVal(NULL)
    
  })
  
  
  output$results_text <- renderUI({  # Modification ici
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
          tags$span(style="font-size:2.2em;", "Hauteur record : "), tags$strong(style="font-size:2.2em;", round(hauteur_record, 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Dernière hauteur : "), tags$strong(style="font-size:2.2em;", round(data_standeur$Hauteur, 2)), tags$strong(style="font-size:2.2em;"," cm"), tags$br(), tags$br(),
          tags$span(style="font-size:2.2em;", "Moyenne : "), tags$strong(style="font-size:2.2em;", round(moyenne, 2)), tags$strong(style="font-size:2.2em;"," cm")
        )
        
        
        # Retourner le texte des résultats
        return(result_text)  # Modification ici
        
      } else {
        return("Le fichier CSV est vide.")
      }
    } else {
      return("Le fichier CSV n'existe pas.")
    }
  })
  
  
  

}
