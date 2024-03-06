library(shiny)
library(dplyr)

# UI module
CMJ_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, align = "center",
             h4("TEST CMJ"),
             fileInput(ns("xml_file"), "Uploader un fichier XML"),
             br(),
             textInput(ns("athlete_name"), label = "Nom de l'athlète"),
             selectInput(ns("gender_select"), label = "Genre", choices = c("Homme", "Femme", "Non binaire")),
             actionButton(ns("submit_button"), label = "Soumettre")
      ),
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("bar_chart"))
      )
    )
  )
}

# Server module
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  cmj_data <- reactiveVal(NULL)
  
  observeEvent(input$submit_button, {
    req(input$xml_file, input$athlete_name, input$gender_select)
    
    
    # Exemple d'utilisation
    xml_file <- input$xml_file$datapath
    columns_to_extract <- c("Elevation")
    
    #Resultats en colonnes
    result <- parse_xml_file(xml_file, columns_to_extract)
    #print(result)

    # Si préférence résultats en ligne

    ###############################################
    result <- as.data.frame(t(result))
    # Définir les noms de colonnes avec la première ligne
    names(result) <- unlist(result[1, ])
    # Supprimer la première ligne car elle est maintenant utilisée comme noms de colonnes
    result <- result[-1, ]
    result <- as.numeric(result)
    
    # Imprimer la plus grande valeur
    max_value <- max(result)
    print(max_value)
    
    ##############################################
    
  })
  
  output$bar_chart <- renderPlotly({
    if (!is.null(cmj_data())) {
      athlete_name <- input$athlete_name
      athlete_data <- cmj_data()[cmj_data()$Nom == athlete_name, ]
      
      if (nrow(athlete_data) > 0) {
        largeur <- c(0.2, 0.2)
        # Créer le graphique en barres empilées avec Plotly
        fig <- plot_ly(athlete_data, x = ~Test, type = 'bar', width = largeur)
        
        # Ajouter une trace pour la deuxième barre avec des valeurs de la deuxième colonne Hauteur
        fig <- fig %>% add_trace(y = ~athlete_data$Hauteur[2], name = 'Hauteur 2', marker = list(color = '#F9E9CA'), text = paste(round(athlete_data$Hauteur[2], 2), "cm"))
        
        # Ajouter une trace pour la première barre avec des valeurs de la première colonne Hauteur
        fig <- fig %>% add_trace(y = ~athlete_data$Hauteur[1], name = 'Hauteur 1', marker = list(color = '#2C2F65'), text = paste(round(athlete_data$Hauteur[1], 2), "cm"))
        
        moyenne <- mean(cmj_data()$Hauteur)
        
        # Définir le mode de barres empilées
        fig <- fig %>% layout(yaxis = list(title = 'Hauteur'), barmode = 'overlay')
        hline <- function(y = mean(cmj_data()$Hauteur), color = "#C5243D") {
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
          barmode = 'overlay'
        )
        
        return(fig)
      } else {
        showNotification("Aucune donnée trouvée pour cet athlète", type = "warning")
      }
    }
  })
}
