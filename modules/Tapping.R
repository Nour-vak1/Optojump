library(shiny)
library(dplyr)

source("./Fonctions.R")

# UI module
TappingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    fluidRow(
      column(width = 6, align = "center",
             h2("Sélections"),
             br(), ),
      fileInput(ns("xml_file"), "Uploader un fichier XML"),
      br(),
      textInput(ns("athlete_name"), label = "Nom de l'athlète"),
      selectInput(
        ns("gender_select"),
        label = "Genre",
        choices = c("Homme", "Femme", "Non binaire")
      ),
      actionButton(ns("submit_button"), label = "Soumettre")
    ),
    fluidRow(
      column(
        width = 6,
        align = "center",
        selectInput(
          ns("column_select"),
          label = "Sélectionner une colonne",
          choices = c(
            "",
            "Fréquence de saut",
            "Temps de vol",
            "Temps de contact",
            "Point d'équilibreX",
            "Décalage latéralX",
            "Largeur de pas"
          ),
          selected = "Fréquence de saut"
        ),
        plotlyOutput(ns("results_plot")) # Ajouter un espace réservé pour le graphique
      )
    )
  )
  
}

# Server module
TappingServer <- function(input, output, session) {
  ns <- session$ns
  
  if (!file.exists("results_globaux_Tapping.csv")) {
    data.frame("Test" = character(),
               "Nom"= character(),
               "Sexe"= character(),
               "G/D" = character(),
               "Temps de vol" = numeric(),
               "Temps de contact" = numeric(),
               "Point d'équilibreX" = numeric(),
               "Décalage latéralX" = numeric(),
               "Largeur de pas" = numeric(),
               "Rythme[p/s]" = numeric(),
               "isAthlete" = logical()) %>%
      write.csv(file = "results_globaux_Tapping.csv", row.names = FALSE)
  }
  
  Tapping_data <- reactiveVal(NULL)
  isDataProcessed <- reactiveVal(NULL)
  
  observeEvent(input$submit_button,
               {
                 print("test")
                 req(input$xml_file, input$athlete_name, input$gender_select)
                 
                 # Exemple d'utilisation
                 xml_file <- input$xml_file$datapath
                 
                 #25 29 30 64 65
                 columns_to_extract <-
                   c(
                     "G/D",
                     "Temps de vol",
                     "Temps de contact",
                     "Rythme[p/s]",
                     "Point d'équilibreX",
                     "Décalage latéralX",
                     "Largeur de pas"
                     
                   )
                 
                 #Resultats en colonnes
                 result <-
                   parse_xml_file(xml_file, columns_to_extract)
                 print("test")
                 
                 # Si préférence résultats en ligne
                 
                 ###############################################
                 result <- as.data.frame(t(result))
                 
                 # Définir les noms de colonnes avec la première ligne
                 names(result) <- unlist(result[1,])
                 print("test")
                 # Supprimer la première ligne car elle est maintenant utilisée comme noms de colonnes
                 result <- result[-1,]
                 print("ici")
                 str(result)
                 print(class(result))
                 print(class(result[1]))
                 
                 result[, 2] <-
                   as.numeric(result[, 2], na.rm = TRUE)
                 print(1)
                 result[, 3] <-
                   as.numeric(result[, 3], na.rm = TRUE)
                 print(2)
                 result[, 4] <-
                   as.numeric(result[, 4], na.rm = TRUE)
                 print(3)
                 result[, 5] <-
                   as.numeric(result[, 5], na.rm = TRUE)
                 print(4)
                 result[, 6] <-
                   as.numeric(result[, 6], na.rm = TRUE)
                 result[, 7] <-
                   as.numeric(result[, 7], na.rm = TRUE)
                 
                 
                 
                 # Calculer la moyenne de chaque colonne
                 col_means <- colMeans(result[, 2:7], na.rm = TRUE)
                 
                 str(result)
                 result[, 2:7] <-
                   lapply(result[, 2:7], function(x) {
                     x[is.na(x)] <- mean(x, na.rm = TRUE)
                     return(x)
                   })
                 
                 # Créer un nouveau data frame avec les nouvelles colonnes en positions 1, 2 et 3
                 # Imprimer la plus grande valeur
                 
                 Test = "Tapping"
                 Nom <- input$athlete_name
                 Sexe <- input$gender_select
                 print("test")
                 # Création d'un dataframe avec les données
                 nouvelle_ligne <-
                   data.frame(
                     Test = Test,
                     Nom = Nom,
                     Sexe = Sexe,
                     "G/D" = result[, 1],
                     "Temps de vol" = result[, 2],
                     "Temps de contact" = result[, 3],
                     "Rythme[p/s]" = result[, 4],
                     "Point d'équilibreX" = result[, 5],
                     "Décalage latéralX" = result[, 6],
                     "Largeur de pas"= result[, 7],
                     isAthlete = FALSE
                   )
                 
                 print("test")
                 
                 # Vérification si le fichier CSV existe
                 
                 if (!file.exists("results_globaux_Tapping.csv")) {
                   # Si le fichier n'existe pas, écrire le dataframe complet
                   write.csv(nouvelle_ligne, file = "results_globaux_Tapping.csv", row.names = FALSE)
                   showNotification(paste("Dataframe créé et rempli avec succès"), type = "message")
                 } else {
                   # Si le fichier existe, lire les données actuelles
                   donnees_existantes <-
                     read.csv("results_globaux_Tapping.csv")
                   
                   # Ajouter la nouvelle ligne aux données existantes
                   donnees_combinees <-
                     rbind(donnees_existantes, nouvelle_ligne)
                   # Réécrire le fichier CSV avec toutes les données
                   write.csv(donnees_combinees,
                             file = "results_globaux_Tapping.csv",
                             row.names = FALSE)
                   isDataProcessed(TRUE)
                   
                 }
                 
               })
  # Observer pour mettre à jour le tableau en fonction de la colonne sélectionnée
  observeEvent(input$column_select, {
    # Lire les données du fichier CSV
    results_globaux_Tapping <-
      read.csv("results_globaux_Tapping.csv", encoding = "utf-8")
    showNotification(paste("Lecture dataframe ok"), type = "message")
    
    # Extraire la dernière ligne du fichier CSV
    last_row <- tail(results_globaux_Tapping, n = 1)
    
    # Extraire la valeur de la colonne "Nom" de la dernière ligne
    last_name <- last_row$Nom
  
    str(results_globaux_Tapping)
    # Vérifier quelle colonne a été sélectionnée
    selected_column <- input$column_select
    if (selected_column == "") {
      
    }
    else if (selected_column == "Fréquence de saut") {
      # Filtrer les données en fonction du temps de vol et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test",
                                                                            "Nom",
                                                                            "Sexe",
                                                                            "G.D",
                                                                            "Rythme.p.s.")]
      head(filtered_results)

      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        # creer_barplot(filtered_results, "Rythme.p.s.")
        plot_smooth_line_ggplot (filtered_results,"Rythme.p.s.")
      })
      
      
    }
    else if (selected_column == "Temps de vol") {
      # Filtrer les données en fonction du temps de vol et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Temps.de.vol")]
      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        creer_camembert(filtered_results, "Temps.de.vol")
      })
    } else if (selected_column == "Temps de contact") {
      # Filtrer les données en fonction du temps de contact et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Temps.de.contact")]
      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        creer_camembert(filtered_results, "Temps.de.contact")
      })
    } else if (selected_column == "Point d'équilibreX") {
      # Filtrer les données en fonction du point d'équilibreX et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Point.d.équilibreX")]
      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        creer_camembert(filtered_results, "Point.d.équilibreX")
      })
    } else if (selected_column == "Décalage latéralX") {
      # Filtrer les données en fonction du décalage latéralX et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D","Rythme.p.s.", "Temps.de.vol","Temps.de.contact", "Décalage.latéralX")]
      str(filtered_results)
      
      RthG <- mean(filtered_results %>% dplyr::filter(G.D == "G") %>% pull("Rythme.p.s."))
      RthD <- mean(filtered_results %>% dplyr::filter(G.D == "D") %>% pull("Rythme.p.s."))
      tcG <- mean(filtered_results %>% dplyr::filter(G.D == "G") %>% pull("Temps.de.vol"))
      tcD <- mean(filtered_results %>% dplyr::filter(G.D == "D") %>% pull("Temps.de.vol"))
      tvG <- mean(filtered_results %>% dplyr::filter(G.D == "G") %>% pull("Temps.de.contact"))
      tvD <- mean(filtered_results %>% dplyr::filter(G.D == "D") %>% pull("Temps.de.contact"))
      
      str(RthD)
      str(RthG)
      str(tcG)
      str(tcD)
      str(tvG)
      str(tvD)
      
      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        radar_chart(filtered_results,7,6,5,8)
      })
    } else if (selected_column == "Largeur de pas") {
      # Filtrer les données en fonction de la largeur de pas et de la valeur de la colonne "Nom" de la dernière ligne
      filtered_results <-
        results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D","Rythme.p.s.", "Temps.de.vol","Temps.de.contact", "Décalage.latéralX")]
      
      # Afficher le graphique camembert
      output$results_plot <- renderPlotly({
        cone_plot(filtered_results, "Rythme.p.s.","Décalage.latéralX","Temps.de.contact")
      })
    }
    isDataProcessed <- reactiveVal(NULL)
  })
  
  
  
  ##############################################
  
  
}
