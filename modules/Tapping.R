library(shiny)
library(dplyr)
source("./Fonctions.R")

# UI module
TappingUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    fluidRow(column(
      width = 6,
      align = "center",
      selectInput(
        ns("column_select"),
        label = "Sélectionner une colonne",
        choices = c(
          "Temps de vol",
          "Temps de contact",
          "Point d'équilibreX",
          "Décalage latéralX",
          "Largeur de pas"
        ),
        selected = NULL
      ),
      plotOutput(ns("results_plot")) # Ajouter un espace réservé pour le graphique
    )
    )
  )
  
}

# Server module
TappingServer <- function(input, output, session) {
  ns <- session$ns
  
  Tapping_data <- reactiveVal(NULL)
  
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
                     "Point d'équilibreX" = result[, 4],
                     "Décalage latéralX" = result[, 5],
                     "Largeur de pas" = result[, 6],
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
      read.csv2("results_globaux_Tapping.csv", encoding = "latin1")
    showNotification(paste("Lecture dataframe ok"), type = "message")
    
    # Extraire la dernière ligne du fichier CSV
    last_row <- tail(results_globaux_Tapping, n = 1)
    
    # Extraire la valeur de la colonne "Nom" de la dernière ligne
    last_name <- last_row$Nom
    
    # Vérifier quelle colonne a été sélectionnée
    selected_column <- input$column_select
    if (selected_column == "Temps de vol") {
      # Filtrer les données en fonction du temps de vol et de la valeur de la colonne "Nom" de la dernière ligne
      #filtered_results <- results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Temps de vol")]
      
      # Afficher les résultats filtrés dans le tableau
      #filtered_results <- renderTable(#filtered_results)
    } else if (selected_column == "Temps de contact") {
      # Filtrer les données en fonction du temps de contact et de la valeur de la colonne "Nom" de la dernière ligne
      #filtered_results <- results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Temps de contact")]
      
      # Afficher les résultats filtrés dans le tableau
      #output$results_table <- renderTable(#filtered_results)
    } else if (selected_column == "Point d'équilibreX") {
      # Filtrer les données en fonction du point d'équilibreX et de la valeur de la colonne "Nom" de la dernière ligne
      #filtered_results <- results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Point d'équilibreX")]
      
      # Afficher les résultats filtrés dans le tableau
      #output$results_table <- renderTable(#filtered_results)
    } else if (selected_column == "Décalage latéralX") {
      # Filtrer les données en fonction du décalage latéralX et de la valeur de la colonne "Nom" de la dernière ligne
      #filtered_results <- results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Décalage latéralX")]
      
      # Afficher les résultats filtrés dans le tableau
      #output$results_table <- renderTable(#filtered_results)
    } else if (selected_column == "Largeur de pas") {
      # Filtrer les données en fonction de la largeur de pas et de la valeur de la colonne "Nom" de la dernière ligne
      #filtered_results <- results_globaux_Tapping[results_globaux_Tapping$Nom == last_name, c("Test", "Nom", "Sexe", "G.D", "Largeur de pas")]
      
      # Afficher les résultats filtrés dans le tableau
      #output$results_table <- renderTable(#filtered_results)
    }
  })
  
  
  
  ##############################################
  
  
}
