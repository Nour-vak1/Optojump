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
      column(width = 3, align = "right",
             plotOutput(ns("bar_chart"))
             
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
      # Ajouter la nouvelle ligne aux données existantes
      donnees_combinees <- rbind(donnees_existantes, nouvelle_ligne)
      # Réécrire le fichier CSV avec toutes les données
      write.csv(donnees_combinees, file = "results_globaux_CMJ.csv", row.names = FALSE)
      isDataProcessed(TRUE)
    }
    
    ##############################################
  })
  
  output$bar_chart <- renderPlot({
    req(input$xml_file, input$athlete_name, input$gender_select, isDataProcessed())
    if (file.exists("results_globaux_CMJ.csv")) {
      donnees <- read.csv("results_globaux_CMJ.csv")
      
      if (!is.null(donnees) && nrow(donnees) > 0) {
        # Filtrer les données pour l'athlète actuel
        athlete_data <- subset(donnees, Nom == input$athlete_name)
        
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
          
          
          record_nour <- max(subset(donnees, isAthlete == TRUE)$Hauteur)
          record_salon <- max(subset(donnees, isAthlete == FALSE)$Hauteur)

          df_result_param <- data.frame(
            labels = c("Record Nour", "Record Salon", "Votre record"),
            valeur = c(record_nour, record_salon, record_visiteur())
          )
          
          moyenne = mean(subset(donnees, isAthlete == FALSE)$Hauteur)
          
          
          
          
          
          
          
          
          
          
          
          
          
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
          print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          print(labels)
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
          
          
          
          
          
          
          
          
          
          
          
          
          
          # return(create_stacked_bar_plot(df_result_param, moy))
          
        } else {
          print("Aucune donnée trouvée pour cet athlète.")
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
