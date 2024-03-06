library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(
    tags$style(HTML(
      "
      body { 
        background-color: #2C2F65 !important;
        color: white !important;
      }
      .shiny-output-error,
      .shiny-output-error-messages,
      .shiny-output-error-message,
      h1, h2, h3, h4, h5, h6 {
        color: white !important;
      }
      .tabset-panel .nav-link:hover {
        color: white !important;
        background-color: #2C2F65 !important;
      }
      .nav-item.active .nav-link {
        background-color: #2C2F65 !important;
        color: white !important;
      }
      .nav-tabs > li > a {
        background-color: #2C2F65 !important;
        color: white !important;
      }
      .nav-tabs > li.active > a {
        background-color: #2C2F65 !important;
        color: white !important;
      }
      "
    ))
  ),
  
  
  
  titlePanel(
    div(
      img(src = "insep.png", height = 50, width = 50), # Ajoute le logo
      "Optojump Tapping et CMJ", 
      windowTitle = "Optojump visualisation"
    )
  ),
  
  tabsetPanel(
    tabPanel(tags$span("Tapping"), TappingUI("Tapping_module")),
    tabPanel(tags$span("CMJ"), CMJ_UI("CMJ_module")),
    id = "myTabsetPanel" # Add an id to the tabsetPanel for CSS targeting
  )
  
)