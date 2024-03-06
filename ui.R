library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  tags$head(
    tags$style(HTML("
    /* Set the background and text colors */
    body {
      background-color: #F5FAFA; /* a very light white */
      color: #000000; /* black for text to ensure readability */
    }
    
    /* Style the navigation bar with a soft blue */
    .navbar, .navbar-default {
      background-color: #ACD1E9; /* soft blue */
    }
    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav>li>a {
      color: #000000; /* black text for contrast */
    }
    
    /* Style buttons with pastel shades */
    .btn-primary {
      background-color: #ffd6a5; /* pastel peach */
      border-color: #fae1dd; /* lighter peach border */
      color: #000000; /* black text for contrast */
    }
    
    /* Customize the look of panels */
    .panel-default>.panel-heading {
      background-color: #caffbf; /* pastel green */
      color: #000000; /* black text for readability */
    }
    
    
    /* Style the wells */
    .well {
      color: #000000; /* black text for readability */
    }
    
    /* Color the table headers */
    table thead {
      background-color: #C1DAD6; /* pastel violet */
      color: #000000; /* black text for contrast */
    }
    
    /* Color the table rows */
    table tbody tr:nth-child(odd) {
      background-color: #ffadad; /* soft red */
      color: #000000; /* black text for contrast */
    }
    table tbody tr:nth-child(even) {
      background-color: #fdffb6; /* soft yellow */
      color: #000000; /* black text for contrast */
    }
    
    /* Additional customization for inputs to make them stand out */
    .shiny-input-container {
      background-color: #bee2ff; /* very soft blue */
      border: 1px solid #a0c4ff; /* solid blue border */
      color: #000000; /* black text for readability */
    }
    
    /* Customize datatable styling */
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select {
      border-radius: 5px;
      border: 1px solid #a0c4ff;
      padding: 5px;
      color: #000000; /* black text for readability */
    }
    
    /* Personalization if necessary */
  ")),
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:400,700", rel = "stylesheet", type = "text/css")
  ),
  titlePanel("Optojump Tapping et CMJ", windowTitle = "Optojump visualisation"),
  tabsetPanel(
    tabPanel("Tapping", TappingUI("Tapping_module")),
    tabPanel("CMJ", CMJ_UI("CMJ_module")),
  )
)
