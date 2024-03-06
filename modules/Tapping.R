library(shiny)
library(dplyr)

# UI module
TappingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, align = "center",
             h4("Sélections"),
             br(),
      ),
    ),
  )
}

# Server module
TappingServer <- function(input, output, session) {
  ns <- session$ns
  
  
}
