library(shiny)
library(dplyr)

# UI module
CMJ_UI <- function(id) {
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
CMJ_Server <- function(input, output, session) {
  ns <- session$ns
  
  
}
