library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  # tags$head(
  #   tags$style(HTML()),
  #   tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:400,700", rel = "stylesheet", type = "text/css")
  # ),
  titlePanel("Optojump Tapping et CMJ", windowTitle = "Optojump visualisation"),
  tabsetPanel(tabPanel("Tapping", TappingUI("Tapping_module")),
              tabPanel("CMJ", CMJ_UI("CMJ_module")),)
)
