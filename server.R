source("global.R")

server <- function(input, output, session) {
  # La logique du serveur ira ici.
  
  # Appeler un module avec callModule
  callModule(TappingServer, "Tapping_module")
  callModule(CMJ_Server, "CMJ_module")
  
}
