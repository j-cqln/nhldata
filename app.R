library(shiny)
library(leaflet)

source("server.R")
source("ui.R")

# Shiny app
shinyApp(ui = ui, server = server)