library(shiny)
library(leaflet)

source("R/process.R")
source("R/rink.R")

source("ui.R")
source("server.R")

# Shiny app
shinyApp(ui = ui, server = server)