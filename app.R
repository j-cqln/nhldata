library(shiny)
library(leaflet)

source("utils/process.R")
source("utils/rink.R")

source("app/ui.R")
source("app/server.R")

# Shiny app
shinyApp(ui = ui, server = server)