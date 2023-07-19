source("helpers/process.R")
source("helpers/rink.R")

source("ui.R")
source("server.R")

# Shiny app
shinyApp(ui = ui, server = server)