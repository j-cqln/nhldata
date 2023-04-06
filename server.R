server <- function(input, output, session) {
  data <- reactive({
    x <- subset(birthplaces, start_year == input$year)
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() %>% addProviderTiles(provider = providers$Stamen.Watercolor, options = providerTileOptions(minZoom = 1)) %>%
                  setView(lng = -20, lat = 30, zoom = 2)
    m
  })
  
  observe({
    df <- data()
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(lat = df$lat,
                       lng = df$lon,
                       popup = paste(df$birth_city, ": ", df$n, " player(s)", sep = ""),
                       radius = df$n)
  })
}