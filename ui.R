ui <- fluidPage(
  # App title
  titlePanel("NHL over the years"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for year input
    sidebarPanel(
      strong("NHL historical data"),
      p("Season by season data from 1917/18 to 2022/23."),
      p("Data retrieved from the NHL API (https://statsapi.web.nhl.com/api/v1/)."),
      br(),
      
      sliderInput(inputId = "year",
                  label = "Season",
                  min = first_year,
                  max = last_year,
                  value = last_year,
                  animate = animationOptions(interval = 300),
                  sep = "") 
    ),
  
    # Main panel for displaying map
    mainPanel(
      tabsetPanel(
        tabPanel("Birthplaces of players active during the season", leafletOutput("birthplaces_map", width = "100%"), br(), p("Click on a map marker to see location name and number of players.")),
        tabPanel("More coming soon")
      )
    )
  )
)