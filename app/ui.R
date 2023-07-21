ui <- fluidPage(
  titlePanel("NHL over the years"),
  
  tabsetPanel(
    tabPanel("Hometowns",
             fluid = TRUE,
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Active players' birthplaces"),
                 p("Birthplaces of players active during the season from 
                   1917/18 to 2022/23. 
                   Some geographical coordinates not available. 
                   Click on map markers to view location name and number of 
                   players."),
                 p("Data retrieved from the NHL API."),
                 br(),
                 
                 sliderInput(inputId = "year_bio",
                             label = "Season",
                             min = FIRST_YEAR,
                             max = LAST_YEAR,
                             value = LAST_YEAR,
                             animate = animationOptions(interval = 300),
                             sep = "")
               ),
               
               mainPanel(
                 leafletOutput("birthplaces_map", height = "500px", width = "100%"),
                 
                 br(),
                 
                 fluidRow(
                   column(h4("Cities with most players"),
                          tableOutput("birthplaces_table1"),
                          width = 4),
                   
                   column(h4("Countries with most players"),
                          tableOutput("birthplaces_table2"),
                          width = 4)
                 )
               )
             )
    ),
    
    tabPanel("Shot explorer",
             fluid = TRUE,
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Shot explorer"),
                 p("Unblocked shots, regular season and playoffs, from 2007/08 
                   to 2022/23."),
                 p("Data retrieved from MoneyPuck.com."),
                 br(),
                 
                 sliderInput(inputId = "year_shots",
                             label = "Season",
                             min = FIRST_YEAR_RTSS,
                             max = LAST_YEAR,
                             value = LAST_YEAR,
                             animate = animationOptions(interval = 7000),
                             sep = ""),
                 
                 selectInput(inputId = "team_name",
                             label = "Team",
                             choices = sort(unique(shots$team[shots$season == LAST_YEAR])),
                             selected = "MTL")
               ),
               
               mainPanel(
                 plotOutput("shots_goals_teams", width = "100%"),
                 plotOutput("shots_map", height = "400px"),
                 plotOutput("shots_distance", width = "100%"),
                 plotOutput("shots_angle", width = "100%")
               )
             )
    )
  )
)