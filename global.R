library(dplyr)
library(httr)
library(jsonlite)

library(maps)
library(countrycode)

library(shiny)
library(leaflet)
library(htmlwidgets)

data(world.cities)
data(canada.cities)
data(us.cities)

# Seasons to include
first_year <- 1917
last_year <- 2022

# Data file name
# birthplaces_csv <- "./birthplaces.csv"
birthplaces_rds <- "./birthplaces.rds"

# Load data file if exists, otherwise use API
if (!file.exists(birthplaces_rds)) {
  geo_data <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(geo_data) <- c("start_year", "person_id", "birth_city", "birth_country", "lat", "long", "nationality")
  
  # Retrieve data for every season
  # start_year of 2022 corresponds to 2022-23 season
  for (start_year in first_year:last_year) {
    # All teams' roster data
    teams_response <- GET("https://statsapi.web.nhl.com/api/v1/teams",
                          query = list(expand = "team.roster", season = start_year * 10000 + (start_year + 1)))
    
    teams_content <- fromJSON(rawToChar(teams_response$content), flatten = TRUE)
    
    # Merge player list for all teams
    players <- bind_rows(teams_content$teams$roster.roster, .id = "column_label")
    
    # Retrieve data for every player
    for (i in 1:length(players$person.id)) {
      # Player data
      player_response <- GET(paste("https://statsapi.web.nhl.com/api/v1/people/", toString(players$person.id[i]), sep = ""))
      player_content <- fromJSON(rawToChar(player_response$content))
      
      player <- player_content$people
      
      nationality <- player$nationality
      
      city <- player$birthCity
      country <- player$birthCountry
      
      # Determine USA/CAN (state/province data provided) or other
      if ("birthStateProvince" %in% colnames(player)) {
        usa_can_city <- paste(city, player$birthStateProvince, sep = " ")
        
        if (country == "CAN") {
          lat <- canada.cities$lat[canada.cities$name == usa_can_city]
          lon <- canada.cities$long[canada.cities$name == usa_can_city]
        } else {
          lat <- us.cities$lat[us.cities$name == usa_can_city]
          lon <- us.cities$long[us.cities$name == usa_can_city]
        }
      } else {
        lat <- world.cities$lat[world.cities$name == city & countrycode(world.cities$country.etc, origin = "country.name", destination = "iso3c") == country]
        lon <- world.cities$long[world.cities$name == city & countrycode(world.cities$country.etc, origin = "country.name", destination = "iso3c") == country]
      }
      
      # If found coordinates and this player hasn't been accounted for in this season, add geographical data
      if (!identical(lat, numeric(0)) & !any(geo_data$person.id == player$id & geo_data$start_year == start_year)) {
        geo_data[nrow(geo_data) + 1,] <- list(start_year, player$id, city, country, lat, lon, nationality)
      }
    }
  }
  
  # Count players by season and birthplace
  birthplaces <- geo_data %>% count(start_year, birth_city, birth_country, lat, long)
  
  # Save birthplaces data
  # write.table(birthplaces, file = birthplaces_csv)
  saveRDS(birthplaces, birthplaces_rds)
}

# Read data
birthplaces <- readRDS(birthplaces_rds)

# Shiny app
shinyApp(ui, server)