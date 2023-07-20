library(httr)
library(jsonlite)

library(dplyr)

library(stringi)
library(maps)
library(countrycode)

library(sp)

# Get birthplaces
get.birthplaces <- function(first_year, last_year, name) {
  data(world.cities)
  data(canada.cities)
  data(us.cities)
  
  world.cities$name <- tolower(world.cities$name)
  world.cities$country.etc <- countrycode(world.cities$country.etc,
                                          origin = "country.name",
                                          destination = "iso3c")
  
  canada.cities$name <- tolower(canada.cities$name)
  us.cities$name <- tolower(us.cities$name)
  
  geo_data <- list()
  geo_data_capacity <- 1
  geo_data_size <- 0
  
  # Retrieve data for every season
  for (start_year in first_year:last_year) {
    # All teams roster data
    teams_response <- GET("https://statsapi.web.nhl.com/api/v1/teams",
                          query = list(expand = "team.roster",
                                       season = start_year * 10000 + (start_year + 1)))
    
    teams_content <- fromJSON(rawToChar(teams_response$content), flatten = TRUE)
    
    # Merge player list for all teams
    players <- bind_rows(teams_content$teams$roster.roster, .id = "column_label")
    
    # Retrieve data for every player
    n <- nrow(players)
    
    if (n > 0) {
      for (i in 1:n) {
        # Player data
        player_response <- GET(paste0("https://statsapi.web.nhl.com/api/v1/people/",
                                      toString(players$person.id[i])))
        
        player_content <- fromJSON(rawToChar(player_response$content))
        
        player <- player_content$people
        
        nationality <- player$nationality
        
        city <- player$birthCity
        country <- player$birthCountry
        
        city_search <- tolower(stri_trans_general(city, "latin-ascii"))
        
        # Determine CAN/USA (state/province data provided) or other
        if ("birthStateProvince" %in% colnames(player)) {
          can_usa_city_search <- tolower(paste(city_search, player$birthStateProvince))
          
          if (country == "CAN") {
            lat <- canada.cities$lat[canada.cities$name == can_usa_city_search]
            lon <- canada.cities$long[canada.cities$name == can_usa_city_search]
          } else {
            lat <- us.cities$lat[us.cities$name == can_usa_city_search]
            lon <- us.cities$long[us.cities$name == can_usa_city_search]
          }
          
        } else {
          lat <- world.cities$lat[world.cities$name == city_search &
                                    world.cities$country.etc == country]
          
          lon <- world.cities$long[world.cities$name == city_search &
                                     world.cities$country.etc == country]
        }
        
        # Add geographical data if player not accounted for this season
        # Coordinates may not be available
        if (!any(geo_data$person.id == player$id &
                 geo_data$start_year == start_year)) {
          
          if (identical(lat, numeric(0))) {
            lat <- NA
            lon <- NA
          }
          
          if (geo_data_capacity == geo_data_size) {
            geo_data_capacity <- geo_data_capacity * 2
            length(geo_data) <- geo_data_capacity
          }
          
          geo_data_size <- geo_data_size + 1
          
          geo_data[[geo_data_size]] <- list(start_year,
                                            player$id,
                                            city,
                                            country,
                                            lat,
                                            lon,
                                            nationality)
        }
      }
    }
  }
  
  geo_data <- do.call(rbind.data.frame, geo_data)
  colnames(geo_data) <- c("start_year", "person_id",
                          "birth_city", "birth_country",
                          "lat", "lon",
                          "nationality")
  
  # Count players by season and birthplace
  birthplaces <- geo_data %>% count(start_year,
                                    birth_city, birth_country,
                                    lat, lon)
  
  # Save birthplaces data
  saveRDS(birthplaces, name)
  
  return(birthplaces)
}

# Helper function for use when creating rink regions
format.region <- function(d, name) {
  df <- d %>%
    as.data.frame() %>%
    setNames(c('x', 'y')) %>%
    mutate(region = name)
  
  return(df)
}

# Assign regions to data
get.regions <- function(d) {
  inner.slot <- cbind(c( -7,   7,   7,  -7,  -7),
                      c(-69, -69, -89, -89, -69))
  right.slot <- cbind(c(-22,  -7,  -7, -22, -22),
                      c(-54, -54, -89, -69, -54))
  left.slot <- cbind(c(  7,  22,  22,   7,   7),
                     c(-54, -54, -69, -89, -54))
  high.slot <- cbind(c( -7,   7,   7,  -7,  -7),
                     c(-54, -54, -69, -69, -54))
  
  right.outside <- cbind(c(-42.5, -22, -22,  -7, -36.75, -42.5, -42.5),
                         c(  -54, -54, -69, -89,    -89,   -72,   -54))
  left.outside <- cbind(c( 22, 42.5, 42.5, 36.75,   7,  22,  22),
                        c(-54,  -54,  -72,   -89, -89, -69, -54))
  
  right.point <- cbind(c(-42.5,  -7,  -7, -42.5, -42.5),
                       c(  -26, -26, -54,   -54,   -26))
  left.point <- cbind(c(  7, 42.5, 42.5,   7,   7),
                      c(-26,  -26,  -54, -54, -26))
  center.point <- cbind(c( -7,   7,   7,  -7,  -7),
                        c(-26, -26, -54, -54, -26))
  
  regions <- rbind(format.region(inner.slot, 'inner.slot'),
                   format.region(right.slot, 'right.slot'),
                   format.region(left.slot, 'left.slot'),
                   format.region(high.slot, 'high.slot'),
                   format.region(right.outside, 'right.outside'),
                   format.region(left.outside, 'left.outside'),
                   format.region(right.point, 'right.point'),
                   format.region(left.point, 'left.point'),
                   format.region(center.point, 'center.point'))
  
  s.inner.slot <- Polygons(list(Polygon(inner.slot)), 'inner.slot')
  s.right.slot <- Polygons(list(Polygon(right.slot)), 'right.slot')
  s.left.slot <- Polygons(list(Polygon(left.slot)), 'left.slot')
  s.high.slot <- Polygons(list(Polygon(high.slot)), 'high.slot')
  
  s.right.outside <- Polygons(list(Polygon(right.outside)), 'right.outside')
  s.left.outside <- Polygons(list(Polygon(left.outside)), 'left.outside')
  
  s.right.point <- Polygons(list(Polygon(right.point)), 'right.point')
  s.left.point <- Polygons(list(Polygon(left.point)), 'left.point')
  s.center.point <- Polygons(list(Polygon(center.point)), 'center.point')
  
  s.regions <- SpatialPolygons(list(s.inner.slot,
                                    s.right.slot,
                                    s.left.slot,
                                    s.high.slot,
                                    s.right.outside,
                                    s.left.outside,
                                    s.right.point,
                                    s.left.point,
                                    s.center.point))
  
  s.regions.df <- SpatialPolygonsDataFrame(s.regions,
                                           data.frame(region = c('inner.slot',
                                                                 'right.slot',
                                                                 'left.slot',
                                                                 'high.slot',
                                                                 'right.outside',
                                                                 'left.outside',
                                                                 'right.point',
                                                                 'left.point',
                                                                 'center.point'),
                                                      row.names = c('inner.slot',
                                                                    'right.slot',
                                                                    'left.slot',
                                                                    'high.slot',
                                                                    'right.outside',
                                                                    'left.outside',
                                                                    'right.point',
                                                                    'left.point',
                                                                    'center.point')))
  points <- d[, c('x','y')]
  coordinates(points) <- ~x+y
  result <- over(points, s.regions.df)
  
  d <- d %>%
    mutate(region = result$region,
           region = ifelse(is.na(region), 'other', region))
  
  return(d)
}

# Get shots
get.shots <- function(first_year, last_year, name) {
  shots <- data.frame()
  
  for (start_year in first_year:last_year) {
    temp <- tempfile()
    download.file(paste0('https://peter-tanner.com/moneypuck/downloads/shots_',
                         as.character(start_year),
                         '.zip'),
                  temp)
    temp_shots <- read.csv(unz(temp,
                               paste0('shots_',
                                      as.character(start_year),
                                      '.csv')))
    
    temp_shots <- temp_shots %>%
      select(where(function (x) any(!is.na(x)))) %>%
      rename(shot_id = shotID,
             away_team = awayTeamCode,
             home_team = homeTeamCode,
             away_goals = awayTeamGoals,
             home_goals = homeTeamGoals,
             away_skaters = awaySkatersOnIce,
             home_skaters = homeSkatersOnIce,
             is_home = isHomeTeam,
             last_event = lastEventCategory,
             time_since_last_event = timeSinceLastEvent,
             shooter = shooterName,
             shooter_id = shooterPlayerId,
             shooter_handedness = shooterLeftRight,
             off_wing = offWing,
             goalie = goalieNameForShot,
             goalie_id = goalieIdForShot,
             distance = shotDistance,
             angle = shotAngle,
             shot_type = shotType,
             on_goal = shotWasOnGoal,
             is_rebound = shotRebound,
             frozen = shotGoalieFroze,
             stopped = shotPlayStopped,
             in_zone = shotPlayContinuedInZone,
             out_zone = shotPlayContinuedOutsideZone,
             generate_rebound = shotGeneratedRebound,
             xg = xGoal) %>%
      mutate(season = start_year,
             goalie = ifelse(is.na(goalie), "Empty net", goalie),
             season_type = ifelse(isPlayoffGame == 0, "regular", "playoffs"),
             x = -yCordAdjusted,
             y = -xCordAdjusted,
             abs.angle = abs(angle),
             opposing_skaters = ifelse(team == "HOME", away_skaters, home_skaters),
             team_skaters = ifelse(team == "HOME", home_skaters, away_skaters),
             opposing_score = ifelse(team == "HOME", away_goals, home_goals),
             team_score = ifelse(team == "HOME", home_goals, away_goals),
             opposing_team = ifelse(team == "HOME", away_team, home_team),
             team = ifelse(team == "HOME", home_team, away_team),
             goal_diff = team_score - opposing_score) %>%
      filter(!(x == 0 & y == 0))
    
    if (start_year == first_year) {
      shots <- temp_shots
    } else {
      shots <- rbind(shots, temp_shots)
    }
  }
  
  # Name standardizations
  shots$team[shots$team == 'L.A'] <- 'LAK'
  shots$team[shots$team == 'N.J'] <- 'NJD'
  shots$team[shots$team == 'S.J'] <- 'SJS'
  shots$team[shots$team == 'T.B'] <- 'TBL'
  shots$opposing_team[shots$opposing_team == 'L.A'] <- 'LAK'
  shots$opposing_team[shots$opposing_team == 'N.J'] <- 'NJD'
  shots$opposing_team[shots$opposing_team == 'S.J'] <- 'SJS'
  shots$opposing_team[shots$opposing_team == 'T.B'] <- 'TBL'
  
  shooters <- shots %>%
    select(shooter, shooter_id) %>%
    filter(!duplicated(shooter_id))
  
  shots <- shots %>% 
    rename(shooter_old = shooter) %>%
    left_join(shooters, by = 'shooter_id')
  
  goalies <- shots %>%
    select(goalie, goalie_id) %>%
    filter(!duplicated(goalie_id))
  
  shots <- shots %>%
    rename(goalie_old = goalie) %>%
    left_join(goalies, by = 'goalie_id')
  
  rm(shooters, goalies)
  
  # Assign regions
  shots <- get.regions(shots)
  shots <- shots %>%
    group_by(region) %>%
    mutate(region_avg_xg = mean(xg),
           region_avg_g = mean(goal)) %>%
    ungroup()
  
  # Reorder, drop columns
  shots <- shots %>% select(season, game_id, shot_id,
                            season_type,
                            period, time,
                            event,
                            last_event, time_since_last_event,
                            team, opposing_team, is_home,
                            team_skaters, opposing_skaters,
                            team_score, opposing_score, goal_diff,
                            shooter, shooter_id, shooter_handedness, off_wing,
                            goalie, goalie_id,
                            x, y, distance, angle, abs.angle, shot_type,
                            goal,
                            on_goal, is_rebound,
                            frozen, stopped,
                            in_zone, out_zone, generate_rebound,
                            xg,
                            region, region_avg_g, region_avg_xg)
  # Save shots data
  saveRDS(shots, name)
  
  return(shots)
}

# Seasons to include
FIRST_YEAR <- 1917
LAST_YEAR <- 2022

FIRST_YEAR_RTSS <- 2007

# Data files
FILE_NAMES <- c("data/birthplaces.rds", "data/shots.rds")

# Load data file if exists, otherwise use API
if (all(file.exists(FILE_NAMES))) {
  birthplaces <- readRDS(FILE_NAMES[1])
  shots <- readRDS(FILE_NAMES[2])
} else {
  birthplaces <- get.birthplaces(FIRST_YEAR, LAST_YEAR, FILE_NAMES[1])
  shots <- get.shots(FIRST_YEAR_RTSS, LAST_YEAR, FILE_NAMES[2])
}