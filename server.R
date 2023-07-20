library(reshape2)

library(ggstar)
library(pubtheme)
library(gridExtra)

server <- function(input, output, session) {
  data <- reactive({
    b <- subset(birthplaces, start_year == input$year_bio)
    s <- subset(shots, season == input$year_shots)
    
    x <- list(birthplaces = b, shots = s)
  })
  
  output$birthplaces_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$Stamen.Watercolor,
                       options = providerTileOptions(minZoom = 1)) %>%
      setView(lng = -20, lat = 30, zoom = 2)
  })
  
  output$birthplaces_table1 <- renderTable({
    b <- birthplaces %>% filter(start_year == input$year_bio)
    
    head(b %>%
           arrange(desc(n)) %>%
           rename(city = birth_city,
                  country = birth_country,
                  players = n) %>%
           select(city, country, players),
         10)
  })
  
  output$birthplaces_table2 <- renderTable({
    b <- birthplaces %>% filter(start_year == input$year_bio)
    
    head(b %>%
           group_by(birth_country) %>%
           summarise(players = n()) %>%
           ungroup() %>%
           arrange(desc(players)) %>%
           mutate(country = countrycode(birth_country,
                                        origin = 'iso3c',
                                        destination = 'country.name')) %>%
           select(country, players),
         10)
  })
  
  output$shots_goals_teams <- renderPlot({
    background_color <- '#ffffff'
    
    s <- shots %>%
      filter(season == input$year_shots) %>%
      group_by(team, season_type) %>%
      summarise(shots = n(),
                goals = sum(goal)) %>%
      arrange(desc(shots)) %>%
      select(team, season_type, shots, goals)
    
    s <- melt(s, id = c('team', 'season_type'))
    
    temp <- s %>%
      group_by(team, variable) %>%
      summarise(value = sum(value)) %>%
      ungroup()
    
    coeff <- 2 * max(temp$value[temp$variable == 'goals']) /
      max(temp$value[temp$variable == 'shots'])
    
    s <- s %>%
      mutate(value = ifelse(variable == 'goals', value / coeff, value))
    
    ggplot(data = s,
           aes(x = variable,
               y = value,
               fill = interaction(season_type, variable, sep = ' '),
               alpha = 0.8)) +
      geom_col(width = 1,
               position = position_stack(reverse = FALSE)) +
      scale_alpha_continuous(guide = FALSE) +
      scale_fill_manual(values = c('#d54545', '#ef9ba3', '#1295d0', '#73c3e8')) +
      scale_y_continuous(name = 'Shots',
                         sec.axis = sec_axis(~.*coeff, name = 'Goals')) +
      labs(title = paste0('Shots and goals per team, ', input$year_shots),
           subtitle = 'Regular season and playoffs (if qualified)',
           caption = 'Created by github.com/j-cqln',
           x = 'Event (shots or goals)',
           fill = 'Season type',
           alpha = '') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            panel.spacing.x = unit(0, units = "in"),
            axis.ticks.x.bottom = element_blank(),
            axis.text.x.bottom = element_blank(),
            strip.text = element_text(colour = pubtextgray,
                                      size = 36/3,
                                      angle = 90)) +
      facet_wrap(vars(team), nrow = 1)
  })
  
  output$shots_map <- renderPlot({
    background_color <- '#ffffff'
    gradient_low_color <- '#bedceb'
    gradient_high_color <- '#004b71'
    gradient_low_color3 <- '#eee2d2'
    gradient_high_color3 <- '#d57700'
    
    title <- paste0('Shots by % goal, ',
                    input$year_shots, ' ')
    title3 <- paste0('Shots against by % goal, ',
                     input$year_shots, ' ')
    
    s <- shots %>% filter(season == input$year_shots)
    
    s_hexbin <- hexbin::hexbin(s$x, s$y, xbins = 21, IDs = TRUE)
    s_hexbin_df <- data.frame(hexbin::hcell2xy(s_hexbin),
                              cell = s_hexbin@cell,
                              count = s_hexbin@count)
    s$cell <- s_hexbin@cID
    
    s2 <- s %>%
      filter(team == input$team_name) %>%
      group_by(cell) %>%
      summarise(shot_count = n(),
                avg_g = mean(goal)) %>%
      ungroup() %>%
      right_join(s_hexbin_df, by = 'cell') %>%
      select(cell, x, y, count, shot_count, avg_g)
    
    s3 <- s %>%
      filter(opposing_team == input$team_name) %>%
      group_by(cell) %>%
      summarise(shot_count = n(),
                avg_g = mean(goal)) %>%
      ungroup() %>%
      right_join(s_hexbin_df, by = 'cell') %>%
      select(cell, x, y, count, shot_count, avg_g)
    
    s <- s %>%
      group_by(cell) %>%
      summarise(shot_count = n(),
                avg_g = mean(goal)) %>%
      ungroup() %>%
      right_join(s_hexbin_df, by = 'cell') %>%
      select(cell, x, y, count, shot_count, avg_g)
    
    p <- rink +
      geom_star(data = s,
                aes(x = x, y = y,
                    fill = avg_g,
                    size = shot_count),
                color = NA,
                starshape = 'hexagon',
                show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color,
                          high = gradient_high_color,
                          na.value = NA,
                          limits = c(0.0, 0.2),
                          breaks = c(0.0, 0.2),
                          labels = c('0%', '20%+'),
                          oob = squish) +
      scale_size_area(limits = c(0, 1000),
                      breaks = c(100, 500, 1000),
                      labels = c('100', '500', '1000+'),
                      oob = squish) +
      labs(title = paste0(title, 'all'),
           subtitle = 'Percent of shots becoming goals by region',
           caption = 'Created by github.com/j-cqln',
           fill = '% goal',
           size = 'Unblocked shot attempts') +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
      guides(size = guide_legend(order = 1,
                                 nrow = 1,
                                 title.position = 'top',
                                 title.hjust = 0,
                                 override.aes = list(fill = pubdarkgray)), 
             fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
    
    p2 <- rink +
      geom_star(data = s2,
                aes(x = x, y = y,
                    fill = avg_g,
                    size = shot_count),
                color = NA,
                starshape = 'hexagon',
                show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color,
                          high = gradient_high_color,
                          na.value = NA,
                          limits = c(0.0, 0.2),
                          breaks = c(0.0, 0.2),
                          labels = c('0%', '20%+'),
                          oob = squish) +
      scale_size_area(limits = c(0, 80),
                      breaks = c(5, 40, 80),
                      labels = c('5', '40', '80+'),
                      oob = squish) +
      labs(title = paste0(title, input$team_name),
           subtitle = 'Percent of shots becoming goals by region',
           caption = 'Created by github.com/j-cqln',
           fill = '% goal',
           size = 'Unblocked shot attempts') +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
      guides(size = guide_legend(order = 1,
                                 nrow = 1,
                                 title.position = 'top',
                                 title.hjust = 0,
                                 override.aes = list(fill = pubdarkgray)), 
             fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
    
    p3 <- rink +
      geom_star(data = s3,
                aes(x = x, y = y,
                    fill = avg_g,
                    size = shot_count),
                color = NA,
                starshape = 'hexagon',
                show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color3,
                          high = gradient_high_color3,
                          na.value = NA,
                          limits = c(0.0, 0.2),
                          breaks = c(0.0, 0.2),
                          labels = c('0%', '20%+'),
                          oob = squish) +
      scale_size_area(limits = c(0, 80),
                      breaks = c(5, 40, 80),
                      labels = c('5', '40', '80+'),
                      oob = squish) +
      labs(title = paste0(title3, input$team_name),
           subtitle = 'Percent of shots becoming goals by region',
           caption = 'Created by github.com/j-cqln',
           fill = '% goal',
           size = 'Unblocked shot attempts') +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
      guides(size = guide_legend(order = 1,
                                 nrow = 1,
                                 title.position = 'top',
                                 title.hjust = 0,
                                 override.aes = list(fill = pubdarkgray)), 
             fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
    
    grid.arrange(p, p2, p3, ncol = 3)
  })
  
  output$shots_distance <- renderPlot({
    background_color <- '#ffffff'
    
    title <- paste0('Shots by distance, ',
                    input$year_shots, ' ')
    title3 <- paste0('Shots against by distance, ',
                     input$year_shots, ' ')
    
    s <- shots %>%
      filter(season == input$year_shots) %>%
      mutate(season_type = factor(season_type,
                                  levels = c('regular', 'playoffs'),
                                  labels = c('regular', 'playoffs')))
    
    s2 <- s %>% filter(team == input$team_name)
    s3 <- s %>% filter(opposing_team == input$team_name)
    
    p <- ggplot(data = s,
                aes(distance,
                    fill = season_type,
                    color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title, 'all'),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Distance from net',
           y = 'Shot density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    p2 <- ggplot(data = s2,
                 aes(distance,
                     fill = season_type,
                     color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title, input$team_name),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Distance from net',
           y = 'Shot density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    p3 <- ggplot(data = s3,
                 aes(distance,
                     fill = season_type,
                     color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title3, input$team_name),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Distance from net',
           y = 'Shot against density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    grid.arrange(p, p2, p3, ncol = 3)
  })
  
  output$shots_angle <- renderPlot({
    background_color <- '#ffffff'
    
    title <- paste0('Shots by angle, ',
                    input$year_shots, ' ')
    
    title3 <- paste0('Shots against by angle, ',
                     input$year_shots, ' ')
    
    s <- shots %>%
      filter(season == input$year_shots) %>%
      mutate(season_type = factor(season_type,
                                  levels = c('regular', 'playoffs'),
                                  labels = c('regular', 'playoffs')))
    
    s2 <- s %>% filter(team == input$team_name)
    
    s3 <- s %>% filter(opposing_team == input$team_name)
    
    p <- ggplot(data = s,
                aes(angle,
                    fill = season_type,
                    color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title, 'all'),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Shot angle relative to net',
           y = 'Shot density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    p2 <- ggplot(data = s2,
                 aes(angle,
                     fill = season_type,
                     color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title, input$team_name),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Shot angle relative to net',
           y = 'Shot density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    p3 <- ggplot(data = s3,
                 aes(angle,
                     fill = season_type,
                     color = season_type)) +
      geom_density(alpha = 0.1) +
      labs(title = paste0(title3, input$team_name),
           subtitle = 'Regular season and playoffs comparison',
           caption = 'Created by github.com/j-cqln',
           x = 'Shot against angle relative to net',
           y = 'Shot against density',
           fill = 'Season type',
           color = 'Season type') +
      theme_pub(type = 'line', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color))
    
    grid.arrange(p, p2, p3, ncol = 3)
  })
  
  observe({
    x <- data()
    b <- x$birthplaces
    s <- x$shots
    
    leafletProxy('birthplaces_map', data = b) %>%
      clearMarkers() %>%
      addCircleMarkers(lat = b$lat,
                       lng = b$lon,
                       popup = paste0(b$birth_city, ': ', b$n, ' player(s)'),
                       radius = b$n)
    
    updateSelectInput(session, 'team_name',
                      choices = sort(unique(s$team)),
                      selected = ifelse(input$team_name %in% sort(unique(s$team)),
                                        input$team_name, sort(unique(s$team))[1])
    )
  })
}