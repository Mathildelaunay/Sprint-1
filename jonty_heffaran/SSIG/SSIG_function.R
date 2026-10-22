#install.packages("tidyr")

library(tidyr)
library(dplyr)

SSIG <- function(team_ratings, game_model) {
  teams <- names(team_ratings)
  
  # Generate home and away fixtures
  fixtures <- expand_grid(
    home_team = teams,
    away_team = teams
  ) %>%
    filter(home_team != away_team)
  
  # Initialise league table
  table <- create_league_table(teams)
  
  # Run simulation
  table <- simulate_season(
    fixtures, team_ratings, table, game_model
  )
  
  league_table <- format_league_table(table)
  
  return(league_table)
}
