source("jonty_heffaran/SSIG/SSIG_function.R")
source("jonty_heffaran/SSIG/season_sim_helper_function.R")
source("jonty_heffaran/SSIG/table_functions.R")
team_ratings <- list(Arsenal = 1, Brighton = 10, Palace = 0.1, Dynamo_Bristol = 1) # nolint: line_length_linter.

pois_game_model <- function(team_ratings, home_team, away_team) {
  home_goals <- rpois(1, lambda = team_ratings[[home_team]])
  away_goals <- rpois(1, lambda = team_ratings[[away_team]])
  return(c(home_goals, away_goals))
}


league_table <- SSIG(team_ratings, pois_game_model)
print(league_table)
