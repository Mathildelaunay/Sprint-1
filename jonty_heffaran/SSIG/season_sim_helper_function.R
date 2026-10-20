#install.packages("tidyr")

library(tidyr)
library(dplyr)

simulate_season <- function(fixtures, team_ratings, table, game_model) {
  
  for (i in seq_len(nrow(fixtures))) {
    
    home_team <- fixtures$home_team[i]
    away_team <- fixtures$away_team[i]
    
    goals <- game_model(team_ratings, home_team, away_team)
    
    home_goals <- goals[1]
    away_goals <- goals[2]
    
    # Find the rows for each team
    home_idx <- match(home_team, table$Team)
    away_idx <- match(away_team, table$Team)
    
    # Update matches played and goals
    table$P[c(home_idx, away_idx)] <-
      table$P[c(home_idx, away_idx)] + 1
    
    table$GF[home_idx] <- table$GF[home_idx] + home_goals
    table$GA[home_idx] <- table$GA[home_idx] + away_goals
    
    table$GF[away_idx] <- table$GF[away_idx] + away_goals
    table$GA[away_idx] <- table$GA[away_idx] + home_goals
    
    # Allocate results and points
    if (home_goals > away_goals) {
      
      table$W[home_idx] <- table$W[home_idx] + 1
      table$L[away_idx] <- table$L[away_idx] + 1
      table$Pts[home_idx] <- table$Pts[home_idx] + 3
      
    } else if (home_goals < away_goals) {
      
      table$W[away_idx] <- table$W[away_idx] + 1
      table$L[home_idx] <- table$L[home_idx] + 1
      table$Pts[away_idx] <- table$Pts[away_idx] + 3
      
    } else {
      
      table$D[c(home_idx, away_idx)] <-
        table$D[c(home_idx, away_idx)] + 1
      
      table$Pts[c(home_idx, away_idx)] <-
        table$Pts[c(home_idx, away_idx)] + 1
    }
  }
  
  return(table)
}
