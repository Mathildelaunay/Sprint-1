#install.packages("tidyr")

library(tidyr)
library(dplyr)

create_league_table <- function(teams) {
  table <- tibble(
    Team = teams,
    P = 0L,
    W = 0L,
    D = 0L,
    L = 0L,
    GF = 0L,
    GA = 0L,
    GD = 0L,
    Pts = 0L
  )
  
  return(table)
}

format_league_table <- function(table) {
  
  table %>%
    mutate(GD = GF - GA) %>%
    arrange(desc(Pts), desc(GD), desc(GF))
}
