
create_features_by_date <- function(data, k, day_levels = NULL) {
  
  data <- data |>
    arrange(Date)
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  if (is.null(day_levels)) {
    day_levels <- sort(unique(
      as.character(data$DayOfWeek[!is.na(data$DayOfWeek)])
    ))
  }
  
  # Identify teams in each season
  season_teams <- lapply(
    split(data, as.character(data$SeasonStart)),
    function(x) {
      unique(c(
        as.character(x$HomeTeam),
        as.character(x$AwayTeam)
      ))
    }
  )
  
  # Initialise all team histories and standings
  teams <- unique(c(
    as.character(data$HomeTeam),
    as.character(data$AwayTeam)
  ))
  
  state <- initialise_state(teams)
  
  dates <- sort(unique(data$Date))
  
  output <- vector("list", length(dates))
  
  # Process matches by date
  for (d in seq_along(dates)) {
    
    current_date <- dates[d]
    
    today <- data |>
      filter(Date == current_date)
    
    season <- as.character(today$SeasonStart[1])
    
    # Handle season changes
    if (
      is.null(state$current_season) ||
      state$current_season != season
    ) {
      
      state <- start_season_fast(
        state,
        season,
        season_teams[[season]]
      )
    }
    
    # Calculate every fixture's features BEFORE updating results
    today_features <- create_today_features_fast(
      today = today,
      state = state,
      k = k,
      day_levels = day_levels
    )
    
    output[[d]] <- today_features
    
    # Only update histories using completed matches
    completed <- which(
      !is.na(today$HomeGoals) &
        !is.na(today$AwayGoals)
    )
    
    for (j in completed) {
      
      state <- update_state_fast(
        state,
        as.character(today$HomeTeam[j]),
        as.character(today$AwayTeam[j]),
        today$HomeGoals[j],
        today$AwayGoals[j]
      )
    }
  }
  
  bind_rows(output)
}





```{r}
get_recent_stats_fast <- function(history, k, venue = NULL) {
  
  if (is.null(venue)) {
    indices <- seq_along(history$GF)
  } else {
    indices <- which(history$Venue == venue)
  }
  
  indices <- tail(indices, k)
  
  if (length(indices) == 0) {
    return(list(
      GoalsScored = 0,
      GoalsConceded = 0,
      Points = 0,
      Wins = 0,
      Draws = 0,
      Losses = 0,
      Matches = 0
    ))
  }
  
  list(
    GoalsScored = mean(history$GF[indices]),
    GoalsConceded = mean(history$GA[indices]),
    Points = mean(history$Points[indices]),
    Wins = sum(history$Points[indices] == 3),
    Draws = sum(history$Points[indices] == 1),
    Losses = sum(history$Points[indices] == 0),
    Matches = length(indices)
  )
}

```



```{r}

initialise_state <- function(teams) {
  
  empty_history <- function() {
    list(
      GF = numeric(),
      GA = numeric(),
      Points = numeric(),
      Venue = character()
    )
  }
  
  list(
    history = setNames(
      lapply(teams, function(x) empty_history()),
      teams
    ),
    
    standings = matrix(
      0,
      nrow = length(teams),
      ncol = 3,
      dimnames = list(
        teams,
        c("Points", "GF", "GA")
      )
    ),
    
    current_season = NULL,
    current_teams = NULL,
    previous_positions = NULL,
    starting_positions = NULL
  )
}

```


```{r}
start_season_fast <- function(state, season, current_teams) {
  
  # Calculate previous season's final positions
  if (!is.null(state$current_season)) {
    
    previous_table <- data.frame(
      Team = state$current_teams,
      Points = state$standings[state$current_teams, "Points"],
      GF = state$standings[state$current_teams, "GF"],
      GA = state$standings[state$current_teams, "GA"]
    )
    
    previous_table$GD <-
      previous_table$GF - previous_table$GA
    
    previous_table <- previous_table[
      order(
        -previous_table$Points,
        -previous_table$GD,
        -previous_table$GF,
        previous_table$Team
      ),
    ]
    
    state$previous_positions <- setNames(
      seq_len(nrow(previous_table)),
      previous_table$Team
    )
  }
  
  state$current_season <- as.character(season)
  state$current_teams <- current_teams
  
  # Initialise previous-season positions
  state$starting_positions <- NULL
  
  if (!is.null(state$previous_positions)) {
    
    starting_positions <-
      state$previous_positions[current_teams]
    
    promoted <- is.na(starting_positions)
    
    # Give all promoted teams the same starting position
    if (any(promoted)) {
      
      starting_positions[promoted] <-
        max(state$previous_positions) + 1
    }
    
    state$starting_positions <- starting_positions
  }
  
  # Reset current-season standings
  state$standings[,] <- 0
  
  state
}

```


```{r}

update_state_fast <- function(state, home, away, hg, ag) {
  
  hp <- if (hg > ag) {
    3
  } else if (hg == ag) {
    1
  } else {
    0
  }
  
  ap <- if (ag > hg) {
    3
  } else if (ag == hg) {
    1
  } else {
    0
  }
  
  # Update home team's match history
  h <- state$history[[home]]
  
  h$GF <- c(h$GF, hg)
  h$GA <- c(h$GA, ag)
  h$Points <- c(h$Points, hp)
  h$Venue <- c(h$Venue, "Home")
  
  state$history[[home]] <- h
  
  # Update away team's match history
  a <- state$history[[away]]
  
  a$GF <- c(a$GF, ag)
  a$GA <- c(a$GA, hg)
  a$Points <- c(a$Points, ap)
  a$Venue <- c(a$Venue, "Away")
  
  state$history[[away]] <- a
  
  # Update league standings
  state$standings[home, "Points"] <-
    state$standings[home, "Points"] + hp
  
  state$standings[home, "GF"] <-
    state$standings[home, "GF"] + hg
  
  state$standings[home, "GA"] <-
    state$standings[home, "GA"] + ag
  
  state$standings[away, "Points"] <-
    state$standings[away, "Points"] + ap
  
  state$standings[away, "GF"] <-
    state$standings[away, "GF"] + ag
  
  state$standings[away, "GA"] <-
    state$standings[away, "GA"] + hg
  
  state
}

```


```{r}
get_current_table_fast <- function(state) {
  
  teams <- state$current_teams
  
  table_now <- data.frame(
    Team = teams,
    
    Points = unname(
      state$standings[teams, "Points"]
    ),
    
    GF = unname(
      state$standings[teams, "GF"]
    ),
    
    GA = unname(
      state$standings[teams, "GA"]
    )
  )
  
  table_now$GD <- table_now$GF - table_now$GA
  
  if (!is.null(state$starting_positions)) {
    
    table_now$PreviousPosition <-
      unname(state$starting_positions[table_now$Team])
    
  } else {
    
    table_now$PreviousPosition <- NA_real_
  }
  
  table_now <- table_now[
    order(
      -table_now$Points,
      -table_now$GD,
      -table_now$GF,
      table_now$PreviousPosition,
      na.last = TRUE
    ),
  ]
  
  table_now$Position <- seq_len(nrow(table_now))
  
  table_now
}

```


```{r}
create_today_features_fast <- function(
    today,
    state,
    k,
    day_levels
) {
  
  # Calculate the league table once for today's fixtures
  table_now <- get_current_table_fast(state)
  
  positions <- setNames(
    table_now$Position,
    table_now$Team
  )
  
  points <- setNames(
    table_now$Points,
    table_now$Team
  )
  
  goal_difference <- setNames(
    table_now$GD,
    table_now$Team
  )
  
  # Generate features for each fixture
  features <- lapply(seq_len(nrow(today)), function(i) {
    
    home <- as.character(today$HomeTeam[i])
    away <- as.character(today$AwayTeam[i])
    
    # Overall recent performance
    home_stats <- get_recent_stats_fast(
      state$history[[home]], k
    )
    
    away_stats <- get_recent_stats_fast(
      state$history[[away]], k
    )
    
    # Venue-specific recent performance
    home_venue_stats <- get_recent_stats_fast(
      state$history[[home]], k, venue = "Home"
    )
    
    away_venue_stats <- get_recent_stats_fast(
      state$history[[away]], k, venue = "Away"
    )
    
    data.frame(
      
      Date = today$Date[i],
      SeasonStart = today$SeasonStart[i],
      
      DayOfWeek = factor(
        as.character(today$DayOfWeek[i]),
        levels = day_levels
      ),
      
      HomeTeam = home,
      AwayTeam = away,
      
      # Overall recent performance
      Home_GS = home_stats$GoalsScored,
      Home_GC = home_stats$GoalsConceded,
      Home_Points = home_stats$Points,
      Home_Wins = home_stats$Wins,
      
      Away_GS = away_stats$GoalsScored,
      Away_GC = away_stats$GoalsConceded,
      Away_Points = away_stats$Points,
      Away_Wins = away_stats$Wins,
      
      # Venue-specific recent performance
      Home_Home_GS = home_venue_stats$GoalsScored,
      Home_Home_GC = home_venue_stats$GoalsConceded,
      
      Away_Away_GS = away_venue_stats$GoalsScored,
      Away_Away_GC = away_venue_stats$GoalsConceded,
      
      # League standings
      Home_Position = unname(positions[home]),
      Away_Position = unname(positions[away]),
      
      Home_Current_Points = unname(points[home]),
      Away_Current_Points = unname(points[away]),
      
      Home_GD = unname(goal_difference[home]),
      Away_GD = unname(goal_difference[away]),
      
      # Historical information
      Home_History = home_stats$Matches,
      Away_History = away_stats$Matches,
      
      # Response variables
      HomeGoals = today$HomeGoals[i],
      AwayGoals = today$AwayGoals[i]
      
    )
  })
  
  do.call(rbind, features)
}

