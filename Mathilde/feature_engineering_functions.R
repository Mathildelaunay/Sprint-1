library(dplyr)
library(tidyr)
library(lubridate)

build_season_dataset <- function(current_data,
                                 previous_data,
                                 season_label) {
  
  # ---- A. Current-season matches ----------------------------------------
  matches <- current_data %>%
    mutate(
      Date = dmy(as.character(Date)),
      Year = season_label,
      match_id = paste0(gsub("/", "", season_label), "_", row_number())
    ) %>%
    arrange(Date, match_id)
  
  # ---- B. Previous-season table (used only for RankCategory) ------------
  previous_season_matches <- bind_rows(
    previous_data %>%
      transmute(
        Team = HomeTeam,
        goals_for = FTHG,
        goals_against = FTAG,
        result = case_when(FTR == "H" ~ "W", FTR == "A" ~ "L", FTR == "D" ~ "D")
      ),
    previous_data %>%
      transmute(
        Team = AwayTeam,
        goals_for = FTAG,
        goals_against = FTHG,
        result = case_when(FTR == "A" ~ "W", FTR == "H" ~ "L", FTR == "D" ~ "D")
      )
  )
  
  previous_season_stats <- previous_season_matches %>%
    group_by(Team) %>%
    summarise(
      previous_season_points =
        sum(if_else(result == "W", 3, if_else(result == "D", 1, 0))),
      previous_season_goal_difference = sum(goals_for - goals_against),
      previous_season_goals_scored    = sum(goals_for),
      .groups = "drop"
    ) %>%
    arrange(
      desc(previous_season_points),
      desc(previous_season_goal_difference),
      desc(previous_season_goals_scored)
    ) %>%
    mutate(
      previous_season_ranking = row_number(),
      RankCategory = case_when(
        previous_season_ranking <= 3  ~ "Top3",
        previous_season_ranking <= 7  ~ "4to7",
        previous_season_ranking <= 12 ~ "8to12",
        previous_season_ranking <= 17 ~ "13to17",
        TRUE ~ NA_character_            # promoted teams get "Promoted" later
      )
    )
  
  # ---- C. Two rows per match (home and away) ----------------------------
  home_data <- matches %>%
    transmute(
      match_id, Date, Year,
      Team = HomeTeam,
      Opponent = AwayTeam,
      HomeAway = "Home",
      goals_for = FTHG,
      goals_against = FTAG,
      result = case_when(FTR == "H" ~ "W", FTR == "D" ~ "D", FTR == "A" ~ "L"),
      red_cards = HR,
      yellow_cards = HY,
      fouls = HF,
      corners = HC,
      shots = HS,
      shots_on_target = HST
    )
  
  away_data <- matches %>%
    transmute(
      match_id, Date, Year,
      Team = AwayTeam,
      Opponent = HomeTeam,
      HomeAway = "Away",
      goals_for = FTAG,
      goals_against = FTHG,
      result = case_when(FTR == "A" ~ "W", FTR == "D" ~ "D", FTR == "H" ~ "L"),
      red_cards = AR,
      yellow_cards = AY,
      fouls = AF,
      corners = AC,
      shots = AS,
      shots_on_target = AST
    )
  
  team_matches <- bind_rows(home_data, away_data) %>%
    arrange(Date, match_id, HomeAway)
  
  # ---- D. Pre-match statistics (only matches before this one) -----------
  team_features <- team_matches %>%
    group_by(Team) %>%
    arrange(Date, match_id, .by_group = TRUE) %>%
    mutate(
      matches_played = row_number() - 1L,
      previous_match_date = lag(Date),
      days_since_last_match = as.numeric(Date - previous_match_date),
      
      wins   = lag(cumsum(result == "W"), default = 0L),
      draws  = lag(cumsum(result == "D"), default = 0L),
      losses = lag(cumsum(result == "L"), default = 0L),
      
      goals_scored_total   = lag(cumsum(goals_for),     default = 0),
      goals_conceded_total = lag(cumsum(goals_against), default = 0),
      red_cards_total      = lag(cumsum(red_cards),     default = 0),
      yellow_cards_total   = lag(cumsum(yellow_cards),  default = 0),
      fouls_total          = lag(cumsum(fouls),         default = 0),
      corners_total        = lag(cumsum(corners),       default = 0),
      shots_total          = lag(cumsum(shots),         default = 0),
      shots_on_target_total = lag(cumsum(shots_on_target), default = 0)
    ) %>%
    mutate(
      win_frequency  = wins   / pmax(matches_played, 1),
      loss_frequency = losses / pmax(matches_played, 1),
      draw_frequency = draws  / pmax(matches_played, 1),
      
      # 0 in a team's first match (placeholder, not an observation)
      goals_scored_per_match   = goals_scored_total   / pmax(matches_played, 1),
      goals_conceded_per_match = goals_conceded_total / pmax(matches_played, 1),
      
      red_cards_per_match = if_else(matches_played > 0,
                                    red_cards_total / matches_played, NA_real_),
      yellow_cards_per_match = if_else(matches_played > 0,
                                       yellow_cards_total / matches_played, NA_real_),
      fouls_per_match = if_else(matches_played > 0,
                                fouls_total / matches_played, NA_real_),
      corners_per_match = if_else(matches_played > 0,
                                  corners_total / matches_played, NA_real_),
      shots_per_match = if_else(matches_played > 0,
                                shots_total / matches_played, NA_real_),
      shots_on_target_per_match = if_else(matches_played > 0,
                                          shots_on_target_total / matches_played, NA_real_)
    ) %>%
    ungroup() %>%
    left_join(
      previous_season_stats %>% select(Team, RankCategory),
      by = "Team"
    ) %>%
    mutate(RankCategory = coalesce(RankCategory, "Promoted"))
  
  # ---- E. Opponent features ---------------------------------------------
  opponent_features <- team_features %>%
    select(
      match_id, Team,
      matches_played,
      days_since_last_match,
      RankCategory,
      win_frequency, loss_frequency, draw_frequency,
      goals_scored_per_match, goals_conceded_per_match,
      red_cards_per_match, yellow_cards_per_match,
      fouls_per_match, corners_per_match,
      shots_on_target_per_match, shots_per_match
    ) %>%
    rename_with(~ paste0("opp_", .x), -c(match_id, Team)) %>%
    rename(Opponent = Team)
  
  # ---- F. Difference features and output --------------------------------
  team_features %>%
    left_join(opponent_features, by = c("match_id", "Opponent")) %>%
    mutate(
      TeamRankCategory = RankCategory,
      OpponentRankCategory = opp_RankCategory,
      
      Round = matches_played,
      OpponentRound = opp_matches_played,
      
      Days = days_since_last_match - opp_days_since_last_match,
      RestAdvantage = case_when(
        is.na(Days) ~ "Equal",
        Days >= 2   ~ "More",
        Days <= -2  ~ "Less",
        TRUE        ~ "Equal"
      ),
      
      DWF = win_frequency  - opp_win_frequency,
      DLF = loss_frequency - opp_loss_frequency,
      DDF = draw_frequency - opp_draw_frequency,
      
      DRF  = red_cards_per_match - opp_red_cards_per_match,
      DYF  = yellow_cards_per_match - opp_yellow_cards_per_match,
      DFF  = fouls_per_match - opp_fouls_per_match,
      DCS  = corners_per_match - opp_corners_per_match,
      DSTF = shots_on_target_per_match - opp_shots_on_target_per_match,
      DSF  = shots_per_match - opp_shots_per_match
    ) %>%
    mutate(
      across(c(DRF, DYF, DFF, DCS, DSTF, DSF), ~ replace_na(.x, 0))
    ) %>%
    transmute(
      match_id, Year, Date, Team, Opponent, HomeAway,
      TeamRankCategory, OpponentRankCategory,
      Round, OpponentRound, RestAdvantage,
      DWF, DLF, DDF,
      goals_scored_per_match,   opp_goals_scored_per_match,
      goals_conceded_per_match, opp_goals_conceded_per_match,
      DRF, DYF, DFF, DCS, DSTF, DSF,
      Goals = goals_for
    ) %>%
    arrange(match_id, desc(HomeAway == "Home"))
}