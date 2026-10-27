library(dplyr)
library(ggplot2)

goal_data <- data.frame(Goals = c(train_data$FTHG, train_data$FTAG))

ggplot(goal_data, aes(x = Goals)) + geom_bar() + scale_x_continuous(breaks = 0:max(goal_data$Goals, na.rm = TRUE)) +labs(title = "Goals scored per match",x = "Goals",y = "Number of team performances") + theme_minimal()

set.seed(1)
predictor_columns <- c("HomeFTGoalsBefore", "AwayFTGoalsBefore", "HTPB", "ATPB", "HomeShotsBefore", "AwayShotsBefore", "HomeShotsOnTargetBefore", "AwayShotsOnTargetBefore", "HomeCornersBefore", "AwayCornersBefore", "HomeFoulsBefore", "AwayFoulsBefore", "HomeYellowCardsBefore", "AwayYellowCardsBefore", "HomeRedCardsBefore", "AwayRedCardsBefore")

#variables we want to apply to our model
home_train <- train_data[, c("FTHG", predictor_columns)]
away_train <- train_data[, c("FTAG", predictor_columns)]

#full model formulas
home_full_formula <- reformulate(predictor_columns, response = "FTHG")
away_full_formula <- reformulate(predictor_columns, response = "FTAG")

#null models
home_null <- glm(FTHG ~ 1, data = home_train, family = poisson)
away_null <- glm(FTAG ~ 1, data = away_train, family = poisson)

#forward backward selection using AIC
home_step <- step(home_null, scope = list(lower = ~1, upper = home_full_formula), direction = "both")
away_step <- step(away_null, scope = list(lower = ~1, upper = away_full_formula), direction = "both")

#full models
home_full <- glm(home_full_formula, data = home_train, family = poisson)
away_full <- glm(away_full_formula, data = away_train, family = poisson)

# comparison
AIC(home_null, home_step, home_full)
AIC(away_null, away_step, away_full)

formula(home_step)
formula(away_step)

home_selected_model <- home_step
away_selected_model <- away_step

par(mfrow = c(2, 2))
plot(home_selected_model)

par(mfrow = c(2, 2))
plot(away_selected_model)

par(mfrow = c(1, 1))

c(Home = sum(residuals(home_selected_model, type = "pearson")^2) /df.residual(home_selected_model),
  Away = sum(residuals(away_selected_model, type = "pearson")^2) /df.residual(away_selected_model))

train_teams<-unique(c(train_data$HomeTeam, train_data$AwayTeam))
length(train_teams)
length(intersect(train_teams, unique(c(test_data$HomeTeam, test_data$AwayTeam))))

length(intersect(train_teams, unique(c(prediction_data$HomeTeam, prediction_data$AwayTeam))))
setdiff(unique(c(prediction_data$HomeTeam, prediction_data$AwayTeam)),train_teams)

text_columns <- names(train_data)[sapply(train_data, is.character)] #names the text columns in the dataset
train_data[text_columns] <- lapply(train_data[text_columns], factor) #applies factor() to the text columns
test_data[text_columns] <- lapply(test_data[text_columns], factor) #applies factor() to the text columns

#predict goals for each match in the test season
test_data$PredHomeGoals <- round(predict(home_selected_model, newdata = test_data, type = "response")) #using home model
test_data$PredAwayGoals <- round(predict(away_selected_model, newdata = test_data, type = "response")) #using away model

make_table <- function(matches, home_goals, away_goals) {
  home_table <- data.frame(Team = matches$HomeTeam,
                           Points = ifelse(home_goals > away_goals,
                                           3,
                                           ifelse(home_goals == away_goals,
                                                  1,
                                                  0)),GF = home_goals,GA = away_goals)
  
  away_table <- data.frame(Team = matches$AwayTeam,
                           Points = ifelse(away_goals > home_goals,
                                           3,
                                           ifelse(away_goals == home_goals,
                                                  1,
                                                  0)), GF = away_goals,GA = home_goals)
  
  all_team_matches <- rbind(home_table, away_table)
  
  table <- aggregate(cbind(Points, GF, GA) ~ Team, data = all_team_matches, sum) #creates the table with all summed up values
  
  table$GD <- table$GF - table$GA #create new column with each elements following this formula
  table <- table[order(-table$Points, -table$GD), ] #ordering the teams wrt conditions given (i.e. points and gd)
  table$Position <- seq_len(nrow(table)) #adding a column that tells us the index
  
  table <- table[c("Position", "Team", "Points", "GF", "GA", "GD")]
  rownames(table) <- NULL
  
  table
}

set.seed(1)
predicted_table <- make_table(test_data, test_data$PredHomeGoals, test_data$PredAwayGoals)

actual_table <- make_table(test_data, test_data$FTHG, test_data$FTAG)

table_comparison <- merge(predicted_table, actual_table, by = "Team", suffixes = c("_Predicted", "_Actual"))

table_comparison$PositionDifference <- table_comparison$Position_Predicted - table_comparison$Position_Actual

table_comparison <- table_comparison[order(table_comparison$Position_Actual),]

table_comparison

final_train_data <- bind_rows(train_data, test_data)

home_final_model <- glm(formula(home_selected_model),data = final_train_data,family = poisson)
away_final_model <- glm(formula(away_selected_model),data = final_train_data,family = poisson)

unplayed <- prediction_data$Date > as.Date("2026-10-19") #rows of unplayed matches

#create new columns for predictions
prediction_data$PredHomeGoals <- NA
prediction_data$PredAwayGoals <- NA

#do prediction in unplayed data
prediction_data$PredHomeGoals[unplayed] <- predict(home_final_model,newdata = prediction_data[unplayed, ],type = "response")
prediction_data$PredAwayGoals[unplayed] <- predict(away_final_model,newdata = prediction_data[unplayed, ],type = "response")

#creates a vector of predictions
final_home_goals <- ifelse(unplayed,prediction_data$PredHomeGoals,prediction_data$FTHG)
final_away_goals <- ifelse(unplayed,prediction_data$PredAwayGoals,prediction_data$FTAG)

#builds the prediction table
predicted_2026_27_table <- make_table(prediction_data,final_home_goals,final_away_goals)

predicted_2026_27_table


