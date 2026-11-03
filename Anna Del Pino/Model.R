library(dplyr)
library(ggplot2)

goal_data <- data.frame(Goals = c(train_data$FTHG, train_data$FTAG))

ggplot(goal_data, aes(x = Goals)) + geom_bar() + scale_x_continuous(breaks = 0:max(goal_data$Goals, na.rm = TRUE)) +labs(title = "Goals scored per match",x = "Goals",y = "Number of team performances") + theme_minimal()

set.seed(1)
predictor_columns <- c("HomeFTGoalsBefore", "AwayFTGoalsBefore", "HTPB", "ATPB", "HomeShotsBefore", "AwayShotsBefore", "HomeShotsOnTargetBefore", "AwayShotsOnTargetBefore", "HomeCornersBefore", "AwayCornersBefore", "HomeFoulsBefore", "AwayFoulsBefore", "HomeYellowCardsBefore", "AwayYellowCardsBefore", "HomeRedCardsBefore", "AwayRedCardsBefore")

#variables we will use in our models (i.e. predict (FTHG and FTAG) and use predictor_columns)
home_train <- train_data[, c("FTHG", predictor_columns)] 
away_train <- train_data[, c("FTAG", predictor_columns)]

#full model formulas
home_full_formula <- reformulate(predictor_columns, response = "FTHG") #FTHG ~ sum(predictor_columns) 
away_full_formula <- reformulate(predictor_columns, response = "FTAG")

#null models
home_null <- glm(FTHG ~ 1, data = home_train, family = poisson(link = "log"))
away_null <- glm(FTAG ~ 1, data = away_train, family = poisson(link = "log"))

#full models
home_full <- glm(home_full_formula, data = home_train, family = poisson(link = "log"))
away_full <- glm(away_full_formula, data = away_train, family = poisson(link = "log"))

#forward backward selection using AIC
home_step <- step(home_null, scope = list(lower = ~1, upper = home_full_formula), direction = "both")
away_step <- step(away_null, scope = list(lower = ~1, upper = away_full_formula), direction = "both")

# comparison
AIC(home_null, home_step, home_full)
AIC(away_null, away_step, away_full)

formula(home_step)
formula(away_step)

# Extract variables used in each model
home_features <- names(coef(home_step))[-1]
away_features <- names(coef(away_step))[-1]

# Count unique features across both models
n_features <- length(unique(c(home_features, away_features)))

n_features

par(mfrow = c(2, 2))
plot(home_step)

par(mfrow = c(2, 2))
plot(away_step)

par(mfrow = c(1, 1))

c(Home = sum(residuals(home_step, type = "pearson")^2) /df.residual(home_step),
  Away = sum(residuals(away_step, type = "pearson")^2) /df.residual(away_step))

train_teams<-unique(c(train_data$HomeTeam, train_data$AwayTeam))
length(train_teams)
length(intersect(train_teams, unique(c(test_data$HomeTeam, test_data$AwayTeam))))

length(intersect(train_teams, unique(c(prediction_data$HomeTeam, prediction_data$AwayTeam))))
setdiff(unique(c(prediction_data$HomeTeam, prediction_data$AwayTeam)),train_teams)

text_columns <- names(train_data)[sapply(train_data, is.character)] #names the text columns in the dataset
train_data[text_columns] <- lapply(train_data[text_columns], factor) #applies factor() to the text columns
test_data[text_columns] <- lapply(test_data[text_columns], factor) #applies factor() to the text columns

#predict goals for each match in the test season
test_data$PredHomeGoals <- round(predict(home_step, newdata = test_data, type = "response")) #using home model
test_data$PredAwayGoals <- round(predict(away_step, newdata = test_data, type = "response")) #using away model

ranking <- function(matches, home_goals, away_goals) {
  #home rank
  home_points <-ifelse(home_goals > away_goals, 3, ifelse(home_goals == away_goals, 1, 0))
  home_ranking <- data.frame(Team = matches$HomeTeam, Points = home_points, GF = home_goals, GA = away_goals)
  #away rank
  away_points <- ifelse(away_goals > home_goals,3, ifelse(away_goals == home_goals, 1, 0))
  away_ranking <- data.frame(Team = matches$AwayTeam, Points = away_points, GF = away_goals, GA = home_goals)
  
  all_team_matches <- rbind(home_ranking, away_ranking)
  
  final_ranking <- aggregate(cbind(Points, GF, GA) ~ Team, data = all_team_matches, sum) #cbind(Points, GF, GA) combines those three columns as the response and ~ Team says to group the rows by Team. sum is the function applied to each column within each team’s group
  
  final_ranking$GD <- final_ranking$GF - final_ranking$GA #create new goal difference column for each team 
  
  #ordering
  final_ranking <- final_ranking[order(-final_ranking$Points, -final_ranking$GD), ] #order() uses GD to break ties in Points, so teams with the same points are ordered by highest goal difference first
  final_ranking$Position <- seq_len(nrow(final_ranking)) #adding a column that tells us the position
  
  final_ranking <- final_ranking[c("Position", "Team", "Points", "GD")]
  rownames(final_ranking) <- NULL
  
  final_ranking
}

set.seed(1)
predicted_table <- ranking(test_data, test_data$PredHomeGoals, test_data$PredAwayGoals)
predicted_table

actual_table <- ranking(test_data, test_data$FTHG, test_data$FTAG)

table_comparison <- merge(predicted_table, actual_table, by = "Team", suffixes = c("_Predicted", "_Actual"))
table_comparison$PositionDifference <- table_comparison$Position_Predicted - table_comparison$Position_Actual
table_comparison <- table_comparison[order(table_comparison$Position_Actual),] #order wrt actual position

table_comparison

played_26_27 <- prediction_data[prediction_data$Date <= as.Date("2026-10-19"),] 
unplayed <- prediction_data$Date > as.Date("2026-10-19")

final_train_data <- bind_rows(train_data, test_data, played_26_27) #known data that will get used to train the model

home_final_model <- glm(formula(home_step), data = final_train_data, family = stats::poisson(link = "log")) 
away_final_model <- glm(formula(away_step), data = final_train_data, family = stats::poisson(link = "log"))

#initialise prediction column filled with NAs
prediction_data$PredHomeGoals <- NA_real_ 
prediction_data$PredAwayGoals <- NA_real_

prediction_data$PredHomeGoals[unplayed] <- round(predict(home_final_model, newdata = prediction_data[unplayed, ], type = "response"))
prediction_data$PredAwayGoals[unplayed] <- round(predict(away_final_model, newdata = prediction_data[unplayed, ], type = "response"))

final_home_goals <- ifelse(unplayed,prediction_data$PredHomeGoals,prediction_data$FTHG) 
final_away_goals <- ifelse(unplayed,prediction_data$PredAwayGoals,prediction_data$FTAG)

#builds the prediction table
predicted_2026_27_table <- ranking(prediction_data,final_home_goals,final_away_goals)

predicted_2026_27_table











