########################################
########## DATA SET CREATION ###########
########################################


# Df creation from annas github
library(dplyr)

#choose columns to include
columns_to_include <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG")

# links to data of interest (note we had to adjust date format in 1617)
epl_1415 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/14_15.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1516 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/15_16.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1617 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/16_17.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%  
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
epl_1718 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/17_18.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1819 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/18_19.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1920 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/19_20.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2021 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/20_21.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2122 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/21_22.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2223 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/22_23.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2324 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/23_24.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2425 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/24_25.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)

# Bind into one df
epl_all <- bind_rows(
  epl_1415 = epl_1415,
  epl_1516 = epl_1516,
  epl_1617 = epl_1617,
  epl_1718 = epl_1718,
  epl_1819 = epl_1819,
  epl_1920 = epl_1920,
  epl_2021 = epl_2021,
  epl_2122 = epl_2122,
  epl_2223 = epl_2223,
  epl_2324 = epl_2324,
  epl_2425 = epl_2425,
  .id = "Season")

#remove individual dfs
remove(epl_1415, epl_1516, epl_1617, epl_1718, 
       epl_1819,epl_1920, epl_2021, epl_2122, 
       epl_2223, epl_2324, epl_2425)

#check we no missing data
colSums(is.na(epl_all))

#the dates are just strings or something, sorting by date is not working so 
#we fix that
epl_all <- epl_all %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  arrange(Date)

#########################################
####### END OF DATA SET CREATION ########
#########################################

#########################################
######## FITTING A POISSON MODEL ########
#########################################

#let us make a df of the 2223 season to practise fitting model on
epl_2223 <-  epl_all %>%
  filter(Season == "epl_2223")

# we need to have data showing a teams performance in a game

home_data <- epl_2223 %>%
  transmute(Date, Team = HomeTeam, Opponent = AwayTeam, Goals = FTHG, Home = 1)

away_data <- epl_2223 %>%
  transmute(Date, Team = AwayTeam, Opponent = HomeTeam, Goals = FTAG, Home = 0)

goal_data_2223 <- bind_rows(home_data, away_data)

# set the factors for the model
goal_data_2223 <- goal_data_2223 %>%
  mutate(Team = factor(Team),Opponent = factor(Opponent))

# tidy up
remove(home_data, away_data)

# first we will suppose the goals are poisson with rate lambdaH / lamdaA
goal_model = glm(Goals ~ Home + Team + Opponent,
                 family = poisson(link = "log"),
                 data = goal_data_2223)

summary(goal_model)

# now to create team ratings
coefficients <- coef(goal_model)


baseline <- unname(coefficients["(Intercept)"]) #extracting these for fture xg calcs
home_advantage <- unname(coefficients["Home"])


team_names <- levels(goal_data_2223$Team)

# We extract the attack coefficients, note that since arsenal is reference we 
# have to manually add. The represent scoring rate relative to arsenals
# ARSENAL IS REF BECAUSE FIRST ALPHABETICALLY, CHECK IN FUTURE SEASONS THIS IS STILL
# THE CASE

attack_coefficients <- coefficients[paste0("Team", team_names)]
attack_coefficients[is.na(attack_coefficients)] <- 0
names(attack_coefficients)[is.na(names(attack_coefficients))] <- "TeamArsenal"

defence_coefficients <- coefficients[paste0("Opponent", team_names)]
defence_coefficients[is.na(defence_coefficients)] <- 0
names(defence_coefficients)[is.na(names(defence_coefficients))] <- "OpponentArsenal"

attack_ratings <- exp(attack_coefficients)
defence_ratings <- exp(defence_coefficients)

# put into a nice df
team_ratings_df <- data.frame(team_names, att_rating = attack_ratings, def_rating = defence_ratings)
rownames(team_ratings_df) <- NULL

# now normalise so as to make comparable with prem ratings after scaling
team_ratings_df <- team_ratings_df %>%
  mutate(
    att_rating_norm = att_rating / exp(mean(log(att_rating))),
    def_rating_norm = def_rating / exp(mean(log(def_rating)))
  )


remove(attack_coefficients, defence_coefficients, coefficients, attack_ratings,
       defence_ratings)


#########################################
###### END FITTING A POISSON MODEL ######
#########################################

################################################
############ INITIAL SIMULATION TIME ###########
################################################


# we have a df which has team name followed by their attack and defence rating 
# relative to arsenal (the benchmark)
# also recall we maximised the likelihood of poisson model, not the dixon coles 
# so will have to think about custom way to do that later on.

# starting with a single run through creating the expected goals given 
# the home and away team

home_team <- 'Brighton'  
away_team <- 'Crystal Palace'

home_attack <- team_ratings_df$att_rating[team_ratings_df$team_names == home_team]
home_defence <- team_ratings_df$def_rating[team_ratings_df$team_names == home_team]
away_attack <- team_ratings_df$att_rating[team_ratings_df$team_names == away_team]
away_defence <- team_ratings_df$def_rating[team_ratings_df$team_names == away_team]


expected_home_goals <- exp(baseline + home_advantage) * home_attack * away_defence
expected_away_goals <- exp(baseline) * away_attack * home_defence

#these rates are now ready to be passed into our game model to simulate games

# sourcing dixon-coles game model
source("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/dixon_coles_game_model.R")

# check it works
scoreline = dixon_coles_game_model(team_ratings = team_ratings_df, 
                                   home_team = home_team,
                                   away_team = away_team, 
                                   rho =0.05 , 
                                   home_advantage =home_advantage ,
                                   baseline = baseline , max_goals = 10)

print(c(scoreline, exp_home_gls = expected_home_goals, 
        exp_away_gls =expected_away_goals))

#looks good

# Time to try simulate a season.
# We will simulate the 2223 season, using the ratings from that season. 
# Obviously we expect good results from this sim since we are using info from
# the season we are simulating

source("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/TB1/Tool box/Sprint_1 individual/(SSIG) Season Simulator Independent Games/SSIG_function.R")
source("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/TB1/Tool box/Sprint_1 individual/(SSIG) Season Simulator Independent Games/season_sim_helper_function.R")
source("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/TB1/Tool box/Sprint_1 individual/(SSIG) Season Simulator Independent Games/table_functions.R")

# As we have done before, we will use a wrapping function to get around the SSIG 
# sillyness

dc_game_model <- function(team_ratings, home_team, away_team) {
  
  dixon_coles_game_model(
    team_ratings = team_ratings,
    home_team = home_team,
    away_team = away_team,
    rho = 0.05,
    home_advantage = home_advantage,
    baseline = baseline,
    max_goals = 10
  )
}

predicted_league_table = SSIG(team_ratings= team_ratings_df, game_model= dc_game_model)
print(predicted_league_table)  

# We now perform a quick sanity check and compare to the realised results table
# using spearmans rank correlation coeff

realised_table_2223 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/22_23_table") %>%
  select(Rk, Squad, MP, W, D, L, GF, GA, GD, Pts)

# Have to standardise the team names 
realised_table_2223 <- realised_table_2223 %>%
  mutate(
    Team = recode(Squad,
      "Manchester City" = "Man City",
      "Manchester Utd" = "Man United",
      "Nottingham" = "Nott'm Forest",
      "Leicester City" = "Leicester",
      "Leeds United" = "Leeds")) %>%
  select(Rk, Team, MP, W, D, L, GF, GA, GD, Pts)

# Now we make a comparison table
comparison_table_2223 <- realised_table_2223 %>%
  select(Team, actual_Rk = Rk, actual_MP = MP, actual_W = W, actual_D = D,
         actual_L = L, actual_GF = GF, actual_GA =GA, actual_GD = GD, 
         actual_Pts = Pts) %>% inner_join(predicted_league_table %>%
         select(Team, predicted_Rk = Rank, predicted_MP = P, predicted_W = W, predicted_D = D,
                 predicted_L = L, predicted_GF = GF, predicted_GA =GA, predicted_GD = GD, 
                 predicted_Pts = Pts),
         by = "Team" )

head(comparison_table_2223)                                            
                                            
remove(predicted_league_table, goal_data_2223, goal_model,
       epl_all)

# And we can do a correlation test
cor.test(comparison_table_2223$actual_Rk, comparison_table_2223$predicted_Rk, 
         method = "spearman")

####################################################
############ END INITIAL SIMULATION TIME ###########
####################################################

####################################################
########### FINISHING RANK EMPIRICAL DIST ##########
####################################################
# okay sick so a single season prediction under the model correlates pretty well
# with the season from which the training data was produced. GOOD SIGNS. Let us
# now simulate a few seasons, get an empirical distribution of finishing positions
# produce an expected league table from this and then compare ranks

#Simulate n seasons

n_seasons <- 250

rank_results <- vector("list", length = n_seasons)

system.time(
for (s in seq(n_seasons)) {
  season_table <- SSIG(team_ratings = team_ratings_df,
    game_model = dc_game_model)
  
  rank_results[[s]] <- season_table %>%
    transmute(Simulation = s, Team, Rank, Pts, GD, GF)
}
)

all_ranks = bind_rows(rank_results)

predicted_position_distribution <- all_ranks %>%
  count(Team, Rank, name = "Count") %>%
  complete(Team, Rank= 1:20, fill = list(Count = 0)) %>%
  group_by(Team) %>%
  mutate(Probability = Count / n_seasons) %>%
  ungroup()

remove(rank_results)
# lets plot these distributions
library(ggplot2)

team_to_plot = "Liverpool"
actual_pos = comparison_table_2223 %>%
  filter(Team == team_to_plot) %>%
  pull(actual_Rk)
predicted_position_distribution %>%
  filter(Team == team_to_plot) %>%
  ggplot(aes(x = Rank, y = Probability)) +
  geom_col() +
  geom_vline(xintercept = actual_pos, colour = "red")+
  scale_x_continuous(breaks = 1:20) +
  labs( title = paste("Distribution of finishing positions:", team_to_plot),
        x = "Finishing position", y = "Probability",
        subtitle= "Red dashed line = actual position") +
  theme_classic()

# We now want to produce a final league table; we must choose how to use our 
# simulated data to do this. Initially, i will just use mean pts, gd, gf over 
# the simulated seasons

mean_league_table <- all_ranks %>%
  group_by(Team) %>%
  summarise(
    Mean_Pts = mean(Pts),
    Mean_GD  = mean(GD),
    Mean_GF  = mean(GF),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Pts), desc(Mean_GD), desc(Mean_GF)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Team, Mean_Pts, Mean_GD, Mean_GF)

mean_league_table

# same procedure to create comparison table
comparison_table_2223 <- realised_table_2223 %>%
  select(Team, actual_Rk = Rk, actual_GF = GF, actual_GD = GD, actual_Pts = Pts) %>% 
  inner_join(mean_league_table %>%
  select(Team, predicted_Rk = Rank, predicted_GF = Mean_GF, predicted_GD = Mean_GD, 
         predicted_Pts = Mean_Pts),
         by = "Team" )

cor.test(comparison_table_2223$actual_Rk, comparison_table_2223$predicted_Rk,
         method = "spearman")

comparison_table_2223 %>%
  summarise(Mean_Absolute_Rank_Error = mean(abs(actual_Rk - predicted_Rk)))

######################################################
########## END FINISHING RANK EMPIRICAL DIST #########
######################################################

############################################
########## WEIGHTED MODEL CREATION #########
############################################

# Okie dokie, this is all good. Now lets see how we can predict next seasons 
# data. First problem: the newly promoted teams will be lacking info
# further, we are yet to incorporate info from multiple seasons or any degree of 
# time weighting.



#let us again build complete df
# links to data of interest (note we had to adjust date format in 1617)
epl_1415 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/14_15.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1516 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/15_16.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1617 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/16_17.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%  
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
epl_1718 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/17_18.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1819 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/18_19.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_1920 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/19_20.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2021 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/20_21.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2122 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/21_22.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2223 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/22_23.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2324 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/23_24.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)
epl_2425 <- read.csv('https://raw.githubusercontent.com/Mathildelaunay/Sprint-1/refs/heads/main/Anna%20Del%20Pino/Premier%20League%20match%20events%20data/24_25.csv')%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)

# Bind into one df
epl_all <- bind_rows(
  epl_1415 = epl_1415,
  epl_1516 = epl_1516,
  epl_1617 = epl_1617,
  epl_1718 = epl_1718,
  epl_1819 = epl_1819,
  epl_1920 = epl_1920,
  epl_2021 = epl_2021,
  epl_2122 = epl_2122,
  epl_2223 = epl_2223,
  epl_2324 = epl_2324,
  epl_2425 = epl_2425,
  .id = "Season")

#remove individual dfs
remove(epl_1415, epl_1516, epl_1617, epl_1718, 
       epl_1819,epl_1920, epl_2021, epl_2122, 
       epl_2223, epl_2324, epl_2425)

# Have to format date properly for epl_all
epl_all <- epl_all %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

####################################
known_date <- as.Date("2020-01-19")
####################################

# we will consider the weighting function

weight <- function(game_age, cutoff, power) {
  ifelse(game_age >= cutoff, 0, 1 - (game_age / cutoff)^power)}
# game_age is num between game we are observing and the most recent game
# cutoff is some value, before which do not observe any data.
# power just affects shape, higher power gives sharper cutoff

epl_known <- epl_all %>%
  filter(Date <= known_date)

# We now need to calculate the time from a game to known date
epl_known <- epl_known %>%
  mutate(days_ago = as.numeric(known_date - Date))

# Now lets add the weighting column
epl_known <- epl_known %>%
  mutate(weight = weight(game_age =days_ago, cutoff = 730, power=2))

# we now use the weightings in the creation of team ratings
home_data <- epl_known %>%
  transmute(Date, Team = HomeTeam, Opponent = AwayTeam, Goals = FTHG, Home = 1,
            obvs_weight = weight)

away_data <- epl_known %>%
  transmute(Date, Team = AwayTeam, Opponent = HomeTeam, Goals = FTAG, Home = 0,
            obvs_weight = weight)

goal_data_known <- bind_rows(home_data, away_data)

remove(home_data, away_data)

goal_model <- glm(
  Goals ~ Home + Team + Opponent,
  family = poisson(link = "log"),
  data = goal_data_known,
  weights = obvs_weight
)

summary(goal_model)

# Okay this gives us sufficient model with time weighting. Now we have to 
# consider how to deal with newly promoted teams.
# Idea: Use championship performance to get ratings. Upon promotion, we multiply
# these by a scalar ie a team which has scored lots in champ will score fewer in
# prem and concede more

##########################################
########## PROMOTION ADJUSTMENT ##########
##########################################

# collect championship data
ech_1415 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c14_15.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)%>%
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1415)
ech_1516 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c15_16.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1516)
ech_1617 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c16_17.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1617)
ech_1718 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c17_18.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%
  mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1718)
ech_1819 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c18_19.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) #%>%
  #mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1819)
ech_1920 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c19_20.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) #%>%
  #mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_1920)
ech_2021 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c20_21.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) #%>%
  #mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_2021)
ech_2122 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c21_22.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) #%>%
  #mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_2122)
ech_2223 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c22_23.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) #%>%
  #mutate(Date = format(as.Date(Date, format = "%d/%m/%y"), "%d/%m/%Y"))
#head(ech_2223)
ech_2324 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c23_24.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) 
#head(ech_2324)
ech_2425 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c24_25.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) 
#head(ech_2425)
ech_2526 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c25_26.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) 
#head(ech_2526)
ech_2627 <- read.csv("C:/Users/jonty/OneDrive - University of Bristol/Year 4 2026-2027/Toolbox/Sprint-1/jonty_heffaran/data/championship_data/game_data/c26_27.csv")%>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG) 
#head(ech_2627)

# Bind into one df
ech_all <- bind_rows(
  ech_1415 = ech_1415,
  ech_1516 = ech_1516,
  ech_1617 = ech_1617,
  ech_1718 = ech_1718,
  ech_1819 = ech_1819,
  ech_1920 = ech_1920,
  ech_2021 = ech_2021,
  ech_2122 = ech_2122,
  ech_2223 = ech_2223,
  ech_2324 = ech_2324,
  ech_2425 = ech_2425,
  ech_2526 = ech_2526,
  ech_2627 = ech_2627,
  .id = "Season")

# remove individual dfs
remove(ech_1415, ech_1516, ech_1617, ech_1718, 
       ech_1819,ech_1920, ech_2021, ech_2122, 
       ech_2223, ech_2324, ech_2425, ech_2526, ech_2627)

# check we no missing data
colSums(is.na(ech_all))

# was one ghost admission so just gonna remove it
ech_all <- ech_all %>%
  filter(!is.na(FTHG), !is.na(FTAG))

# now we will attempt fit same model as we did for prem teams
# Have to format date properly for ech_all
ech_all <- ech_all %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

####################################
known_date <- as.Date("2020-01-19")
cut_off <- 730
####################################

ech_known <- ech_all %>%
  filter(Date <= known_date)

# We now need to calculate the time from a game to known date and remove any
# games which will have zero weighting
ech_known <- ech_known %>%
  mutate(days_ago = as.numeric(known_date - Date))%>%
  filter(days_ago < cut_off)

# Now lets add the weighting column
weight <- function(game_age, cutoff, power) {
  ifelse(game_age >= cutoff, 0, 1 - (game_age / cutoff)^power)}

ech_known <- ech_known %>%
  mutate(weight = weight(game_age =days_ago, cutoff = cut_off, power=2))

# we now use the weightings in the creation of team ratings
home_data <- ech_known %>%
  transmute(Date, Team = HomeTeam, Opponent = AwayTeam, Goals = FTHG, Home = 1,
            obvs_weight = obvs_weight)

away_data <- ech_known %>%
  transmute(Date, Team = AwayTeam, Opponent = HomeTeam, Goals = FTAG, Home = 0,
            obvs_weight = obvs_weight)

ech_goal_data_known <- bind_rows(home_data, away_data)

remove(home_data, away_data)

ech_goal_model <- glm(
  Goals ~ Home + Team + Opponent,
  family = poisson(link = "log"),
  data = ech_goal_data_known,
  weights = obvs_weight
)

summary(ech_goal_model)

# now to create team ratings
coefficients <- coef(ech_goal_model)


baseline <- unname(coefficients["(Intercept)"]) #extracting these for fture xg calcs
home_advantage <- unname(coefficients["Home"])


team_names <- sort(unique(ech_goal_data_known$Team))

attack_coefficients <- coefficients[paste0("Team", team_names)]
attack_coefficients[is.na(attack_coefficients)] <- 0
names(attack_coefficients)[is.na(names(attack_coefficients))] <- "TeamAston Villa"

defence_coefficients <- coefficients[paste0("Opponent", team_names)]
defence_coefficients[is.na(defence_coefficients)] <- 0
names(defence_coefficients)[is.na(names(defence_coefficients))] <- "OpponentAston Villa"

attack_ratings <- exp(attack_coefficients)
defence_ratings <- exp(defence_coefficients)

# put into a nice df
ech_team_ratings_df <- data.frame(team_names, att_rating = attack_ratings, def_rating = defence_ratings)
rownames(team_ratings_df) <- NULL

# now normalise so as to make comparable with prem ratings after scaling
ech_team_ratings_df <- ech_team_ratings_df %>%
  mutate(
    att_rating_norm = att_rating / exp(mean(log(att_rating))),
    def_rating_norm = def_rating / exp(mean(log(def_rating)))
  )

remove(attack_coefficients, defence_coefficients, coefficients, attack_ratings,
       defence_ratings)

################################################################################
# GONNA END THIS NOTEBOOK FOR NOW AND START WORKING ON A TIDY RMD FILE         #
################################################################################