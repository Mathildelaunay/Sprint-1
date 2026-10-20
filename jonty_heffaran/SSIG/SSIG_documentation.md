# Season Simulator Independent Games

## (SSIG)



The SSIG function allows a common interface for the testing of different models under the assumption of independent games.



Precisely: we assume that a games result is dependent only on the model specified, and the team ratings. The team ratings are constant throughout a season.  



The simulator does not prescribe how game outcomes or goals are generated. This is determined entirely by the supplied game model.



Run the following code in order to call the required helper functions when you are using SSIG:



source("jonty_heffaran/SSIG/SSIG_function.R")
source("jonty_heffaran/SSIG/season_sim_helper_function.R")
source("jonty_heffaran/SSIG/table_functions.R")



##### Inputs

SSIG takes as input a specified model function and a named list I.e: 



team\_ratings = list("team\_1\_name"= list(rating = team\_1\_rating),..., 		             "team\_n\_name"= list(rating = team\_n\_rating))



. Note that we can use this to input attack and defence ratings for each team, or any other ratings, even ratings by player if you fancy, this is determined by the model you want to test.



pois\_game\_model <- function(team\_ratings, home\_team, away\_team){

&#x20; home\_goals <- rpois(1, lambda=team\_ratings\[\[home\_team]])

&#x20; away\_goals <- rpois(1, lambda = team\_ratings\[\[away\_team]])

&#x20; 

&#x20; return(c(home\_goals, away\_goals))

}



. It is imperative that the game model function returns results in form c(home\_goals\_scored, away\_goals\_scored).



##### Outputs

SSIG returns a tibble representing the final league table after simulating a season of games using the specified game model. 

&#x09;

This league table includes columns Team, P, W, D, L, GF, GA, GD, Pts. This is the team name, games played, games won, drawn, lost, goals for, goals against, goal difference, and accumulated points. 

&#x09;

The tibble is sorted by Pts in descending order, ties are broken by GD, and failing that GF, as per premier league regulation. 





