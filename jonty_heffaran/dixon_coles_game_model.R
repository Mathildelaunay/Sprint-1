"""DIXON - COLES  GAME MODEL: # nolint
   We define game model, as proposed in M.J. Dixon, and S.J. Coles' 1996 paper.
   This paper is a cornerstone of modern football scoreline prediction. It builds # nolint
   upon work by Maher (1982), introducing a correction to the poisson model to
   address the prevelance of common low scorelines.
   
   This game model takes as input:
   (team_ratings, home_team, away_team, home_advantage, rho, max_goals)
   
   The model returns:
   c(home_goals = scores$home_goals[i], away_goals = scores$away_goals[i])
   
   Note that team ratings must be supplied as a name list of the form shown
   in the example below:
   
   team_ratings <- list(
    Arsenal = list(attack_rating = .8, def_rating =1 ),
    Chelsea = list(attack_rating = 1.1, def_rating =.2 ),
    ...,
   )
    
   Here, a high attack rating indicates a strong attack, and a low defence rating
   indicates a strong defence.
   
   This model is to be used in combination with the SSIG function as previously 
   defined.
"""

dixon_coles_game_model <- function(team_ratings, home_team, away_team,
home_advantage, rho, max_goals = 5) {

  ###--- extract the team ratings ---###
  alpha_i <- team_ratings[[home_team]]$attack_rating
  beta_i  <- team_ratings[[home_team]]$def_rating
  alpha_j <- team_ratings[[away_team]]$attack_rating
  beta_j  <- team_ratings[[away_team]]$def_rating

  ###--- lambda and mu rates for goals scored by home and away teams ---###
  lambda <- alpha_i * beta_j * (1+ home_advantage)
  mu     <- alpha_j * beta_i
  
  ###--- grid of possible scorelines as upto max allowed by param ---###
  scores <- expand.grid(home_goals= 0:max_goals, away_goals= 0:max_goals)
  
  x <- scores$home_goals
  y <- scores$away_goals
  
  ###--- prob of scoreline under independent poisson ---### 
  prob <- dpois(x, lambda) * dpois(y, mu)
  
  ###--- calc dixon-coles correction ---###
  tau <- rep(1, nrow(scores))
  
  tau[x == 0 & y == 0] <- 1 - lambda * mu * rho
  tau[x == 0 & y == 1] <- 1 + lambda * rho
  tau[x == 1 & y == 0] <- 1 + mu * rho
  tau[x == 1 & y == 1] <- 1 - rho
  
  ###--- apply correction ---###
  prob <- prob * tau
  
  ###--- normalise probabilities over the scorelines ---###
  prob <- prob / sum(prob)
  
  ###--- take sample from the adjusted probs ---###
  i <- sample.int(nrow(scores), size = 1, prob = prob)
  
  ###--- return sim scoreline ---###
  return(c(home_goals = scores$home_goals[i], 
  away_goals = scores$away_goals[i]))
}
