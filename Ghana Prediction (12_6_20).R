set.seed(103031)

## Bayesian calculation if Akufo-Addo clears 50+1 in Round 1 based on 20,000 simulations

Ghana_Outcome <- function(mu, tau, bias_sd, avg, sd){
  sigma = sqrt(sd^2 + bias_sd^2) 
  B = ((sigma^2) / (sigma^2 + tau^2)) 
  posterior_mean = (B*mu + (1-B)*avg) 
  posterior_se = (sqrt( 1 / ((1/sigma^2) + (1/tau^2))))  
  NPP <- replicate(20000, {
    results_beta = (rnorm(1, posterior_mean, posterior_se)) 
    AA = (ifelse(results_beta > 0.5, 1, 0)) 
    mean(AA) 
    
  })
  answer <- ifelse(NPP > 0, 1, 0) 
  answer 
}

## SCENARIO 1: Basic Adjusted Polls

mu <- 0.5214 # Weighted Historical Round 1 NPP Vote, Adjusted for Weighted Incumbency Effect
tau <- 0.0268 # SE of Historical Round 1 NPP Vote
bias_sd <- 0.0390 # historical poll bias against NPP vote SD
avg <- 0.5558 # NPP average position in weighted polls
sd <- 0.0315 # SD of polls

## SCENARIO 2: 2016 NPP Vote, Adjusted Polls

mu2 <- 0.5473 # 2016 NPP Vote, Adjusted for Weighted Incumbency Effect
tau2 <- 0.0268
bias_sd2 <- 0.0390
avg2 <- 0.5558
sd2 <- 0.0315

## SCENARIO 3: Blended 

mu3 <- 0.5368 # 0.40 * Scenario 1 + 0.60 * Scenario 2
tau3 <- 0.0268
bias_sd3 <- 0.0390
avg3 <- 0.5558
sd3 <- 0.0315

## SCENARIO 4: Blended, Adjusted Models

mu4 <- 0.5368
tau4 <- 0.0268 
bias_sd4 <- 0.0418 # historical projection model bias NPP SD
avg4 <- 0.5010 # avg NPP position in projections
sd4 <- 0.0219 # sd of projection models

# SCENARIO 5: Blended, Blended Polls and Models

mu5 <- 0.5368
tau5 <- 0.0268
bias_sd5 <- 0.03956 # blended bias NPP SD
avg5 <- 0.5448 # 0.80 * Adjusted Polls + 0.20 * Adjusted Models
sd5 <- 0.0296 # sd of polls and models

## SCENARIO M: Blended, Blended Polls and Models (Mahama R1 win)

muM <- 0.45108 # Weighted Historical Round 1 NDC Vote, Adjusted for Weighted Incumbency Effect
tauM <- 0.0278 # SE of Historical Round 1 NDC Vote
bias_sdM <- 0.03956 # historical poll bias against NDC vote SD
avgM <- 0.3993 # NDC average position in weighted polls
sdM <- 0.0637 # SD of polls

## Run Simulations

Ghana <- Ghana_Outcome(mu, tau, bias_sd, avg, sd)
Ghana2 <- Ghana_Outcome(mu2, tau2, bias_sd2, avg2, sd2)
Ghana3 <- Ghana_Outcome(mu3, tau3, bias_sd3, avg3, sd3)
Ghana4 <- Ghana_Outcome(mu4, tau4, bias_sd4, avg4, sd4)
Ghana5 <- Ghana_Outcome(mu5, tau5, bias_sd5, avg5, sd5)
GhanaM <- Ghana_Outcome(muM, tauM, bias_sdM, avgM, sdM)

## Sort victory simulations

b <- c(1:20000) 
Ghana <- data.frame(b, Ghana)
Ghana2 <- data.frame(b, Ghana2)
Ghana3 <- data.frame(b, Ghana3)
Ghana4 <- data.frame(b, Ghana4)
Ghana5 <- data.frame(b, Ghana5)
GhanaM <- data.frame(b, GhanaM)

AA_Ghana <- sum(Ghana$Ghana > 0)
AA_Ghana2 <- sum(Ghana2$Ghana2 > 0)
AA_Ghana3 <- sum(Ghana3$Ghana3 > 0)
AA_Ghana4 <- sum(Ghana4$Ghana4 > 0)
AA_Ghana5 <- sum(Ghana5$Ghana5 > 0)
AA_GhanaM <- sum(GhanaM$GhanaM > 0)

## calculate win percent

win_percent <- (AA_Ghana / 20000)
win_percent2 <- (AA_Ghana2 / 20000)
win_percent3 <- (AA_Ghana3 / 20000)
win_percent4 <- (AA_Ghana4 / 20000)
win_percent5 <- (AA_Ghana5 / 20000)
win_percentM <- (AA_GhanaM / 20000)

win_percent
win_percent2
win_percent3
win_percent4
win_percent5

round1_win <- .2*win_percent + 0.1*win_percent2 + .2*win_percent3 + .15*win_percent4 + .35*win_percent5

round1_win
win_percentM