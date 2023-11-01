ORL <- function(payoff,ntrials,a_rew,a_pun,K,theta,omega_f,omega_p) {

  # arrays to populate for simulation
  x <- array(NA,c(ntrials))
  X <- array(NA,c(ntrials))

  Ev_update <- array(NA,c(ntrials,4))
  Ev <- array(NA,c(ntrials,4))
    
  signX <- array(NA,c(ntrials))
  Ef_cho <- array(NA,c(ntrials,4))
  Ef_not <- array(NA,c(ntrials,4))
  Ef <- array(NA,c(ntrials,4))
  
  PS <- array(NA,c(ntrials,4))
  
  V <- array(NA,c(ntrials,4))
  
  exp_p <- array(NA,c(ntrials,4))
  p <- array(NA,c(ntrials,4))
  
  # free parameters - turn back on when constructing
  #a_rew <- .3
  #a_pun <- .3
  #K <- 3
  #theta <- 3
  #omega_f <- .7
  #omega_p <- .7
  
  x[1] <- rcat(1,c(.25,.25,.25,.25))
  
  X[1] <- payoff[1, x[1]]
  
  Ev[1,] <- rep(0,4)
  
  Ef[1,] <- rep(0,4)
  
  PS[1,] <- rep(0,4)
  
  for (t in 2:ntrials) {
    
    #this is important mention this as constructing model
    signX[t] <- ifelse(X[t-1]<0,-1,1)
    
    for (d in 1:4) {
      
      # -------- Updating expected values ------------------------
      #### ... ###
                            
      Ev[t,d] <- ifelse(d==x[t-1],Ev_update[t,d],Ev[t-1,d])
      
      # -------- Updating expected frequencies ------------------------
      #update expected frequencies for ALL decks - AS IF THEY WERE ALL CHOSEN
      #### ... ###
      
      #update expected frequencies for ALL decks - AS IF THEY WERE ALL UNCHOSEN. 
      #### ... ###
      
      #copy appropriate values to ef variable
      Ef[t,d] <- ifelse(d==x[t-1],Ef_cho[t,d],Ef_not[t,d])  
      
      #-----------Perseverance----------------------------------
      #ifelse needed to disctiminate chosen and unchosen decks
      #### ... ###
      
      #-----------Valence model------------------------------
      #### ... ###
      
      #----------softmax part 1-------------
      exp_p[t,d] <- exp(theta*V[t,d])
      
    }
    
    #----------softmax part 2-------------
    for (d in 1:4) {
      p[t,d] <- exp_p[t,d]/sum(exp_p[t,])
    }
      
    x[t] <- rcat(1,p[t,])
    
    X[t] <- payoff[t,x[t]]
    
  }
  
  result <- list(x=x,
                 X=X,
                 Ev=Ev,
                 Ef=Ef,
                 PS=PS,
                 V=V)
  
  return(result)
  
  
  #turn back on when building
  #par(mfrow=c(2,2))
  #plot(Ev[,1])
  #plot(Ev[,2])
  #plot(Ev[,3])
  #plot(Ev[,4])
  #plot(x)
  
}
