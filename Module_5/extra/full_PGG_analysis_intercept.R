seed_id = 1982
set.seed(seed_id)

install.packages("pacman")
pacman::p_load(R2jags, parallel, polspline, ggplot2, glue)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


##### Preprocessing #####

groupSize <- 4
ntrials <- 10
pi <- 1.6 # used to be 1.4, but the original paper (and Josh' preprint both say 1.6)
ntokens <- 20
vals <- seq(0,ntokens,1)
#vals <- seq(1,21,1) #possible values to contribute - from 0 to 20 tokens

rawDat <- read.csv("data/HerrmannThoeniGaechterDATA.csv", skip = 3) # Public goods game

#- create covariates in raw data matrix
# nation index
rawDat$nation <- c()
rawDat$nation[rawDat$city=="Melbourne"]=1
rawDat$nation[rawDat$city=="Minsk"]=2
rawDat$nation[rawDat$city=="Chengdu"]=3
rawDat$nation[rawDat$city=="Copenhagen"]=4
rawDat$nation[rawDat$city=="Bonn"]=5
rawDat$nation[rawDat$city=="Athens"]=6
rawDat$nation[rawDat$city=="Seoul"]=7
rawDat$nation[rawDat$city=="Samara"]=8
rawDat$nation[rawDat$city=="Zurich"]=9
rawDat$nation[rawDat$city=="St. Gallen"]=9
rawDat$nation[rawDat$city=="Istanbul"]=10
rawDat$nation[rawDat$city=="Nottingham"]=11
rawDat$nation[rawDat$city=="Dnipropetrovs'k"]=12
rawDat$nation[rawDat$city=="Boston"]=13

# create variable for GINI. Old data from 
# http://hdr.undp.org/sites/default/files/reports/269/hdr_2009_en_complete.pdf,
# see the suppl material in the published paper for the updated values:
# https://www.sciencedirect.com/science/article/pii/S2666622723000254#sec0014

# # Old pre-published Gini-values
# rawDat$gini <- c()
# rawDat$gini[rawDat$city=="Melbourne"]=34.3
# rawDat$gini[rawDat$city=="Minsk"]=25.3
# rawDat$gini[rawDat$city=="Chengdu"]=38.5
# rawDat$gini[rawDat$city=="Copenhagen"]=28.7
# rawDat$gini[rawDat$city=="Bonn"]=31.9
# rawDat$gini[rawDat$city=="Athens"]=34.4
# rawDat$gini[rawDat$city=="Seoul"]=31.6
# rawDat$gini[rawDat$city=="Samara"]=37.5
# rawDat$gini[rawDat$city=="Zurich"]=32.7
# rawDat$gini[rawDat$city=="St. Gallen"]=32.7
# rawDat$gini[rawDat$city=="Istanbul"]=41.9
# rawDat$gini[rawDat$city=="Nottingham"]=34.8
# rawDat$gini[rawDat$city=="Dnipropetrovs'k"]=26.1
# rawDat$gini[rawDat$city=="Boston"]=41.1

# Gini-values taken directly from the supplementary materials in the published paper
rawDat$gini <- c()
rawDat$gini[rawDat$city=="Melbourne"]=33.3
rawDat$gini[rawDat$city=="Minsk"]=28.3
rawDat$gini[rawDat$city=="Chengdu"]=41.5
rawDat$gini[rawDat$city=="Copenhagen"]=25.4
rawDat$gini[rawDat$city=="Bonn"]=30.7
rawDat$gini[rawDat$city=="Athens"]=34
rawDat$gini[rawDat$city=="Seoul"]=31.7
rawDat$gini[rawDat$city=="Samara"]=38
rawDat$gini[rawDat$city=="Zurich"]=31.7
rawDat$gini[rawDat$city=="St. Gallen"]=31.7
rawDat$gini[rawDat$city=="Istanbul"]=41.4
rawDat$gini[rawDat$city=="Nottingham"]=35.2
rawDat$gini[rawDat$city=="Dnipropetrovs'k"]=29.1
rawDat$gini[rawDat$city=="Boston"]=40.8

# Variable for individualism. Data from
# https://www.hofstede-insights.com/product/compare-countries/
# Reference: Hofstede, Hofstede, & Minkov, 2010
# Used instead of data from paper - includes all nations, and closer in time
rawDat$ind <- c()
rawDat$ind[rawDat$city=="Melbourne"]=90
rawDat$ind[rawDat$city=="Minsk"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Chengdu"]=20
rawDat$ind[rawDat$city=="Copenhagen"]=74
rawDat$ind[rawDat$city=="Bonn"]=67
rawDat$ind[rawDat$city=="Athens"]=35
rawDat$ind[rawDat$city=="Seoul"]=18
rawDat$ind[rawDat$city=="Samara"]=25 #updated in 2010 edition - paper says 39
rawDat$ind[rawDat$city=="Zurich"]=68
rawDat$ind[rawDat$city=="St. Gallen"]=68
rawDat$ind[rawDat$city=="Istanbul"]=37
rawDat$ind[rawDat$city=="Nottingham"]=89
rawDat$ind[rawDat$city=="Dnipropetrovs'k"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Boston"]=91

# Variable for per capita GDP. Included data from paper. In thousands
# 2000 to 2009 data from IMF estimates included in comments)
# https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)#IMF_estimates_from_2000_to_2009
rawDat$gdp <- c()
rawDat$gdp[rawDat$city=="Melbourne"]=32.9 #40822
rawDat$gdp[rawDat$city=="Minsk"]=8.9 #14051
rawDat$gdp[rawDat$city=="Chengdu"]=7.6 #8215
rawDat$gdp[rawDat$city=="Copenhagen"]=36.5 #42096
rawDat$gdp[rawDat$city=="Bonn"]=31.1 #37949
rawDat$gdp[rawDat$city=="Athens"]=26.0 #29312
rawDat$gdp[rawDat$city=="Seoul"]=23.9 #28820
rawDat$gdp[rawDat$city=="Samara"]=12.1 #20123
rawDat$gdp[rawDat$city=="Zurich"]=37.4 #54812
rawDat$gdp[rawDat$city=="St. Gallen"]=37.4 #54812
rawDat$gdp[rawDat$city=="Istanbul"]=9.1 #16093
rawDat$gdp[rawDat$city=="Nottingham"]=35.1 #35372
rawDat$gdp[rawDat$city=="Dnipropetrovs'k"]=7.6 #7845
rawDat$gdp[rawDat$city=="Boston"]=43.4 #47008 

# Variable for trust/social cohesion. Data from paper
# Also available from world value and european values surveys (commented out)
# https://ourworldindata.org/trust?utm_source=Our+World+in+Data+%E2%80%93+Newsletter&utm_campaign=ab0d6124e2-Newsletter_OurWorldInData_No4_Trust&utm_medium=email&utm_term=0_2e166c1fc1-ab0d6124e2-284219045
# values from 2009 - also csv saved in data folder. Source is wave five survey 
# Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014. 
# World Values Survey: Round Five - Country-Pooled Datafile Version:
# Values for Denmark and Greece from Halman, L. 2001. The European Values Study: A Third Wave. Tilburg: EVS, WORC, Tilburg
# University. Quoted here: https://www.oecd.org/innovation/research/2381883.pdf
# Data from the EVS and WVS are stable and comparable for countries that are included on both
# https://www.researchgate.net/publication/287200653_Trust_in_institutions_and_human_values_in_the_European_context_A_comparison_between_the_World_Value_Survey_and_the_European_Social_Survey
rawDat$trust <- c()
rawDat$trust[rawDat$city=="Melbourne"]=.40 #.48 #2009
rawDat$trust[rawDat$city=="Minsk"]= .42 #.41 #2008-2010 EVS - european values survey (only 2014 value of .33 available for WVS)
rawDat$trust[rawDat$city=="Chengdu"]= .55 #.49 #2009
rawDat$trust[rawDat$city=="Copenhagen"]= .67 #.75 #2008-2010 EVS - european values survey
rawDat$trust[rawDat$city=="Bonn"]=.38 #.32 #2009
rawDat$trust[rawDat$city=="Athens"]=.24 #.19 #2008-2010 #EVS - european values survey
rawDat$trust[rawDat$city=="Seoul"]=.27 #.30 #2009
rawDat$trust[rawDat$city=="Samara"]=.24 #.25 #2009
rawDat$trust[rawDat$city=="Zurich"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="St. Gallen"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="Istanbul"]=.16 #.05 #2009
rawDat$trust[rawDat$city=="Nottingham"]= .29 #.30 #2009
rawDat$trust[rawDat$city=="Dnipropetrovs'k"]=.27 #.27 #2009
rawDat$trust[rawDat$city=="Boston"]=.36 #.40 #2009

# extract every third line - data file has lines representing others responses and we don't need that
redDat <- rawDat[seq(1,length(rawDat$sessionid),3),]

group_names <- unique(redDat$groupid)
ngroups <- length(group_names)

# THIS WILL REMOVE SUBJECTS WITH MISSING DATA IN NO PUNISHMENT CONDITION
ngroups <- 269

subject_names <- unique(redDat$subjectid)
nsubjects <- length(subject_names)

# data for no punishment condition #
c_no_punish <- array(0,c(groupSize,ntrials,ngroups)) # choices
Gga_no_punish <- array(0,c(ntrials,ngroups)) # group-averaged contribution (only 1 entry per group - hence the G + ga)
Ggas_no_punish <- array(0,c(groupSize,ntrials,ngroups)) # same as Gga, but specified for each subject (cuz that's how the JAGS-code wants it) - hence the s
Gc_no_punish <- array(0,c(groupSize,ntrials,ngroups)) # summed ("cumulated" hence the c) group contribution not including oneself (therefore specified for each subject) - we don't use this - this refers to the sum-command in the loop-structure
Ga_no_punish <- array(0,c(groupSize,ntrials,ngroups)) # group-averaged contribution without oneself - we don't use this cuz the participants don't see this (hence, they have to do quite a bit of mental arithmetics to represent this), and thus we're modeling their conditional preference relative to the averaged group contribution (including their own)

missing <- array(0,ngroups)

for (g in 1:ngroups) {
  c_no_punish[,,g] <- rbind(redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][1:10],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][11:20],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][21:30],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][31:40])
  
  Gga_no_punish[,g] <- colMeans(c_no_punish[,,g])
  
  missing[g] <- is.na(c_no_punish[1,1,g])
  
  for (s in 1:groupSize) {
    Gc_no_punish[s,,g] <- colSums(c_no_punish[-s,,g])
    Ga_no_punish[s,,g] <- colMeans(c_no_punish[-s,,g])
    Ggas_no_punish[s,,g] <- colMeans(c_no_punish[,,g])
  }
}

# data for punishment condition #
c_punish <- array(0,c(groupSize,ntrials,ngroups)) # choices
Gga_punish <- array(0,c(ntrials,ngroups)) # group-averaged contribution (only 1 entry per group - hence the G + ga)
Ggas_punish <- array(0,c(groupSize,ntrials,ngroups)) # same as Gga, but specified for each subject (cuz that's how the JAGS-code wants it) - hence the s
Gc_punish <- array(0,c(groupSize,ntrials,ngroups)) # summed ("cumulated" hence the c) group contribution not including oneself (therefore specified for each subject) - we don't use this - this refers to the sum-command in the loop-structure
Ga_punish <- array(0,c(groupSize,ntrials,ngroups)) # group-averaged contribution without oneself - we don't use this cuz the participants don't see this (hence, they have to do quite a bit of mental arithmetics to represent this), and thus we're modeling their conditional preference relative to the averaged group contribution (including their own)

for (g in 1:ngroups) {
  c_punish[,,g] <- rbind(redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][1:10],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][11:20],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][21:30],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][31:40])
  
  Gga_punish[,g] <- colMeans(c_punish[,,g])
  
  for (s in 1:groupSize) {
    Gc_punish[s,,g] <- colSums(c_punish[-s,,g])
    Ga_punish[s,,g] <- colMeans(c_punish[-s,,g])
    Ggas_punish[s,,g] <- colMeans(c_no_punish[,,g])
  }
}

# compile data from each condition into 4D matrix
c <- array(0,c(groupSize,ntrials,ngroups,2))
c[,,,1] <- c_no_punish
c[,,,2] <- c_punish

Gga <- array(0,c(ntrials,ngroups,2))
Gga[,,1] <- Gga_no_punish
Gga[,,2] <- Gga_punish

Ggas <- array(0,c(groupSize,ntrials,ngroups,2))
Ggas[,,,1] <- Ggas_no_punish
Ggas[,,,2] <- Ggas_punish


Gc <- array(0,c(groupSize,ntrials,ngroups,2))
Gc[,,,1] <- Gc_no_punish
Gc[,,,2] <- Gc_punish

Ga <- array(0,c(groupSize,ntrials,ngroups,2))
Ga[,,,1] <- Ga_no_punish
Ga[,,,2] <- Ga_punish

c_choice_index <- c

Gini <- array(0, ngroups)
Nation <- array(0, ngroups)
Nation <- Nation[!is.na(Nation)]
Civic <- array(0,c(ngroups))
Indiv <- array(0,c(ngroups))
GDP <- array(0,c(ngroups))
Trust <- array(0,c(ngroups))
for (g in 1:ngroups) {
  Gini[g] <- mean(redDat$gini[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])
  Nation[g] <- mean(redDat$nation[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])
  Civic[g] <- mean(redDat$civic[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])
  Indiv[g] <- mean(redDat$ind[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])
  GDP[g] <- mean(redDat$gdp[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])  
  Trust[g] <- mean(redDat$trust[redDat$groupid==group_names[g]&redDat$p=="N-experiment"])
}

c_win <- c_no_punish[,,!is.na(Gini)]
c_keep <- rep()-c_win

Gga_punish <- Gga_punish[,!is.na(Gini)]
Gga_no_punish <- Gga_no_punish[,!is.na(Gini)]

c <- c[,,!is.na(Gini),]
Gga <- Gga[,!is.na(Gini),]
Ggas <- Ggas[,,!is.na(Gini),]
Gc <- Gc[,,!is.na(Gini),]
Ga <- Ga[,,!is.na(Gini),]
Gini <- Gini[!is.na(Gini)]
Nation <- Nation[!is.na(Nation)]
Indiv <- Indiv[!is.na(Gini)]
Civic <- civic[!is.na(Gini)]
GDP <- GDP[!is.na(Gini)]
Trust <- Trust[!is.na(Gini)]

#redefine number of groups after removing those without civic scores
ngroups <- length(Gini)

# aggregate Gini to just 1 number per Nation-index (using the mean here should be unproblematic since all groups within a given nation should have been given the same Gini-coefficient)
Gini <- aggregate(Gini~Nation, FUN=mean)[,2]
GDP <- aggregate(GDP~Nation,FUN=mean)[,2]
Indiv <- aggregate(Indiv~Nation,FUN=mean)[,2]
Trust <- aggregate(Trust~Nation,FUN=mean)[,2]
Civic <- aggregate(Civic~Nation,FUN=mean)[,2]

nnations <- length(Gini)

# calculate the winnings (i.e. apply the multiplication-factor to the sum of each groups contributions)
winnings <-  array(0, ngroups)
for (g in 1:ngroups) {
  winnings[g] <- sum(colSums(c_win[,,g])*pi)
}

################################################################################
########################### Conditional cooperation model ######################
################################################################################

# JZS priors for partial correlation. Method described here
# https://link.springer.com/article/10.3758/s13423-012-0295-x
# Code available here
# https://github.com/MicheleNuijten/BayesMed/blob/master/R/jzs_corSD.R
# Paper where code is used here (mediation paper)
# https://link.springer.com/article/10.3758/s13428-014-0470-2

#################################################################
#------------------ Winnings analysis ---------------------------
#################################################################

#-------------------  Regress Gini on winnings ---------------

# standardise variables

X <- Gini
X <- (X-mean(X))/sd(X)

invSigma <- solve(t(X)%*%X) # required for JZS priors

Y <- (winnings-mean(winnings))/sd(winnings)

data <- list("ngroups", "Y", "nnations","X","Nation","invSigma") 
params <- c("beta0","betaX") 

# - run jags code
win.samples <- jags.parallel(data, inits=NULL, params,
                    model.file ="win_corr.txt",
                    n.chains=3, n.iter=15000, n.burnin=5000, n.thin=1, n.cluster=3)

#----------------------- Control for all four national variables using partial correlation _------------
fullControl.win <- function (X1,X2,X3,X4,X5,ngroups,nnations,winnings,Nation) {

  # standardise covariates
  X1 <- (X1-mean(X1))/sd(X1)
  X2 <- (X2-mean(X2))/sd(X2)
  X3 <- (X3-mean(X3))/sd(X3)
  X4 <- (X4-mean(X4))/sd(X4)
  X5 <- (X5-mean(X5))/sd(X5)

  X <- cbind(X1,X2,X3,X4,X5)
  V <- solve(t(X)%*%X)

  Y <- (winnings-mean(winnings))/sd(winnings)

  data <- list("ngroups", "Y", "nnations","X","V","Nation") #data inputted into jags
  params <- c("beta0","betaX","prior_T") #parameters we'll track in jags

  # - run jags code
  win_par.samples <- jags.parallel(data, inits=NULL, params,
                      model.file ="win_corr_partial.txt",
                      n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=3)

  return(list(win_par.samples))

}

Full_control.win <- fullControl.win(GDP,Indiv,Trust,Civic,Gini,ngroups,nnations,winnings,Nation)

#################################################################
#------------------ CC model analysis ---------------------------
#################################################################

#-------------------  Regress Gini on belief weights and slope of prefs in CC model ---------------

# standardise covariate
X <- Gini
X <- (X-mean(X))/sd(X)
invSigma <- solve(t(X)%*%X) # required for JZS priors

# Ga_old <- Ga
# Ga <- Ggas

data <- list("groupSize", "ngroups", "ntrials", "nnations","c","Ga","X","Nation","invSigma") 
params <- c("beta0_alpha","betaX_alpha","beta0_pi","betaX_pi",
            "beta0_rho","betaX_rho","beta0_omega","betaX_omega") 

# - run jags code
start_time = Sys.time()
CC.samples <- jags.parallel(data, inits=NULL, params,
                   model.file ="extra/CC_corr_intercept.txt",
                   n.chains=3, n.iter=15000, n.burnin=5000, n.thin=1, n.cluster=3)
end_time = Sys.time()
end_time - start_time

# #----------------------- Control for all four variables using partial correlation _------------
# fullControl.CC <- function (X1,X2,X3,X4,X5,groupSize,ngroups,ntrials,nnations,c,Ga,Nation) {
#   
#   # standardise covariates
#   X1 <- (X1-mean(X1))/sd(X1)
#   X2 <- (X2-mean(X2))/sd(X2)
#   X3 <- (X3-mean(X3))/sd(X3)
#   X4 <- (X4-mean(X4))/sd(X4)
#   X5 <- (X5-mean(X5))/sd(X5)
#     
#   X <- cbind(X1,X2,X3,X4,X5)
#   V <- solve(t(X)%*%X)
#   
#   data <- list("groupSize", "ngroups", "ntrials", "nnations","c","Ga","X","V","Nation") #data inputted into jags
#   params <- c("beta0_alpha","betaX_alpha","beta0_rho","betaX_rho","beta0_omega","betaX_omega") #parameters we'll track in jags
#   
#   # - run jags code
#   CC.samples <- jags(data, inits=NULL, params,
#                      model.file ="CC_partcor_4control.txt",
#                      n.chains=3, n.iter=15000, n.burnin=5000, n.thin=1)
#   
#   return(list(CC.samples))
#   
# }
# 
# Full_control.CC <- fullControl.CC(GDP,Indiv,Trust,Civic,Gini,groupSize,ngroups,ntrials,nnations,c,Ga,Nation)

#################################################################
#------------------ Plotting ---------------------------
#################################################################

# ------ Create empirical and parameter arrays for plots -------
# empirical group winnings data - means and standard deviations
empirical.win <- array(0,c(3,length(Gini)))
for (i in 1:length(Gini)) {
  empirical.win[1,i] <- mean(winnings[Nation==i]) - sd(winnings[Nation==i]) 
  empirical.win[2,i] <- mean(winnings[Nation==i]) 
  empirical.win[3,i] <- mean(winnings[Nation==i]) + sd(winnings[Nation==i])
}

empirical.initial <- array(0,c(3,length(Gini)))
for (i in 1:length(Gini)) {
  empirical.initial[1,i] <- mean(colMeans(c[,1,,1])[Nation==i]) - sd(colMeans(c[,1,,1])[Nation==i]) 
  empirical.initial[2,i] <- mean(colMeans(c[,1,,1])[Nation==i]) 
  empirical.initial[3,i] <- mean(colMeans(c[,1,,1])[Nation==i]) + sd(colMeans(c[,1,,1])[Nation==i])
}

empirical.contrib <- array(0,c(3,length(Gini)))
for (i in 1:length(Gini)) {
  empirical.contrib[1,i] <- mean(colSums(colSums(c_win[,,Nation==i]))) - sd(colSums(colSums(c_win[,,Nation==i]))) 
  empirical.contrib[2,i] <- mean(colSums(colSums(c_win[,,Nation==i]))) 
  empirical.contrib[3,i] <- mean(colSums(colSums(c_win[,,Nation==i]))) + sd(colSums(colSums(c_win[,,Nation==i])))
}

# # summarise posterior estimates for controlled winnings model
# win.model.summary <- cbind(Full_control.win[[1]]$BUGSoutput$summary[2:6,3],
#                             Full_control.win[[1]]$BUGSoutput$summary[2:6,1],
#                             Full_control.win[[1]]$BUGSoutput$summary[2:6,7])
# 
# # summarise posterior estimates for controlled CC model - initial beliefs
# CC.model.summary.alpha <- cbind(Full_control.CC[[1]]$BUGSoutput$summary[4:8,3],
#                                 Full_control.CC[[1]]$BUGSoutput$summary[4:8,1],
#                                 Full_control.CC[[1]]$BUGSoutput$summary[4:8,7])
# 
# # summarise posterior estimates for controlled CC model - belief weighting
# CC.model.summary.omega <- cbind(Full_control.CC[[1]]$BUGSoutput$summary[9:13,3],
#                               Full_control.CC[[1]]$BUGSoutput$summary[9:13,1],
#                               Full_control.CC[[1]]$BUGSoutput$summary[9:13,7])
# 
# # summarise posterior estimates for controlled CC model - preference slopes
# CC.model.summary.rho <- cbind(Full_control.CC[[1]]$BUGSoutput$summary[14:18,3],
#                               Full_control.CC[[1]]$BUGSoutput$summary[14:18,1],
#                               Full_control.CC[[1]]$BUGSoutput$summary[14:18,7])
# 

#-------------- create high res png for saving ---------------------------------

png(filename="PGG_intercept_figure.png",
    height = 1400,
    width = 1000)

# set layout for graphic
layout(rbind(c(1,1,1,2,2,2,4,4,4), 
             c(1,1,1,2,2,2,4,4,4),
             c(1,1,1,3,3,3,4,4,4), 
             c(1,1,1,3,3,3,4,4,4),
             
             c(5,5,5,7,7,7,9,9,9), 
             c(5,5,5,7,7,7,9,9,9),
             c(6,6,6,8,8,8,10,10,10), 
             c(6,6,6,8,8,8,10,10,10)))

# text scale plot parameter
par(cex=1.5)

# set margins - will change when there is no title required
par(mar=c(5,3,5,2))

#----- empirical correlation - Gini and group winnings --------------------
plot(c(22,45), c(0,900), type = "n", main = "A: Data - Winnings", 
     xlab = "National Gini Coefficient", ylab = "Group winnings",axes=FALSE)
for (i in 1:length(Gini)) {
  lines(c(Gini[i],Gini[i]),c(empirical.win[1,i],empirical.win[3,i]))
  points(Gini[i],empirical.win[2,i])
}
axis(1)
axis(2)

#----- posterior of effect - Gini and group contributions --------------------
plot(density(win.samples$BUGSoutput$sims.list$betaX),frame=FALSE,lwd=2,ylim=c(0,7),
     cex=2,xlab = "Standardised Effect of Gini",ylab = " ",main="B: Contribution")
COL <- adjustcolor(c("red"))
lines(c(win.samples$BUGSoutput$summary[2,3],win.samples$BUGSoutput$summary[2,7]),c(-.1,-.1),col=COL,lwd=2)
points(win.samples$BUGSoutput$summary[2,1],c(-.1),pch=19,col=COL)
abline(h=0,v=0,col="gray60")

par(mar=c(5,3,1,2)) # reset margin because no title

# #----- Controls - group winnings --------------------
# plot(c(-1,1), c(-.5,-5.5), type = "n", main = " ", xlab = "Standardised Estimates", 
#      ylab = " ",axes=FALSE)
# for (i in 1:5) {
#   ifelse(sign(win.model.summary[i,1]) == sign(win.model.summary[i,3]),
#          COL <- adjustcolor(c("red")),
#          COL <- adjustcolor(c("black")))
#   lines(c(win.model.summary[i,1],win.model.summary[i,3]),c(-i,-i),col=COL,lwd=2)
#   points(win.model.summary[i,2],c(-i),pch=19,col=COL)
#   abline(h=0,v=0,col="gray60")
# }
# axis(1)
# axis(2, at = c(-1, -2, -3, -4, -5),
#      labels = c("GDP","Indiv.","Trust","Civic",
#                 "Gini"),las=1,pos=-1)  
# 
# par(mar=c(5,3,5,2))

#----- empirical correlation - Gini and initial contribution --------------------
plot(c(22,45), c(0,20), type = "n", main = "C: Data - Initial Contrib.", 
     xlab = "National Gini Coefficient", ylab = "Initial Contribution",axes=FALSE)
for (i in 1:length(Gini)) {
  lines(c(Gini[i],Gini[i]),c(empirical.initial[1,i],empirical.initial[3,i]))
  points(Gini[i],empirical.initial[2,i])
}
axis(1)
axis(2)

#---------- Initial Belief -------------------------------------------------------

plot(density(CC.samples$BUGSoutput$sims.list$betaX_alpha),frame=FALSE,lwd=2,ylim=c(0,8),
     cex=2,xlab = "Standardised Effect of Gini",ylab = " ",main="D: Initial Belief")
COL <- adjustcolor(c("red"))
lines(c(CC.samples$BUGSoutput$summary["betaX_alpha",3],CC.samples$BUGSoutput$summary["betaX_alpha",7]),c(-.1,-.1),col=COL,lwd=2)
points(CC.samples$BUGSoutput$summary["betaX_alpha",1],c(-.1),pch=19,col=COL)
abline(h=0,v=0,col="gray60")

par(mar=c(5,3,1,2)) # reset margin because no title

# plot(c(-.6,.6), c(-.5,-5.5), type = "n", main = " ", xlab = "Standardised Estimates", 
#      ylab = " ",axes=FALSE)
# for (i in 1:5) {
#   ifelse(sign(CC.model.summary.alpha[i,1]) == sign(CC.model.summary.alpha[i,3]),
#          COL <- adjustcolor(c("red")),
#          COL <- adjustcolor(c("black")))
#   lines(c(CC.model.summary.alpha[i,1],CC.model.summary.alpha[i,3]),c(-i,-i),col=COL,lwd=2)
#   points(CC.model.summary.alpha[i,2],c(-i),pch=19,col=COL)
#   abline(h=0,v=0,col="gray60")
# }
# axis(1)
# axis(2, at = c(-1, -2, -3, -4, -5),
#      labels = c("GDP","Indiv.","Trust","Civic",
#                 "Gini"),las=1,pos=-.6)  
# 
# par(mar=c(5,3,5,2))

#---------- Belief Learning -------------------------------------------------------

plot(density(CC.samples$BUGSoutput$sims.list$betaX_omega),frame=FALSE,lwd=2,ylim=c(0,10),
     cex=2,xlab = "Standardised Effect of Gini",ylab = " ",main="E: Belief Learning Weight")
COL <- adjustcolor(c("red"))
lines(c(CC.samples$BUGSoutput$summary["betaX_omega",3],CC.samples$BUGSoutput$summary["betaX_omega",7]),c(-.1,-.1),col=COL,lwd=2)
points(CC.samples$BUGSoutput$summary["betaX_omega",1],c(-.1),pch=19,col=COL)
abline(h=0,v=0,col="gray60")

par(mar=c(5,3,1,2))

# plot(c(-.6,.6), c(-.5,-5.5), type = "n", main = NULL, xlab = "Standardised Estimates", 
#      ylab = " ",axes=FALSE)
# for (i in 1:5) {
#   ifelse(sign(CC.model.summary.omega[i,1]) == sign(CC.model.summary.omega[i,3]),
#          COL <- adjustcolor(c("red")),
#          COL <- adjustcolor(c("black")))
#   lines(c(CC.model.summary.omega[i,1],CC.model.summary.omega[i,3]),c(-i,-i),col=COL,lwd=2)
#   points(CC.model.summary.omega[i,2],c(-i),pch=19,col=COL)
#   abline(h=0,v=0,col="gray60")
# }
# axis(1)
# axis(2, at = c(-1, -2, -3, -4, -5),
#      labels = c("GDP","Indiv.","Trust","Civic",
#                 "Gini"),las=1,pos=-.6)  
# 
# par(mar=c(5,3,5,2))

#---------- Preference Slope -------------------------------------------------------

plot(density(CC.samples$BUGSoutput$sims.list$betaX_rho),frame=FALSE,lwd=2,ylim=c(0,10),
     cex=2,xlab = "Standardised Effect of Gini",ylab = " ",main="F: Conditional Preferences")
COL <- adjustcolor(c("black"))
lines(c(CC.samples$BUGSoutput$summary["betaX_rho",3],CC.samples$BUGSoutput$summary["betaX_rho",7]),c(-.1,-.1),col=COL,lwd=2)
points(CC.samples$BUGSoutput$summary["betaX_rho",1],c(-.1),pch=19,col=COL)
abline(h=0,v=0,col="gray60")

par(mar=c(5,3,1,2))

# plot(c(-.6,.6), c(-.5,-5.5), type = "n", main = " ", xlab = "Standardised Estimates", 
#      ylab = " ",axes=FALSE)
# for (i in 1:5) {
#   ifelse(sign(CC.model.summary.rho[i,1]) == sign(CC.model.summary.rho[i,3]),
#          COL <- adjustcolor(c("red")),
#          COL <- adjustcolor(c("black")))
#   lines(c(CC.model.summary.rho[i,1],CC.model.summary.rho[i,3]),c(-i,-i),col=COL,lwd=2)
#   points(CC.model.summary.rho[i,2],c(-i),pch=19,col=COL)
#   abline(h=0,v=0,col="gray60")
# }
# axis(1)
# axis(2, at = c(-1, -2, -3, -4, -5),
#      labels = c("GDP","Indiv.","Trust","Civic",
#                 "Gini"),las=1,pos=-.6)  
# 

#---------- Preference Intercept -------------------------------------------------------

plot(density(CC.samples$BUGSoutput$sims.list$betaX_pi),frame=FALSE,lwd=2,ylim=c(0,10),
     cex=2,xlab = "Standardised Effect of Gini",ylab = " ",main="G: Conditional Intercept")
COL <- adjustcolor(c("black"))
lines(c(CC.samples$BUGSoutput$summary["betaX_pi",3],CC.samples$BUGSoutput$summary["betaX_pi",7]),c(-.1,-.1),col=COL,lwd=2)
points(CC.samples$BUGSoutput$summary["betaX_pi",1],c(-.1),pch=19,col=COL)
abline(h=0,v=0,col="gray60")

par(mar=c(5,3,1,2))

#----- empirical correlation - Gini and group contributions --------------------
plot(c(22,45), c(0,900), type = "n", main = "H: Data - Contributions", 
     xlab = "National Gini Coefficient", ylab = "Group contributions",axes=FALSE)
for (i in 1:length(Gini)) {
  lines(c(Gini[i],Gini[i]),c(empirical.contrib[1,i],empirical.contrib[3,i]))
  points(Gini[i],empirical.contrib[2,i])
}
axis(1)
axis(2)

dev.off()

