# tidy code - courtesy of Mia Jacobsen

# (set working directory and) load packages
install.packages("pacman")
pacman::p_load(tidyverse, R2jags, parallel, polspline)

### looking at describing outcome in public goods from gini-coefficient 

# pre-processing

##### info about game #####
groupSize <- 4
ntrials <- 10
pi <- 1.4 # multiplication factor in game
ntokens <- 20
vals <- seq(0,ntokens,1) #possible values to contribute - from 0 to 20 tokens

##### load data #####
raw <- read.csv('data/HerrmannThoeniGaechterDATA.csv', skip = 3, header = T)

### removing punishment condition - remove line if you want to retain 
raw <- filter(raw, p == "N-experiment")

### setting up cities with their corresponding nation id + getting their data 
city = c("Melbourne", "Minsk", "Chengdu", "Copenhagen", "Bonn", "Athens", 
         "Seoul", "Samara", "Zurich", "St. Gallen", "Istanbul", "Nottingham", 
         "Dnipropetrovs'k", "Boston")
nation = c(1,2,3,4,5,6,7,8,9,9,10,11,12,13)

# doing this separately so we avoid the repeating nation (#9) in these lists
# bc then they become super useful later (hint: covariates at nation level)
id = seq(1,13,1)
##### outdated Gini-values - see other scripts for an update on this
#gini = c(34.3, 25.3, 38.5, 28.7, 31.9, 34.4, 31.6, 37.5, 32.7, 41.9, 34.8, 26.1, 41.1)
gini = c(33.3, 28.3, 41.5, 25.4, 30.7, 34, 31.7, 38, 31.7, 41.4, 35.2, 29.1, 40.8)
ind = c(90, 25, 20, 74, 67, 35, 18, 25, 68, 37, 89, 25, 91)
gdp = c(32.9, 8.9, 7.6, 36.5, 31.1, 26.0, 23.9, 12.1, 37.4, 9.1, 35.1, 7.6, 43.4)
trust = c(.40, .42, .55, .67, .38, .24, .27, .24, .37, .16, .29, .27, .36)

### first round of merge
data <- data.frame(id, gini, ind, gdp, trust) %>% 
  rename(nation = id)
cities = data.frame(city, nation)
data = merge(data, cities)

### second round
# merge automatically removes countries without data
# if you want to retain add all = T after on = 'city'
df <- merge(raw, data , on = "city") %>% 
  # select every third row
  filter(row_number() %% 3 == 1)

#### setting up other things ####

## group average contribution on each trial 
ngroups = length(unique(df$groupid))

Ga_np <- df %>% 
  group_by(groupid, period) %>% # add p if you are interested in punishment conditions
  summarise(mean_send = mean(senderscontribution))

# make it an array 
Ga <- array(data = Ga_np$mean_send, dim = c(ntrials, ngroups))

## winnings
winnings <- df %>% 
  group_by(groupid) %>% 
  summarise(win = sum(senderscontribution)*pi)

## getting nation for every group 
Nation <- df %>% 
  group_by(groupid) %>% 
  summarise(nation = mean(nation)) %>% 
  select(nation)
# make it an array
Nation <- array(data = unlist(Nation))

### the other covarites at nation level are specified  at the beginning

################# JAGS - CORRELATION: WINNING, GINI ###############

ngroups = length(unique(df$groupid))
nnations = length(unique(df$nation))

# standardize and make arrays of X and Y
Y = array((winnings$win - mean(winnings$win)) / sd(winnings$win))
X = array((gini-mean(gini))/sd(gini))

invSigma <- solve(t(X)%*%X) # required for JZS priors

data <- list("ngroups", "Y", "nnations","X","Nation","invSigma") 
params <- c("beta0","betaX") 

# run jags code
win.samples <- jags.parallel(data, inits=NULL, params,
                             model.file ="216377/Module5/win_corr.txt",
                             n.chains=3, n.iter=15000, n.burnin=5000, n.thin=1, n.cluster=4)