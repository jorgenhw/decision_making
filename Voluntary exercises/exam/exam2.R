library(rjags)

model_string <- """
  model {
    # Prior distributions
    for (i in 1:num_players) {
      roll_rate[i] ~ dexp(1)
      steal_prob[i] ~ dbeta(2, 2)
    }

    # Likelihood
    for (i in 1:num_players) {
      for (j in 1:num_players) {
        if (i != j) {
          logit_p[i, j] <- roll_rate[i] * (1 - exp(-distance / friendship[i, j])) * (1 - exp(-forgiveness[i])) * (1 - exp(-risk_aversion[j]))
          p[i, j] <- 1 / (1 + exp(-logit_p[i, j]))
          steals[i, j] ~ dbern(p[i, j])
        }
      }
    }
  }
"""






# Simulate the game
set.seed(123)
num_players <- 4



#### DISTANCE
# Create a 4x4 matrix with random distances between 50 and 200
distance_matrix <- matrix(runif(num_players * num_players, min = 50, max = 200), nrow = num_players)
# Set the matrix to be symmetric
distance_matrix[lower.tri(distance_matrix)] <- distance_matrix[upper.tri(distance_matrix)]
# Set diagonal elements to 0
diag(distance_matrix) <- 0
distance <- distance_matrix

#### FORGIVENESS
forgiveness <- runif(num_players, min = 1, max = 7)

#### FRIENDSHIP
# Create a 4x4 matrix with random friendship values between 1 and 7
friendship <- matrix(runif(num_players * num_players, min = 1, max = 7), nrow = num_players)
# Set diagonal elements to 0
diag(friendship) <- 0

#### FAIRNESS
fairness <- runif(num_players, min = 1, max = 7)
fairness

# Combine data
data <- list(
  num_players = num_players,
  distance = distance,
  forgiveness = forgiveness,
  friendship = friendship,
  fairness = fairness
)

# Initialize JAGS model
jags_model <- jags.model(textConnection(model_string), data = data, n.chains = 4)

jags.parallel(data, inits=NULL, params,
                model.file ="Voluntary exercises/exam/model.txt",
                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)


# Burn-in and update
update(jags_model, 1000)

# Run the simulation
samples <- coda.samples(jags_model, variable.names = c("roll_rate", "steal_prob"), n.iter = 1000)

# Summary of the results
summary(samples)








