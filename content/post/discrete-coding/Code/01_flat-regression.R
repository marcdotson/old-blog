# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(cmdstanr)
library(posterior)
# library(bayesplot)
# library(tidybayes)

# Set the simulation seed.
set.seed(42)

# Generate Data -----------------------------------------------------------
# Specify data and parameter values.
sim_values <- list(
  N = 50,             # Number of observations.
  I = 3,              # Number of covariates.
  alpha = 1,          # Intercept.
  beta = c(4, 3, -2), # Vector of slopes.
  tau = 1             # Variance of the regression.
)

# Matrix of dummy-coded covariates.
X <- matrix(data = 0, nrow = sim_values$N, ncol = (sim_values$I + 1))
for (n in 1:sim_values$N) {
  X[n, sample(seq(1, (sim_values$I + 1)), 1)] <- 1
}
sim_values$X <- X[,-1]

# Compile the model.
generate_flat_data <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "generate_flat_data.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Generate data.
sim_data <- generate_flat_data$sample(
  data = sim_values,
  seed = 42,
  fixed_param = TRUE
)



