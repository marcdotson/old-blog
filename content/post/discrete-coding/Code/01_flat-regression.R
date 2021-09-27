# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(cmdstanr)
library(posterior)
library(tidybayes)
# library(bayesplot)

# Set the simulation seed.
set.seed(42)

# Generate Data -----------------------------------------------------------
# Specify data and parameter values.
sim_values <- list(
  N = 50,                # Number of observations.
  I = 4,                 # Number of covariates.
  beta = c(1, 4, 3, -2), # Vector of slopes.
  tau = 1                # Variance of the regression.
)

# Matrix of covariates.
sim_X <- matrix(data = 0, nrow = sim_values$N, ncol = (sim_values$I))
for (n in 1:sim_values$N) {
  sim_X[n, sample(seq(1, (sim_values$I)), 1)] <- 1
}
sim_values$X <- sim_X

# Compile the model for generating data.
generate_flat_data <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "generate_flat_data.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Generate data.
sim_data <- generate_flat_data$sample(
  data = sim_values,
  chains = 1,
  iter_sampling = 1,
  seed = 42,
  fixed_param = TRUE
)

# Extract generated data.
sim_y <- sim_data$draws(variables = "y", format = "draws_list") %>%
  pluck(1) %>%
  flatten_dbl()

# Dummy Coding ------------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  I = ncol(sim_X) - 1, # Number of covariates.
  y = sim_y,           # Vector of observations.
  X = sim_X[,-1]       # Matrix of covariates.
)

# Compile the model.
flat_regression_dummy <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_dummy.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Calibrate the model.
fit_dummy <- flat_regression_dummy$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Extract draws.
fit_dummy$draws(variables = c("alpha", "beta"), format = "draws_df") %>%
  # pivot_longer(alpha:beta[3], values_to = "draw", names_to = "parameter") %>%
  # gather_draws() %>% # tidybayes?
  ggplot(aes(x = "beta[1]")) +
  geom_histogram()

