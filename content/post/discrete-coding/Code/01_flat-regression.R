# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidybayes)

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

# Fit the model.
fit_dummy <- flat_regression_dummy$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Check diagnostics.
fit_dummy$cmdstan_diagnose()

# Recover parameters?
bayesplot_grid(
  mcmc_hist(fit_dummy$draws("alpha")) + vline_at(sim_values$beta[1], size = 1.5),
  mcmc_hist(fit_dummy$draws("beta[1]")) + vline_at(sim_values$beta[2], size = 1.5),
  mcmc_hist(fit_dummy$draws("beta[2]")) + vline_at(sim_values$beta[3], size = 1.5),
  mcmc_hist(fit_dummy$draws("beta[3]")) + vline_at(sim_values$beta[4], size = 1.5)
)

# Extract draws and compare contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:(sim_values$I - 1)),
  values = c(
    sim_values$beta[4] - sim_values$beta[1],
    sim_values$beta[4] - sim_values$beta[2],
    sim_values$beta[4] - sim_values$beta[3]
  )
)
fit_dummy$draws(variables = c("alpha", "beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[3]` - alpha,
    contrast2 = `beta[3]` - `beta[1]`,
    contrast3 = `beta[3]` - `beta[2]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# # Save output.
# fit_dummy$save_object(file = here::here("content", "post", "discrete-coding", "Output", "fit_dummy.rds"))

# Index Coding ------------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  I = ncol(sim_X),     # Number of covariates.
  y = sim_y,           # Vector of observations.
  X = sim_X            # Matrix of covariates.
)

# Compile the model.
flat_regression_index <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_index.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_index <- flat_regression_index$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Check diagnostics.
fit_index$cmdstan_diagnose()

# Recover parameters?
bayesplot_grid(
  mcmc_hist(fit_index$draws("beta[1]")) + vline_at(sim_values$beta[1], size = 1.5),
  mcmc_hist(fit_index$draws("beta[2]")) + vline_at(sim_values$beta[2], size = 1.5),
  mcmc_hist(fit_index$draws("beta[3]")) + vline_at(sim_values$beta[3], size = 1.5),
  mcmc_hist(fit_index$draws("beta[4]")) + vline_at(sim_values$beta[4], size = 1.5)
)

# Extract draws and compare contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:(sim_values$I - 1)),
  values = c(
    sim_values$beta[4] - sim_values$beta[1],
    sim_values$beta[4] - sim_values$beta[2],
    sim_values$beta[4] - sim_values$beta[3]
  )
)
fit_index$draws(variables = c("beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[4]` - `beta[1]`,
    contrast2 = `beta[4]` - `beta[2]`,
    contrast3 = `beta[4]` - `beta[3]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# # Save output.
# fit_index$save_object(file = here::here("content", "post", "discrete-coding", "Output", "fit_index.rds"))

