# Preamble ----------------------------------------------------------------
# Load libraries.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)

# Set Stan options.
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

# 01 Simple Hierarchical Regression ---------------------------------------
# Hierarchical regression with no covariates and known variance.

# Specify data and hyperparameter values.
sim_values <- list(
  N = 100,     # Number of observations.
  mu = 5,      # Mean of the population-level model.
  tau = 3      # Variance of the population-level model.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "generate_data_01.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data and individual-level coefficients.
sim_y <- extract(sim_data)$y
sim_beta <- extract(sim_data)$beta

# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  y = as.vector(sim_y) # Vector of observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "hierarchical_regression_01.stan"),
  data = data,
  seed = 42
)

# Diagnostics.
source(here::here("content", "post", "stan-hierarchical", "stan_utility.R"))
check_all_diagnostics(fit)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = c("mu", "tau"),
    n_warmup = 500,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

beta_sample <- sample(1:data$N, 12)
fit %>%
  mcmc_trace(
    pars = str_c("beta[", beta_sample, "]"),
    n_warmup = 500,
    facet_args = list(nrow = 3, labeller = label_parsed)
  )

# Recover hyperparameter and parameter values.
hyperpar_values <- tibble(
  .variable = c("mu", "tau"),
  values = c(sim_values$mu, sim_values$tau),
)

par_values <- tibble(
  n = as.factor(beta_sample),
  beta = as.vector(sim_beta[beta_sample])
)

fit %>%
  gather_draws(mu, tau) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  facet_wrap(
    ~ .variable,
    nrow = 2,
    scales = "free"
  ) +
  geom_vline(aes(xintercept = values), hyperpar_values, color = "red")

fit %>%
  spread_draws(beta[n]) %>%
  filter(n %in% beta_sample) %>%
  ggplot(aes(x = beta, y = n)) +
  geom_halfeyeh(.width = .95) +
  facet_wrap(
    ~ n,
    nrow = 3,
    scales = "free"
  ) +
  geom_vline(aes(xintercept = beta), par_values, color = "red")

# Save data and model output.
run <- list(data, fit)
write_rds(run, here::here("content", "post", "stan-hierarchical", "run_01.rds"))

