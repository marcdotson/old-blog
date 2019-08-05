# Preamble ----------------------------------------------------------------
# Load libraries.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)

# Set Stan options.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Intercept-Only Hierarchical Regression ----------------------------------
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

# fit %>%
#   mcmc_trace(
#     pars = str_c("beta[", 1:data$N, "]"),
#     n_warmup = 500,
#     facet_args = list(nrow = 5, labeller = label_parsed)
#   )

# Recover hyperparameter values.
par_values <- tibble(
  n = 1:sim_values$N,
  mu = sim_values$mu,
  tau = sim_values$tau,
  beta = as.vector(sim_beta)
)

fit %>%
  gather_draws(mu, tau) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = c(.95, .95)) +
  geom_vline(aes(xintercept = mu), par_values, color = "red") +
  geom_vline(aes(xintercept = tau), par_values, color = "red")

# fit %>%
#   spread_draws(theta[n]) %>%
#   ggplot(aes(x = theta, y = n)) +
#   geom_halfeyeh(.width = c(.95, .95)) +
#   facet_wrap(
#     ~ as.factor(n),
#     nrow = 5,
#     scales = "free"
#   ) +
#   geom_vline(aes(xintercept = beta), par_values, color = "red")

# Save data and model output.
run <- list(data, fit)
write_rds(run, here::here("content", "post", "stan-hierarchical", "run_01.rds"))

