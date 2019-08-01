# Model Building ----------------------------------------------------------
# Load libraries.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)

# Specify data and hyperprior values.
sim_values <- list(
  N = 100,     # Number of observations.
  mu = 5,      # Mean of the upper-level model.
  tau = 3      # Variance of the upper-level model.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data.
sim_y <- extract(sim_data)$y
sim_theta <- extract(sim_data)$theta

# Model Calibration -------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  y = as.vector(sim_y) # Vector of observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "hierarchical_regression.stan"),
  data = data,
  seed = 42
)

# Model Validation --------------------------------------------------------
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

fit %>%
  mcmc_trace(
    pars = str_c("theta[", 1:data$N, "]"),
    n_warmup = 500,
    facet_args = list(nrow = 5, labeller = label_parsed)
  )

# Recover parameter values.
par_values <- tibble(
  n = 1:sim_values$N,
  mu = sim_values$mu,
  tau = sim_values$tau,
  theta = as.vector(sim_theta)
)

fit %>%
  gather_draws(mu, tau) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = c(.95, .95)) +
  geom_vline(aes(xintercept = mu), par_values, color = "red") +
  geom_vline(aes(xintercept = tau), par_values, color = "red")

fit %>%
  spread_draws(theta[n]) %>%
  ggplot(aes(x = theta, y = n)) +
  geom_halfeyeh(.width = c(.95, .95)) +
  facet_wrap(
    ~ as.factor(n),
    nrow = 5,
    scales = "free"
  ) +
  geom_vline(aes(xintercept = theta), par_values, color = "red")

