# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(loo)
library(bayesplot)
library(tidybayes)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generate Data -----------------------------------------------------------
# Specify data and hyperparameter values.
sim_values <- list(
  N = 500,                            # Number of observations.
  K = 5,                              # Number of groups.
  # I = 7,                              # Number of observation-level covariates.
  # J = 3,                              # Number of population-level covariates.
  g = sample(5, 500, replace = TRUE), # Vector of group assignments.

  # Matrix of observation-level covariates.
  X = cbind(
    rep(1, 500),
    matrix(runif(500 * (7 - 1), min = 1, max = 10), nrow = 500)
  ),

  # Matrix of population-level covariates.
  # Z = cbind(
  #   rep(1, 5),
  #   matrix(runif(5 * (3 - 1), min = 2, max = 5), nrow = 5)
  # ),
  mu = 5,

  tau = 1,                            # Variance of the population model.
  sigma = 1                           # Variance of the likelihood.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "non-centered", "Code", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Save simulation values and data.
write_rds(
  sim_values,
  # path = here::here("content", "post", "non-centered", "Data", "sim_values.rds")
  path = here::here("content", "post", "non-centered", "Data", "sim_values2.rds")
)
write_rds(
  sim_data,
  # path = here::here("content", "post", "non-centered", "Data", "sim_data.rds")
  path = here::here("content", "post", "non-centered", "Data", "sim_data2.rds")
)

# Load simulation values and data.
sim_values <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_values.rds"))
sim_data <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_data.rds"))
# sim_values <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_values2.rds"))
# sim_data <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_data2.rds"))

# Extract simulated data and group intercepts.
sim_y <- extract(sim_data)$y
# sim_Gamma <- extract(sim_data)$Gamma
# sim_Beta <- extract(sim_data)$Beta
sim_beta <- extract(sim_data)$beta

# Fit Models --------------------------------------------------------------
# Centered parameterization.
data <- list(
  N = sim_values$N,     # Number of observations.
  K = sim_values$K,     # Number of groups.
  # I = sim_values$I,     # Number of observation-level covariates.
  # J = sim_values$J,     # Number of population-level covariates.
  y = as.vector(sim_y), # Vector of observations.
  g = sim_values$g     # Vector of group assignments.
  # X = sim_values$X     # Matrix of observation-level covariates.
  # Z = sim_values$Z      # Matrix of population-level covariates.
)

fit_centered <- stan(
  file = here::here("content", "post", "non-centered", "Code", "hlm_centered.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model run.
write_rds(
  fit_centered,
  path = here::here("content", "post", "non-centered", "Output", "fit_centered.rds")
)

# Load model run.
fit_centered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_centered.rds"))

# Check trace plots.
fit_centered %>%
  mcmc_trace(
    pars = c("mu", "tau", str_c("beta[", 1:data$K, "]"), "sigma"),
    n_warmup = 500,
    facet_args = list(nrow = 5, labeller = label_parsed)
  )

# Recover hyperparameter and parameter values.
hyper_par_values <- tibble(
  .variable = c("mu", "tau", "sigma"),
  values = c(sim_values$mu, sim_values$tau, sim_values$sigma),
)

beta_values <- tibble(
  n = 1:data$K,
  beta = as.vector(sim_beta)
)

fit_centered %>%
  gather_draws(mu, tau, sigma) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), hyper_par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 3,
    scales = "free"
  )

fit_centered %>%
  spread_draws(beta[n]) %>%
  ggplot(aes(x = beta, y = n)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = beta), beta_values, color = "red") +
  facet_wrap(
    ~ n,
    nrow = 3,
    scales = "free"
  )

# Non-centered parameterization.
data <- list(
  N = sim_values$N,     # Number of observations.
  K = sim_values$K,     # Number of groups.
  # I = sim_values$I,     # Number of observation-level covariates.
  # J = sim_values$J,     # Number of population-level covariates.
  y = as.vector(sim_y), # Vector of observations.
  g = sim_values$g     # Vector of group assignments.
  # X = sim_values$X,     # Matrix of observation-level covariates.
  # Z = sim_values$Z      # Matrix of population-level covariates.
)

fit_noncentered <- stan(
  file = here::here("content", "post", "non-centered", "Code", "hlm_noncentered.stan"),
  # file = here::here("content", "post", "non-centered", "Code", "simple_hlm.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model run.
write_rds(
  fit_noncentered,
  path = here::here("content", "post", "non-centered", "Output", "fit_noncentered.rds")
)

# Check trace plots.
fit_noncentered %>%
  mcmc_trace(
    pars = c("mu", "tau", str_c("beta[", 1:data$K, "]"), "sigma"),
    n_warmup = 500,
    facet_args = list(nrow = 5, labeller = label_parsed)
  )

# Recover hyperparameter and parameter values.
hyper_par_values <- tibble(
  .variable = c("mu", "tau", "sigma"),
  values = c(sim_values$mu, sim_values$tau, sim_values$sigma),
)

beta_values <- tibble(
  n = 1:data$K,
  beta = as.vector(sim_beta)
)

fit_noncentered %>%
  gather_draws(mu, tau, sigma) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), hyper_par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 3,
    scales = "free"
  )

fit_noncentered %>%
  spread_draws(beta[n]) %>%
  ggplot(aes(x = beta, y = n)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = beta), beta_values, color = "red") +
  facet_wrap(
    ~ n,
    nrow = 3,
    scales = "free"
  )

# Compute Model Fit -------------------------------------------------------
# Load model runs.
fit_centered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_centered.rds"))
fit_noncentered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_noncentered.rds"))

# Centered parameterization.
log_lik_centered <- extract_log_lik(fit_centered, merge_chains = FALSE)
r_eff_centered <- relative_eff(exp(log_lik_centered))
loo(log_lik_centered, r_eff = r_eff_centered)
loo(fit_centered, save_psis = TRUE)

loo_centered <- loo(fit_centered, save_psis = TRUE)

# Non-centered parameterization.
log_lik_noncentered <- extract_log_lik(fit_noncentered, merge_chains = FALSE)
r_eff_noncentered <- relative_eff(exp(log_lik_noncentered))
loo(log_lik_noncentered, r_eff = r_eff_noncentered)
loo(fit_noncentered, save_psis = TRUE)

loo_noncentered <- loo(fit_noncentered, save_psis = TRUE)

# Compare model fit.
loo_compare(loo_centered, loo_noncentered)

