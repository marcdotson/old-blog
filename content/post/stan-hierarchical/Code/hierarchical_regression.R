# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(brms)
library(rstan)
library(bayesplot)
library(tidybayes)

# Set Stan options.
options(mc.cores = parallel::detectCores())

# 00 Simple Regression ----------------------------------------------------
# Simple (non-hierarchical) regression with no covariates and known variance.

# Specify data and parameter values.
sim_values <- list(
  N = 100,                            # Number of observations.
  mu = 5,                             # Mean of the regression.
  tau = 1                             # Variance of the regression.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data.
sim_y <- extract(sim_data)$y

# Specify data.
data <- list(
  N = length(sim_y),                 # Number of individuals.
  y = as.vector(sim_y)               # Vector of observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "regression.stan"),
  data = data,
  seed = 42
)

# Diagnostics.
source(here::here("content", "post", "stan-hierarchical", "Code", "stan_utility.R"))
check_all_diagnostics(fit)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = c("mu", "tau"),
    n_warmup = 500,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

# Recover parameter values.
par_values <- tibble(
  .variable = c("mu", "tau"),
  values = c(sim_values$mu, sim_values$tau),
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
  geom_vline(aes(xintercept = values), par_values, color = "red")

ggsave(
  "marginals.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

# 01 Simple Hierarchical Regression ---------------------------------------
# Hierarchical regression with no covariates and known variance.

# Specify data and hyperparameter values.
sim_values <- list(
  N = 100,                            # Number of individuals.
  K = 3,                              # Number of groups.
  g = sample(3, 100, replace = TRUE), # Vector of group assignments.
  mu = 5,                             # Mean of the population model.
  tau = 1                             # Variance of the population model.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_data_01.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data and group intercepts.
sim_y <- extract(sim_data)$y
sim_beta <- extract(sim_data)$beta

# Specify data.
data <- list(
  N = length(sim_y),                 # Number of individuals.
  K = sim_values$K,                  # Number of groups.
  y = as.vector(sim_y),              # Vector of observations.
  g = sim_values$g                   # Vector of group assignments.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "hierarchical_regression_01.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Diagnostics.
source(here::here("content", "post", "stan-hierarchical", "Code", "stan_utility.R"))
check_all_diagnostics(fit)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = c("mu", "tau", str_c("beta[", 1:data$K, "]")),
    n_warmup = 500,
    facet_args = list(nrow = 5, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace-01.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 6, units = "in"
)

# Recover hyperparameter and parameter values.
hyperpar_values <- tibble(
  .variable = c("mu", "tau"),
  values = c(sim_values$mu, sim_values$tau),
)

par_values <- tibble(
  n = 1:data$K,
  beta = as.vector(sim_beta)
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

ggsave(
  "marginals-01a.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

fit %>%
  spread_draws(beta[n]) %>%
  ggplot(aes(x = beta, y = n)) +
  geom_halfeyeh(.width = .95) +
  facet_wrap(
    ~ n,
    nrow = 3,
    scales = "free"
  ) +
  geom_vline(aes(xintercept = beta), par_values, color = "red")

ggsave(
  "marginals-01b.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 5, units = "in"
)

# 02 Multiple Hierarchical Regression -------------------------------------
# Hierarchical regression with covariates and known variance.

# Specify data and hyperparameter values.
sim_values <- list(
  N = 100,                            # Number of individuals.
  K = 3,                              # Number of groups.
  I = 4,                              # Number of observation-level covariates.
  g = sample(3, 100, replace = TRUE), # Vector of group assignments.
  mu = 5,                             # Mean of the population model.
  tau = 1                             # Variance of the population model.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_data_02.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data and group intercepts.
sim_y <- extract(sim_data)$y
sim_X <- extract(sim_data)$X
sim_Beta <- extract(sim_data)$Beta

# Specify data.
data <- list(
  N = sim_values$N,                  # Number of individuals.
  K = sim_values$K,                  # Number of groups.
  I = sim_values$I,                  # Number of observation-level covariates.

  # Matrix of observation-level covariates.
  X = matrix(
    as.vector(sim_X),
    nrow = sim_values$N,
    ncol = sim_values$I
  ),

  y = as.vector(sim_y),              # Vector of observations.
  g = sim_values$g                   # Vector of group assignments.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "hierarchical_regression_02.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Diagnostics.
source(here::here("content", "post", "stan-hierarchical", "Code", "stan_utility.R"))
check_all_diagnostics(fit)

# Check trace plots.
par_string <- str_c("Beta[", 1:data$K, ",", 1, "]")
for (i in 2:data$I) {
  temp <- str_c("Beta[", 1:data$K, ",", i, "]")
  par_string <- c(par_string, temp)
}
fit %>%
  mcmc_trace(
    pars = c("mu", "tau", par_string),
    n_warmup = 500,
    facet_args = list(
      nrow = (data$I * data$K + 2) / 2,
      ncol = 2,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-02.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 6, units = "in"
)

# Recover hyperparameter and parameter values.
hyperpar_values <- tibble(
  .variable = c("mu", "tau"),
  values = c(sim_values$mu, sim_values$tau),
)

par_values <- tibble(
  n = sort(rep(1:(data$K), data$I)),
  i = rep(1:(data$I), data$K),
  .variable = str_c("Beta", "_", n, "_", i),
  values = as.vector(t(matrix(sim_Beta, ncol = data$I)))
) %>%
  select(.variable, values)

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

ggsave(
  "marginals-02a.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

fit %>%
  gather_draws(Beta[n, i]) %>%
  unite(.variable, .variable, n, i) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = data$K,
    ncol = data$I,
    scales = "free"
  )

ggsave(
  "marginals-02b.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 10, height = 5, units = "in"
)

# # Check Stan code using brms.
# get_prior(
#   y ~ (1 | g),
#   data = data,
#   family = gaussian()
# )
#
# test_data <- make_standata(
#   y ~ (1 | g),
#   data = data,
#   family = gaussian(),
#   prior = c(
#     prior(normal(0, 5), class = Intercept),
#     prior(cauchy(0, 2.5), class = sigma)
#   )
# )
#
# make_stancode(
#   y ~ (1 | g),
#   data = data,
#   family = gaussian(),
#   prior = c(
#     prior(normal(0, 5), class = Intercept),
#     prior(cauchy(0, 2.5), class = sigma)
#   )
# )
