# Load libraries.
library(tidyverse)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load simulated data.
sim_data <- readRDS(here::here("Data", "sim_data.RDS"))

# Extract the data from the first simulated dataset.
Y <- extract(sim_data)$Y[1,]
X <- extract(sim_data)$X[1,,,]

# Specify the data for calibration in a list.
data <- list(
  N = length(Y),           # Number of observations.
  P = nrow(X[1,,]),        # Number of product alternatives.
  L = ncol(X[1,,]),        # Number of (estimable) attribute levels.
  Y = Y,                   # Vector of observed choices.
  X = X                    # Experimental design for each observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("Code", "mnl_estimate.stan"),
  data = data,
  seed = 42
)

# Check divergences.
library(bayesplot)
source(here::here("Code", "stan_utility.R"))

check_div(fit)

as.matrix(fit) %>% 
  mcmc_scatter(
    pars = c("beta[1]", "beta[2]"), 
    np = nuts_params(fit),
    np_style = scatter_style_np(div_color = "green", div_alpha = 0.5)
  )

ggsave(
  "mcmc_scatter.png", 
  path = here::here("Figures"), 
  width = 6, height = 3, units = "in"
)

# Check the effective sample size.
check_n_eff(fit)

# Check the Rhat statistic.
check_rhat(fit)

# Check trace plots.
fit %>% 
  extract(
    inc_warmup = TRUE, 
    permuted = FALSE
  ) %>% 
  mcmc_trace(
    pars = c(
      "beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
      "beta[6]", "beta[7]", "beta[8]", "beta[9]", "beta[10]"
    ),
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace.png", 
  path = here::here("Figures"), 
  width = 9, height = 3, units = "in"
)

# Recover parameter values.
beta <- extract(sim_data)$beta[1,]

as.array(fit) %>% 
  mcmc_areas(pars = c("beta[3]")) +
  vline_at(beta[3], color = "red")

ggsave(
  "mcmc_hist.png", 
  path = here::here("Figures"), 
  width = 6, height = 3, units = "in"
)

