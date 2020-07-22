# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(loo)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generate Data -----------------------------------------------------------
# Specify data and hyperprior values.
sim_values <- list(
  R = 500,           # Number of respondents.
  S = 10,            # Number of choice tasks.
  A = 4,             # Number of choice alternatives.
  I = 12,            # Number of observation-level covariates.
  J = 3,             # Number of population-level covariates.

  Gamma_mean = 0,    # Mean of population-level means.
  Gamma_scale = 5,   # Scale of population-level means.
  Omega_shape = 2,   # Shape of population-level scale.
  tau_df = 2         # Degrees of freedom of population-level scale.
)

# Array of observation-level covariates.
X <- array(
  NA,
  dim = c(sim_values$R, sim_values$S, sim_values$A, sim_values$I)
)
for (r in 1:sim_values$R) {
  for (s in 1:sim_values$S) {
    X[r, s, , ] <- matrix(
      round(runif(sim_values$A * sim_values$I)),
      nrow = sim_values$A,
      ncol = sim_values$I
    )
  }
}
sim_values$X <- X

# Matrix of population-level covariates.
Z <- cbind(
  rep(1, sim_values$R),
  matrix(
    runif(sim_values$R * (sim_values$J - 1), min = 2, max = 5),
    nrow = sim_values$R
  )
)
sim_values$Z <- Z

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "choice-models", "Code", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Save simulation values and data.
write_rds(
  sim_values,
  path = here::here("content", "post", "choice-models", "Data", "sim_values.rds")
)
write_rds(
  sim_data,
  path = here::here("content", "post", "choice-models", "Data", "sim_data.rds")
)

# Load simulation values and data.
# sim_data <- read_rds(here::here("content", "post", "choice-models", "Data", "old_sim_data.rds"))
sim_values <- read_rds(here::here("content", "post", "choice-models", "Data", "sim_values.rds"))
sim_data <- read_rds(here::here("content", "post", "choice-models", "Data", "sim_data.rds"))

# Extract simulated data and parameters.
sim_Y <- extract(sim_data)$Y[1,,]
sim_Gamma <- extract(sim_data)$Gamma[1,,]
sim_Omega <- extract(sim_data)$Omega[1,,]
sim_tau <- extract(sim_data)$tau[1,]
sim_Beta <- extract(sim_data)$Beta[1,,]

# Centered Parameterization -----------------------------------------------
data <- list(
  R = sim_values$R,    # Number of respondents.
  S = sim_values$S,    # Number of choice tasks.
  A = sim_values$A,    # Number of choice alternatives.
  I = sim_values$I,    # Number of observation-level covariates.
  J = sim_values$J,    # Number of population-level covariates.

  Gamma_mean = 0,      # Mean of population-level means.
  Gamma_scale = 5,     # Scale of population-level means.
  Omega_shape = 2,     # Shape of population-level scale.
  tau_mean = 0,        # Mean of population-level scale.
  tau_scale = 5,       # Scale of population-level scale.

  Y = sim_Y,           # Matrix of observations.
  X = sim_values$X,    # Array of observation-level covariates.
  Z = sim_values$Z     # Matrix of population-level covariates.
)

fit_centered <- stan(
  file = here::here("content", "post", "choice-models", "Code", "hmnl_centered.stan"),
  data = data,
  # iter = 6000,
  # thin = 3,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model output.
write_rds(
  fit_centered,
  here::here("content", "post", "choice-models", "Output", "hmnl-centered_02.rds")
)

# Load model output.
fit_centered <- read_rds(
  here::here("content", "post", "choice-models", "Output", "hmnl-centered_fit.rds")
)

# Model fit.
loo(fit_centered)

# Check trace plots.
fit_centered %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_centered_01.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 6, units = "in"
)

# Recover parameter values.
Theta <- tibble(i = as.factor(1:ncol(sim_data$Theta)), Theta = t(sim_data$Theta))

draws_centered <- fit_centered %>%
  spread_draws(Theta[i, j]) %>%
  select(.chain, .iteration, .draw, i, j, Theta) %>%
  ungroup()

draws_centered %>%
  ggplot(aes(x = Theta)) +
  geom_halfeyeh(.width = c(.95, .95)) +
  facet_wrap(
    ~ as.factor(i),
    nrow = 3,
    ncol = 4,
    scales = "free_x"
  ) +
  geom_vline(aes(xintercept = Theta), Theta, color = "red")

ggsave(
  "mcmc_marginals_centered_01.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Non-Centered Parameterization -------------------------------------------
# Specify the data for calibration in a list.
data <- list(
  N = nrow(sim_data$Y),    # Number of respondents.
  S = ncol(sim_data$Y),    # Number of choice tasks per respondent.
  P = dim(sim_data$X)[3],  # Number of product alternatives per choice task.
  L = dim(sim_data$X)[4],  # Number of (estimable) attribute levels.
  C = ncol(sim_data$Z),    # Number of respondent-level covariates.

  Theta_mean = 0,          # Mean of coefficients for the heterogeneity model.
  Theta_scale = 1,         # Scale of coefficients for the heterogeneity model.
  alpha_mean = 0,          # Mean of scale for the heterogeneity model.
  alpha_scale = 10,        # Scale of scale for the heterogeneity model.
  lkj_corr_shape = 5,      # Shape of correlation matrix for the heterogeneity model.

  Y = sim_data$Y,          # Matrix of observed choices.
  X = sim_data$X,          # Array of experimental designs per choice task.
  Z = sim_data$Z           # Matrix of respondent-level covariates.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "choice-models", "Code", "hmnl_noncentered.stan"),
  data = data,
  seed = 42
)

# Save model output.
write_rds(fit, here::here("content", "post", "choice-models", "Output", "hmnl-noncentered_01.rds"))

# Conjugate Parameterization ----------------------------------------------
# Specify the data for calibration in a list.
Data = list(
  N = nrow(sim_data$Y),    # Number of respondents.
  S = ncol(sim_data$Y),    # Number of choice tasks per respondent.
  P = dim(sim_data$X)[3],  # Number of product alternatives per choice task.
  L = dim(sim_data$X)[4],  # Number of (estimable) attribute levels.
  C = ncol(sim_data$Z),    # Number of respondent-level covariates.

  y = sim_data$Y_list,     # List of choices.
  X = sim_data$X_list,     # List of design matrices.
  Z = sim_data$Z           # Covariates for the upper-level model.
)

# Specify the prior for calibration in a list.
Prior = list(
  gammabar = matrix(rep(0, Data$C * Data$L), ncol = Data$L), # Means for normal prior on Gamma.
  Agamma = 0.01 * diag(Data$C),                              # Precision matrix for normal prior on Gamma.
  nu = Data$L + 3,                                           # DF for IW prior on Vbeta.
  V = (Data$L + 3) * diag(Data$L)                            # Location for IW prior on Vbeta.
)

# Specify the MCMC parameters in a list.
Mcmc = list(
  R = 20000,               # Number of iterations in the Markov chain.
  keep = 20,               # Thinning parameter.
  step = .08,              # RW step (scaling factor) for the beta draws.
  cont_ind = 0             # Indicates a run continuation.
)

# Calibrate the model.
source(here::here("content", "post", "choice-models", "Code", "hmnl_conjugate.R"))
fit <- hier_mnl(Data, Prior, Mcmc)

# Save model output.
write_rds(fit, here::here("content", "post", "choice-models", "Output", "hmnl-conjugate_01.rds"))

# Model Fit ---------------------------------------------------------------
# Load model output.
fit_noncentered <- read_rds(here::here("Output", "hmnl-noncentered_fit.RDS"))
fit_conjugate <- read_rds(here::here("Output", "hmnl-conjugate-20k_fit.RDS"))
colnames(fit_conjugate$Gammadraw) <-
  c(
    "Theta[1,1]", "Theta[2,1]", "Theta[3,1]", "Theta[4,1]", "Theta[5,1]", "Theta[6,1]",
    "Theta[7,1]", "Theta[8,1]", "Theta[9,1]", "Theta[10,1]", "Theta[11,1]", "Theta[12,1]"
  )


# Check trace plots.
fit_noncentered %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_noncentered.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

fit_conjugate$Gammadraw %>%
  mcmc_trace(
    n_warmup = 500,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_conjugate.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Recover parameter values.
Theta <- tibble(i = as.factor(1:ncol(sim_data$Theta)), Theta = t(sim_data$Theta))

draws_centered <- fit_centered %>%
  spread_draws(Theta[i, j]) %>%
  mutate(model = "centered") %>%
  select(model, .chain, .iteration, .draw, i, j, Theta) %>%
  ungroup()

draws_noncentered <- fit_noncentered %>%
  spread_draws(Theta[i, j]) %>%
  mutate(model = "noncentered") %>%
  select(model, .chain, .iteration, .draw, i, j, Theta) %>%
  ungroup()

draws_conjugate <- as_tibble(fit_conjugate$Gammadraw) %>%
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>%
  gather(key = i, value = Theta, -c(.draw, .iteration)) %>%
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>%
  separate(col = i, into = c("i", "j"), sep = ",") %>%
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>%
  mutate(
    model = "conjugate",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>%
  select(model, .chain, .iteration, .draw, i, j, Theta) %>%
  arrange(.iteration) %>%
  filter(.iteration > 500)

draws <- draws_centered %>%
  bind_rows(draws_noncentered) %>%
  bind_rows(draws_conjugate)

draws %>%
  mutate(
    model = factor(model),
    model = fct_relevel(
      model, "noncentered", "centered", "conjugate"
    )
  ) %>%
  ggplot(aes(x = Theta, y = model)) +
  geom_halfeyeh(.width = c(.95, .95)) +
  facet_wrap(
    ~ as.factor(i),
    nrow = 3,
    ncol = 4,
    scales = "free_x"
  ) +
  geom_vline(aes(xintercept = Theta), Theta, color = "red")

ggsave(
  "mcmc_marginal_posteriors.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

