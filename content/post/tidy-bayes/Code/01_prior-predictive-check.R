# Load libraries.
library(tidyverse)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Specify the data values for simulation in a list.
sim_values <- list(
  N = 1000,          # Number of observations.
  P = 3,             # Number of product alternatives.
  L = 2              # Number of (estimable) attribute levels.
)
# Experimental design for each observations.
X <- NULL
for (n in 1:sim_values$N) {
  X[[n]] <- matrix(rnorm(sim_values$P * sim_values$L), ncol = sim_values$L)
}
sim_values$X <- X



# # Array of observation-level covariates.
# X <- array(
#   NA,
#   dim = c(sim_values$R, sim_values$S, sim_values$A, sim_values$I)
# )
# for (r in 1:sim_values$R) {
#   for (s in 1:sim_values$S) {
#     X[r, s, , ] <- matrix(
#       round(runif(sim_values$A * sim_values$I)),
#       nrow = sim_values$A,
#       ncol = sim_values$I
#     )
#   }
# }
# sim_values$X <- X


# Specify the number of draws (i.e., simulated datasets).
R <- 1

# Simulate data.
sim_data <- stan(
  file = here::here("content", "post", "tidy-bayes", "Code", "mnl_simulate.stan"),
  data = sim_values,
  iter = R,
  warmup = 0,
  chains = 1,
  refresh = R,
  seed = 42,
  algorithm = "Fixed_param"
)

# saveRDS(sim_values, file = here::here("content", "post", "tidy-bayes", "Data", "sim_values.rds"))
# saveRDS(sim_data, file = here::here("content", "post", "tidy-bayes", "Data", "sim_data.rds"))

# Extract simulated data and parameters.
sim_x <- sim_values$X
sim_b <- extract(sim_data)$beta

# Compute the implied choice probabilities.
probs <- NULL
for (r in 1:R) {
  probs_temp <- NULL
  for (n in 1:sim_values$N) {
    exp_xb <- exp(sim_x[[n]] %*% sim_b[r,])
    max_prob <- max(exp_xb / sum(exp_xb))
    probs <- c(probs, max_prob)
  }
  probs <- cbind(probs, probs_temp)
}

# Make sure there aren't dominating alternatives.
tibble(probs) %>%
  ggplot(aes(x = probs)) +
  geom_histogram()

# ggsave(
#   "probs_plot.png",
#   path = here::here("Figures"),
#   width = 6, height = 3, units = "in"
# )

# Possible next steps:
# - Plot the uncertainty in the prior predictive distribution.
# - Utilize tidybayes and bayesplot.

# # Compute the implied choice probabilities.
# probs <- tribble(~ draw, ~ prob)
# for (r in 1:R) {
#   probs_r <- tibble(
#     draw = r,
#     prob = rep(NA, sim_values$N)
#   )
#   for (n in 1:sim_values$N) {
#     exp_xb <- exp(sim_x[r,n,,] %*% sim_b[r,])
#     max_prob <- max(exp_xb / sum(exp_xb))
#     probs_r[n, "prob"] <- max_prob
#   }
#   probs <- bind_rows(probs, probs_r)
# }
#
# # Make sure there aren't dominating alternatives.
# probs %>%
#   ggplot(aes(x = prob)) +
#   geom_histogram() +
#   facet_wrap(~ draw)

# # Count of attribute levels in the chosen alternative.
# library(tidybayes)
# sim_data %>%
#   spread_draws(Y[n], X[n][p, l]) %>%
#   filter(Y == p) %>%
#   group_by(l) %>%
#   summarize(sum_x = sum(X)) %>%
#   ggplot(aes(x = as.factor(l), y = sum_x)) +
#   geom_col() +
#   labs(
#     title = "Count of attribute levels in the chosen alternative",
#     x = "Attribute Levels",
#     y = "Count"
#   ) +
#   coord_flip()
