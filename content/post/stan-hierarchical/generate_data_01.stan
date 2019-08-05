// Hierarchical regression with no covariates and known variance.

// Index and hyperparameter values.
data {
  int<lower=1> N;     // Number of observations.
  real mu;            // Mean of the population-level model.
  real<lower=0> tau;  // Variance of the population-level model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;        // Vector of observations.
  vector[N] beta;     // Vector of individual-level coefficients.

  // Draw parameter values from the prior, generate data.
  for (n in 1:N) {
    beta[n] = normal_rng(mu, tau);
    y[n] = normal_rng(beta[n], 1);
  }
}
