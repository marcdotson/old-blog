// Hierarchical regression with no covariates and known variance.

// Index values and observations.
data {
  int<lower=1> N;     // Number of observations.
  vector[N] y;        // Vector of observations.
}

// Parameters at both the individual and population-levels.
parameters {
  real mu;            // Mean of the population-level model.
  real<lower=0> tau;  // Variance of the population-level model.
  vector[N] beta;     // Vector of individual-level coefficients.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  mu ~ normal(0, 5);
  tau ~ normal(0, 5);

  // Population-level model and likelihood.
  beta ~ normal(mu, tau);
  y ~ normal(beta, 1);
}
