// Hierarchical regression with covariates and known variance.

// Index values, observations, covariates, and hyperprior values.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> C;               // Number of observations-level covariates.
  int<lower = 1> J;               // Number of population-level covariates.

  real<lower=0> tau_scale;   // Scale of variance in the individual-level model.
  real gamma_mean;           // Mean of coefficients for the population-level model.
  real<lower=0> gamma_scale; // Scale of coefficients for the population-level model.
  real<lower=0> Sigma_scale; // Scale of covariance for the population-level model.

  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, C] X;                 // Matrix of observation-level covariates.
  matrix[K, J] Z;                 // Matrix of population-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[N, C] Beta;         // Matrix of individual-level coefficients.
  matrix[K, J] Gamma;        // Matrix of population-level coefficients.
  real<lower=0> tau;         // Variance of the population-level model.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  Gamma ~ normal(0, 5);
  tau ~ normal(0, 5);
  // gamma ~ multi_normal(gamma_mean, gamma_scale);
  // Sigma ~ inv_wishart(Sigma_scale);

  // Population-level model and likelihood.
  Beta ~ multi_normal(Z * Gamma, tau);
  for (n in 1:N) {
    y[n] ~ normal(X[n,] * Beta[n,], 1);
  }
}
