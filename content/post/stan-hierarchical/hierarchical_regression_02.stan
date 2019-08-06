// Hierarchical regression with covariates and known variance.

// Index values, observations, covariates, and prior values.
data {
  int<lower=1> N;            // Number of observations.
  int<lower=1> K;            // Number of population-level groups.
  int<lower=1> C;            // Number of individual-level covariates.
  int<lower=1> P;            // Number of population-level covariates.

  vector[N] y;               // Vector of observations.
  matrix[N, C] X;            // Matrix of individual-level covariates.
  matrix[K, P] z;            // Matrix of population-level covariates.

  real<lower=0> tau_scale;   // Scale of variance in the individual-level model.
  real gamma_mean;           // Mean of coefficients for the population-level model.
  real<lower=0> gamma_scale; // Scale of coefficients for the population-level model.
  real<lower=0> Sigma_scale; // Scale of covariance for the population-level model.
}

// Parameters at both the individual and population-levels.
parameters {
  matrix[N, C] Beta;         // Matrix of beta coefficients.
  vector[P] gamma;           // Vector of coefficients for the heterogeneity model.
  real mu;                   // Mean of the upper-level model.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  gamma ~ multi_normal(gamma_mean, gamma_scale);
  Sigma ~ inv_wishart(Sigma_scale);

  // Population-level model and likelihood.
  for (n in 1:N) {
    Beta[n,] ~ multi_normal(Z[n,] * gamma, sigma);
    y[n] ~ normal(X[n,] * Beta[n,]', 1);
  }
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
