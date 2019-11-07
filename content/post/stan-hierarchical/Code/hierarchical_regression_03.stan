// Hierarchical regression with covariates and unknown variance.

// Index values, observations, covariates, and hyperprior values.
data {
  int<lower = 1> N;               // Number of individuals.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.
  int<lower = 1> J;               // Number of population-level covariates.

  // real<lower=0> tau_scale;   // Scale of variance in the individual-level model.
  // real gamma_mean;           // Mean of coefficients for the population-level model.
  // real<lower=0> gamma_scale; // Scale of coefficients for the population-level model.
  // real<lower=0> Sigma_scale; // Scale of covariance for the population-level model.

  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  matrix[K, J] Z;                 // Matrix of population-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[K, I] Beta;              // Matrix of observation-level coefficients.
  // matrix[K, I] Sigma;             // Covariance matrix of the observation-level model.
  matrix[J, I] Gamma;             // Matrix of population-level coefficients.
  real<lower=0> tau;              // Variance of the population-level model.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  // Sigma ~ inv_wishart(Sigma_scale);
  for (j in 1:J) {
    Gamma[j,] ~ normal(0, 5);
  }
  tau ~ cauchy(0, 2.5);

  // Population model and likelihood.
  for (k in 1:K) {
    Beta[k,] ~ normal(Z[k,] * Gamma, tau);
  }
  for (n in 1:N) {
    // y[n] ~ normal(Beta[g[n],] * X[n,]', Sigma);
    y[n] ~ normal(X[n,] * Beta[g[n],]', 1);
  }
}

