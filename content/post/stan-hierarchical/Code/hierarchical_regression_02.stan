// Hierarchical regression with covariates and known variance.

// Index values, observations, and covariates.
data {
  int<lower = 1> N;               // Number of individuals.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.

  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[K, I] Beta;              // Matrix of observation-level coefficients.
  real mu;                        // Mean of the population model.
  real<lower=0> tau;              // Variance of the population model.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 2.5);

  // Population model and likelihood.
  for (k in 1:K) {
    Beta[k,] ~ normal(mu, tau);
  }
  for (n in 1:N) {
    y[n] ~ normal(Beta[g[n],] * X[n,]', 1);
  }
}
