// Hierarchical regression with covariates and known variance.

// Index values, observations, and covariates.
data {
  int<lower = 1> N;               // Number of individuals.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.

  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  real mu;                        // Mean of the population model.
  real<lower=0> tau;              // Variance of the population model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;                    // Vector of observations.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  matrix[K, I] Beta;              // Matrix of group-level coefficients.

  // Draw parameter values and generate data.
  for (k in 1:K) {
    for (i in 1:I) {
      Beta[k, i] = normal_rng(mu, tau);
    }
  }
  for (n in 1:N) {
    for (i in 1:I) {
      if (i == 1) X[n, i] = 1;
      if (i != 1) X[n, i] = uniform_rng(1, 7);
    }
    y[n] = normal_rng(Beta[g[n],] * X[n,]', 1);
  }
}
