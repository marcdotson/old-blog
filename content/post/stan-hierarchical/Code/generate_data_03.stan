// Hierarchical regression with covariates and unknown variance.

// Index values, observations, covariates, and hyperprior values.
data {
  int<lower = 1> N;               // Number of individuals.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.
  int<lower = 1> J;               // Number of population-level covariates.

  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  real<lower=0> tau;              // Variance of the population model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;                    // Vector of observations.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  matrix[K, J] Z;                 // Matrix of population-level covariates.
  matrix[K, I] Beta;              // Matrix of group-level coefficients.
  matrix[J, I] Gamma;             // Matrix of population-level coefficients.

  // Draw parameter values and generate data.
  for (k in 1:K) {
    for (j in 1:J) {
      if (j == 1) Z[k, j] = 1;
      if (j != 1) Z[k, j] = uniform_rng(1, 7);
    }
  }
  for (j in 1:J) {
    for (i in 1:I) {
      Gamma[j, i] = normal_rng(0, 5);
    }
  }
  for (k in 1:K) {
    for (i in 1:I) {
      Beta[k, i] = normal_rng(Z[k,] * Gamma[,i], tau);
    }
  }

  // Generate data, both X (including the intercept) and y.
  for (n in 1:N) {
    for (i in 1:I) {
      if (i == 1) X[n, i] = 1;
      if (i != 1) X[n, i] = uniform_rng(1, 7);
    }
    y[n] = normal_rng(Beta[g[n],] * X[n,]', 1);
  }
}
