// Index values, observations, and covariates.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.
  // int<lower = 1> J;               // Number of population-level covariates.

  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  // matrix[K, J] Z;                 // Matrix of population-level covariates.
}

// Parameters and hyperparameters.
parameters {
  // matrix[J, I] Gamma;             // Matrix of population-level coefficients.
  real mu;                        // Mean of the population model.
  real<lower = 0> tau;            // Variance of the population model.
  matrix[K, I] Delta;             // Matrix of non-centered observation-level coefficients.
  // real<lower = 0> sigma;          // Variance of the likelihood.
  real<lower = 0> sigma[K];       // Vector of population-level variances.
}

// Deterministic transformation.
transformed parameters {
  // Matrix of centered observation-level coefficients.
  matrix[K, I] Beta;

  // Non-centered parameterization.
  for (k in 1:K) {
    // Beta[k,] = Z[k,] * Gamma + tau * Delta[k,];
    Beta[k,] = mu + tau * Delta[k,];
  }
}

// Hierarchical regression.
model {
  // Hyperpriors.
  // for (j in 1:J) {
  //   Gamma[j,] ~ normal(0, 5);
  // }
  mu ~ normal(0, 5);
  tau ~ normal(0, 5);

  // Prior.
  // sigma ~ normal(0, 5);

  // Non-centered population model and likelihood.
  for (k in 1:K) {
    sigma[k] ~ normal(0, 5);
    Delta[k,] ~ normal(0, 1);
  }
  for (n in 1:N) {
    y[n] ~ normal(X[n,] * Beta[g[n],]', sigma);
  }
}

// Quantities conditioned on parameter draws.
generated quantities {
  // Log likelihood to estimate loo.
  vector[N] log_lik;
  for (n in 1:N) {
    // log_lik[n] = normal_lpdf(y[n] | X[n,] * Beta[g[n],]', sigma);
    log_lik[n] = normal_lpdf(y[n] | X[n,] * Beta[g[n],]', sigma[g[n]]);
  }
}
