// Number of observations and covariates and hyperparameter values.
data {
  int<lower=0> N;     // Number of observations.
  // int<lower=0> L;     // Number of (estimable) covariates.
  real mu;            // Mean of the upper-level model.
  real<lower=0> tau;  // Variance of the upper-level model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;        // Vector of observations.
  // matrix[N, L] X;     // Matrix of covariates.
  vector[N] theta;    // Vector of lower-level theta coefficients.

  // Draw parameter values from hyperprior, generate covariates and data.
  for (n in 1:N) {
    theta[n] = normal_rng(mu, tau);
    // X[n, ] = binomial_rng(1, 0.5);
    y[n] = normal_rng(theta[n], 1);
  }
}
