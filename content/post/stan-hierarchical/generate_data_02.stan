// Hierarchical regression with known variance and no covariates.

// Number of observations and covariates and hyperparameter values.
data {
  int<lower=0> N;     // Number of observations.
  int<lower=0> L;     // Number of (non-intercept) covariates.
  vector[3] mu;       // Vector of means for the upper-level model.
  vector[3] sigma;    // Vector of variance of the upper-level model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;        // Vector of observations.
  matrix[N, L] X;     // Matrix of covariates.
  matrix[N, L] beta;  // Matrix of lower-level beta coefficients.

  // Draw parameter values from hyperprior, generate covariates and data.
  for (n in 1:N) {
    real beta[n, ] = normal_rng(mu, sigma);
    // X[n, ] = binomial_rng(1, 0.5);
    y[n] = normal_rng(theta[n], 1);
  }
}
