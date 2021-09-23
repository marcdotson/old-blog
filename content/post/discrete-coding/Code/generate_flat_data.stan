// Index and parameter values.
data {
  int<lower = 1> N;    // Number of observations.
  int<lower = 1> I;    // Number of covariates.
  matrix[N, I] X;      // Matrix of covariates.

  real alpha;          // Intercept.
  vector[I - 1] beta;  // Vector of slopes.
  real<lower = 0> tau; // Variance of the regression.
}

// Generate data according to the simple regression.
generated quantities {
  // Vector of observations.
  vector[N] y;

  // Generate data.
  for (n in 1:N) {
    y[n] = normal_rng(alpha + X[n,] * beta, tau);
  }
}