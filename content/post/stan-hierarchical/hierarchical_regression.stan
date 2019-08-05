// Observations and covariates.
data {
  int<lower=0> N;     // Number of observations.
  vector[N] y;        // Vector of observations.
  // int<lower=0> C;     // Number of covariates.
  // matrix[N, C] X;     // Matrix of covariates.
}

// Parameters for the hierarchical regression.
parameters {
  real mu;            // Mean of the upper-level model.
  real<lower=0> tau;  // Variance of the upper-level model.
  vector[N] theta;    // Vector of lower-level theta coefficients.
}

// Hierarchical regression.
model {
  // Upper-level hyperpriors.
  mu ~ normal(0, 5);
  tau ~ normal(0, 5);

  // Lower-level prior and model.
  theta ~ normal(mu, tau);
  y ~ normal(theta, 1);
}

// generated quantities {
//   vector[N] y_ppc;
//   for (n in 1:N)
//     y_ppc[n] = normal_rng(theta[n], sigma);
// }
