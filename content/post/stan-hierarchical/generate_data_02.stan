// Hierarchical regression with covariates and known variance.

// Index and hyperparameter values.
data {
  int<lower=1> N;     // Number of observations.
  int<lower=1> C;     // Number of individual-level covariates.
  int<lower=1> P;     // Number of population-level covariates.

  vector[3] mu;       // Vector of means for the population-level model.
  vector[3] sigma;    // Vector of variances for the population-level model.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;        // Vector of observations.
  matrix[N, L] X;     // Matrix of indicovariates.
  matrix[N, L] beta;  // Matrix of lower-level beta coefficients.

  // Draw parameter values from hyperprior, generate covariates and data.
  for (n in 1:N) {
    real beta[n, ] = normal_rng(mu, sigma);
    // X[n, ] = binomial_rng(1, 0.5);
    y[n] = normal_rng(theta[n], 1);
  }
}

// // Index and hyperparameter values.
// data {
//   int<lower=1> N;     // Number of observations.
//   real mu;            // Mean of the population-level model.
//   real<lower=0> tau;  // Variance of the population-level model.
// }
//
// // Generate data according to the hierarchical regression.
// generated quantities {
//   vector[N] y;        // Vector of observations.
//   vector[N] beta;     // Vector of individual-level coefficients.
//
//   // Draw parameter values from the prior, generate data.
//   for (n in 1:N) {
//     beta[n] = normal_rng(mu, tau);
//     y[n] = normal_rng(beta[n], 1);
//   }
// }
