// Index values, hyperprior values, and covariates.
data {
  int<lower = 1> R;                  // Number of respondents.
  int<lower = 1> S;                  // Number of choice tasks.
  int<lower = 2> A;                  // Number of choice alternatives.
  int<lower = 1> I;                  // Number of observation-level covariates.
  int<lower = 1> J;                  // Number of population-level covariates.

  real Theta_mean;                   // Mean of population-level mean hyperparameters.
  real<lower=0> Theta_scale;         // Scale of population-level mean hyperparameters.
  real tau_mean;                     // Mean of population-level scale hyperparameters.
  real<lower=0> tau_scale;           // Scale of population-level scale hyperparameters.
  real<lower=0> Omega_shape;         // Shape of population-level scale correlation matrix.

  matrix[A, I] X[R, S];              // Array of experimental designs.
  matrix[R, J] Z;                    // Matrix of respondent-level covariates.
}

// Generate data according to the hierarchical multinomial logit.
generated quantities {
  int<lower = 1, upper = A> Y[R, S]; // Matrix of observations.
  matrix[R, I] Beta;                 // Matrix of beta (part-worth) coefficients.
  matrix[J, I] Theta;                // Matrix of coefficients for the heterogeneity model.
  vector<lower = 0>[I] tau;          // Vector of scale parameters for the heterogeneity model.
  corr_matrix[I] Omega;              // Correlation matrix for the heterogeneity model.

  // Draw parameter values and generate data.
  for (j in 1:J) {
    for (i in 1:I) {
      Theta[j, i] = normal_rng(Theta_mean, Theta_scale);
    }
  }
  for (i in 1:I) {
    tau[i] = cauchy_rng(tau_mean, tau_scale);
  }
  Omega = lkj_corr_rng(I, Omega_shape);
  for (r in 1:R) {
    Beta[r,] = multi_normal_rng(Z[r,] * Theta, quad_form_diag(Omega, tau))';
    for (s in 1:S) {
      Y[r, s] = categorical_logit_rng(X[r, s] * Beta[r,]');
    }
  }
}
