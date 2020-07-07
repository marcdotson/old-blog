// Index values, observations, and covariates.
data {
  int<lower = 1> N;                  // Number of respondents.
  int<lower = 1> S;                  // Number of choice tasks per respondent.
  int<lower = 2> P;                  // Number of product alternatives per choice task.
  int<lower = 1> L;                  // Number of (estimable) attribute levels.
  int<lower = 1> C;                  // Number of respondent-level covariates.

  int<lower = 1, upper = P> Y[N, S]; // Matrix of observed choices.
  matrix[P, L] X[N, S];              // Array of experimental designs per choice task.
  matrix[N, C] Z;                    // Matrix of respondent-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[N, L] Beta;                 // Matrix of beta (part-worth) coefficients.
  matrix[L, C] Theta;                // Matrix of coefficients for the heterogeneity model.
  vector<lower = 0>[L] tau;          // Vector of scale parameters for the heterogeneity model.
  corr_matrix[L] Omega;              // Correlation matrix for the heterogeneity model.
}

// Deterministic transformation.
transformed parameters {
  // Covariance matrix for the heterogeneity model.
  cov_matrix[L] Sigma = quad_form_diag(Omega, tau);
}

// Hierarchical multinomial logit.
model {
  // Hyperpriors on Theta, tau, and Omega (and thus Sigma).
  to_vector(Theta) ~ normal(Theta_mean, Theta_scale);
  tau ~ cauchy(tau_mean, tau_scale);
  Omega ~ lkj_corr(Omega_shape);

  // Hierarchical multinomial logit.
  for (n in 1:N) {
    Beta[n,] ~ multi_normal(Theta * Z[n,]', Sigma);
    for (s in 1:S) {
      Y[n, s] ~ categorical_logit(X[n, s] * Beta[n,]');
    }
  }
}
