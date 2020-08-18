// Index values, hyperprior values, observations, and covariates.
data {
  int<lower = 1> R;                  // Number of respondents.
  int<lower = 1> S;                  // Number of choice tasks.
  int<lower = 2> A;                  // Number of choice alternatives.
  int<lower = 1> I;                  // Number of observation-level covariates.
  int<lower = 1> J;                  // Number of population-level covariates.

  real Gamma_mean;                   // Mean of population-level means.
  real<lower=0> Gamma_scale;         // Scale of population-level means.
  real<lower=0> Omega_shape;         // Shape of population-level scale.
  real tau_mean;                     // Mean of population-level scale.
  real<lower=0> tau_scale;           // Scale of population-level scale.

  int<lower = 1, upper = A> Y[R, S]; // Matrix of observations.
  matrix[A, I] X[R, S];              // Array of observation-level covariates.
  matrix[R, J] Z;                    // Matrix of population-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[J, I] Gamma;                // Matrix of population-level hyperparameters.
  corr_matrix[I] Omega;              // Population model correlation matrix hyperparameters.
  vector<lower = 0>[I] tau;          // Population model vector of scale hyperparameters.
  matrix[R, I] Delta;                // Matrix of non-centered observation-level parameters.

  // cholesky_factor_corr[I] Omega;           // Cholesky factorization for heterogeneity covariance.
  // vector<lower=0, upper=pi()/2>[I] tau_unif; // Initialized parameter value for a Cauchy draw.
  // matrix[I, R] alpha;                        // Standard deviations for heterogeneity covariance.
}

// Deterministic transformation.
transformed parameters {
  // Matrix of centered observation-level parameters.
  matrix[R, I] Beta;

  // Non-centered parameterization.
  for (r in 1:R) {
    Beta[r,] = Z[r,] * Gamma + Delta[r,] * quad_form_diag(Omega, tau);
  }

  // matrix[R, I] Beta;                              // Matrix of Beta coefficients.
  // vector<lower=0>[I] tau;                         // Scale for heterogeneity covariance.
  // for (l in 1:I) tau[l] = 2.5 * tan(tau_unif[l]); // Inverse probability Cauchy draw.
  //
  // // Draw of Beta following non-centered parameterization.
  // Beta = Gamma * Z + (diag_pre_multiply(tau,Omega) * alpha)';
}

// Hierarchical multinomial logit model.
model {
  // Hyperpriors.
  to_vector(Gamma) ~ normal(Gamma_mean, Gamma_scale);
  Omega ~ lkj_corr(Omega_shape);
  tau ~ normal(tau_mean, tau_scale);

  // Non-centered population model and likelihood.
  for (r in 1:R) {
    Delta[r,] ~ normal(0, 1);
    for (s in 1:S) {
      Y[r, s] ~ categorical_logit(X[r, s] * Beta[r,]');
    }
  }

  // to_vector(alpha) ~ normal(tau_mean, tau_scale);
  // L_Omega ~ lkj_corr_cholesky(Omega_shape);
  // // Hierarchical multinomial logit.
  // for (n in 1:R) {
  //   for (s in 1:S) {
  //     Y[n, s] ~ categorical_logit(X[n, s] * Beta[n,]');
  //   }
  // }
}

// // Quantities conditioned on parameter draws.
// generated quantities {
//   // Log likelihood to estimate loo.
//   matrix[R, S] log_lik;
//   for (r in 1:R) {
//     for (s in 1:S) {
//       log_lik[r, s] = categorical_logit_lpmf(Y[r, s] | X[r, s] * Beta[r,]');
//     }
//   }
// }

// generated quantities {
//   // Yp is predicted choices for new data.
//   real Y_ppc[R, S];
//   vector[R*S] log_lik;
//   {
//     matrix[R, S] temp_log_lik;
//     for (r in 1:R) {
//       for (t in 1:S) {
//         Y_ppc[r, t] = categorical_logit_rng(X[r, t] * Beta[r]');
//         temp_log_lik[r, t] = categorical_logit_lpmf(Y[r, t] | X[r, t] * Beta[r]');
//       }
//     }
//     log_lik = to_vector(temp_log_lik);
//   }
// }
