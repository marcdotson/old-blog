// Aggregate multinomial logit for modeling conjoint data.

// Observed choices and the experimental design.
data {
  int N;             // Number of observations.
  int P;             // Number of product alternatives.
  int L;             // Number of (estimable) attribute levels.

  int Y[N];          // Vector of observed choices.
  matrix[P, L] X[N]; // Array of experimental designs per observations.
}

// Parameters for the multinomial logit.
parameters {
  vector[L] beta;    // Vector of aggregate beta coefficients.
}

// Multinomial logit model.
model {
  // Standard normal prior for beta.
  beta ~ normal(0, 5);

  // Multinomial logit.
  for (n in 1:N) {
    Y[n] ~ categorical_logit(X[n] * beta);
  }
}

// // Generate draws from the posterior predictive distribution.
// generated quantities {
//   // Vector of predictions.
//   int Y_new[N];
//
//   // Generate a prediction for each observation.
//   for (n in 1:N) {
//     Y_new[n] = categorical_logit_rng(X[n] * beta);
//   }
// }
