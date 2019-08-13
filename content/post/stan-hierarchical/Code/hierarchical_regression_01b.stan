// Hierarchical regression with no covariates and known variance.

// Index values and observations.
data {
  int<lower = 1> N;               // Number of individuals.
  int<lower = 1> K;               // Number of groups.
  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
}

// Parameters and hyperparameters.
parameters {
  real var_beta[K];               // Non-centered vector of group intercepts.
  real mu;                        // Mean of the population model.
  real<lower=0> tau;              // Variance of the population model.
}

// Deterministic transformation for non-centered parameterization.
transformed parameters {
  real beta[K];                   // Deterministic vector of group intercepts.
  for (k in 1:K) {
    beta[k] = tau * var_beta[k] + mu;
  }
}

// Hierarchical regression.
model {
  // Hyperpriors.
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 2.5);

  // Population model and likelihood.
  var_beta ~ normal(0, 1);
  for (n in 1:N) {
    y[n] ~ normal(beta[g[n]], 1);
  }
}
