// generated with brms 2.9.0
// functions {
// }
data {
  int<lower=1> N;      // number of observations
  vector[N] Y;         // response variable
  // data for group-level effects of ID 1
  int<lower=1> N_1;    // number of groups
  // int<lower=1> M_1;    // 1
  int<lower=1> J_1[N]; // group assignment
  // vector[N] Z_1_1;     // vector of 1's
  // int prior_only;      // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real temp_Intercept;        // temporary intercept
  real<lower=0> sigma;        // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];       // unscaled group-level effects
}
transformed parameters {
  // group-level effects
  vector[N_1] r_1_1 = (sd_1[1] * (z_1[1]));
}
model {
  vector[N] mu = temp_Intercept + rep_vector(0, N);
  for (n in 1:N) {
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
  }
  // priors including all constants
  target += student_t_lpdf(temp_Intercept | 3, 6, 10);
  target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += normal_lpdf(z_1[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = temp_Intercept;
}
