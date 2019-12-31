# // generated with brms 2.10.0
# functions {
# }
# data {
#   int<lower=1> N;  // number of observations
#   vector[N] Y;  // response variable
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   int<lower=1> J_1[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_1;
#   vector[N] Z_1_2;
#   vector[N] Z_1_3;
#   vector[N] Z_1_4;
#   int<lower=1> NC_1;  // number of group-level correlations
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
# }
# parameters {
#   // temporary intercept for centered predictors
#   real Intercept;
#   real<lower=0> sigma;  // residual SD
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   matrix[M_1, N_1] z_1;  // standardized group-level effects
#   // cholesky factor of correlation matrix
#   cholesky_factor_corr[M_1] L_1;
# }
# transformed parameters {
#   // actual group-level effects
#   matrix[N_1, M_1] r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
#   // using vectors speeds up indexing in loops
#   vector[N_1] r_1_1 = r_1[, 1];
#   vector[N_1] r_1_2 = r_1[, 2];
#   vector[N_1] r_1_3 = r_1[, 3];
#   vector[N_1] r_1_4 = r_1[, 4];
# }
# model {
#   // initialize linear predictor term
#   vector[N] mu = Intercept + rep_vector(0, N);
#   for (n in 1:N) {
#     // add more terms to the linear predictor
#     mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n];
#   }
#   // priors including all constants
#   target += normal_lpdf(Intercept | 0, 5);
#   target += cauchy_lpdf(sigma | 0, 2.5)
#     - 1 * cauchy_lccdf(0 | 0, 2.5);
#   target += student_t_lpdf(sd_1 | 3, 0, 248)
#     - 4 * student_t_lccdf(0 | 3, 0, 248);
#   target += normal_lpdf(to_vector(z_1) | 0, 1);
#   target += lkj_corr_cholesky_lpdf(L_1 | 1);
#   // likelihood including all constants
#   if (!prior_only) {
#     target += normal_lpdf(Y | mu, sigma);
#   }
# }
# generated quantities {
#   // actual population-level intercept
#   real b_Intercept = Intercept;
#   // group-level correlations
#   corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
#   vector<lower=-1,upper=1>[NC_1] cor_1;
#   // extract upper diagonal of correlation matrix
#   for (k in 1:M_1) {
#     for (j in 1:(k - 1)) {
#       cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
#     }
#   }
# }
