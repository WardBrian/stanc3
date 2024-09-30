data {
  real<lower=-1, upper=1> rho_ss;
  vector[2] loc_ss;
  vector<lower=0>[2] scale_ss;
  int<lower=0> N;
  int<lower=0, upper=N> n;
}
transformed data {
  cov_matrix[2] Sigma_ss = [[scale_ss[1] ^ 2,
                             scale_ss[1] * scale_ss[2] * rho_ss],
                            [scale_ss[1] * scale_ss[2] * rho_ss,
                             scale_ss[2] ^ 2]];
}
parameters {
  real<lower=0, upper=1> prevalence;
}
generated data {
  vector<lower=0, upper=1>[2] sens_spec = inv_logit(multi_normal_rng(
                                                    loc_ss, Sigma_ss));
}
model {
  real sens = sens_spec[1];
  real spec = sens_spec[2];
  real pos_test_prob = prevalence * sens + (1 - prevalence) * (1 - spec);
  n ~ binomial(N, pos_test_prob);
  prevalence ~ beta(2, 100);
}
