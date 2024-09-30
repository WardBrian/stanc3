data {
  int<lower=0> N; // # observations
  vector[N] y; // observations
}
parameters {
  vector[2] mu; // component locations
  vector<lower=0>[2] sigma; // component scales
  real<lower=0, upper=1> lambda; // mixture ratio
}
generated data {
  // sample z ~ p(z | y, lambda, mu, sigma)
  array[N] int<lower=1, upper=2> z;
  for (n in 1 : N) {
    real log_z_eq_1 = log(lambda) + normal_lpdf(y[n] | mu[1], sigma[1]);
    real log_z_eq_2 = log1m(lambda) + normal_lpdf(y[n] | mu[2], sigma[2]);
    real log_p = log_z_eq_1 - log_sum_exp(log_z_eq_1, log_z_eq_2);
    z[n] = 1 + bernoulli_rng(exp(log_p));
  }
}
model {
  // target += p(z | lambda, N)
  sum(z) ~ binomial(N, lambda);
  
  // target += p(y | mu, sigma, z)
  y ~ normal(mu[z], sigma[z]);
  
  // target += p(mu, sigma, lambda)
  mu ~ normal(0, 1);
  sigma ~ exponential(1);
  lambda ~ beta(2, 2);
}
