parameters {
  real y_real;
}
generated data {
  real y = y_real;
}
model {
  y ~ normal(0,1);
}
