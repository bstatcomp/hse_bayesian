data {
  int n;
  real y[n];
  real mu_min;
  real mu_max;
}

parameters {
  real<lower=mu_min,upper=mu_max> mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
}

