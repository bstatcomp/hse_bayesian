data {
  int<lower=0> n;
  vector[n] y;
}

parameters {
  real<lower = 0.0> s2;
  real mu;
}

model {
  // if we don't place priors, Stan will default to flat (improper) priors
  y ~ normal(mu, sqrt(s2));
}
