data {
  int<lower=0> n;
  int<lower=0> ng;
  vector[n] y;
  int g[n];
}

parameters {
  real<lower = 0.0> s2;
  vector[ng] mu;
}

model {
  // if we don't place priors, Stan will default to flat (improper) priors
  for (i in 1:n) {
      y[i] ~ normal(mu[g[i]], sqrt(s2));
  }
}
