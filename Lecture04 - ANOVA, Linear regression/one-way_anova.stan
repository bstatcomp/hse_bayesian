data {
  int n; // total number of observations
  int k; // number of groups
  int  id[n];
  real weight[n];
  real mu_min;
  real mu_max;
}

parameters {
  real<lower=mu_min,upper=mu_max> mu[k];
  real<lower=0> sigma;
}

model {
  for (i in 1:n) {
    weight[i] ~ normal(mu[id[i]], sigma);
  }
}

