data {
  int n;
  int y[n];
}

parameters {
  real<lower=0> lambda;
}

model {
  
  for (i in 1:n) {
    y[i] ~ poisson(lambda);
  }
  lambda ~ uniform(5, 100);
}
