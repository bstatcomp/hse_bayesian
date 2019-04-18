data {
  int n;
  int y[n];
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
  
  for (i in 1:n) {
    y[i] ~ bernoulli(theta);
  }
  
  theta ~ beta(1,1);
}
