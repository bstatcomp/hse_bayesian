data {
  int n;
  int y[n];
}

parameters {
  real theta;
  real beta;
}

model {
  
  for (i in 1:n) {
    y[i] ~ bernoulli(inv_logit(beta * (i - 1) + theta));
  }
}
