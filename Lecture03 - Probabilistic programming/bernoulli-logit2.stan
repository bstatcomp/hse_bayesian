data {
  int n;
  int y[n];
}

parameters {
  real<lower=0,upper=1> theta;
  real beta;
}

model {
  
  for (i in 1:n) {
    y[i] ~ bernoulli(inv_logit(beta * (i - 1) + theta));
  }
  
  theta ~ beta(1,1);
}

generated quantities {
  real probs[n];
  for (i in 1:n) {
    probs[i] = inv_logit(beta * (i - 1) + theta);
  }
}
