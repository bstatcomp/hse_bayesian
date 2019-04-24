data {
  int<lower=0> n;
  int<lower=0> m;
  int<lower=0> k; 
  matrix[n, k] X;
  matrix[m, k] X_test;
  vector[n] y;
}

parameters {
  real alpha; 
  vector[k] beta; 
  real<lower=0> sigma; 
  real<lower=0> lambda;
}

model {
  y ~ normal(X * beta + alpha, sigma);
  beta ~ normal(0, lambda);
}

generated quantities {
  vector[n] pred;
  vector[m] pred_test;
  pred = X * beta + alpha;
  pred_test = X_test * beta + alpha;
}
