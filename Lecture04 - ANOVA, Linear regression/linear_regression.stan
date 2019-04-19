data {
  int<lower=0> n;
  int<lower=0> k; 
  matrix[n, k] X;
  vector[n] y;
}

parameters {
  vector[k] beta; 
  real<lower=0> sigma; 
}

model {
  y ~ normal(X * beta, sigma);
}

generated quantities {
  vector[n] pred;
  pred = X * beta;
}
