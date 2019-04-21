data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}

parameters {
  real beta;
  real alpha;
  real<lower=0> nu;
  real<lower=0> sigma;
}

model {
  for (i in 1:n) {
    y[i] ~ student_t(nu, beta * x[i] + alpha , sigma);
  }
}
