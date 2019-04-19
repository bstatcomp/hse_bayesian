data {
  int<lower=1> d;
  int<lower=0> n;
  matrix[n,d] y;
}

parameters {
  cov_matrix[d] Sigma;
  vector[d] mu;
}

model {
  mu ~ multi_normal(rep_vector(0, d), diag_matrix(rep_vector(1000,d)));
  Sigma ~ inv_wishart(d + 2, 2 * diag_matrix(rep_vector(1,d)));
  for (i in 1:n) {
    y[i] ~ multi_normal(mu, Sigma);
  }
}

generated quantities {
  corr_matrix[d] corr;
  matrix[d,d] tmp;
  vector[d] tmpD;
  
  tmpD = diagonal(Sigma);
  for (i in 1:d) tmpD[i] = sqrt(tmpD[i]);
  tmp = inverse(diag_matrix(tmpD));
  corr = tmp * Sigma * tmp;
}
