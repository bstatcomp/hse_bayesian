data {
  int n;        // total number of candy
  int m;        // number of different bags
  int k;        // number of different types of candy
  int y[n];     // type id for each piece
  int b[n];     // bag id for each piece
}

// here is a good example of when we can use the transformed data column
// instead of precomputing the number of pieces in each bag, we can just
// input the bag number for each piece of candy into Stan and then do the
// necessary adding in Stan
transformed data {
  int counts[m]; // count the number of candy in each bag
  
  for (i in 1:m) {
    counts[i] = 0;
  }
  
  for (i in 1:n) {
    counts[b[i]] = counts[b[i]] + 1;
  }
  print(counts);
}

parameters {
  real<lower=0> lambda;   // candy count rate
  
  simplex[k] candy_probs; // probs for each type of candy;
                          // a simplex is a more formal way of saying that it
                          // is a vector of numbers between 0 and 1 that need
                          // to add up to 1, that is, a vector of probabilities; using
                          // vector[k] would not be OK, because Stan would
                          // not know that these need to be probabilities - it would
                          // be equivalent to not putting a <lower=0> range constraint
                          // on scale/variance parameters.
}

model {
  // candy count part of model
  // -------------------------
  // this is just the Poisson model from before; 
  // we're basically independently modelling probabilities and count,
  // but doing it in the same Stan model
  lambda ~ uniform(5, 100);
  counts ~ poisson(lambda);
  
  // candy type part of model
  // -------------------------
  // the categorical distribution is a generalization of the Bernoilli to more
  // than 2 possible outcomes; it has K parameters - the probability of each
  // of K possible outcomes; effectively, it has only K-1 paramters, because
  // the probabilities need to sum up to 1.
  y ~ categorical(candy_probs);
  
  // NOTE: If we have n observations from a Bernoulli, we could instead
  // of modelling each separately, model their sum a Binomially distributed r.v.
  // (sum of iid Bernoulli is binomial with same theta). The end results would be
  // equivalent... Similarly, we could model the sum of iid categorical r.v. with
  // a multinomial distribution.
}
