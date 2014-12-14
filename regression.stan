data {
  int N ;
  int K ;
  matrix[N, K] X ;
  vector[N] y ;
}
parameters {
  real<lower=0> sigma ;
  vector[K] beta;
}
model {
  beta ~ normal(0, 5) ;
  sigma ~ cauchy(0, 5) ;
  y ~ normal(X*beta, sigma) ;
}