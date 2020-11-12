data {
  int N;
  real y[N];
}

parameters {
  real theta;
}

model {
  theta ~ normal(0,1);
  y ~ normal(theta,1);
}
