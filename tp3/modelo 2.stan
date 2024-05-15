//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}
transformed data{
  real y_mean = mean(y);
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  beta0 ~ normal(y_mean, 10);
  beta1 ~ normal(0, 0.5);
  sigma ~ normal(0,5); // no es normal 0, sigma 5,se miran restricciones(sigma positivo), es medianormal pero stan no la tiene
  y ~ normal(beta0+beta1*x, sigma);
}
generated quantities {
  vector[N] mu = beta0 + beta1*x;
}