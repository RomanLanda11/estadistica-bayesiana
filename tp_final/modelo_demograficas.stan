data {
  int<lower=1> N; // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Respuesta binaria
  int<lower=1, upper=2> sexo_idx[N]; 
  vector[N] edad;
}
parameters {
  //vector[2] a; 
  //vector[2] b; 
  real a; 
  real b; 
}

model {
  y ~ bernoulli_logit(a + b * edad);
}

