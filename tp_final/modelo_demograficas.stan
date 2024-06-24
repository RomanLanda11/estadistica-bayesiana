data {
  int<lower=1> N; // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Respuesta binaria
  int<lower=1, upper=2> sexo_idx[N]; 
  vector[N] menor_58; 
  vector[N] menor_72; 
  vector[N] menor_77; 
  vector[N] mayor_77; 
}
parameters {
  real a; 
  real b1;
  real b2;
  real b3;
  real b4; 
}

model {
  a ~ normal(0,2);
  b1 ~ normal(0,1.5);
  b2 ~ normal(0,1.5);
  b3 ~ normal(0,1.5);
  b4 ~ normal(0,1.5);
  
  y ~ bernoulli_logit(a + b1*menor_58 + b2*menor_72  + b3*menor_77 + b4*mayor_77);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_likelihood;

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    y_rep[i] = bernoulli_logit_rng(a + b1*menor_58[i] + b2*menor_72[i]  + b3*menor_77[i] + b4*mayor_77[i]);

    // Cálculo de la log-verosimilitud
    log_likelihood[i] = bernoulli_logit_lpmf(y[i] | a + b1*menor_58[i] + b2*menor_72[i]  + b3*menor_77[i] + b4*mayor_77[i]);
  }
}

