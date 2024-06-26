data {
  int<lower=1> N; // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Respuesta binaria
  vector[N] menor_58; 
  vector[N] menor_72; 
  vector[N] menor_77; 
  vector[N] mayor_77;
  
  vector[N] hipocampo;        
  vector[N] sup_frontal;
  
  int<lower=1, upper=2> teslas_idx[N]; // Índice de teslas (1 para 1.5T, 2 para 3T)
}
parameters {
  real a; 
  real e1;
  real e2;
  real e3;
  real e4; 
  
  real m1;
  real m4;
  
  vector[2] t; // Coeficiente para cada nivel de teslas
}

model {
  a ~ normal(0,2);
  e1 ~ normal(0,1.5);
  e2 ~ normal(0,1.5);
  e3 ~ normal(0,1.5);
  e4 ~ normal(0,1.5);
  
  m1 ~ normal(0,1);
  m4 ~ normal(0,1);
  
  y ~ bernoulli_logit(a + e1*menor_58 + e2*menor_72  + e3*menor_77 + e4*mayor_77 + m1*hipocampo  + m4*sup_frontal + t[teslas_idx]);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_likelihood;

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    //y_rep[i] = bernoulli_logit_rng(a[resonador_idx[i]]);
    y_rep[i] = bernoulli_logit_rng(a + e1*menor_58[i] + e2*menor_72[i]  + e3*menor_77[i] + e4*mayor_77[i] + m1*hipocampo[i]  + m4*sup_frontal[i] + t[teslas_idx[i]]);

    // Cálculo de la log-verosimilitud
    log_likelihood[i] = bernoulli_logit_lpmf(y[i] | a + e1*menor_58[i] + e2*menor_72[i]  + e3*menor_77[i] + e4*mayor_77[i] + m1*hipocampo[i]  + m4*sup_frontal[i] + t[teslas_idx[i]]);
  }
}