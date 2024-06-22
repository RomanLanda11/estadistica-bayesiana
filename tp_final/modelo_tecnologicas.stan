data {
  int<lower=1> N; // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Respuesta binaria
  int<lower=1, upper=3> resonador_idx[N]; // Índice del resonador (1 = GE, 2 = Phillips, 3 = Siemens)
  int<lower=1, upper=2> teslas_idx[N]; // Índice de teslas (1 para 1.5T, 2 para 3T)
}

parameters {
  vector[3] a; // Intercepto para cada resonador
  //vector[2] b; // Coeficiente para cada nivel de teslas
}

model {
  // Priors
  a ~ normal(0, 1.5); // Prior para interceptos
  //b ~ normal(0, 1.5); // Prior para coeficientes

  y ~ bernoulli_logit(a[resonador_idx]);// + b[teslas_idx]);
  //y ~ bernoulli_logit(b[teslas_idx]);
}