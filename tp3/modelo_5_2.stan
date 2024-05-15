// Para definir los parametros necesitamos explicitarle el tipo,
// dado que nuestros parametros son vectores de reales, necesitamos
// tambien especificarle el largo del vector
// la primera linea del data indica que el largo del vector tiene que ser mayor a 1
// modelo ej 5

data {
int<lower=0> N;
vector[N] y;
vector[N] x;
}

// Definimos los parametros, de que tipo son, y sus cotas
parameters {
real beta0;
real beta1;
real<lower=0> sigma;
}

// Pasamos el modelo
model {
  beta0 ~  normal(15,8);
  beta1 ~  normal(0,3);
  sigma ~  normal(0,);
  y ~ normal(beta0 + beta1 * x, sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] y_rep ;
  vector[N] log_likelihood ;
  mu=beta0+beta1*x;
  for (i in 1:N){
    // obtencion de muestras de la dist predictiva a posteriori
    y_rep[i]= normal_rng(mu[i], sirgma);
    // calcula la log verosimiliutd
    log_likelihood[i]=normal_lpdf(y[i]|mu[i], sigma);
  }
}