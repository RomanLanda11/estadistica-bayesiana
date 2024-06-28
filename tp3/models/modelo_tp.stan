// Para definir los parametros necesitamos explicitarle el tipo,
// dado que nuestros parametros son vectores de reales, necesitamos
// tambien especificarle el largo del vector
// la primera linea del data indica que el largo del vector tiene que ser mayor a 1
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
  beta0 ~  normal(2.7,0.1);
  beta1 ~  normal(-0.15, 0.04);
  sigma ~  normal(0,1);
  y ~ normal(beta0 + beta1 * x, sigma);
}

