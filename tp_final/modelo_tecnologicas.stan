//data {
  //int<lower=0> N;             // un entero no negativo
  //real<lower=0> sigma;        // un número real no negativo
  //vector[N] y;                // un vector de N números reales
  //matrix[N, K] X;             // una matriz de N x K números reales
  //int y_cat[N];               // un array de N enteros (p.ej., categorías)
  //array[N] real z;            // un array de N números reales
  //matrix[K, L] Z[M];          // un array de M matrices K x L
//}

data {
  int<lower=1> N;                       // Cantidad de observaciones                    
  int<lower=0, upper=1> y[N];           // Respuesta binaria
  int<lower=1, upper=2> teslas[N];      // Intensidad de campo como variable categórica (1 = 1.5T, 2 = 3T)
  int<lower=1, upper=3> resonador_idx[N]; // Índice del resonador (1, 2 o 3)
}

parameters {
  vector[3] a;                          // Intercepto para cada resonador
  vector[2] b;                          // Coeficiente para cada resonador
}

model {
  y ~ bernoulli_logit(a[resonador_idx] + b[teslas]);
}




