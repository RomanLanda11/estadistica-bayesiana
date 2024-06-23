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
  vector[N] hipocampo;        
  vector[N] intercraneal;
  vector[N] fusiforme;
  vector[N] sup_frontal;
}

parameters {
  real a;
  real b1; 
  //real b2; 
  //real b3;
  real b4; 
  
}

model {
  a ~ normal(0,2);
  b1 ~ normal(0,1);
  //b2 ~ normal(0,1);
  //b3 ~ normal(0,1);
  b4 ~ normal(0,1);
  
  
  //y ~ bernoulli_logit( a+ b1*hipocampo + b2*intercraneal + b4*sup_frontal);
  y ~ bernoulli_logit( a+ b1*hipocampo  + b4*sup_frontal);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_likelihood;

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    //y_rep[i] = bernoulli_logit_rng( a+ b1*hipocampo[i] + b2*intercraneal[i] + b4*sup_frontal[i]);
    y_rep[i] = bernoulli_logit_rng( a+ b1*hipocampo[i]  + b4*sup_frontal[i]);

    // Cálculo de la log-verosimilitud
    //log_likelihood[i] = bernoulli_logit_lpmf(y[i] | a+ b1*hipocampo[i] + b2*intercraneal[i] + b4*sup_frontal[i]);
    log_likelihood[i] = bernoulli_logit_lpmf(y[i] | a+ b1*hipocampo[i]  + b4*sup_frontal[i]);
  }
}




