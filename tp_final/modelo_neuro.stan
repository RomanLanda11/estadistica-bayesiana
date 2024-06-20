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
  int<lower=1> N;                           
  int<lower=0, upper=1> y[N];               //respuesta binaria
  real edad[N];                             
  real sexo[N];                           
  real resonador_fab[N];                  
  real intensidad_campo[N];       
  real lh_hippocampus_cm3[N];              
  real vol_intracraneal_cm3[N];           
  real fusiforme_cm3[N];                
}

parameters {
  real a;                                   
  real b_edad;                                
  real b_sexo;                                
  real b_resonador_fab;                     
  real b_intensidad_campo;                  
  real b_lh_hippocampus_cm3;                
  real b_vol_intracraneal_cm3;              
  real b_fusiforme_cm3;                     
}

model {
  // Priors uniformes
  y ~ bernoulli_logit(a + b_edad * edad + b_sexo * sexo + 
                      b_resonador_fab * resonador_fab + 
                      b_intensidad_campo * intensidad_campo + 
                      b_lh_hippocampus_cm3 * lh_hippocampus_cm3 + 
                      b_vol_intracraneal_cm3 * vol_intracraneal_cm3 + 
                      b_fusiforme_cm3 * fusiforme_cm3);
}

generated quantities {
  // Probabilidades predichas para cada observación
  vector[N] y_pred;
  for (n in 1:N)
    y_pred[n] = bernoulli_logit_rng(a + b_edad * edad[n] + b_sexo * sexo[n] + 
                                    b_resonador_fab * resonador_fab[n] + 
                                    b_intensidad_campo * intensidad_campo[n] + 
                                    b_lh_hippocampus_cm3 * lh_hippocampus_cm3[n] + 
                                    b_vol_intracraneal_cm3 * vol_intracraneal_cm3[n] + 
                                    b_fusiforme_cm3 * fusiforme_cm3[n]);
}


