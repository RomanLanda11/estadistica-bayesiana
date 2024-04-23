
###########
sample_mh <- function (d_objetivo, r_propuesta, d_propuesta, p_inicial, n){
  stopifnot(n>0)
  muestras <- numeric(n)
  muestras[1]<-p_inicial
  for (i in 2:n) {
    p_actual <- muestras[i-1]
    p_nuevo <- r_propuesta(p_actual)
    f_actual <- d_objetivo(p_actual)
    f_nuevo <- d_objetivo(p_nuevo)
    
    q_actual <- d_propuesta(p_actual,mean=p_nuevo)
    q_nuevo <- d_propuesta(p_nuevo,mean=p_actual)
    
    alpha <- min(1, (f_nuevo/f_actual)*(q_actual/q_nuevo))
    
    aceptar <- rbinom(1,1, alpha)
    if (aceptar){
      muestras[i] <- p_nuevo
    } else {
      muestras[i] <- p_actual
    }
    
  }
  return ( muestras ) 
}

d_objetivo <- function(x) dnorm(x, mean=1, sd=2)
r_propuesta <- function(x) rnorm(1, mean=x, sd=0.25)
d_propuesta <- function(x, mean) dnorm(x, mean=mean, sd=0.25)

muestras <- sample_mh(d_objetivo, r_propuesta, d_propuesta, p_inicial= 0.8, n=50000 )

plot(muestras, type= "l")

hist(muestras)

plot_hist <- function(x, d_objetivo){
  x_seq <- seq(min(x), max(x), length.out = 200)
  df_hist <- data.frame(x = muestras)
  #df_line <- data.frame(x = muestras)
}
########
d_t <- function(x) dt(x, df=5)
r_propuesta <- function(x) rnorm(1, mean=x, sd=3)
d_propuesta <- function(x, mean) dnorm(x, mean=mean, sd=3)

muestras <- sample_mh(d_t, r_propuesta, d_propuesta, p_inicial= 0.8, n=5000 ) 
plot(muestras, type= "l")

hist(muestras)


#####ej 3
d_beta <- function(x) dbeta(x,4,8)
get_beta_pars <- function(r)#completar
r_propuesta <- function(x) {
    kappa <- 15
    pars <- get_beta_pars(x, kappa)
    dnorm(x, me)
    rbeta(1, pars$alpha, pars$beta)
}
d_propuesta <- function(x) {
  kappa <- 10
  pars <- get_beta_pars(x, kappa)
  dbeta(1, pars$alpha, pars$beta)
}
d_propuesta <- function(x, mean) dnorm(x, mean=mean, sd=3)

muestras <- sample_mh(d_beta, r_propuesta, d_propuesta, p_inicial= 0.8, n=50000 ) 
plot(muestras, type= "l")

hist(muestras)
