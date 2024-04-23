###########################################
fx <- function(x){
  a=2
  b=5
  if(x<0 || x>1){
    f=0
  }else{
    f <- a*b*(x^(a-1))*(1-x^a)^(b-1)
  }
  return(f)
}
theta <- double()
theta[1] <- 0.5
for(i in 1:9999){ 
    propuesta <- rnorm(1, mean = theta[i], sd = 0.4)
    
    f_actual <- fx(theta[i])
    f_propuesta <- fx(propuesta)
    
    alpha <- min(c(1,f_propuesta/f_actual))
    
    quehacemos <- sample(c("salto","no salto"), 
                        size = 1, 
                        prob = c(alpha,1-alpha))
    
    if(quehacemos=="salto") {
      theta[i+1] <- propuesta 
    } else {
      theta[i+1] <- theta[i]
    }
}

plot(theta, type= "l")

hist(theta)

# hacer para 5 combinaciones de a y b