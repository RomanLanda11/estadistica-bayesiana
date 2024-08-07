library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
n_simulaciones=1000
set.seed(491)
resultados=rbinom(n_simulaciones, 366, 0.55)
datos=data.frame(resultados)
summary(datos) # mejorar


ganacias_df <- data.frame(anios=1:n_simulaciones, ganacias=resultados)
ggplot(ganacias_df) +
  aes(x = ganacias) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)



################################################################################
######################### funcion para graficar ################################
graficar<-function(cont_tiradas, cont_exitos, wins, alphas, betas){
  df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                   freq = cont_tiradas,
                   exitos = cont_exitos,
                   fracasos = cont_tiradas - cont_exitos)
  g1<-ggplot(df, aes(x = maq, fill = maq)) +
    geom_bar(aes(y = freq ), stat = "identity", color = "black") +
    geom_bar(aes(y = exitos ), stat = "identity", color = "black") +
    geom_text(aes(y = exitos, label = "exitos"), position = position_stack(vjust = 0.5)) +
    geom_text(aes(y = freq, label = "fracasos"), position = position_stack(vjust = 0.8)) +
    labs(title = "Comparación de frecuencias",
         x = "Máquina",
         y = "Frecuencia") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Máquinas"))
  
  
  # Crear el gráfico de ganancias
  df <- data.frame(dias = 1:366, wins=wins)
  g2<-ggplot(df, aes(x = dias, y = wins)) +
    geom_line(color = "blue")+
    labs(title = "Valores acumulados en 366 días",
         x = "Día",
         y = "Valor acumulado")
  
  # Crear el gráfico de postiriors
  x1=seq(0,1,length.out =200)
  posterior1=dbeta(x1,alphas[1],betas[1])
  posterior2=dbeta(x1,alphas[2],betas[2])
  posterior3=dbeta(x1,alphas[3],betas[3])
  
  df <- data.frame(
    theta = x1,
    posterior = rep(c(posterior1,posterior2, posterior3), time = 1),
    maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(x1))
  )
  
  g3<-ggplot(df)+
    aes(x = theta, y = posterior, color = maquina)+
    theme(legend.position = "right")+
    geom_line() +
    geom_area(aes(fill = maquina), alpha = 0.4, position = "identity") +
    labs(x = expression(theta), y = expression("p(" ~ theta ~ "| y)"))
  print(g1)
  print(g2)
  print(g3)
}
################################################################################
################################################################################

## Estrategia 1: Completamente al azar
####################### 1
al_azar <- function(lista1) {
  alphas <- lista1[[1]]
  betas <- lista1[[2]]
  tethas <- lista1[[3]]
  prior_df <- lista1[[4]]
  x1=seq(0,1,length.out =200)

  sample <- sample(1:length(alphas), 1)
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  
  postirior <- dbeta(x1, alphas[sample],  betas[sample])
  prior_df[, sample] <- postirior
  salida1_list <- list(alphas, betas, tethas, prior_df, sample, juego)
  return(salida1_list)
}

####################### 2
set.seed(491)
alphas<-c(2,2,2)
betas<-c(2,2,2)
tethas=c(0.3,0.55,0.45)

x1=seq(0,1,length.out =200)
prior_df <- data.frame(prior1=dbeta(x1,alphas[1],betas[1]), 
                       prior2=dbeta(x1,alphas[2],betas[2]), 
                       prior3=dbeta(x1,alphas[3],betas[3]))

wins=numeric(366)
cont_exitos= c(0,0,0)
cont_tiradas = c(0, 0, 0)
juego=0
sample=0
entrada1_list = list(alphas, betas, tethas, prior_df, sample, juego)
graf_list=list()

for (i in 1:366) {
  entrada1_list=al_azar(lista1=entrada1_list)
  sample <- entrada1_list[[5]]
  juego <- entrada1_list[[6]]
  cont_exitos[sample] <- cont_exitos[sample] + juego
  cont_tiradas[sample] <- cont_tiradas[sample] + 1
  prior_df <- entrada1_list[[4]]

  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
  prior_df_largo <- data.frame(
    theta = x1,
    posterior = rep(c(prior_df$prior1, 
                      prior_df$prior2, 
                      prior_df$prior3), time = 1),
    maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = 200)
  )
  graf_list[[i]] <- ggplot(prior_df_largo)+
    aes(x = theta, y = posterior, color = maquina)+
    theme(legend.position = "none")+
    geom_line() +
    geom_area(aes(fill = maquina), alpha = 0.4, position = "identity") +
    labs(x = expression(theta), y = expression("p(" ~ theta ~ "| y)"))
}

grid.arrange(graf_list[[1]],graf_list[[2]],graf_list[[3]],graf_list[[4]],
             graf_list[[363]],graf_list[[364]],graf_list[[365]],graf_list[[366]],
             ncol = 4)


# Crear el gráfico de barras
df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont_tiradas,
                 exitos = cont_exitos,
                 fracasos = cont_tiradas - cont_exitos)
ggplot(df, aes(x = maq, fill = maq)) +
  geom_bar(aes(y = freq ), stat = "identity", color = "black") +
  geom_bar(aes(y = exitos ), stat = "identity", color = "black") +
  geom_text(aes(y = exitos, label = "exitos"), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = freq, label = "fracasos"), position = position_stack(vjust = 0.8)) +
  labs(title = "Comparación de frecuencias",
       x = "Máquina",
       y = "Frecuencia") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Máquinas"))


df <- data.frame(dias = 1:366, wins=wins)

# Crear el gráfico evolucion ganancia
ggplot(df, aes(x = dias, y = wins)) +
  geom_line(color = "blue")+
  labs(title = "Valores acumulados en 366 días",
       x = "Día",
       y = "Valor acumulado")


######################## 3 1000 de 365 dias
wins1_anio = numeric(n_simulaciones)

for (anio in 1:n_simulaciones) {
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  tethas=c(0.3,0.55,0.45)
  x1=seq(0,1,length.out =200)
  prior_df <- data.frame(prior1=dbeta(x1,alphas[1],betas[1]), 
                         prior2=dbeta(x1,alphas[2],betas[2]), 
                         prior3=dbeta(x1,alphas[3],betas[3]))
  wins=numeric(366)
  cont_exitos= c(0,0,0)
  cont_tiradas = c(0, 0, 0)
  juego=0
  sample=0
  entrada1_list = list(alphas, betas, tethas, prior_df, sample, juego)
  graf_list=list()
  
  for (i in 1:366) {
    entrada1_list=al_azar(lista1=entrada1_list)
    sample <- entrada1_list[[5]]
    juego <- entrada1_list[[6]]
    cont_exitos[sample] <- cont_exitos[sample] + juego
    cont_tiradas[sample] <- cont_tiradas[sample] + 1
    prior_df <- entrada1_list[[4]]
    
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins1_anio[anio] <- wins[366]
}

ganacias_df <- data.frame(anios=1:n_simulaciones, ganacias=wins1_anio)
ggplot(ganacias_df) +
  aes(x = ganacias) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)

#4 no es bayesiano, es al azar perreque

################################################################################
################################################################################
################################################################################

## Estrategia 2: Greedy con tasa obs

###### 1 Fijamos la semilla
set.seed(412)
greedy_tasa_obs<-function(lista2){
  fl_calentamiento <- lista2[[5]]
  t <- lista2[[2]]
  tethas <- lista2[[1]]
  if (fl_calentamiento){
    sample <- sample(1:3, 1)
  }
  else{
    sample <- order(t + runif(3,0.001, 0.009), decreasing = TRUE)[1]
  }
  juego <- rbinom(1, 1, tethas[sample])
  lista2_salida<-list(tethas, t, juego, sample, fl_calentamineto)
  return(lista2_salida)
}

# Defino variables necesarias
tethas=c(0.3,0.55,0.45)
t = c(0.5,0.5,0.5)
cont_exitos= c(0,0,0)
cont_tiradas = c(0, 0, 0)
juego=0
sample=0
fl_calentamineto=FALSE
wins=numeric(366)
lista2_entrada=list(tethas, t, juego, sample, fl_calentamineto)

for(i in 1:366){
  fl_calentamineto <- ifelse(i>10, FALSE, TRUE )

  lista2_entrada <- greedy_tasa_obs(lista2 = lista2_entrada)
  juego <- lista2_entrada[[3]]
  sample <- lista2_entrada[[4]]
  cont_exitos[sample] <- cont_exitos[sample] + juego
  cont_tiradas[sample] <- cont_tiradas[sample] + 1
  lista2_entrada[[2]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}

alphas <- c(2+cont_exitos[1], 2+cont_exitos[2], 2+cont_exitos[3])
betas <- c(2+cont_tiradas[1]-cont_exitos[1],
           2+cont_tiradas[2]-cont_exitos[2],
           2+cont_tiradas[3]-cont_exitos[3])
# Graficos
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)


######################## 3 1000 de 365 dias

wins2_anio = numeric(n_simulaciones)

for (anio in 1:n_simulaciones) {
  # Definimos parametros
  tethas=c(0.3,0.55,0.45)
  t = c(0.5,0.5,0.5)
  cont_exitos= c(0,0,0)
  cont_tiradas = c(0, 0, 0)
  juego=0
  sample=0
  fl_calentamineto=TRUE
  wins=numeric(366)
  lista2_entrada=list(tethas, t, juego, sample, fl_calentamineto)
  #Simulamos anio
  for(i in 1:366){
    fl_calentamineto <- ifelse(i>10, FALSE, TRUE )
    
    lista2_entrada <- greedy_tasa_obs(lista2 = lista2_entrada)
    juego <- lista2_entrada[[3]]
    sample <- lista2_entrada[[4]]
    cont_exitos[sample] <- cont_exitos[sample] + juego
    cont_tiradas[sample] <- cont_tiradas[sample] + 1
    lista2_entrada[[2]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins2_anio[anio] <- wins[366]
}

ganacias_df <- data.frame(anios=1:n_simulaciones, ganacias=wins2_anio)
ggplot(ganacias_df) +
  aes(x = ganacias) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)
#Se ve bine la suma de 3 normales de 3 maquinas, esta bueno

#4 creo que es un metodo frecuentista porque juega nu par de veces y decide


################################################################################
################################################################################
################################################################################

# Estrategia 3: Greedy con probabilidad a posteriorir
# entiendo que elige la que tenga mejor esperanza hasta el momento

set.seed(412)
greedy_prob_posterior<-function(lista3){
  tethas <- lista3[[1]]
  alphas <- lista3[[2]]
  betas <- lista3[[3]]
  
  esperanzas = c(alphas[1]/(alphas[1]+betas[1]),
                alphas[2]/(alphas[2]+betas[2]),
                alphas[3]/(alphas[3]+betas[3]))
  #print(esperanzas)
  sample <- order(esperanzas + runif(3,0.001, 0.009)  , decreasing = TRUE)[1]
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  
  lista3_salida<-list(tethas, alphas, betas, juego, sample)
  return(lista3_salida)
}

# Defino variables necesarias
tethas=c(0.3,0.55,0.45)
alphas<-c(2,2,2)
betas<-c(2,2,2)
juego=0
sample=0
cont_exitos= c(0,0,0) 
cont_tiradas = c(0, 0, 0)

wins=numeric(366)
lista3_entrada=list(tethas, alphas, betas, juego, sample)

for(i in 1:366){
  lista3_entrada <- greedy_prob_posterior(lista3 = lista3_entrada)
  juego <- lista3_entrada[[4]]
  alphas <- lista3_entrada[[2]]
  betas <- lista3_entrada[[3]]
  sample <- lista3_entrada[[5]]
  cont_exitos[sample] = cont_exitos[sample] + juego 
  cont_tiradas[sample] = cont_tiradas[sample] + 1
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}
alphas <- lista3_entrada[[2]]
betas<- lista3_entrada[[3]]
# Graficos
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)


######################## 3 1000 de 365 dias
wins3_anio=numeric(n_simulaciones)
for (anio in 1:n_simulaciones) {
  # Defino variables necesarias
  tethas=c(0.3,0.55,0.45)
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  juego=0
  sample=0
  cont_exitos= c(0,0,0) 
  cont_tiradas = c(0, 0, 0)
  
  wins=numeric(366)
  lista3_entrada=list(tethas, alphas, betas, juego, sample)
  
  for(i in 1:366){
    lista3_entrada <- greedy_prob_posterior(lista3 = lista3_entrada)
    juego <- lista3_entrada[[4]]
    alphas <- lista3_entrada[[2]]
    betas <- lista3_entrada[[3]]
    sample <- lista3_entrada[[5]]
    cont_exitos[sample] = cont_exitos[sample] + juego 
    cont_tiradas[sample] = cont_tiradas[sample] + 1
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins3_anio[anio]<-wins[366]
}

ganancia_df <- data.frame(wins = wins3_anio)
ggplot(ganancia_df) +
  aes(x = wins) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)

##4 me parece un metodo bayesiano porque usa las 

################################################################################
################################################################################
################################################################################
# Estrategia 4: e-greedy con tasa obs

set.seed(412)
e_greedy_tasa_obs<-function(lista4){
  e<-lista4[[3]]
  explote<-runif(1)
  lista4[[6]] = 0
  lista4[[7]] = 0
  if(explote<=1-e){#Elige el mejor
    lista4[[7]] = 1 # exploto
    sample <-  order(t + runif(3,0.001, 0.009), decreasing = TRUE)[1]
  }else{#Elige aleatoriamente 
    lista4[[6]] = 1 # exploro
    sample <-  as.numeric(sample(1:3, 1))
  }

  juego <- rbinom(1, 1, tethas[sample])
  lista4_salida<-list(tethas, t, e, juego, sample, lista4[[6]], lista4[[7]])
  return(lista4_salida)
}

############# e = 0.2 ######################
e=0.2
# Defino variables necesarias
tethas=c(0.3,0.55,0.45)
t = c(0.5,0.5,0.5)
cont_exitos= c(0,0,0) 
cont_tiradas = c(0, 0, 0)
juego=0
sample=0
exploro=0
exploto=0
wins=numeric(366)
lista4_entrada=list(tethas, t, e, juego, sample, exploro, exploto)

for(i in 1:366){
  lista4_entrada <- e_greedy_tasa_obs(lista4 = lista4_entrada)
  juego <- lista4_entrada[[4]]
  sample <- lista4_entrada[[5]]
  exploro <- exploro + lista4_entrada[[6]]
  exploto <- exploto + lista4_entrada[[7]]
  cont_exitos[sample] <- cont_exitos[sample] + juego
  cont_tiradas[sample] <- cont_tiradas[sample] + 1
  lista4_entrada[[2]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}
alphas <- c(2+cont_exitos[1], 2+cont_exitos[2], 2+cont_exitos[3])
betas <- c(2+cont_tiradas[1]-cont_exitos[1],
           2+cont_tiradas[2]-cont_exitos[2],
           2+cont_tiradas[3]-cont_exitos[3])

# Graficos
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)


######################## 3 1000 de 365 dias
wins4_anio=numeric(n_simulaciones)
for (anio in 1:n_simulaciones) {
  # Defino variables necesarias
  tethas=c(0.3,0.55,0.45)
  t = c(0.5,0.5,0.5)
  cont_exitos= c(0,0,0) 
  cont_tiradas = c(0, 0, 0)
  juego=0
  sample=0
  exploro=0
  exploto=0
  
  wins=numeric(366)
  lista4_entrada=list(tethas, t, e, juego, sample, exploro, exploto)
  
  for(i in 1:366){
    lista4_entrada <- e_greedy_tasa_obs(lista4 = lista4_entrada)
    juego <- lista4_entrada[[4]]
    sample <- lista4_entrada[[5]]
    exploro <- exploro + lista4_entrada[[6]]
    exploto <- exploto + lista4_entrada[[7]]
    cont_exitos[sample] <- cont_exitos[sample] + juego
    cont_tiradas[sample] <- cont_tiradas[sample] + 1
    lista4_entrada[[2]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins4_anio[anio]<-wins[366]
}

ganancia_df <- data.frame(wins = wins4_anio)
ggplot(ganancia_df) +
  aes(x = wins) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)


###### 3 e distintos
graf_list <- list()
j=1
for (e in c(0.1, 0.2, 0.3)){
  wins4_anio=numeric(n_simulaciones)
  for (anio in 1:n_simulaciones) {
    # Defino variables necesarias
    tethas=c(0.3,0.55,0.45)
    t = c(0.5,0.5,0.5)
    cont_exitos= c(0,0,0) 
    cont_tiradas = c(0, 0, 0)
    juego=0
    sample=0
    exploro=0
    exploto=0
    
    wins=numeric(366)
    lista4_entrada=list(tethas, t, e, juego, sample, exploro, exploto)
    
    for(i in 1:366){
      lista4_entrada <- e_greedy_tasa_obs(lista4 = lista4_entrada)
      juego <- lista4_entrada[[4]]
      sample <- lista4_entrada[[5]]
      exploro <- exploro + lista4_entrada[[6]]
      exploto <- exploto + lista4_entrada[[7]]
      cont_exitos[sample] <- cont_exitos[sample] + juego
      cont_tiradas[sample] <- cont_tiradas[sample] + 1
      lista4_entrada[[2]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
      if(i==1){
        wins[i]=juego
      }else{
        wins[i]=wins[i-1]+juego
      }
    }
    wins4_anio[anio]<-wins[366]
  }
  
  ganancia_df <- data.frame(wins = wins4_anio)
  graf_list[[j]] <-ggplot(ganancia_df) + #Reemplazar por boxplots
        aes(x = wins) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
        geom_density(color = "blue", size = 1)
  j=j+1
}

grid.arrange(graf_list[[1]],
             graf_list[[2]],
             graf_list[[3]],
             ncol=1)

#4 es bayesiano?

################################################################################
################################################################################
################################################################################
# Estrategia 5: soft max

#Seleccion de t, similar al e de e-greedy. Determina las tiradas dedicadas a la exploracion
#A mayores valores de t, la cant de veces que se elige una  maquina al azar aumenta
#Menor valor de t menos explora
#Función Softmax, dandole la tasa y la "temperatura" nos devuelve la
#probabilidad de elegir cada maquina

softmax= function(tau,pi){
  pr_soft1 = exp(pi[1]/tau)/sum(exp(pi/tau))
  pr_soft2 = exp(pi[2]/tau)/sum(exp(pi/tau))
  pr_soft3 = exp(pi[3]/tau)/sum(exp(pi/tau))
  pr_soft = c(pr_soft1,pr_soft2,pr_soft3)
  return(pr_soft)
}
softmax(t=0.1, pi=c(0.5,0.5,0.5) )

#Creamos la función para una tirada
tirada_softmax <- function(lista5){
  alphas <- lista5[[1]]
  betas <- lista5[[2]]
  tethas <- lista5[[3]]
  pi <- lista5[[4]]
  tau=0.1
  
  sample <- sample(1:3,1,prob=softmax(tau,pi))
  
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  
  lista5_salida<-list(alphas, betas, tethas,pi, juego,  sample)
  return(lista5_salida)
}


#Utilizamos la función para realizar 366 tiradas
set.seed(492)
tethas=c(0.3,0.55,0.45)
alphas<-c(2,2,2)
betas<-c(2,2,2)
pi <- c(0.5,0.5,0.5)
cont_exitos= c(0,0,0) 
cont_tiradas = c(0, 0, 0)

wins=numeric(366)
lista5_entrada=list(alphas,betas, tethas,pi)

for(i in 1:366){
  lista5_entrada <- tirada_softmax(lista5_entrada)
  juego <- lista5_entrada[[5]]
  alphas <- lista5_entrada[[1]]
  betas <- lista5_entrada[[2]]
  sample <- lista5_entrada[[6]]
  cont_exitos[sample] = cont_exitos[sample] + juego 
  cont_tiradas[sample] = cont_tiradas[sample] + 1
  lista5_entrada[[4]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}

#Gráficos
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)

#1000 dias
n_simulaciones=1000

wins5_anio=numeric(n_simulaciones)
for (anio in 1:n_simulaciones) {
  # Defino variables necesarias
  tethas=c(0.3,0.55,0.45)
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  pi <- c(0.5,0.5,0.5)
  cont_exitos= c(0,0,0) 
  cont_tiradas = c(0, 0, 0)
  
  wins=numeric(366)
  lista5_entrada=list(alphas,betas, tethas,pi)
  
  for(i in 1:366){
    lista5_entrada <- tirada_softmax(lista5_entrada)
    juego <- lista5_entrada[[5]]
    alphas <- lista5_entrada[[1]]
    betas <- lista5_entrada[[2]]
    sample <- lista5_entrada[[6]]
    cont_exitos[sample] = cont_exitos[sample] + juego 
    cont_tiradas[sample] = cont_tiradas[sample] + 1
    lista5_entrada[[4]][sample] <- cont_exitos[sample] / cont_tiradas[sample]
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins5_anio[anio]<-wins[366]
}

ganancia_df <- data.frame(wins = wins5_anio)
ggplot(ganancia_df) +
  aes(x = wins) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)

#Es bayesiano? Si, porque modifica la probabilidad de eleccion de la maquina
#a traves de la informacion de las nuevas tiradas.  

################################################################################
################################################################################
################################################################################
# Estrategia 6: upper bound
set.seed(412)
upper_bound <- function(lista6){
  tethas <- lista6[[1]]
  alphas <- lista6[[2]]
  betas <- lista6[[3]]
  
  UCB <- c(qbeta(0.95, alphas[1], betas[1]), 
           qbeta(0.95, alphas[2], betas[2]), 
           qbeta(0.95, alphas[3], betas[3]))
  sample <- order(UCB + runif(3, 0.001, 0.009), decreasing = TRUE)[1]
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  
  lista6_salida<-list(tethas, alphas, betas, UCB, juego, sample, cont_exitos, cont_tiradas)
  return(lista6_salida)
}

tethas=c(0.3,0.55,0.45)
cont_exitos= c(0,0,0) 
cont_tiradas = c(0, 0, 0)
juego=0
UCB <- c(0, 0, 0)
alphas <- c(2, 2, 2)
betas <- c(2, 2, 2)
wins <- c(0, 0, 0)
lista6_entrada <- list(tethas, alphas, betas, UCB, juego, sample, cont_exitos, cont_tiradas)

for(i in 1:366){
  lista6_entrada <- upper_bound(lista6 = lista6_entrada)
  juego <- lista6_entrada[[5]]
  alphas <- lista6_entrada[[2]]
  betas <- lista6_entrada[[3]]
  sample <- lista6_entrada[[6]]
  cont_exitos <- lista6_entrada[[7]]
  cont_tiradas <- lista6_entrada[[8]]
  cont_exitos[sample] = cont_exitos[sample] + juego 
  cont_tiradas[sample] = cont_tiradas[sample] + 1
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}

#Graficos
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)

######################## 3 1000 de 365 dias
wins6_anio=numeric(n_simulaciones)
for (anio in 1:n_simulaciones) {
  # Defino variables necesarias
  tethas=c(0.3,0.55,0.45)
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  juego=0
  sample=0
  cont_exitos= c(0,0,0) 
  cont_tiradas = c(0, 0, 0)
  
  wins=numeric(366)
  lista6_entrada <- list(tethas, alphas, betas, UCB, juego, sample, cont_exitos, cont_tiradas)
  
  for(i in 1:366){
    lista6_entrada <- upper_bound(lista6 = lista6_entrada)
    juego <- lista6_entrada[[5]]
    alphas <- lista6_entrada[[2]]
    betas <- lista6_entrada[[3]]
    sample <- lista6_entrada[[6]]
    cont_exitos <- lista6_entrada[[7]]
    cont_tiradas <- lista6_entrada[[8]]
    cont_exitos[sample] = cont_exitos[sample] + juego 
    cont_tiradas[sample] = cont_tiradas[sample] + 1
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins6_anio[anio]<-wins[366]
}

ganancia_df <- data.frame(wins = wins6_anio)
ggplot(ganancia_df) +
  aes(x = wins) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)


################################################################################
################################################################################
################################################################################
# Estrategia 7: thompson sampling
# Se usa la media para comparar muestras
thompson_sampling <- function(lista7){
  alphas <- lista7[[1]]
  betas <- lista7[[2]]
  tethas <- lista7[[3]]
  muestra<- c(rbeta(1,alphas[1],betas[1]), 
               rbeta(1,alphas[2],betas[2]), 
               rbeta(1,alphas[3],betas[3]))
  #print(muestra)
  sample <- order(muestra, decreasing = TRUE)[1]
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  
  lista7_salida<-list(alphas, betas, tethas,  muestra, juego,  sample)
  return(lista7_salida)
}

#### 2 
set.seed(490)
alphas<-c(2,2,2)
betas<-c(2,2,2)
tethas=c(0.3,0.55,0.45)
wins=numeric(366)
cont_exitos= c(0,0,0)
cont_tiradas = c(0, 0, 0)
juego=0
sample=0
muestra=c()

lista7_entrada<-list(alphas, betas, tethas,  muestra, juego,  sample)

for(i in 1:366){
  lista7_entrada <- thompson_sampling( lista7_entrada )
  juego <- lista7_entrada[[5]]
  sample <- lista7_entrada[[6]]
  cont_exitos[sample] <- cont_exitos[sample] + juego
  cont_tiradas[sample] <- cont_tiradas[sample] + 1

  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}
alphas <- lista7_entrada[[1]]
betas <- lista7_entrada[[2]]
# Graficar
graficar(cont_tiradas, cont_exitos, wins, alphas, betas)

######################## 3 1000 de 365 dias
wins7_anio=numeric(n_simulaciones)
for (anio in 1:n_simulaciones) {
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  tethas=c(0.3,0.55,0.45)
  wins=numeric(366)
  cont_exitos= c(0,0,0)
  cont_tiradas = c(0, 0, 0)
  juego=0
  sample=0
  muestra=c()
  
  lista7_entrada<-list(alphas, betas, tethas,  muestra, juego,  sample)
  
  for(i in 1:366){
    lista7_entrada <- thompson_sampling( lista7_entrada )
    juego <- lista7_entrada[[5]]
    sample <- lista7_entrada[[6]]
    cont_exitos[sample] <- cont_exitos[sample] + juego
    cont_tiradas[sample] <- cont_tiradas[sample] + 1
    
    if(i==1){
      wins[i]=juego
    }else{
      wins[i]=wins[i-1]+juego
    }
  }
  wins7_anio[anio]<-wins[366]
}

ganancia_df <- data.frame(wins = wins7_anio)
ggplot(ganancia_df) +
  aes(x = wins) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "blue", size = 1)

