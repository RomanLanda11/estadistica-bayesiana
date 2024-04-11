library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
n_simulaciones=100
set.seed(491)
resultados=rbinom(n_simulaciones, 366, 0.55)
datos=data.frame(resultados)
summary(datos) # mejorar


ggplot(data.frame(x = resultados), aes(x)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  #geom_density(color = "red", size = 1.5) +  # Agrega la línea suavizada
  labs(title = "Histograma de datos",
       x = "Valores",
       y = "Frecuencia")

#en promedio esperamos que gane 200.6 veces o 201


################################################################################
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
    #Recomendo Nacho: sample <- order(t, decreasing = TRUE)[1] 
    sample <-  case_when(
      # t[1] > t[2] & t[1] > t[3] ~ as.numeric(1),
      # t[2] > t[1] & t[2] > t[3] ~  as.numeric(2),
      # t[3] > t[1] & t[3] > t[2] ~  as.numeric(3),
      t[1] == t[2] & t[1] == t[3] ~ as.numeric(sample(1:3, 1)),
      t[1] == t[2] & t[1] > t[3] ~ as.numeric(sample(1:2, 1)),
      t[1] == t[3] & t[1] > t[2] ~ as.numeric(sample(c(1, 3), 1)),
      t[2] == t[3] & t[2] > t[1] ~ as.numeric(sample(2:3, 1)),
      .default = order(t, decreasing = TRUE)[1]
    )
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
fl_calentamineto=TRUE
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


# Graficos
df <- data.frame(cont = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont_tiradas)

ggplot(df, aes(x = cont, y = freq, fill = cont)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Gráfico de Barras de la frecuencia de cada máquina",
       x = "Máquina",
       y = "Frecuencia") +
  theme_minimal() + 
  guides(fill = guide_legend(title = "Máquinas"))


df <- data.frame(dias = 1:366, wins=wins)
# Crear el gráfico de barras
ggplot(df, aes(x = dias, y = wins)) +
  geom_line(color = "blue")+
  labs(title = "Valores acumulados en 366 días",
       x = "Día",
       y = "Valor acumulado")


# Crear el gráfico de postiriors
x1=seq(0,1,length.out =200)
posterior1=dbeta(x1,2+cont_exitos[1],2+cont_tiradas[1]-cont_exitos[1])
posterior2=dbeta(x1,2+cont_exitos[2],2+cont_tiradas[2]-cont_exitos[2])
posterior3=dbeta(x1,2+cont_exitos[3],2+cont_tiradas[3]-cont_exitos[3])

df <- data.frame(
  theta = x1,
  posterior = rep(c(posterior1,posterior2, posterior3), time = 1),
  maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(x1))
)

ggplot(df)+
  aes(x = theta, y = posterior, color = maquina)+
  theme(legend.position = "right")+
  geom_line() +
  geom_area(aes(fill = maquina), alpha = 0.4, position = "identity") +
  labs(x = expression(theta), y = expression("p(" ~ theta ~ "| y)"))


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
  sample <-  case_when(
    esperanzas[1] == esperanzas[2] & esperanzas[1] == esperanzas[3] ~ as.numeric(sample(1:3, 1)),
    esperanzas[1] == esperanzas[2] & esperanzas[1] > esperanzas[3] ~ as.numeric(sample(1:2, 1)),
    esperanzas[1] == esperanzas[3] & esperanzas[1] > esperanzas[2] ~ as.numeric(sample(c(1, 3), 1)),
    esperanzas[2] == esperanzas[3] & esperanzas[2] > esperanzas[1] ~ as.numeric(sample(2:3, 1)),
    .default = order(esperanzas, decreasing = TRUE)[1]
  )
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


# Crear el gráfico de barras

df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont_tiradas)
ggplot(df, aes(x = maq, y = freq, fill = maq)) +
  geom_bar(stat = "identity", color = "black") +
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



# Crear el gráfico de postiriors
x1=seq(0,1,length.out =200)
posterior1=dbeta(x1,alphas[1],betas[1])
posterior2=dbeta(x1,alphas[2],betas[2])
posterior3=dbeta(x1,alphas[3],betas[3])
df <- data.frame(
  theta = x1,
  posterior = c(posterior1, posterior2, posterior3),
  maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(x1))
)

ggplot(df)+
  aes(x = theta, y = posterior, color = maquina)+
  theme(legend.position = "right")+
  geom_line() +
  geom_area(aes(fill = maquina), alpha = 0.4, position = "identity") +
  labs(x = expression(theta), y = expression("p(" ~ theta ~ "| y)"))

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

mean(wins3_anio)
mean(wins2_anio)
mean(wins_anio)

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
    sample <-  case_when(
      t[1] == t[2] & t[1] == t[3] ~ as.numeric(sample(1:3, 1)),
      t[1] == t[2] & t[1] > t[3] ~ as.numeric(sample(1:2, 1)),
      t[1] == t[3] & t[1] > t[2] ~ as.numeric(sample(c(1, 3), 1)),
      t[2] == t[3] & t[2] > t[1] ~ as.numeric(sample(2:3, 1)),
      .default = order(t, decreasing = TRUE)[1]
    )
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

# Graficos
df <- data.frame(cont = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont_tiradas)

ggplot(df, aes(x = cont, y = freq, fill = cont)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Gráfico de Barras de la frecuencia de cada máquina",
       x = "Máquina",
       y = "Frecuencia") +
  theme_minimal() + 
  guides(fill = guide_legend(title = "Máquinas"))


df <- data.frame(dias = 1:366, wins=wins)
# Crear el gráfico de barras
ggplot(df, aes(x = dias, y = wins)) +
  geom_line(color = "blue")+
  labs(title = "Valores acumulados en 366 días",
       x = "Día",
       y = "Valor acumulado")

# Crear el gráfico de postiriors
x1=seq(0,1,length.out =200)
posterior1=dbeta(x1,2+cont_exitos[1],2+cont_tiradas[1]-cont_exitos[1])
posterior2=dbeta(x1,2+cont_exitos[2],2+cont_tiradas[2]-cont_exitos[2])
posterior3=dbeta(x1,2+cont_exitos[3],2+cont_tiradas[3]-cont_exitos[3])

df <- data.frame(
  theta = x1,
  posterior = rep(c(posterior1,posterior2, posterior3), time = 1),
  maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(x1))
)

ggplot(df)+
  aes(x = theta, y = posterior, color = maquina)+
  theme(legend.position = "right")+
  geom_line() +
  geom_area(aes(fill = maquina), alpha = 0.4, position = "identity") +
  labs(x = expression(theta), y = expression("p(" ~ theta ~ "| y)"))

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
