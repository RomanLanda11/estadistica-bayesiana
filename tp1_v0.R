library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)

set.seed(491)
resultados=rbinom(1000, 366, 0.55)
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
al_azar <- function(lista) {
  alphas <- lista[[1]]
  betas <- lista[[2]]
  tethas <- lista[[3]]
  prior_df <- lista[[4]]
  x1 <- lista[[5]]
  cont <- lista[[6]]

  
  sample <- sample(1:length(alphas), 1)
  rst_juego <- rbinom(1, 1, tethas[sample])
  postirior <- dbeta(x1, alphas[sample] + rst_juego, betas[sample] + 1 - rst_juego)
  prior_df[, sample] <- postirior
  alphas[sample] <- alphas[sample] + rst_juego
  betas[sample] <- betas[sample] + 1 - rst_juego
  cont[sample] = cont[sample] + 1
  
  salida_list <- list(alphas, betas, tethas, prior_df, x1, cont, rst_juego)
  return(salida_list)
}

####################### 2

set.seed(491)
alphas<-c(2,2,2)
betas<-c(2,2,2)
tethas=c(0.3,0.55,0.45)

x1=seq(0,1,length.out =200)
prior1=dbeta(x1,alphas[1],betas[1])
prior2=dbeta(x1,alphas[2],betas[2])
prior3=dbeta(x1,alphas[3],betas[3])
prior_df <- data.frame(prior1, prior2, prior3)

wins=numeric(366)
cont<-c(0,0,0)
rst_juego=0
entrada_list = list(alphas, betas, tethas, prior_df, x1, cont, rst_juego)
graf_list=list()

for (i in 1:366) {
  entrada_list=al_azar(lista=entrada_list)

  if(i==1){
    wins[i]=entrada_list[[7]]
  }else{
    wins[i]=wins[i-1]+entrada_list[[7]]
  }
  prior_df_largo <- data.frame(
    theta = entrada_list[[5]],
    posterior = rep(c(entrada_list[[4]]$prior1, 
                      entrada_list[[4]]$prior2, 
                      entrada_list[[4]]$prior3), time = 1),
    maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(entrada_list[[5]]))
  )
  graf_list[[i]] <- ggplot(prior_df_largo)+
                      aes(x = theta, y = posterior, color = maquina)+
                      theme(legend.position = "none")+
                      geom_line()
}


grid.arrange(graf_list[[1]],graf_list[[2]],graf_list[[3]],graf_list[[4]],
             graf_list[[363]],graf_list[[364]],graf_list[[365]],graf_list[[366]],
             ncol = 4)


prior_df <- entrada_list[[4]]

# Crear el gráfico de barras

df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = entrada_list[[6]])
ggplot(df, aes(x = maq, y = freq, fill = maq)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Comparación de frecuencias",
       #x = "Variable",
       y = "Frecuencia") +
  theme_minimal()


df <- data.frame(dias = 1:366, wins=wins)
# Crear el gráfico de barras
ggplot(df, aes(x = dias, y = wins)) +
  geom_line(color = "blue")+
  labs(title = "Valores acumulados en 366 días",
       x = "Día",
       y = "Valor acumulado")



######################## 3 1000 de 365 dias
wins_anio = numeric(1000)

for (anio in 1:1000) {
  # Definimos parametros
  alphas<-c(2,2,2)
  betas<-c(2,2,2)
  tethas=c(0.3,0.55,0.45)
  
  x1=seq(0,1,length.out =200)
  prior1=dbeta(x1,alphas[1],betas[1])
  prior2=dbeta(x1,alphas[2],betas[2])
  prior3=dbeta(x1,alphas[3],betas[3])
  prior_df <- data.frame(prior1, prior2, prior3)
  
  wins=numeric(366)
  cont<-c(0,0,0)
  rst_juego=0
  entrada_list = list(alphas, betas, tethas, prior_df, x1, cont, rst_juego)
  graf_list=list()
  # Simulamos anio
  for (i in 1:366) {
    entrada_list=al_azar(lista=entrada_list)
    if(i==1){
      wins[i]=entrada_list[[7]]
    }else{
      wins[i]=wins[i-1]+entrada_list[[7]]
    }
  }  
  wins_anio[anio] <- wins[366]
}

ganacias_df <- data.frame(anios=1:1000, ganacias=wins_anio)
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

df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont_tiradas)

ggplot(df, aes(x = maq, y = freq, fill = maq)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Gráfico de Barras de la frecuencia de cada máquina",
       #x = "Variable",
       y = "Frecuencia") +
  theme_minimal()


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
  theme(legend.position = "none")+
  geom_line()

######################## 3 1000 de 365 dias

wins_anio = numeric(1000)

for (anio in 1:1000) {
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
  wins_anio[anio] <- wins[366]
}

ganacias_df <- data.frame(anios=1:1000, ganacias=wins_anio)
ggplot(ganacias_df) +
  aes(x = ganacias) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightgreen", bins = 30) +
  geom_density(color = "green", size = 1)
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
                alphas[2]/(alphas[1]+betas[1]),
                alphas[2]/(alphas[2]+betas[2]))
  print(esperanzas)
  sample <-  case_when(
    # t[1] > t[2] & t[1] > t[3] ~ as.numeric(1),
    # t[2] > t[1] & t[2] > t[3] ~  as.numeric(2),
    # t[3] > t[1] & t[3] > t[2] ~  as.numeric(3),
    esperanzas[1] == esperanzas[2] & esperanzas[1] == esperanzas[3] ~ as.numeric(sample(1:3, 1)),
    esperanzas[1] == esperanzas[2] & esperanzas[1] > esperanzas[3] ~ as.numeric(sample(1:2, 1)),
    esperanzas[1] == esperanzas[3] & esperanzas[1] > esperanzas[2] ~ as.numeric(sample(c(1, 3), 1)),
    esperanzas[2] == esperanzas[3] & esperanzas[2] > esperanzas[1] ~ as.numeric(sample(2:3, 1)),
    .default = order(esperanzas, decreasing = TRUE)[1]
  )
  juego <- rbinom(1, 1, tethas[sample])
  alphas[sample] <- alphas[sample] + juego
  betas[sample] <- betas[sample] + 1 - juego
  cont[sample] = cont[sample] + 1
  
  lista3_salida<-list(tethas, alphas, betas, juego, sample)
  return(lista3_salida)
}

# Defino variables necesarias
tethas=c(0.3,0.55,0.45)
alphas<-c(2,2,2)
betas<-c(2,2,2)
juego=0
sample=0
cont<-c(0,0,0)

wins=numeric(366)
lista3_entrada=list(tethas, alphas, betas, juego, sample)

for(i in 1:366){
  lista3_entrada <- greedy_prob_posterior(lista3 = lista3_entrada)
  juego <- lista3_entrada[[4]]
  alphas <- lista3_entrada[[2]]
  betas <- lista3_entrada[[3]]
  sample <- lista3_entrada[[5]]
  cont[sample] <- cont[sample] + 1
  if(i==1){
    wins[i]=juego
  }else{
    wins[i]=wins[i-1]+juego
  }
}


# Graficos

df <- data.frame(maq = c("Maquina 1", "Maquina 2", "Maquina 3"),
                 freq = cont)

ggplot(df, aes(x = maq, y = freq, fill = maq)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Gráfico de Barras de la frecuencia de cada máquina",
       #x = "Variable",
       y = "Frecuencia") +
  theme_minimal()


df <- data.frame(dias = 1:366, wins=wins)
# Crear el gráfico de barras
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
  posterior = rep(c(posterior1,posterior2, posterior3), time = 1),
  maquina = rep(c("Maquina 1", "Maquina 2", "Maquina 3"), each = length(x1))
)

ggplot(df)+
  aes(x = theta, y = posterior, color = maquina)+
  theme(legend.position = "none")+
  geom_line()
