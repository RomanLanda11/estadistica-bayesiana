Sys.which("make")
library(rstan)

# Ejemplo
N <- 20
y <- 4

model_beta1_stan <- "
data {
  int N;     
  int Y; 
}
parameters {
  real<lower=0, upper=1> pi;
}
model {
  pi ~ beta(2,2); // prior
  Y ~ binomial(N, pi);  // likelihood
}"

model_beta1 <- stan_model(model_code = model_beta1_stan)

data_list <- list(Y = y, N = N)

model_beta1_fit <- sampling(
  object = model_beta1, 
  data = data_list, 
  chains = 2, 
  iter = 500,
  warmup = 100
)

# Ejercicio 1
datos <- read.csv("https://raw.githubusercontent.com/estadisticaunr/estadistica-bayesiana/main/datos/sales.csv")
ggplot(datos)+
  geom_point(aes(x, y))+
  labs(x = "Publicidad ($)", y = "Ventas ($)")

modelo <- stan_model("C:/Users/alumno/Downloads/modelo 1.stan")
data <- list(N=nrow(datos),x=datos$x,y=datos$y)
posterior <- sampling(modelo, data)


#################### Ejercicio 5 ##########################3
datos <- read.csv("https://raw.githubusercontent.com/estadisticaunr/estadistica-bayesiana/main/datos/weather_WU.csv")
ggplot(datos)+
  geom_point(aes(x= temp9am, y=temp3pm, color= location))+
  labs(x = "Temp 9am", y = "Temp 3pm")
table(datos$location)
# Vemos en el grafico que se diferencian dos grupos segun la localidad y que parecen tener la
# misma pendiente esto quiere decir que la 
# Vemos además que las variables se comportan de forma lineal

# Definicion del modelo
# Y_i / mu_i, sigma ~ N(mu_i, sigma)  OBS: Y_i = temp3pm
#       mu_i = B_0 + B_1*temp9am
#       B_0 ~ N(15, 8^2)
#       B_1 ~ N(0, 3^2)
#       sigma ~ N+(12^2)
library(rstan)
stan_data <- list(N=nrow(datos),
                  x=datos$temp9am,
                  y=datos$temp3pm)
modelo <- stan_model(file ="C:/Users/landar/Documents/Facultad/Bayes/estadistica-bayesiana/tp3/modelo 5.stan",
                     data = stan_data,
                     model_name = "modelo_5", 
                     chains = 4,
                     refresh = 1,
                     seed= 2022) #no corre buscar en recursos

posterior <- sampling(modelo, data)
# por caa grado que cambia a las 9 de la mañana se espera que se incrementen entre 0.9 y 1.2 grados promedio a la tarde
# temperatura promedio a las 3 de la taede es (B_0) cuando la temperatura a las 9 de la mañana es 0 grados

# Modelo 2
# Y_i / mu_i, sigma ~ N(mu_i, sigma)  OBS: Y_i = temp3pm
#       mu_i = B_0 + B_1*D_1i     OBS: D_1i = 1 si W y 0 si U
#           Otra forma: mu_i = B_0,j[i]
#           Otra forma: mu_i = B_0,1 + D_1i + B_0,2 + (1-D_1i)
#       B_0,1 ~ N(20, 10^2)
#       B_0,2 ~ N(20, 10^2)
#       sigma ~ N+(15^2)

# B_0 es la temp promedio a las 3 de la tarde en la ciudad U


# Conclusiones: Como vemos que el sigma del modelo 2 es más grande podemos decir que la locación explica menos que la temperatura a las 9 de la mañana para predecir la temp. a las 3 de la tarde
# Existe más incertidumbre en las predicciones de las temp a las 9 cuadno solo conozco la locacion

# Modelo 3
# Y_i / mu_i, sigma ~ N(mu_i, sigma)  OBS: Y_i = temp3pm
#       mu_i = B_0 + B_1*temp9am + B_2*D_i
#       B_0 ~ N(15, 8^2)
#       B_1 ~ N(0, 3^2)
#       B_2 ~ 
#       sigma ~ N+(12^2)

# B_0 es la temp promedio a las 3 de la tarde cuadno estoy en la ciudad de referencia y la temp a las 9 es 0