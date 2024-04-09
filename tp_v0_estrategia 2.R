
#Paso 1: Elegir una maquina al azar(Misma tasa de exitos a priori)
sample=sample(1:3,1)

#Paso 2: Realizar la tirada 

tethas=c(0.3,0.55,0.45)
juego=rbinom(1, 1, tethas[sample])

#Paso 3:Actualizar la tasa de exitos
t3=juego/1


t = c(0.5,0.5,0.5)
cont= c(0,0,0)
if(t[1]>t[2] & t[1]>t[3]){
  juego=rbinom(1, 1, tethas[1])
  cont[1]= cont[1] + juego
  t[1]= cont[1]/1
} else if(t[2]>t[1] & t[2]>t[3]){
  juego=rbinom(1, 1, tethas[2])
  cont[2]= cont[2] + juego
  t[2]= cont[2]/1
} else if(t[3]>t[1] & t[3]>t[2]){
  juego=rbinom(1, 1, tethas[3])
  cont[3]= cont[3] + juego
  t[3]= cont[3]/1
} else if(t[1]==t[2] & t[1]==t[3]){
  sample=sample(1:3,1)
  juego=rbinom(1, 1, tethas[sample])
  cont[sample]= cont[sample] + juego
  t[sample]= cont[sample]/1
  
} else if(t[1]==t[2] & t[1]>t[3]){
  sample=sample(1:2,1)
  juego=rbinom(1, 1, tethas[sample])
  cont[sample]= cont[sample] + juego
  t[sample]= cont[sample]/1
} else if(t[1]==t[3] & t[1]>t[2]){
  sample=sample(c(1,3),1)
  juego=rbinom(1, 1, tethas[sample])
  cont[sample]= cont[sample] + juego
  t[sample]= cont[sample]/1
} else if(t[2]==t[3] & t[2]>t[1]){
  sample=sample(2:3,1)
  juego=rbinom(1, 1, tethas[sample])
  cont[sample]= cont[sample] + juego
  t[sample]= cont[sample]/1
}