
library(dplyr)

base<-read.csv("Simulación QATAR 2022.csv")

#Vamos a realizar primero una simulacion de los resultados posibles del mundial QATAR 2022
#Para ello el criterio será simplemente utilizar el ranking de la fifa a fecha 25/agosto/2022

#La formula para el enfrentamiento será la siguiente: 

#Ejemplo: Brasil tiene 1838 puntos e Inglaterra 1737 puntos

#Se calcula que tanto "más fuerte" es Brasil de Inglaterra 
#A través de 1838/1737 = 1.058 
#Por lo que Brasil es 5.8% más fuerte que Inglaterra

#Se simulará una variable binomial p=0.5+0.058 
#Donde p es la probabilidad de que gané el más fuerte (Brasil en este caso)
#Así el modelo no es determinista y nos apoyamos del ranking otorgado por la FIFA
#El supuesto es qué tan objetivo es el ranking, lo cual es complicado debido a la naturaleza
#del fútbol, el procedimiento del cálculo del puntaje se puede ver en el siguiente link:
#https://www.benditofutbol.com/futbol-internacional/ranking-fifa-cabeza-de-serie-calculos.html

#Por simplicidad del modelo, no consideramos empates

simular_enfrentamiento<-function(equipo1, equipo2){
  equipo_mayor<-ifelse(base[base$PAIS==equipo1, 3]>base[base$PAIS==equipo2, 3],
                                                        equipo1, equipo2)
  equipo_menor<-ifelse(base[base$PAIS==equipo1, 3]<base[base$PAIS==equipo2, 3],
                       equipo1, equipo2)
  
  puntaje_mayor<-base[base$PAIS==equipo_mayor, 3]
  puntaje_menor<-base[base$PAIS==equipo_menor, 3]
  p=0.5+(puntaje_mayor/puntaje_menor-1)
  ganador<-ifelse(rbinom(1, 1, p)==1, equipo_mayor, equipo_menor)
  ganador
}
#Despues afinamos mejor la funcion

# x<-NULL
# for(i in 1:100){
#   x[i]<-simular_enfrentamiento("BRASIL", "MARRUECOS")
# }
# x[x=="BRASIL"]

ganador<-0
finalista1<-0
finalista2<-0
semi1<-0
semi2<-0
semi3<-0
semi4<-0

datos<-data.frame(ganador, finalista1, finalista2, semi1, semi2, semi3, semi4)
datos
    
set.seed(0)  
for(i in 1:1000){
  #### GRUPO A ####
  grupo<-filter(base, GRUPO=="A")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoA<-grupo
  grupoA<-arrange(grupoA, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoA[1,1] #PRIMERO DE GRUPO
  grupoA[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO B ####
  grupo<-filter(base, GRUPO=="B")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoB<-grupo
  grupoB<-arrange(grupoB, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoB[1,1] #PRIMERO DE GRUPO
  grupoB[2,1] #SEGUNDO DE GRUPO
  
  
  #### GRUPO C ####
  grupo<-filter(base, GRUPO=="C")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoC<-grupo
  grupoC<-arrange(grupoC, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoC[1,1] #PRIMERO DE GRUPO
  grupoC[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO D ####
  grupo<-filter(base, GRUPO=="D")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoD<-grupo
  grupoD<-arrange(grupoD, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoD[1,1] #PRIMERO DE GRUPO
  grupoD[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO E ####
  grupo<-filter(base, GRUPO=="E")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoE<-grupo
  grupoE<-arrange(grupoE, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoE[1,1] #PRIMERO DE GRUPO
  grupoE[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO F ####
  grupo<-filter(base, GRUPO=="F")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoF<-grupo
  grupoF<-arrange(grupoF, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoF[1,1] #PRIMERO DE GRUPO
  grupoF[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO G ####
  grupo<-filter(base, GRUPO=="G")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoG<-grupo
  grupoG<-arrange(grupoG, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoG[1,1] #PRIMERO DE GRUPO
  grupoG[2,1] #SEGUNDO DE GRUPO
  
  #### GRUPO H ####
  grupo<-filter(base, GRUPO=="H")
  grupo$PUNTOS<-0
  grupo
  
  #Enfrentamiento 1-2
  g<-simular_enfrentamiento(grupo[1,1], grupo[2,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-3
  g<-simular_enfrentamiento(grupo[1,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 1-4
  g<-simular_enfrentamiento(grupo[1,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-3
  g<-simular_enfrentamiento(grupo[2,1], grupo[3,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 2-4
  g<-simular_enfrentamiento(grupo[2,1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  #Enfrentamiento 3-4
  g<-simular_enfrentamiento(grupo[3, 1], grupo[4,1])
  grupo[grupo$PAIS==g, 4]<-grupo[grupo$PAIS==g, 4]+3
  
  grupoH<-grupo
  grupoH<-arrange(grupoH, desc(PUNTOS), desc(PUNTAJE))
  
  
  grupoH[1,1] #PRIMERO DE GRUPO
  grupoH[2,1] #SEGUNDO DE GRUPO
  
  #### OCTAVOS DE FINAL ####
  
  A1_B2<-simular_enfrentamiento(grupoA[1,1], grupoB[2,1])
  C1_D2<-simular_enfrentamiento(grupoC[1,1], grupoD[2,1])
  
  E1_F2<-simular_enfrentamiento(grupoE[1,1], grupoF[2,1])
  G1_H2<-simular_enfrentamiento(grupoG[1,1], grupoH[2,1])
  
  
  B1_A2<-simular_enfrentamiento(grupoB[1,1], grupoA[2,1])
  D1_C2<-simular_enfrentamiento(grupoD[1,1], grupoC[2,1])
  
  F1_E2<-simular_enfrentamiento(grupoF[1,1], grupoE[2,1])
  H1_G2<-simular_enfrentamiento(grupoH[1,1], grupoG[2,1])
  
  #### CUARTOS DE FINAL ####
  
  cuarto1<-simular_enfrentamiento(A1_B2, C1_D2)
  cuarto2<-simular_enfrentamiento(E1_F2, G1_H2)
  datos[i, 4]<-cuarto1
  datos[i, 5]<-cuarto2
  
  cuarto3<-simular_enfrentamiento(B1_A2, D1_C2)
  cuarto4<-simular_enfrentamiento(F1_E2, H1_G2)
  datos[i, 6]<-cuarto3
  datos[i, 7]<-cuarto4
  
  
  #### SEMIFINALES ####
  
  semi1<-simular_enfrentamiento(cuarto1, cuarto2)
  semi2<-simular_enfrentamiento(cuarto3, cuarto4)
  
  datos[i, 2]<-semi1
  datos[i, 3]<-semi2
  
  #### FINAL ####
  
  final<-simular_enfrentamiento(semi1, semi2)
  datos[i, 1]<-final
  
}

datos %>% group_by(ganador) %>% 
  summarize(victorias=n()) %>%
  mutate(porcentaje_victoria=victorias/1000) %>% 
  arrange(-porcentaje_victoria)
  
head(datos)

base %>% arrange(-PUNTAJE)

