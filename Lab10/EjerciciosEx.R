
# EJERCICIOS DE EXAMEN

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 1
# La probabilidad de obtener un 6 en un dado trucado es 0.4. Si lanzamos el dado 7 veces, siguiendo su probabilidad 
# una distribución binomial, calcular la probabilidad de obtener un 6 en 5 de los lanzamientos.
dbinom(5,size=7,prob=0.4)

# La probabilidad de obtener un 6 en un dado trucado es 0,7. Si lanzamos el dado 6 veces, siguiendo su probabilidad una 
#distribución binomial, calcular la probabilidad de obtener un 6 en a lo sumo 5 de los lanzamientos.
dbinom(5,size=6,prob=0.7)

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 2
# Durante un día la media de gente que finaliza sus estudios de doctorado en España es 3. Obtener la probabilidad de 
# que al menos 2 de los estudiantes finalice su doctorado hoy.
1 - ppois(1, lambda = 3)


# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 3
# Asumiendo que la probabilidad de superar unas oposiciones y obtener una plaza de funcionario sigue una distribución 
# uniforme entre las calificaciones 5 y 10, calcular la probabilidad de obtener la plaza de funcionario con una 
# calificación en las oposiciones de 8.8.
punif(8.8,min=5,max=10)

# Asumiendo que la probabilidad de superar unas oposiciones y obtener una plaza de funcionarios sigue una distribución
# uniforme entre las calificaciones 5 y 10, calcular la probabilidad de obtener una plaza de funcionario con una 
# calificación en las oposiciones de 6.2.
punif(6.2,min=5,max=10)

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 4
# El indice de masa corporal (IMC) sirve para cuantificar la cantidad de masa corporal de una persona. Este índice 
# sigue una distribución normal de media 22.5 y desviación típica 1.5, considerando que una persona se encuentra en 
# estado de delgadez cuando su valor es menor que 18.5. Obtener la probabilidad de que el IMC de una persona sea 
# menor que 18.1 sabiendo que está delgada.
pDelgada=pnorm(18.5, mean=22.5, sd=1.5)
pDelgada
p2=pnorm(18.1, mean=22.5, sd=1.5)
p2
p2/pDelgada


# El índice de masa corporal (MC) sirve para cuantificar la cantidad de masa corporal de una persona. Este índice 
# sigue una distribución normal de media 22.5 y desviación típica 1.5. Considerando que una persona se encuentra en 
# estado de delgadez cuando su valor es menor que 18.5, obtener la probabilidad de que una persona sea menor que 17.2 
# sabiendo que está delgada.
pDelgada=pnorm(18.5, mean=22.5, sd=1.5)
pDelgada
p2=pnorm(17.2, mean=22.5, sd=1.5)
p2
p2/pDelgada

# El índice de masa corporal (MC) sirve para cuantificar la cantidad de masa corporal de una persona. Este índice 
# sigue una distribución normal de media 22.5 y desviación típica 1.5. Considerando que una persona se encuentra en 
# estado de delgadez cuando su valor es menor que 18.5, obtener la probabilidad de que una persona se encuentra 
# entre le estado de delgadez y un valor de 21.4.
pDelgada=pnorm(18.5, mean=22.5, sd=1.5)
p2=pnorm(21.4, mean=22.5, sd=1.5)
p2 - pDelgada

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 5
# Asumiendo que E es una variable aleatoria que sigue una distribución exponencial con una proporción (rate) de 0.15, 
# obtener el valor de a que cumple P(E ≥ alfa) = 0.75
qexp(0.75,rate=0.15, lower.tail = F)

# Asumiendo que E es una variable aleatoria que sigue una distribución exponencial con una proporción de 0.25, 
# obtener el valor de ¿alfa? Que cumple P(E >= alfa) = 0.5.
qexp(0.5,rate=0.25, lower.tail = F)
  
# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 6
# Se han sometido 20 discos duros (10 de la marca Western Digital y 10 de Seagate) a varias pruebas de estrés para 
# comprobar la fiabilidad de los mismos, obteniéndose el número de sectores erróneos producidos en cada uno de ellos 
# conforme se muestra en la siguiente tabla:

#Western Digital 150 175 210 190 182 177 130 199 187 130
#Seagate 250 231 299 125 100 220 190 191 230 185

#Considerando que esta variable sigue una distribución normal, 
#y para un nivel de significación del 5%, podemos decir que:

western=c(150, 175, 210, 190, 182, 177, 130, 199, 187, 130)
seagate=c(250 ,231 ,299 ,125, 100, 220, 190, 191, 230, 185)

#1º Comprobar si la varianza es igual
var.test(western,seagate,conf.level = 0.95)
pvalueVar=0.03645
pvalueVar<=0.05
#Varianzas distintas

t.test(western,seagate,var.equal = FALSE, alt="two.sided", conf.level = 0.95)
pvalueMed=0.1773
pvalueMed<=0.05
#El p-valor no es menor o igual que la significación, por tanto aceptamos Ho.


# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 7
# Una muestra de 5 medidas de temperatura obtiene los siguientes valores: 18.5, 17, 10.5, 15.5, y 17: Considerando 
# que la población sigue una distribución normal, obtener el límite inferior de intervalo de confianza con una 
# confidencia de 90%. SOLUCION 15.998271
temperaturas <- c(18.5, 17, 10.5, 15.5, 17)
t.test(temperaturas, conf.level = 0.9, alternative = "less")

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 8
# En una muestra aleatoria de 1000 personas, 471 dispone de conexión a internet ADSL. Considerando que queremos saber 
# si el porcentaje de la población completa que dispone de internet ADSL es 0.3, obtener el valor del estadístico que 
# sería necesario para ser comparado con la región de rechazo en un test de hipótesis de proporción.
#H0: prop = 0.3
#H1: prop != 0.3
prop.test(x=471,n=1000,p=0.3,alt="two.sided")

# En una muestra aleatoria de 1000 personas, 488 dispone de conexión a internet ADSL. Considerando que queremos saber
# el porcentaje de la población completa que dispone de internet ADSL es 0.6. Obtener el valor estadístico que sería 
# necesario para ser comparado con la región de rechazo de un test de hipótesis de proporción. SOLUCION 0.529
#H0: prop = 0.6
#H1: prop != 0.6
prop.test(x=488,n=1000,p=0.6,alt="two.sided")

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 9
# Una muestra aleatoria de 5 medidas de temperatura obtiene los siguientes valores: 16.5, 16, 16.5, 17 y 18.
# Considerando que la población sigue una distribución normal, ¿podemos decir que la media de la temperatura de la 
# población es mayor que 16 con una confidencia del 97%?
temperaturas=c(16.5, 16, 16.5, 17, 18)
#H0: u<=16
#H1: u>16
t.test(temperaturas,mu=16,alt="greater",conf.level = 0.97)

# Una muestra aleatoria de medidas de temperatura obtiene los siguientes valores 20, 17, 15.5, 19.8, 10.8. 
# Considerando que la población sigue una distribución normal, ¿Podemos decir que la media de la temperatura de la 
#población es mayor que 13 con una confianza del 98%? SOLUCION Sí rechazando la hipótesis nula. H0 es menor que ¿alfa? Igual a 13.
temperaturas=c(20, 17, 15.5, 19.8, 10.8)
#H0: u<=13
#H1: u>13
t.test(temperaturas,mu=13,alt="greater",conf.level = 0.98)

# -------------------------------------------------------------------------------------------------------------------
# EJERCICIO 10
# En una muestra aleatoria de 1000 personas, 379 son fumadores. ¿Podemos considerar, con una confidencia del 97%, 
# que la proporción de fumadores de la población es diferente de 0.38?
#Ho: prop=0.38
#H1: prop no =0.38
prop.test(x=379,n=1000,p=0.38,alt="two.sided",conf.level=0.97)

# En una muestra aleatoria de 1000 personas, 456 son fumadores. ¿Podemos considerar con una confianza del 98% que la 
# proporción de fumadores de la población es diferente de 0.44? SOLUCION No. Aceptando la hipótesis nula


