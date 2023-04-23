#--------------Sesión 8 -----------------

insulation=c(10.3,10.1,9.8,9.9,10.2,10.1,9.7,9.9,9.7,10.2)

#T distribución
t.test(insulation, conf.level=0.95)
# Conf.level = nivel de confianza (POR DEFCTO 0,95)
# -Estadistico t de Student, para ver si se encuentra en la region de aceptacion de H0 o de rechazo
# -df grados de libertad (n-1) 
# -p.valor, si es menor que la significacion (0,05), se rechaza H0 y se acepta H1
# -Intervalo de confianza


#Test de hipotesis para mu
#Bilateral (POR DEFECTO two.sided)
t.test(insulation,mu=10,alt="two.sided")
# Nivel de confianza (nc) = 0,95
# Nivel de significacion (1 - nc) = 0,05
# p-value = 0,888 < 0,05 -> No significante no podemos rechazar H0

#Unilateral con la alternativa menor
t.test(insulation,mu=10,alt="less")
# p-value = 0,444 < 0,5 -> Significativo podemos aceptar H0


#TEST DE HIPOTESIS PARA PI (PROPORCION)
# Bilateral cuando estimamos la media (POR DEFECTO two.sided)
# Unilateral cuando la media es mayor o menor que un valor (alt = greater or less)

#En una muestra aleatoria de 500 familias que tienen 
#televisores en cierta ciudad, se encuentra que 340 están
#suscritas a vía digital. Encontrar un intervalo de confianza al
#97% para la proporción real de familias de esa ciudad que están
#suscritas a la vía digital.

prop.test(x=340,n=500,conf.level=0.97)


# Un proveedor de piezas asegura que la proporcion de piezas
# defectuosas es del 2%. Se muestraron 500 partes, hubo 17 defectuosas
# BILATERAL HT
# H0 : p = 0,02
# H1 : p != 0,02
prop.test(x=17,n=500,p=0.02,alt="two.sided")
# UNILATTERAL HT
# H0 : p <= 0,02
# H1 : p > 0,02
prop.test(x=17,n=500,p=0.02,alt="greater")




#1) Una empresa de fabricación de piezas de engranaje ha tomado
#datos del diámetro de diez de ellas (suponer que sigue una distribución Normal):
diametros=c(15.7, 15.4, 15.9, 16.1, 16.7, 15.8, 16.3, 16.4, 15.7, 16)

# (a) Construir un intervalo de confianza para la media de la
#característica en estudio para un nivel de confianza del 90%.
t.test(diametros,conf.level = 0.90) 
#El intervalo de confianza te dice que la media está entre 15.77632 y
#16.22368 con un 90% de probabilidad. La media es 16

# (b) Contrastar la hipótesis de que la media de los diámetros de las
#piezas es menor que 16.2 a un nivel de significación α del 5%.
t.test(diametros,mu=16.2,alt="less",conf.level=0.95)
#La hipótesis es verdadera. La media es menor que 16.2 con una probabilidad del 95%


#2) El servicio médico de una consultora informática decidió hacer un estudio acerca 
#del estado dental de los empleados de la misma en una de sus fábricas, tomando con tal 
#fin una muestra piloto de 160 individuos y observando que 37 de ellos presentaban algún diente cariado.

#(a) Calcular el intervalo de confianza al 99% de la proporción de
#empleados sin problemas dentales.
prop.test(x=123,n=160,conf.level=0.99)
#El intervalo de confianza te dice que la proporción de empleados sin problemas dentales está entre 0.6698621  y
#0.8454791 con un 90% de probabilidad. La proporción es 0.76875


#(b) Contrastar la hipótesis de que la proporción de empleados con
#dientes cariados es igual a un 10% a un nivel de significación α del 1%.
# H0 = 10% -> p-value <= significacion (0,01) Se rechaza H0
# H1 != 10%
prop.test(x=37,n=160,p=0.1,alt="two.sided",conf.level = 0.99)
#Acepta la hipótesis alternativa. La proporción de empleados con dientes cariados no es 0.1 con una probabilidad del 99%


