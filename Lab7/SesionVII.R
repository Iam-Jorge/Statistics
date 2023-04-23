# EJERCICIOS SESION VII

# 1. La probabilidad de que un servidor se caiga, bajo condiciones
# intensas de uso, en un día es 0,05. Un centro de calculo
# tiene 16 servidores operando en condiciones similares.
# Calcular la probabilidad de que:

  # X=nº servidores que se caen en un día
  # n=16

# a) Se caigan 3 servidores en un dia
dbinom(3,size=16,prob=0.05) 

# b) A lo sumo se caigan 2 servidores al dia
pbinom(2,size=16,prob=0.05)

# c) Al menos 4 se caigan
1-pbinom(3,size=16,prob=0.05)
pbinom(3,size=16,prob=0.05, lower.tail = FALSE)

#-----------------------------------------------------

#2) Supongamos que cada niño tiene probabilidad 0,51 de ser varón, 
# hallar la probabilidad de que una familia de 6 hijos tenga:

  # X=tener niño
  # n=6

# Por lo menos un varón
1-pbinom(0,size=6,prob=0.51)

# Por lo menos una niña
1-dbinom(6,size=6,prob=0.51)
1-pbinom(0,size=6,prob=0.49)

#-----------------------------------------------------

#3) Calcular la probabilidad de que 5 individuos sean atendidos un día cualquiera en el servicio 
#de urgencias del hospital clínico de Málaga, que tiene 500.000 habitantes, suponiendo que 
#la probabilidad de que en Málaga una persona tenga un accidente es p=1/10.000 (pequeña).
n=500000
p=1/10000
lambda=n*p

dpois(5,lambda)


#----------------------------------------------------

#4) Durante un experimento de laboratorio el número promedio de partículas radioactivas que pasan
#a través de un contador en una milésima de segundo es cuatro.

lambda=4

#a) ¿Cuál es la probabilidad de que seis partículas entren al contador en un milisegundo?
dpois(6,lambda)

#b) Encontrar la probabilidad de que pasen 3 o menos partículas en un milisegundo. P(X<=3)
ppois(3,lambda)

#----------------------------------------------------

#5) Una empresa dedicada a la fabricación de circuitos, encuentra que los contratos nacionales 
#tienen licitaciones bajas distribuidas uniformemente entre 20 y 25 unidades (en miles de dólares).
#Calcule la probabilidad de que la baja licitación de embarque del próximo contrato nacional:


# NOTA: lower.tai Parametro booleando, si es TRUE (por defecto), las
# probabilidades son P[X<=x], de lo contrario P[X>x]

#a) Sea inferior a 22 000 dólares.
punif(22,min=20,max=25,lower.tail = T)
#Para resolver este apartado: P(X<22)
#LowerTail es como se recorre el area. Es como si hacemos 1-x

#b) Rebase los 24 000 dólares.
punif(24,min=20,max=25,lower.tail = F)
#Para resolver este apartado: P(X>24)

#c) P(X<x) = 1/5.
a=qunif(1/5,min=20,max=25,lower.tail = T)
a
dunif(a,min=20,max=25)

#d) P(X>x) = 2/5.
b=qunif(2/5,min=20,max=25,lower.tail = F)
dunif(b,min = 20, max = 25)

#-----------------------------------------------------

#6) Sea Z una variable aleatoria normal con una media de 0 y una desviación estándar igual a 1.
#Determinar:

desv<-1
media<-0

#a) P(Z > 2).
pnorm(2,media,desv,lower.tail = F)
1-pnorm(2,mean=0,sd=1)

#b) P(-2 ≤ Z ≤ 2).
pnorm(2,media,desv)-pnorm(-2,media,desv)
pnorm(2,mean=0,sd=1)-(1-pnorm(2,mean = 0,sd=1))

#c) P(0 ≤ Z ≤ 1.73).
pnorm(1.73,media,desv)-pnorm(0,media,desv)

#d) P(Z ≤ a) = 0.5793.
qnorm(0.5793,media,desv )

#e) P(Z > 200). Siendo la media 100 y la desviación estándar 50.
pnorm(200,mean=100,sd=50,lower.tail = F)
1-pnorm(200,mean=100,sd=50)

#---------------------------------------------------

#7) La tensión a la que está sometida la malla que llevan los tubos de rayos catódicos de una 
#terminal gráfica puede medirse en milivoltios (mV) mediante un dispositivo. La lectura de dicha 
#tensión sigue una distribución normal de media 275mV y desviación típica 43mV. La tensión mínima 
#para que la malla no se arrugue es de 200mV y la tensión máxima que soporta sin romperse es de
#375mV.

#a) Calcular la probabilidad de que la malla se arrugue.
pnorm(200,mean=275,sd=45)
#b) Encontrar la probabilidad de que una malla no esté ni arrugada, ni rota.
pnorm(375,mean=275,sd=43)-pnorm(200,mean=275,sd=43)
#c) Si una malla se ha arrugado, ¿Cuál es la probabilidad de que se le haya aplicado una tensión inferior a 175mV?
pnorm(175,mean=275,sd=43)/pnorm(200,mean=275,sd=43)

#---------------------------------------------------

#8) La longitud L en milímetros de las piezas fabricadas en un proceso es una variable aleatoria que se
#distribuye según una N(32,o.32). Considerándose aceptable aquellas cuya medida se encuentra en 
#intervalo (31.1, 32.6). Calcular la probabilidad de que una pieza elegida al azar sea aceptable.
pnorm(32.5,mean=32,sd=0.32)-pnorm(31.1,mean=32,sd=0.32)


#---------------------------------------------------

# Ejercicios 2022/2023
# 4.2.15) Supongamos que en una empresa de ordenadores el
# 80% son de sobremesa y el resto portatiles
# a) Si elegimos al azar 15 ordenadores de esa empresa,
# calcular la probabilidad de elegir menos de 2 portatiles
pbinom(2,size=15,prob=0.2)

# b) Si elegimos aleatoriamente 700 ordenadores de la empresa,
# calcular la probabilidad de que el numero de ordenadores
# portatiles elegido sea mayor de 140
1-pbinom(140,size=700,prob=0.2)  

# 4.2.17) En un estudio estadistico sobre la altura de los
# españoles y sobre los ingleses se han obtenido los siguientes
# datos: Media Españoles 170,2 e Ingleses 175,4
# Desviacion tipoca Españoles 6,4 e Ingleses 5,9

# a) Quien es mas alto en su pais, un español que mide 177cm
# o un ingles que mide 181cm
qnorm(177,170.2,6.4)

# b) cual es la probabilidad de que un español mida mas de 180cm
1-pnorm(180,mean=170.2,sd=6.4)

# c) Cual es la prob. de que un ingles mida entre 160 y 170cm
normal160<-pnorm(160,mean=175.4,sd=5.9)
normal170<-pnorm(170,mean=175.4,sd=5.9)
normal170-normal160

# d) Cual es la prob. de que un español sea mas alto que un ingles





##########----------------EJERCICIOS CUESTIONARIO 2-----------------
# EJERCICIOS DISCRETAS
#Distribución Binomial.

#1)La última novela de un autor ha tenido un gran éxito, hasta el punto 
#de que el 80% de los lectores ya la han leido. Un grupo de 4 amigos son
#aficionados a la lectura:

#a)¿Cuál es la probabilidad de que en el grupo hayan leido la novela 2 personas?
dbinom(2,size=4,prob=0.8)
#b) ¿Y cómo máximo 2?
pbinom(2,size=4,prob=0.8)


#2) Un agente de seguros vende pólizas a cinco personas de la misma edad y que 
#disfrutan de buena salud. Según las tablas actuales, la probabilidad de que una
#persona en estas condiciones viva 30 años o más es 2/3. 
#Hállese la probabilidad de que, transcurridos 30 años, vivan:

#a)Las cinco personas
dbinom(5,size=5,prob=2/3)

#b)Al menos las tres personas  --> P(X>2)
1-pbinom(2,size=5,prob=2/3)

#c)Exactamente dos personas
dbinom(2,size=5,prob=2/3)

#3)Se lanza una moneda cuatro veces. Calcular la probabilidad de 
#que salgan más caras que cruces.

#La probabilidad de salir cara es 0.5
#Para que salgan mas carás que cruces, deben salir al menos 3 caras.
1 - pbinom(2,size=4, prob=0.5)

#4) Si de seis a siete de la tarde se admite que un número de teléfono
#de cada cinco está comunicando, 
#¿cuál es la probabilidad de que, cuando se marquen 10 números de teléfono
#elegidos al azar, sólo comuniquen dos?

dbinom(2,size=10,prob=1/5)

#5) La probabilidad de que un hombre acierte en el blanco es 1/4. 
#Si dispara 10 veces ¿cuál es la probabilidad de que acierte exactamente 
#en tres ocasiones? 
dbinom(3,size=10,prob=1/4)

#¿Cuál es la probabilidad de que acierte por lo menos en una ocasión?
1-pbinom(0,size=10,prob=1/4)


#9)Un laboratorio afirma que una droga causa efectos secundarios en una proporción
#de 3 de cada 100 pacientes. Para contrastar esta afirmación, otro laboratorio 
#elige al azar a 5 pacientes a los que aplica la droga. 
#¿Cuál es la probabilidad de los siguientes sucesos?

#1. Ningún paciente tenga efectos secundarios
dbinom(0,size=5,prob=3/100)
#2. Al menos dos tengan efectos secundarios
#P(X>=2) = 1- P(X<=1)
1-pbinom(1,size=5,prob=3/100)



#Distribución Poisson.

#1) Si un banco recibe en promedio 6 cheques sin fondo por día, 
#¿cuáles son las probabilidades de que reciba, 
#a) cuatro cheques sin fondo en un día dado,
dpois(4,lambda=6)
#b) 10 cheques sin fondos en cualquiera de dos días consecutivos?
dpois(10,lambda = 12)

#2) En la inspección de hojalata producida por un proceso electrolítico continuo, 
#se identifican 0.2 imperfecciones en promedio por minuto. 
#Determine las probabilidades de identificar 
#a) una imperfección en 3 minutos,
dpois(1,lambda = 0.2*3)
#b) al menos dos imperfecciones en 5 minutos, 
1-ppois(1,lambda = 5*0.2)
#c) cuando más una imperfección en 15 minutos.
ppois(1,lambda = 15*0.2)



#3.El 30% de un determinado pueblo ve un concurso que hay en televisi?n.
#Desde el concurso se llama por tel?fono a 10 personas del pueblo elegidas al azar.
#Calcular la probabilidad de que, entre las 10 personas, estuvieran viendo el programa: 
#a) M?s de ocho personas
1-pbinom(8,size=10,prob=0.3)

#b) Algunas de las diez personas
1-pbinom(0,size=10,prob=0.3)

#4) El jefe de recursos humanos de una empresa realiza un test de diez ?tems
#a los aspirantes a un puesto, teniendo en cada ?tems cuatro posibles respuestas, de las
#que s?lo una es correcta. Suponiendo que los aspirantes teniendo la misma probabilidad
#de responder. Se pide hallar las probabilidades para el aspirante:

#P(estar bien)=1/4; p(falsa)=3/4

# a) Conteste todos los ?tems mal
dbinom(10,size=10,prob=3/4)

#b) Conteste al menos cuatro ?tems bien
1 - pbinom(3,size=10,prob=1/4)

#c) Conteste entre cuatro y seis ?tems bien
#P(4<=x<=6)= P(x>=4) - P (x>6)
(1-pbinom(3,size=10,prob=1/4))-(1-pbinom(6,size=10,prob=1/4))

#d) Conteste todos los ?tems bien
dbinom(0,size=10,prob=3/4)
dbinom(10,size=10,prob=1/4)

#e) Conteste menos de tres ?tems bien
pbinom(2,size=10,prob=1/4)




##########----------------EJERCICIOS CUESTIONARIO 2-----------------
# EJERCICIOS CONTINUAS
#Distribución Uniforme

#1. Un reloj de manecillas se detuvo en un punto que no sabemos.
#Determine la probabilidad de que se haya detenido en los primeros 25 
#minutos luego de señalar la hora en punto.
punif(25,min=0,max=60)

#2. Un llamada telefónica llego a un conmutador en un tiempo, al azar, dentro de 
#un periodo de un minuto. El conmutador estuvo ocupado durante
#15 segundos en ese minuto. Calcula la probabilidad de que la llamada 
#haya llegado mientras el comuntador no estuvo ocupado.
punif(45,min=0,max=60)


#DISTRIBUCIÓN NORMAL
#Se supone que los resultados de un examen siguen una distribución normal 
#con media 78 y desviación típica 36. Se pide:

# 1¿Cuál es la probabilidad de que una persona que se presenta el examen 
#obtenga una calificación superior a 72?
1-pnorm(72,mean=78,sd=36) 

#2Calcular la proporción de estudiantes que tienen puntuaciones que exceden 
#por lo menos en cinco puntos de la puntuación que marca la frontera entre 
#el Apto y el No-Apto 
#(son declarados No-Aptos el 25% de los estudiantes que obtuvieron las puntuaciones más bajas)

#P(X<=N)=0.25
qnorm(0.25,mean=78,sd=36) #Redondear respuesta a 54

#3Si se sabe que la calificación de un estudiante es mayor que 72 
#¿cuál es la probabilidad de que su calificación sea, de hecho, superior a 84?
#P(X>84) 
p1=1-pnorm(84, mean=78, sd=36)

#P(X>72) 
p2=1-pnorm(72, mean=78, sd=36)

resultado=p1/p2
resultado








# OTROS EJERCICIOS
#Ejercicio 4.2.7 
#a)
1-pexp(8,1/5)
#b)
dbinom(2,size=5,prob=0.2)

#Ejercicio 4.2.14
#a)
1-pnorm(120,mean=122,sd=2)
#b)
Z<-qnorm(0.98, sd = 1, lower.tail = FALSE)
media=Z*2-120 
0-media
