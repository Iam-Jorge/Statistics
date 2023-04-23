# EJERCICIOS SESIONES 6-9

# ******************************************************************************
# DISTRIBUCION EXPONENCIAL
# ******************************************************************************

# Ejercicio 4.2.7 
# Consideramos que la variable aleatoria T “tiempo de fallo en años” de un cierto componente de un sistema se modela 
# bien por una distribución exponencial con tiempo medio para el fallo de 5 años.

# a) Calcular la probabilidad de que un componente dado funcione después de ocho años.P(X>8)
1-pexp(8,1/5)
pexp(8, 1/5, lower.tail = FALSE)

# b) Si se instalan cinco de estos componentes en diferentes sistemas, ¿cuál es la probabilidad de que dos funcionen 
# al final de ocho años?
dbinom(2,size=5,prob=0.2)



# ******************************************************************************
# DISTRIBUCION NORMAL
# ******************************************************************************

# Ejercicio 4.2.14 
# Una normativa europea obliga a que en los envases de yogur no debe haber menos de 120 gramos. La máquina dosificadora 
# de una empresa láctea hace los envases de yogur según una ley normal de desviación típica de 2 gramos y media 122 
# gramos. Contestar, razonadamente, las cuestiones:
# a) ¿Qué tanto por ciento de los envases de yogur de esta empresa cumplirá la normativa?
1-pnorm(120,mean=122,sd=2) # P(X>=120) = 1-P(x<120)
pnorm(120,mean=122,sd=2,lower.tail = F) # lower.tail = F para decir que es P(x>=120)
# b) ¿Cuál deberá de ser la media (μ) de la ley normal con la que la máquina dosificadora hace envases para que el 98% 
# de la producción de yogures de esta empresa cumpla la normativa? (manteniéndose la desviación típica igual a 2gr.
dt = 2
valor = qnorm(0.98, mean=0, sd=1, lower.tail = T); valor
media = 120-valor*dt; media
# Media = 115.8925 valor por debajo de 120. No es el resultado esperado
valor = qnorm(0.98, mean=0, sd=1, lower.tail = F); valor

# b) ¿Cuál	 deberá	 de	 ser	 la	 media	 (µ)	 de	 la	 ley	 normal	 con	 la	 que	 la	 máquina dosificadora	hace	
# envases	para	que	el	98%	de	los	productos	de	yogures	de	estaempresa	cumpla	la	normativa? (manteniéndose	la	
# desviación	típica	igual	a	2gr.)
Z<-qnorm(0.98, sd = 1, lower.tail = FALSE)
media=Z*2-120 
0-media

# ---------------------------------------------------------------------------------------------------------------------
# Ejercicio 4.2.17) 
# En un estudio estadistico sobre la altura de los españoles y sobre los ingleses se han obtenido los siguientes
# datos: Media Españoles 170,2 e Ingleses 175,4 Desviacion tipoca Españoles 6,4 e Ingleses 5,9
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

# ---------------------------------------------------------------------------------------------------------------------
# 1). Sea Z una variable aleatoria normal con una media de 0 y una desviación estándar igual a 1.Determinar:
desv<-1
media<-0
# a) P(Z > 2).
pnorm(2,media,desv,lower.tail = F)
1-pnorm(2,mean=0,sd=1)
# b) P(-2 ≤ Z ≤ 2).
pnorm(2,media,desv)-pnorm(-2,media,desv)
pnorm(2,mean=0,sd=1)-(1-pnorm(2,mean = 0,sd=1))
# c) P(0 ≤ Z ≤ 1.73).
pnorm(1.73,media,desv)-pnorm(0,media,desv)
# d) P(Z ≤ a) = 0.5793.
qnorm(0.5793,media,desv )
# e) P(Z > 200). Siendo la media 100 y la desviación estándar 50.
pnorm(200,mean=100,sd=50,lower.tail = F)
1-pnorm(200,mean=100,sd=50)

# ---------------------------------------------------------------------------------------------------------------------
# 2). La tensión a la que está sometida la malla que llevan los tubos de rayos catódicos de una terminal gráfica puede 
# medirse en milivoltios (mV) mediante un dispositivo. La lectura de dicha tensión sigue una distribución normal de 
# media 275mV y desviación típica 43mV. La tensión mínima para que la malla no se arrugue es de 200mV y la tensión 
# máxima que soporta sin romperse es de 375mV.
#a) Calcular la probabilidad de que la malla se arrugue.
pnorm(200,mean=275,sd=45)
#b) Encontrar la probabilidad de que una malla no esté ni arrugada, ni rota.
pnorm(375,mean=275,sd=43)-pnorm(200,mean=275,sd=43)
#c) Si una malla se ha arrugado, ¿Cuál es la probabilidad de que se le haya aplicado una tensión inferior a 175mV?
pnorm(175,mean=275,sd=43)/pnorm(200,mean=275,sd=43)

# ---------------------------------------------------------------------------------------------------------------------
# 3). La longitud L en milímetros de las piezas fabricadas en un proceso es una variable aleatoria que se distribuye 
# según una N(32,o.32). Considerándose aceptable aquellas cuya medida se encuentra en intervalo (31.1, 32.6). 
# Calcular la probabilidad de que una pieza elegida al azar sea aceptable.
pnorm(32.5,mean=32,sd=0.32)-pnorm(31.1,mean=32,sd=0.32)


# ---------------------------------------------------------------------------------------------------------------------
# 4). Se supone que los resultados de un examen siguen una distribución normal con media 78 y desviación típica 36. Se pide:
# a) ¿Cuál es la probabilidad de que una persona que se presenta el examen obtenga una calificación superior a 72?
1-pnorm(72,mean=78,sd=36) 

# b) Calcular la proporción de estudiantes que tienen puntuaciones que excedenpor lo menos en cinco puntos de la 
# puntuación que marca la frontera entre el Apto y el No-Apto (son declarados No-Aptos el 25% de los estudiantes que 
# obtuvieron las puntuaciones más bajas)
#P(X<=N)=0.25
qnorm(0.25,mean=78,sd=36) #Redondear respuesta a 54

# c) Si se sabe que la calificación de un estudiante es mayor que 72 ¿cuál es la probabilidad de que su calificación 
# sea, de hecho, superior a 84?
#P(X>84) 
p1=1-pnorm(84, mean=78, sd=36)
#P(X>72) 
p2=1-pnorm(72, mean=78, sd=36)
resultado=p1/p2
resultado


# ******************************************************************************
# DISTRIBUCION DE POISSON
# ******************************************************************************

# 1). Si hay 12 automoviles que cruzan un puente por minuto en promedio, ¿cual es la probabilidad de que 17 o mas crucen 
# el puente en un minuto dado?
mu = 12
ppois(16, mu, lower.tail = FALSE)
# lower.tail = FALSE (Probabilidad mayor que > )

# ---------------------------------------------------------------------------------------------------------------------
# 2) Calcular la probabilidad de que 5 individuos sean atendidos un día cualquiera en el servicio de urgencias del 
# hospital clínico de Málaga, que tiene 500.000 habitantes, suponiendo que la probabilidad de que en Málaga una persona 
# tenga un accidente es p=1/10.000 (pequeña).
n=500000
p=1/10000
lambda=n*p
dpois(5,lambda)

# ---------------------------------------------------------------------------------------------------------------------
# 3). Durante un experimento de laboratorio el número promedio de partículas radioactivas que pasan a través de un 
# contador en una milésima de segundo es cuatro.
lambda=4
#a) ¿Cuál es la probabilidad de que seis partículas entren al contador en un milisegundo?
dpois(6,lambda)
#b) Encontrar la probabilidad de que pasen 3 o menos partículas en un milisegundo. P(X<=3)
ppois(3,lambda)

# ---------------------------------------------------------------------------------------------------------------------
# 4). Si un banco recibe en promedio 6 cheques sin fondo por día, ¿cuáles son las probabilidades de que reciba, 
#a) cuatro cheques sin fondo en un día dado,
dpois(4,lambda=6)
#b) 10 cheques sin fondos en cualquiera de dos días consecutivos?
dpois(10,lambda = 12)

# ---------------------------------------------------------------------------------------------------------------------
# 5). En la inspección de hojalata producida por un proceso electrolítico continuo, se identifican 0.2 imperfecciones en 
# promedio por minuto. Determine las probabilidades de identificar 
#a) una imperfección en 3 minutos,
dpois(1,lambda = 0.2*3)
#b) al menos dos imperfecciones en 5 minutos, 
1-ppois(1,lambda = 5*0.2)
#c) cuando más una imperfección en 15 minutos.
ppois(1,lambda = 15*0.2)


# ******************************************************************************
# DISTRIBUCION DE BINOMIAL (Numero de exitos)
# ******************************************************************************

# Ejercicio 4.2.15) 
# Supongamos que en una empresa de ordenadores el 80% son de sobremesa y el resto portatiles
# a) Si elegimos al azar 15 ordenadores de esa empresa,calcular la probabilidad de elegir menos de 2 portatiles
pbinom(2,size=15,prob=0.2)

# b) Si elegimos aleatoriamente 700 ordenadores de la empresa,calcular la probabilidad de que el numero de ordenadores
# portatiles elegido sea mayor de 140
1-pbinom(140,size=700,prob=0.2) 


# ---------------------------------------------------------------------------------------------------------------------
# 1). La probabilidad de que un servidor se caiga, bajo condiciones intensas de uso, en un día es 0,05. Un centro de 
# calculo tiene 16 servidores operando en condiciones similares. Calcular la probabilidad de que:
# X=nº servidores que se caen en un día
# n=16

# a) Se caigan 3 servidores en un dia
dbinom(3,size=16,prob=0.05) 
# b) A lo sumo se caigan 2 servidores al dia
pbinom(2,size=16,prob=0.05)
# c) Al menos 4 se caigan
1-pbinom(3,size=16,prob=0.05)
pbinom(3,size=16,prob=0.05, lower.tail = FALSE)


# ---------------------------------------------------------------------------------------------------------------------
# 2). Supongamos que cada niño tiene probabilidad 0,51 de ser varón, hallar la probabilidad de que una familia 
# de 6 hijos tenga:
# X=tener niño
# n=6
# Por lo menos un varón
1-pbinom(0,size=6,prob=0.51)

# Por lo menos una niña
1-dbinom(6,size=6,prob=0.51)
1-pbinom(0,size=6,prob=0.49)


# ---------------------------------------------------------------------------------------------------------------------
# 3). La última novela de un autor ha tenido un gran éxito, hasta el punto de que el 80% de los lectores ya la han leido. 
# Un grupo de 4 amigos son aficionados a la lectura:

#a)¿Cuál es la probabilidad de que en el grupo hayan leido la novela 2 personas?
dbinom(2,size=4,prob=0.8)
#b) ¿Y cómo máximo 2?
pbinom(2,size=4,prob=0.8)

# ---------------------------------------------------------------------------------------------------------------------
# 4). Un agente de seguros vende pólizas a cinco personas de la misma edad y quedisfrutan de buena salud. Según las tablas 
# actuales, la probabilidad de que una persona en estas condiciones viva 30 años o más es 2/3. Hállese la probabilidad 
#de que, transcurridos 30 años, vivan:

#a)Las cinco personas
dbinom(5,size=5,prob=2/3)
#b)Al menos las tres personas  --> P(X>2)
1-pbinom(2,size=5,prob=2/3)
#c)Exactamente dos personas
dbinom(2,size=5,prob=2/3)

# ---------------------------------------------------------------------------------------------------------------------
# 5). Se lanza una moneda cuatro veces. Calcular la probabilidad de que salgan más caras que cruces.
# a) La probabilidad de salir cara es 0.5. Para que salgan mas carás que cruces, deben salir al menos 3 caras.
1 - pbinom(2,size=4, prob=0.5)

# b) Si de seis a siete de la tarde se admite que un número de teléfono de cada cinco está comunicando, ¿cuál es la 
# probabilidad de que, cuando se marquen 10 números de teléfono elegidos al azar, sólo comuniquen dos?
dbinom(2,size=10,prob=1/5)

# ---------------------------------------------------------------------------------------------------------------------
# 6). La probabilidad de que un hombre acierte en el blanco es 1/4. 
# a) Si dispara 10 veces ¿cuál es la probabilidad de que acierte exactamente en tres ocasiones? 
dbinom(3,size=10,prob=1/4)
# b) ¿Cuál es la probabilidad de que acierte por lo menos en una ocasión?
1-pbinom(0,size=10,prob=1/4)

# ---------------------------------------------------------------------------------------------------------------------
# 7). Un laboratorio afirma que una droga causa efectos secundarios en una proporción de 3 de cada 100 pacientes. 
# Para contrastar esta afirmación, otro laboratorio elige al azar a 5 pacientes a los que aplica la droga. 
# a) Ningún paciente tenga efectos secundarios
dbinom(0,size=5,prob=3/100)
# b) Al menos dos tengan efectos secundarios
#P(X>=2) = 1- P(X<=1)
1-pbinom(1,size=5,prob=3/100)

# ---------------------------------------------------------------------------------------------------------------------
# 8). El 30% de un determinado pueblo ve un concurso que hay en television. Desde el concurso se llama por telofono a 10 
# personas del pueblo elegidas al azar. Calcular la prob. de que, entre las 10 personas, estuvieran viendo el programa: 
# a) Mas de ocho personas
1-pbinom(8,size=10,prob=0.3)
# b) Algunas de las diez personas
1-pbinom(0,size=10,prob=0.3)

# ---------------------------------------------------------------------------------------------------------------------
# 9). El jefe de recursos humanos de una empresa realiza un test de diez ?tems a los aspirantes a un puesto, teniendo 
# en cada ?tems cuatro posibles respuestas, de las que solo una es correcta. Suponiendo que los aspirantes teniendo la 
# misma probabilidad de responder. Se pide hallar las probabilidades para el aspirante:
# P(estar bien)=1/4; p(falsa)=3/4
# a) Conteste todos los ?tems mal
dbinom(10,size=10,prob=3/4)

# b) Conteste al menos cuatro ?tems bien
1 - pbinom(3,size=10,prob=1/4)

# c) Conteste entre cuatro y seis ?tems bien. P(4<=x<=6)= P(x>=4) - P (x>6)
(1-pbinom(3,size=10,prob=1/4))-(1-pbinom(6,size=10,prob=1/4))

#d) Conteste todos los ?tems bien
dbinom(0,size=10,prob=3/4)
dbinom(10,size=10,prob=1/4)

#e) Conteste menos de tres ?tems bien
pbinom(2,size=10,prob=1/4)



# ******************************************************************************
# DISTRIBUCION DE UNIFORME
# ******************************************************************************

# 1). Una empresa dedicada a la fabricación de circuitos, encuentra que los contratos nacionales tienen licitaciones 
# bajas distribuidas uniformemente entre 20 y 25 unidades (en miles de dólares).Calcule la probabilidad de que la baja 
# licitación de embarque del próximo contrato nacional:

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


# ---------------------------------------------------------------------------------------------------------------------
# 2). Un reloj de manecillas se detuvo en un punto que no sabemos.Determine la probabilidad de que se haya detenido 
# en los primeros 25 minutos luego de señalar la hora en punto.
punif(25,min=0,max=60)

# ---------------------------------------------------------------------------------------------------------------------
# 3). Un llamada telefónica llego a un conmutador en un tiempo, al azar, dentro de un periodo de un minuto. 
# El conmutador estuvo ocupado durante 15 segundos en ese minuto. Calcula la probabilidad de que la llamada 
# haya llegado mientras el comuntador no estuvo ocupado.
punif(45,min=0,max=60)











# ******************************************************************************
# INFERENCIA ESTADISTICA
# ******************************************************************************

insulation=c(10.3,10.1,9.8,9.9,10.2,10.1,9.7,9.9,9.7,10.2)

#T distribución
t.test(insulation, conf.level=0.95)
# Conf.level = nivel de confianza (POR DEFCTO 0,95)
# -Estadistico t de Student, para ver si se encuentra en la region de aceptacion de H0 o de rechazo
# -df grados de libertad (n-1) 
# -p.valor, si es menor que la significacion (0,05), se rechaza H0 y se acepta H1


#Test de hipotesis para mu
#Bilateral (POR DEFECTO two.sided)
t.test(insulation,mu=10,alt="two.sided")
# Nivel de confianza (nc) = 0,95
# Nivel de significacion (1 - nc) = 0,05
# p-value = 0,888 < 0,05 -> No significante no podemos rechazar H0

#Unilateral con la alternativa menor
t.test(insulation,mu=10,alt="less")
# p-value = 0,444 < 0,05 -> No significativo no podemos rechazar H0


#TEST DE HIPOTESIS PARA PI (PROPORCION)
# Bilateral cuando estimamos la media (POR DEFECTO two.sided)
# Unilateral cuando la media es mayor o menor que un valor (alt = greater or less)

# 1). En una muestra aleatoria de 500 familias que tienentelevisores en cierta ciudad, se encuentra que 340 están suscritas 
# a vía digital. Encontrar un intervalo de confianza al 97% para la proporción real de familias de esa ciudad que 
# están suscritas a la vía digital.
prop.test(x=340,n=500,conf.level=0.97)


# 2). Un proveedor de piezas asegura que la proporcion de piezas defectuosas es del 2%. Se muestraron 500 partes, 
# hubo 17 defectuosas
# BILATERAL HT
# H0 : p = 0,02
# H1 : p != 0,02
prop.test(x=17,n=500,p=0.02,alt="two.sided")

# UNILATTERAL HT
# H0 : p <= 0,02
# H1 : p > 0,02
prop.test(x=17,n=500,p=0.02,alt="greater")


# 3). Una empresa de fabricación de piezas de engranaje ha tomado datos del diámetro de diez de ellas 
# (suponer que sigue una distribución Normal):
diametros=c(15.7, 15.4, 15.9, 16.1, 16.7, 15.8, 16.3, 16.4, 15.7, 16)

# a) Construir un intervalo de confianza para la media de la característica en estudio para un nivel de confianza del 90%.
t.test(diametros,conf.level = 0.90) 
#El intervalo de confianza dice que la media está entre 15.77632 y 16.22368 con un 90% de probabilidad. La media es 16

# b) Contrastar la hipótesis de que la media de los diámetros de las piezas es menor que 16.2 a un nivel de 
# significación α del 5%.
# H0 : diametro <= 16.2
# H1 : diametro > 16.2
t.test(diametros,mu=16.2,alt="less",conf.level=0.95)
# La hipótesis es verdadera. La media es menor que 16.2 con una probabilidad del 95%
# p-value = 0.06781 < 0.05 -> No podemos rechazar H0


# 4). El servicio médico de una consultora informática decidió hacer un estudio acerca del estado dental de los 
# empleados de la misma en una de sus fábricas, tomando con tal fin una muestra piloto de 160 individuos y observando 
# que 37 de ellos presentaban algún diente cariado.

# a) Calcular el intervalo de confianza al 99% de la proporción de empleados sin problemas dentales.
prop.test(x=123,n=160,conf.level=0.99)
# El intervalo de confianza te dice que la proporción de empleados sin problemas dentales está entre 0.6698621  y
# 0.8454791 con un 90% de probabilidad. La proporción es 0.76875

# b) Contrastar la hipótesis de que la proporción de empleados con dientes cariados es igual a un 10% a un nivel de 
# significación α del 1%.
# H0 = 10% -> p-value <= significacion (0,01) Se rechaza H0
# H1 != 10%
prop.test(x=37,n=160,p=0.1,alt="two.sided",conf.level = 0.99)
# Acepta la hipótesis alternativa. La proporción de empleados con dientes cariados no es 0.1 con una probabilidad del 99%


# ******************************************************************************
# INFERENCIA CON DOS VARIABLES
# ******************************************************************************

# 1). Se han probado dos marcas de automóviles comparando las velocidades máximas que alcanzan. Se han hecho 12 
# pruebas con cada uno de los vehículos obteniendo los siguientes resultados:
# A: 256, 239, 222, 207, 228, 241, 212, 216, 236, 219, 225, 230
# B: 212, 240, 263, 275, 237, 271, 261, 223, 234, 220, 232, 230
# a). Suponiendo normalidad en la variable ¿existe evidencia estadística de que el vehículo de la marca B alcanza velocidades
# máximas en promedio mayores que el de la marca A? (Considerar una significancia del 5%)

velocidadA=c(256, 239, 222, 207, 228, 241, 212, 216, 236, 219, 225, 230)
velocidadB=c(212, 240, 263, 275, 237, 271, 261, 223, 234, 220, 232, 230)
# Son medias independientes

# H0: Las varianzas son iguales
# H1: Las varianzas son distintas
var.test(velocidadA,velocidadB)
# p-value = 0.1866 < 0.05 significacion. Se acepta H0: Las varianzas son iguales


# b). Suponiendo que los datos siguen una distribución normal, ¿existe evidencia estadística de que el vehículo 
# de la marca B es  5km/h mayor que la velocidad máxima media de la marca A con una significancia del 5%?

# H0: mediaA <= mediaB
# H1: mediaA > mediaB
t.test(velocidadA,velocidadB,var.equal = TRUE,alt="less")
# p-value = 0.0338 < 0.05 significacion. Se rechaza H0: mediaA <= mediaB
# Existe evidencia de que la velocidad maxima media del vehiculo marca B es mayor que la del vehiculo marca A


# 2). El departamento de calidad de una empresa quiere comprobar si se mantiene el nivel de calidad de un producto de una 
# semana a otra, tras un incidente registrado durante el fin de semana. La calidad del artículo se supone normalmente 
# distribuida y se han obtenido los siguientes resultados de una m.a.s de 8 productos:
# Semana 1: 93, 86, 90, 90, 94, 91, 92, 96
# Semana 2: 93, 87, 97, 90, 88, 87, 84, 93
# Realizar el contraste correspondiente a un nivel de significación del 1% que permita discriminar si se mantiene el 
# nivel de calidad.

semana1=c(93, 86, 90, 90, 94, 91, 92, 96)
semana2=c(93, 87, 97, 90, 88, 87, 84, 93)
# Son datos pareados

# H0: calidad semana1 = calidad semana2
# H1: calidad semana1 != calidad semana2
t.test(semana1,semana2,paired = TRUE,conf.level = 0.99)
# p-value = 0.3596 < 0.01 significacon. Se acepta H0: calidad semana1 = calidad semana2



# Examen Extraordinario 2018
# El número diario de twits realizados durante los 5 días previos al examen que contienen el hashtag #laestadisticamola 
# han sido: 50, 42, 53, 60, 37. Mientras tanto, el número de twits que contenían el hashtag #aprobareestadistica esos 
# mismos días ha sido de: 40, 51, 62, 55 y 64. Si suponemos que ambas distribuciones de tuits se pueden considerar 
# normales y considerando un nivel de significación del 5%, se pide:

laEstadisticaMola=c(50, 42, 53, 60, 37)
aprobareEstadistica=c(40, 51, 62, 55, 64)

# ¿Podemos concluir que el número de twits medio que contienen el hastagh #laestadisticamola es mayor que 39?

# H0: nº twits medio de #laestadisticamola > 39
# H1: nº twits medio de #laestadisticamola <= 39
t.test(laEstadisticaMola,39,var.equal = TRUE,alt="great")
# p-value = 0.1989 < 0,05. Se acepta H0: nº twits #laestadisticamola > 39

# Comprobar si podemos llegar a la conclusión de que las medias del número de tuits que usan esos hashtags difieren.
t.test(laEstadisticaMola,aprobareEstadistica,var.equal = TRUE,alt="great")

