# SESION VI Distribuciones de probabilidad

# DISTRIBUCIONES
# dxxx(x, ...) Funcion de masa de probabilidad f(x)
# pxxx(q, ...) Funcion de distribucion acumulada hasta q, F(x)
# qxxx (p, ...) Cuantil para el cual P(X <= q) = p
# rxxx (n, ...) Generador de numeros aleatorios
# xxx = nombre de la distribucion (binom, pois ...)


# Binomial (n, p) y se desea calcular X
dbinom(5,size=10,prob=1/2) # pdf
# Probabilidad de hasta 5 suceso (X<=5) un n y todos los anteriores
pbinom(5,size=10,prob=1/2) # cdf

# Probabilidad de más de 5 suceso (X>5)
1-pbinom(5,size=10,prob=1/2)
pbinom(5,size=10,prob=1/2, lower.tail = FALSE)
# Quantil P(X<=v)=0,8
qbinom(0.8, size=10,prob=1/2)

# Muestra aleatoria de 1000 valores simulados a partir de (10,1/2)
binoR<-rbinom(1000,size=10,prob=1/2); binoR
par(mfrow=c(1,2)) # divide la pantalla gráfica
graphics.off()
barplot(table(binoR)) # diagrama barras datos generados antes
binof<-dbinom(0:10,size=10,prob=1/2) # función de densidad
plot(0:10,binof,type="h");

# POISSON (Succesos raros)
# n grande n > 30 y p pequeña p < 0,1
# landa = numero de ocurrencias en un determinado espacio o tiempo
# Se usan para modelar eventos en los que se cuentan los resultados,
# las veces que occure un evento dentro de un peiodo de tiempo o disntacia
poissonR<-rpois(100,lambda=2);poissonR 
barplot(table(poissonR))
poissonf<-dpois(0:10,lambda=2) 
plot(0:10,poissonf,type="h")

#DISTRIBUCI?N UNIFORME
graphics.off()
u10<-runif(10,min=2,max=8) # k = 10 values 
u100<-runif(100,2,8) # k = 100 values 
u1000<-runif(1000,2,8) # k = 1000 values 
hist(u100,prob=TRUE) 
hist(u1000,prob=TRUE)

#DISTRIBUCION NORMAL
x<-seq(-3,3,length=300)
plot(x,dnorm(x,mean=0,sd=1),type="l")
plot(x,pnorm(x,0,1),type="l")
dnorm(-1,0,1);pnorm(-1,0,1)

normalR100<-rnorm(100,mean=12,sd=2) # k=100 values 
normalR1000<-rnorm(1000,12,2) #k=1000 values 
hist(normalR100) 
hist(normalR1000)

#DISTRIBUCI)N EXPONENCIAL
# Tiempos de vida de individuos, componentes, tiepos de servicio
# Funcion de densidad
x <-



exponR<-rexp(100,rate=1/5);exponR 
par(fig=c(0,1,0,.4) ) #figuracon 40% del diagrama 
boxplot(exponR,horizontal=TRUE) 
par(fig=c(0,1,.25,1), new=TRUE) 
hist(exponR,prob=TRUE,main="",ylim=c(0,0.2)) 
curve(dexp(x,rate=1/5), lwd=2, add=TRUE); lines(density(exponR),lty=2)

#TEOREMA CENTRAL DEL LIMITE
x1<-rpois(30,2);x2<-rpois(30,2);x3<-rpois(30,2) 
par(mfrow=c(1,3))
graphics.off()
barplot(table(x1)/length(x1),ylim=c(0,0.3))
barplot(table(x2)/length(x2),ylim=c(0,0.3)) 
barplot(table(x3)/length(x3),ylim=c(0,0.3)) 
S3a<-((x1+x2+x3)-3*2)/sqrt(3*2)

x1<-rpois(1000,2);x2<-rpois(1000,2);x3<-rpois(1000,2)
barplot(table(x1)/length(x1),ylim=c(0,0.3))
barplot(table(x2)/length(x2),ylim=c(0,0.3))
barplot(table(x3)/length(x3),ylim=c(0,0.3)) 
S3b<-((x1+x2+x3)-3*2)/sqrt(3*2) 
par(mfrow=c(1,2)) 
points<-seq(-3,3,length=length(S3b))
points.normal <-dnorm(points, mean = 0,sd = 1) 
hist(S3a,probability=TRUE);lines(points, points.normal)
hist(S3b,probability=TRUE);lines(points, points.normal)


# EJERCICIOS
# Ejercicio 4.2.7 Consideramos que la v. a. T “tiempo de fallo en años” 
# de un cierto componente de un sistema se modela bien por una 
# distribución exponencial con tiempo medio para el fallo de 5 años.

# a)Calcular la probabilidad de que un componente dado funcione 
# después de ocho años.
1-pexp(8,1/5)

# b)Si se instalan cinco de estos componentes en diferentes sistemas, 
# ¿cuál es la probabilidad de que dos funcionen al final de ocho años? 
dbinom(2,size=5,prob=0.2)


# Ejercicio 4.2.14 
# Una normativa europea obliga a que en los envases de yogur no debe haber 
# menos de 120 gramos. La máquina dosificadora de una empresa láctea hace los 
# envases de yogur según una ley normal de desviación típica de 2 gramos y 
# media 122 gramos. Contestar, razonadamente, las cuestiones:

# a) ¿Qué tanto por ciento de los envases de yogur de esta empresa 
# cumplirá la normativa?
1-pnorm(120,mean=122,sd=2) # P(X>=120) = 1-P(x<120)
pnorm(120,mean=122,sd=2,lower.tail = F) # lower.tail = F para decir que es P(x>=120)

# b) ¿Cuál deberá de ser la media (μ) de la ley normal con la que la máquina 
# dosificadora hace envases para que el 98% de la producción de yogures de esta
# empresa cumpla la normativa? (manteniéndose la desviación típica igual a 2gr.
dt = 2
valor = qnorm(0.98, mean=0, sd=1, lower.tail = T); valor
media = 120-valor*dt; media
# Media = 115.8925 valor por debajo de 120. No es el resultado esperado
valor = qnorm(0.98, mean=0, sd=1, lower.tail = F); valor

# Otros
# Si hay 12 automoviles que cruzan un puente por minuto en promedio,
# ¿cual es la probabilidad de que 17 o mas crucen el puente en un minuto dado?
u = 12
ppois(16, u, lower.tail = FALSE)
# lower.tail = FALSE (Probabilidad mayor que > )
