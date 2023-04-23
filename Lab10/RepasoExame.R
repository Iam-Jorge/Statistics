# DISTRIBUCIONES DE PROBABILIDAD

# DISTRIBUCIONES DISCRETAS (Toman numeros enteros)

# ------------------------------------------------------------------------------

# BINOMIAL
# dbinom(X, n, p) Funcion de masa de probabilidad f(x)
# pbinom(X, n, p) Funcion de distribucion acumulada hasta q, F(x)
# qbinom(X, n, p) Toma la probabilidad y devuelve el valor de x que acumula esa probabilidad
# rbinom(X, n, p) Generador de numeros aleatorios
# x = Tamaño de la muestra (numero de exitos)
# n = Total de ensayos
# p = Probabilidad de exito

# Ejemplo binomial (x, n, p)
# Se revisan 18 buses, se sabe que un 10% generan mayor cantidad de humo de la permitida
# a) Calcular la prop de que se encuentre exactamente 2 buses
# P(X) = 2, n = 18, p = 0.1
dbinom(x=2, size=18, prob=0.10)

# b) Prob 3 o menos buses emitan gases por encima de lo permitido
# P(X <= 3), n = 18, p = 0.1
pbinom(q=3, size=18, prob=0.10)

# c) Calcular la prob de que el num de buses que sobrepasen el limite de genracion de gases sea al menos 4
# P(X >= 4), n = 18, p = 0.1
# lower.tail = FALSE -> Valores acumulados a la derecha
1-pbinom(q=4, size=18, prob=0.10)
pbinom(q=4, size=18, prob=0.10, lower.tail = FALSE)

# d) Cual es el valor de la variable X (personas con COVID) cuando la probabilidd es de 0.3827
qbinom(0.3827, 10, 0.3, lower.tail = TRUE)

# ------------------------------------------------------------------------------

# POISSON
# dpois(x, lambda) Funcion de masa de probabilidad f(x)
# ppois(x, lambda) Funcion de distribucion acumulada hasta q, F(x)
# qpois(x, lambda) Toma la probabilidad y devuelve el valor de x que acumula esa probabilidad
# rpois(x, lambda) Generador de numeros aleatorios
# x = Valor que toma la variables
# lambda = Media de sucesos en un intervalo de tiempo/espacio

# Ejemplo poisson (X, lambda)
# En una editorial se asume que todo libro de 250 pag tiene en promedio 50 erratas
# a) Prop de que en una pag cualquiera no se encuentren errores
# P(X = 0), lambda = numero de eventos esperados por unidad de tiempo (50/250) = 0.2
dpois(x=0, lambda=0.2)

# P(X<=3)
ppois(3, 5, lower.tail = TRUE)

# P(X>3)
ppois(3, 5, lower.tail = FALSE)
1-ppois(3, 5, lower.tail = TRUE)

# ------------------------------------------------------------------------------

# DISTRIBUCIONES CONTINUAS (Valores con decimales)

# ------------------------------------------------------------------------------

# NORMAL o GAUSS
# dnorm(x, mu, sigma) Funcion de masa de probabilidad f(x)
# pnorm(x, mu, sigma) Funcion de distribucion acumulada hasta q, F(x)
# qnorm(probabilidad_deseada, mu, sigma) Toma la probabilidad y devuelve el valor de x que acumula esa probabilidad
# rnorm(x, mu, sigma) Generador de numeros aleatorios
# x = Valor al que buscamos la probabilidad
# mu = Media
# sigma = Desviacion

# a) P(X=5)
# Ejemplo mu = 10, sigma = 7
dnorm(5, 10, 7)

# b) P(X<=4)
pnorm(4, 10, 7)

# c) P(X>=4)
pnorm(4, 10, 7, lower.tail = FALSE)

# d) P(X<=a) = 0,3 --> De a hacia abajo hay un 30% de probabilidad
qnorm(0.3, 10, 7)
# Validacion -> 13.6708 valor devuelto por qnorm
pnorm(6.329196, 10, 7, lower.tail = TRUE)

# Ejemplo
# Una empresa elabora tornillos, el diametro sigue un distribucion normal con media de 10mm y var de 4mm^2
# a) Un tornillo cumple las especificaciones si su diametro 
# esta entre 9 y 11mm. Calcular porcentaje que lo cumplen
# P(q = 9 < Z < q = 11), mean = 10, desviacion tipica = sqrt(var) = 2
pnorm(q=11, mean=10, sd=2) - pnorm(q=9, mean=10, sd=2)

# b) Tornillo con diametro mayor a 11mm (PROBABILIDAD A DERECHA = LOWER.TAIL)
# P(Z > 11)
norm(q=11, mean=10, sd=2, lower.tail=FALSE)

# c) El 5% de los tornillos mas delgados no se pueden reprocesar
# Encuntrar el cuantil tal que P(Diametro < q) = 0.05
qnorm(p=0.05, mean=10, sd=2)

# d) El 10% de los tornillos mas gruesos son considerados como sobredimensionados
# P(Diametro > q) = 0.10
qnorm(p=0.10, mean=10, sd=2, lower.tail=FALSE)


# ------------------------------------------------------------------------------

# EXPONENCIAL
# Probabilidad del tiempo o espaio entre dos eventos de un proceso de Poisson
# dpexp(x, rate=(1/beta_o_lambda) sigma) Funcion de masa de probabilidad f(x)
# ppexp(x, rate=(1/beta_o_lambda) Funcion de distribucion acumulada hasta q, F(x)
# qpexp(p, rate=(1/beta_o_lambda) Toma la probabilidad y devuelve el valor de x que acumula esa probabilidad
# rpexp(n, rate=(1/beta_o_lambda) Generador de numeros aleatorios
# x = Valor al que buscamos la probabilidad
# Rate = 1/E(X) = 1/beta = lambda

# Ejemplo
# X = Tiempo en horas hasta que un componente falla
# X sigue una distribucion exponencial de media 360 horas -> Esperanza = E(x) = 360

# a) P(x=400)
dpexp(400, 1/360)

# b) P(X<400)
pexp(400, 1/360)

# c) P(X>400)
1-pexp(400, 1/360)
pexp(400, 1/360, lower.tail = FALSE)

# d) P(X>a) = 0.4 o 40%. Para P(X<a) lower.tail = FALSE
qexp(0.4, 1/360)
pexp(qexp(0.4, 1/360, lower.tail = TRUE), 1/360, lower.tail = TRUE)

# ------------------------------------------------------------------------------

# UNIFORME
# Cada valor entre un intervalo a y b tiene la misma probabilidad de ser elegido 
# dunif(x, min, max) sigma) Funcion de masa de probabilidad f(x)
# punif(x, min, max) sigma) Funcion de distribucion acumulada hasta q, F(x)
# qunif(x, min, max) sigma) Toma la probabilidad y devuelve el valor de x que acumula esa probabilidad
# runif(x, min = 0, max = 1) sigma) Generador de numeros aleatorios
# x = Valor al que buscamos la probabilidad

# Ejemplo
# Un autobus llega a la parada de bus cada 20 minutos.
# Si llegas a la parada del autobus, ¿cual es la probabilidad de que el autobus aparezca en 8 min o menos?
punif(8, 0, 20)

# ------------------------------------------------------------------------------

# Teorema central del limite
# Establece que dada una muestra suficientemente grande la distribucion de las medias seguira una distribucion normal
x <- rnorm(50, 10, 2.5)

# Calcular la media y desviación estándar muestral
mean_x <- mean(x)
sd_x <- sd(x)

# Calcular el valor z-score
z_score <- (mean_x - 10) / (sd_x / sqrt(50))

# Calcular la probabilidad utilizando pnorm()
prob <- pnorm(z_score)

# Imprimir la probabilidad
prob

# La salida será un valor entre 0 y 1 que representa la probabilidad de que la media muestral esté por debajo del valor 
# de 10. Si la muestra es lo suficientemente grande, este valor se aproximará a una distribución normal.
# ------------------------------------------------------------------------------

# INFERENCIA ESTADISTICA
# Tratamos de averiguar caractersiticas de una POBLACON a partir del estudio de una MUESTRA de esta
# IMPORTANTE comprobar si el enunciado indica que se trata de una MUESTRA

# Contraste de Hipotesis para una muestra
insulation=c(10.3,10.1,9.8,9.9,10.2,10.1,9.7,9.9,9.7,10.2)
t.test(insulation)
# POR DEFECTO: Nivel de confianza = 0.95 y alt="two.sided" (bilateral)
# 9.833818 -10.146182 -> La media esta entre estos dos valores con un 95% de seguridad
# Cuando un intervalo de confianza crece alverga mas valoes, menor probabilidad de fallo
# P-VALUE si es menor que la significacion (0.05) se rechaza H0 y se acepta H1

# INTERVALO DE CONFIANZA PARA LA MEDIA BILATERAL
# H0 : μ = 10 
# H1 : μ ≠ 10 (two-sided) 
t.test(insulation,mu=10,alt="two.sided")
# p-value 0.888 > 0.05 -> No podemos rechazar H0

# INTERVALO DE CONFIANZA PARA LA MEDIA UNILATERAL
# H0 : μ ≥ 10
# H1 : μ < 10 -> alt="less"

# H0 : μ < 10
# H1 : μ ≥ 10 -> alt="greater"


# GRAFICAR LA T DE STUDENT
area_t<-function(x,gl){
  t<-seq(-4,4,0.01)
  fdp<-dt(t,gl)
  plot(t,fdp, type="l")
  polygon(c(t[t<=x],x), c(fdp[t<=x], fdp[t==-4]), col="red")
}

area_t(0.888,95)

# RECHAZAR H0 CUANDO:
# 1). Estadístico está en la región de rechazo (α)
valor_t = -0.14484 # Valor t devuelto por t.test
valor_critico <- qt(1 - 0.95/2, 9) # qt(1 - nc/2, df) Donde nc = Nivel de confianza y df = Grados de libertad
if (abs(valor_t) > valor_critico) {
  cat("No se rechaza la hipótesis nula H0")
} else {
  cat("Se rechaza la hipótesis nula H0")
}
# 2). p-value <= nc


# TEST DE HIPOTESIS PARA PROPORCIONES
# x es el número de éxitos en la muestra.
# n es el tamaño de la muestra.
# p es la proporcion, IMPORTANTE se especifica solo cuando la conocemos de ante mano
# alternative es el tipo de prueba de hipótesis que se desea realizar
# conf.level es el nivel de confianza deseado para el intervalo de confianza. Por defecto, es 0.95.
prop.test(x=340, n=500, conf.level=0.97) 
prop.test(x=17,n=500,p=0.02,alt="greater")


# INTERVALO DE CONFIANZA PARA LA MEDIA DE DOS MUESTRAS
# Identificar que datos tenmos
insulation1<-c(10.3,10.1,9.8,9.9,10.2,10.1,9.7,9.9,9.7,10.2)
insulation2<-c(9.9,10.1,9.5,9.3,10.2,10,9.1,9.5,9.8,10.1)
# Datos INDEPENDIENTES: Los datos no tienen una relación entre si
  # Test de varianzas desconocidad o iguales
  var.test(insulation1,insulation2)
  # Test de media -> IMPORTANTE indicar si tenemos igualdad de varianzas o no
  t.test(insulation1,insulation2,var.equal = TRUE)

  # Datos PAREADOS: Datos relacionados, normalmente datos antes y despues de algo
  # Test de media (muestras relacionadas)
  t.test(insulation1, insulation2, paired = TRUE)

