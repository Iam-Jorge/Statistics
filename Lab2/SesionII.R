espera <- scan(file = "espera.txt")
summary(espera) #Muestra el valor mas pequeño, grande, mediana, media, 1 y 3 cuartil

# Histograma
hist(espera)
hist(espera) $counts #Valor de cada barra
hist(espera,breaks=3)
hist(espera,breaks=30)$breaks
hist(espera,probability=TRUE, main="Proceso de 
     llenado", xlab="tiempo")
?hist


#--------------MEDIDAS PARA LA TENDENCIA CENTRAL---------------#
# Media, mediana y moda
mean(espera)
median(espera)
which.max(table(espera))

#Ver como afecta un valor extremo a estas medidas
esperaE<-c(espera,150)
mean(esperaE)
median(esperaE)
which.max(table(esperaE))
#Mediana y moda no varian, media si


#--------------MEDIDAS DE DISPERSION---------------#
var(espera) # Varianza
sd(espera) # Desviacion tipica
sd(espera)/abs(mean(espera)) #Coef de variacion
IQR(espera) #Rango intercuartil (diferencia entre el tercer y el primer cuartil)
range(espera) #Rango. Proporciona el mayor y el menor



#----------CUARTILES Y PERCENTILES-------------#
# Devuelve mas pequeño 25% mediana 75% y bigote mas grande
# No se ve afectado por outliers
quantile(espera) #Cuartiles 0, 25, 50, 75, 100
quantile(espera, 0.1) #Cuartil 10%
quantile(espera, c(.21,.15,.9)) #Cuartiles 21, 15, 90 

# Five number summary o turkey 5 number
# Devuelve valor máximo real (outlier)
fivenum(espera) #Cuartiles 0, 25, 50, 75, 100

#--------------- OUTLIERS -----------------#
# Boxplot
boxplot(espera)$stats
fivenum(espera)
esperaE=c(espera,120) # Añadimos un outlier
box_var=boxplot(esperaE) # Valores extremos o atipicos (outlier)
box_var$out # Valor numerico de los outliers si los hay



#-------------- Simetria ---------------#
# Miden la mayor o menor simetria de la distribucion
# coeficiente > 0 asimetria positiva
# coeficiente = 0 simetrica
# coeficiente < 0 asimetria negativa
sum((espera-mean(espera))^3)/(length(espera)*sd(espera)^3)
fBasics::skewness(espera)


#------------------- CURTOSIS ---------------#
# Mide la mayor o menor concentracion de datos alrededor de la media
# lepticurtica > 0 menos aplanada
# mesocurtica = 0
# platicurtica < 0 mas aplanada
sum((espera-mean(espera))^4)/(length(espera)*sd(espera)^4)-3 
fBasics::kurtosis(espera)

#----------------VARIABLES CUALITATIVAS----------------#
encuesta=c("si","no","no","si","nc","si","no","si","nc","no")
satisfaccion=c(1,3,2,1,1,2,1,2,3,3,1,1,2,3,2,1,2,3,3,2)

# Tabla de frecuencias
table(encuesta) # Frecuencia absoluta
table(satisfaccion) 
table(encuesta)/length(encuesta) # Frecuencia relativa
table(satisfaccion)/length(satisfaccion)
cumsum(table(encuesta)) # Frecuencias acumuladas


# Diagrama de barras
barplot(satisfaccion,xlab="Satisfacción",ylab="frecuencias")
barplot(table(satisfaccion),xlab="Satisfacción", 
        ylab="frecuencias")


# Grafio circular
pie(satisfaccion,main="Satisfacción")
pie(table(satisfaccion),main="Satisfacción")


# Medidas de tendencia central
which.max(table(encuesta))
which.max(table(satisfaccion))


#------------------- TIPICACION ---------------#
# Comparar dos valores para ver cual tiene mas valor
calificaciones=replicate(10, runif(20)*10) # Matriz 10x20 aleatoria
print(calificaciones)
tip=NULL # Inicializamos la variable
nota = 8
for(i in 1:ncol(calificaciones)){ # recorremos por columnas
  aux=calificaciones[,i] # guardamos la columna
  tip=c(tip,(nota-mean(aux))/sd(aux)) #añadimos la tip al vector
}
which(max(tip)==tip) # cual es la mayor
