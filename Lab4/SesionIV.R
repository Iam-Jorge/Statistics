datos <- load("Session4.Rdata")

#  Guardar en un objeto las opiniones de los maridos y las
# mujeres y mostrar la table con valores marginales y sumatorio.
Hombres <- SexualFun$Husband
Mujeres <- SexualFun$Wife
tab.opinion <- table(Hombres, Mujeres)
tab.opinion
margin.table(tab.opinion,1)
margin.table(tab.opinion,2)
addmargins(tab.opinion)

# Realizar con leyenda los diagramas de barras
barplot(tab.opinion)
barplot(t(tab.opinion))
barplot(t(tab.opinion), beside=TRUE, legend=TRUE)

# Analizando los diagramas, ¿Cuál es lo opinión más
# compartida por hombre y mujeres ? ¿Y la menos compartida?
barplot(t(tab.opinion), legend=TRUE)
print.noquote('Opinion mas compartida: always.fun')
print.noquote('Opinion menos compartida: never.fun')

# Obtener las frencuencias relativas.
table(Hombres,Mujeres)/length(Hombres)


#  Repetir diagramas de barras con commando beside=TRUE 
# para separar variables en varias columnas.
barplot(tab.opinion, legend=TRUE, beside=TRUE)
barplot(t(tab.opinion), legend=TRUE, beside=TRUE)


# EJERCICIO 2
#1 Realiza un histograma con las notas de los 100 alumnos en la 
# asignatura primera (cordata) 
notas <- print(corData[1:100])
boxplot(notas)
hist(corData[1,1:100])

#2  Realiza un diagrama de cajas comparando las dos primeras 
# asignaturas.
vars=data.frame(corData[1,],corData[2,])
boxplot(vars)

#3 Realiza un diagrama de cajas de todas las asignaturas
vars=data.frame(corData[1,],corData[2,],corData[3,],corData[4,])
boxplot(vars)

#4 ¿Hay algún outliers en alguna asignatura?
corData[1,1]=30 # para que tengamos algun outlier
for (i in 1:row(corData)) { # column loop
  aux = corData[i,] # aux variable for column
  box_var=boxplot(aux) # calculate all means
  if(length(box_var$out)>0) {
      print(i)
      print(box_var$out)
    }
}

# Otra solucion
n_outliers = 0
for (i in 1:nrow(corData)){
  aux = corData[i,]
  box = boxplot(aux)
  if (length(box$out)>0){
    print(i)
    n_outliers + 1
  }
}

#5 Realiza un plot para comparar las asignaturas 3 y 4
plot(corData[3, ],corData[4,])

#6 Crea un objeto y realiza una matriz con la relación de todas 
# las variables.
pairs(vars)
# Otra solucion
variables <- data.frame(corData[1,], corData[2,], corData[3,], corData[4,])
pairs(variables)

#7 ¿Qué percentil representa una nota de 8 para la primera
# asignatura?
nota = 8
sum(corData[1,]<= nota /length(corData[1,]))

#8 Calcula el coeficiente de correlación de Pearson entre todas 
#las asignaturas y analiza los resultados obtenidos.
cor(vars, use = "pairwise.complete.obs")
# Hay relacion muy buena entre 2 y 4

#9 y 10
# 9. Elige las asignaturas con mayor relación lineal y realiza un 
# Modelo de Regresión Lineal
# 10. Ajusta el Modelo de Regresión Lineal e indica la bondad 
# obtenida por dicho modelo 
Asignatura2=corData[2,]; Asignatura4=corData[4,] # asignaturas con mayor relacion linneal
model1 <- Asignatura2~Asignatura4 # definicion del modelo
regl <- lm(model1) # ajustar el modelo
summary(regl)
# el R-Squared es casi 1 (0,9904) -> buena bondad



#apartado 9
Asignatura2 <- corData[2,]
Asignatura4 <- corData[4,]
modelo1 <- Asignatura2 ~ Asignatura4

#apartado 10
regresion1 <- lm(modelo1)
summary(regresion1)
print.noquote('La bondad es 0.9904')





#11. Realiza predicciones de una de las asignaturas en función de 
# la nota obtenida en la otra y razona tus resultados obtenidos. 
# Realiza un gráfico que muestre varias predicciones en color rojo 
# en el Modelo de Regresión Lineal realizado.
plot(model1);abline(regl)
pred<-predict(regl,data.frame(Asignatura4<-c(10,7,8)))
print(pred)
points(cbind(Asignatura4,pred),col="red")
# X = Asignatura 4, la variable independiente



#12 ¿En cuál de las 4 asignaturas tiene más valor sacar un 9
tip = NULL
nota = 9
for (i in 1:nrow(corData)) { # recorrer columnas
  aux=corData[i,] # guardar la columna
  tip=c(tip,(nota-median(aux))/sd(aux)) # añadimos la tip al vector
}
print.noquote("Tiene mas merito sacar un 9 en la asignatura:");print(which(max(tip)==tip))
