# EJERCICIOS SESION II
rm(list = ls())
cat("\014")

# 1) Obtener summary con los principales valores de la estadística descriptiva de todas las asignaturas contenidas en el dataset.
load("dataset.RData")
summary(dataset)

# 2) Obtener summary con los principales valores de la estadística descriptiva de la primera asignatura.
summary(dataset)[,1]

# 3) ¿Cuál es la asignatura con mayor nota media?
all_mean=NULL
for(i in 1:ncol(dataset)){ #Se recorren las columnas
  aux=dataset[,i]
  all_mean=c(all_mean,mean(aux)) #Se calculan las medias
}

which(max(all_mean)==all_mean)

# Otra forma para complicarse la vida
vectorNotas <- as.vector(dataset)
print(vectorNotas)
suma <- 0
maxMedia <- 0
asignatura <- 0
asignaturaMaxMedia <- 0
for(i in 1:length(vectorNotas)){
  suma = suma + vectorNotas[i]
  nuevaMedia = suma / 20
  if(nuevaMedia > maxMedia) {
    maxMedia <- nuevaMedia
    asignaturaMaxMedia = asignatura + 1
  }
  if(i %% 20 == 0) {
    suma <- 0
    asignatura = asignatura + 1;
  }
}
cat("V",asignaturaMaxMedia , " con una media de:", maxMedia)


# 4) Realizar un histograma completo del dataset
hist(dataset)

#  5) ¿Hay algún outliers en alguna asignatura? Piensa que podría causar.
box_var=boxplot(dataset)
box_var$out

# 6) ¿Cuántas asignaturas tienen más del 40% de estudiantes con una nota mayor que 8?
for(i in 1: ncol(dataset)){
  aux=dataset[,i]
  if(quantile(aux,0.6)>8){
    print(i)
  }
}



# 7) Comparando la séptima y la novena asignatura: ¿Cual de ellas es más similar a una distribución normal? 
# ¿Como son llamadas dependiendo del valor del parámetro utilizado?
#of the parameter used?
print(fBasics::kurtosis(dataset[,7])) #Leptokurtic
print(fBasics::kurtosis(dataset[,9])) #Platykurtic
  
# 8) Comparando la quinta, octava y décima asignatura: ¿Cual de ellas podemos considerar más simétrica?
# ¿Qué podemos decir de las otras asignaturas?
print(fBasics::skewness(dataset[,5])) #Simetrica
print(fBasics::skewness(dataset[,8])) #Una poca asimetria negativa
print(fBasics::skewness(dataset[,10])) #Una pequeña asimetria positiva


# 9) ¿Qué percentil representa una nota de 8 para la primera asignatura?
#Nos pregunta la k. Hay dos soluciones
sum(dataset[,1]<=8)/length(dataset[,1])

# Segunda solucion
aux=dataset[,1] #SOLUCION 
per=0;
while (round(quantile(aux,per),3)!=8){
  per=per+0.001;
}
per


# 10) ¿En qué asignatura tiene más mérito obtener un 9?
tip=NULL
nota = 9
for(i in 1:ncol(dataset)){
  aux=dataset[,i]
  tip=c(tip,(nota-mean(aux))/sd(aux))
}
which(max(tip)==tip)

