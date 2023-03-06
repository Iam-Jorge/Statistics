# SESION III
# Estadistica descriptiva multivariable

primeraOpinion = c('b', 'b', 'g', 'b', 'b', 'g', 'b', 'g', 'g', 'b')
segundaOpinion = c(1, 0, 1, 1, 0, 1,0, 0, 1, 1)


summary(primeraOpinion)
summary(segundaOpinion)

table(primeraOpinion, segundaOpinion)
table(primeraOpinion,segundaOpinion)/length(segundaOpinion)

# Tabla marginales
tab.p <- table(primeraOpinion,segundaOpinion)/length(segundaOpinion)*100
# Marginal por filas
margin.table(tab.p,1)
# Marginal por columnas
margin.table(tab.p,2)
# Añadir sumatorios a la tabla
addmargins(tab.p)


# Graficos: Variables estadisticas multivariables
#Las variables cualitativas se representan con diagramas de barras
# Por columnas
barplot(tab.p)
# Transpuesta
barplot(t(tab.p))
# Columnas
barplot(t(tab.p),beside = TRUE)


#----------VARIABLES CUANTITATIVAS-----------#
load("Variables.Rdata")
ls() #Vemos el contenido. Las variables que acabamos de cargar
summary(Var1);summary(Var2)


# Data frame
data = data.frame(primeraOpinion,Var4,Var1,segundaOpinion)
# Acceder a la 4 variable
data$Var4
# Otra forma de acceder obtiene toda la columna
data[,2]
# Fila 23, Columna 4
data[23,4]
# Obtener fila 27
data[27,]
data$Var4 == Data[,4] #label ≠ posición

# Tamaño data frame
length(data)
nrow(data);ncol(data)
dim(data) #nº filas y columnas
#Importante para el cuestionario. Para
#recorrer un data frame con un for se utiliza dim

# Guardar como Rdata
save(data,file="DataV.Rdata")
load("DataV.Rdata")
# Genera una copia temporalmente permite usar variables por el nombre sin el símbolo $ 
attach(data)
detach(data)


# Histograma
hist(Var1)

# Boxplot
boxplot(Var1,Var3,horizontal=TRUE)
boxplot(Var1,Var2,Var3,Var4) # Mejor con objetos
Vars<-data.frame(Var1,Var2,Var3,Var4)
boxplot(Vars)

# Scatterplot
#Para poder comparar las variables se utilizan los scatterplots
plot(Var1,Var2) #plot(x,y) ≠ plot(y,x)
#Estas variables no tienen relación.

#------------ RELACION DE TODAS LA VARIABLES--------------#
Vars<-data.frame(Var1,Var3,Var2,Var4)
# Matriz que compara la relacion de todas las variables
pairs(Vars)


#---------------------- PEARSON --------------------#
#¿Que variables presentan una relación mayor? Utilizar Pearson
cat("Correlacion Var1 y Var3 =", cor(Var1,Var3))
cat("Correlacion Var1 y Var2 =", cor(Var1,Var2))
cat("Correlacion Var1 y Var4 =", cor(Var1,Var4))
# use="pairwise.complete.obs" necesario cuando hay NA

# Moodelo de regresión lineal
#Pretendemos calcular una recta que nos permita estimar datos.
load("emisiones.Rdata")
head(emisiones)
summary(emisiones)
pairs(emisiones)
cor(emisiones) #Pearson de cada variable con cada variable



# Modelo lineal CO2 = a + b · PIB + €
CO2<-emisiones$CO2;PIB<-emisiones$PIB
model1<-CO2~PIB #defición del modelo co2 variable y pib variable x
reg1<-lm(model1) #ajustar el modelo
summary(reg1)

# Estimate valores a y b con la significacion que se han estimado
# Bondad de ajuste = Multiple R-squared
# valor p-value Indica si el modelo es significativo o no 


# Modelo ajustado
plot(model1);abline(reg1)
pred<-predict(reg1,data.frame(PIB<-c(2e6,4e6,6e6)))
points(cbind(PIB,pred),col="red")
#Para ver la respuesta exacta hay que ver el contenido del vector pred
print(pred)
