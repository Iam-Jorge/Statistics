# TRABAJO TEORICO NFL

# Limpiar workspace
rm(list = ls())
cat("\014")
Sys.setenv(`_R_USE_PIPEBIND_` = TRUE)

# Cargamos los datos
NFL <- read.csv(file = "NFL2016.csv", sep=",")

# VARIABLES CUALITATIVAS
# Tabla de contingencia analizamos la relacion entre las variables division y conference
tabla <- table(NFL$Division, NFL$Conference)
tabla
# La tabla nos permite ver de un vistazo que que existe una pariedad total entre los equipos de cada region
# El resultado es otro objeto de la clase table al que se le han añadido una o varias filas o columnas, que contienen las frecuencias marginales, tanto absolutas como relativas
addmargins(tabla)

# Tambien podemos ver las frecuencias relativas y absolutas en terminos de proporciones
prop.table(tabla)
addmargins(prop.table(tabla))

# Podemos representar esta informacion de una forma más grafica con un diagrama de barras
barplot(tabla1,
        main = "Gráfico de barras",
        sub = "Número de equipos según división",
        ylim = c(0,5),
        xlab = "Divisiones",
        ylab = "Número de equipos",
        axes = TRUE, 
        beside = TRUE)




# VARIABLES CUALITATIVAS
# Division con más victorias
FactorDivision <-factor(NFL$Division) # Factorizamos Division (East, West, North, South)
DivisionWins<-tapply(NFL$Wins,FactorDivision,sum) # tapply sumatorio de victorias por region
print(DivisionWins)
TotalWins = DivisionWins[1] + DivisionWins[2] + DivisionWins[3] + DivisionWins[4]
DivisionPercentage = DivisionWins
DivisionPercentage[1] = paste("East", format(round((as.integer(DivisionWins[1]) * 100 / TotalWins), 2), nsmall = 2), "%")
DivisionPercentage[2] = paste("North", format(round((as.integer(DivisionWins[2]) * 100 / TotalWins), 2), nsmall = 2), "%")
DivisionPercentage[3] = paste("South", format(round((as.integer(DivisionWins[3]) * 100 / TotalWins), 2), nsmall = 2), "%")
DivisionPercentage[4] = paste("West", format(round((as.integer(DivisionWins[4]) * 100 / TotalWins), 2), nsmall = 2), "%")
pie(DivisionWins, labels = DivisionPercentage, main = "Division Wins")


# PORCIENTO DE TDs POR FEDERACION
FactorDivision <-factor(NFL$Division)
DivisionTDs<-tapply(NFL$TDs,FactorDivision,sum)
TotalTDs = DivisionTDs[1] + DivisionTDs[2] + DivisionTDs[3] + DivisionTDs[4]
DivisionTDPercentage = DivisionTDs
DivisionTDPercentage[1] = paste("East", format(round((as.integer(DivisionTDs[1]) * 100 / TotalTDs), 2), nsmall = 2), "%")
DivisionTDPercentage[2] = paste("North", format(round((as.integer(DivisionTDs[2]) * 100 / TotalTDs), 2), nsmall = 2), "%")
DivisionTDPercentage[3] = paste("South", format(round((as.integer(DivisionTDs[3]) * 100 / TotalTDs), 2), nsmall = 2), "%")
DivisionTDPercentage[4] = paste("West", format(round((as.integer(DivisionTDs[4]) * 100 / TotalTDs), 2), nsmall = 2), "%")
pie(DivisionTDs, labels = DivisionTDPercentage, main = "Division TDs")


# DIAGRAMA DE BARRAS PARA LA DISTRIBUCION DE FRECUENCIAS CATTEGORIAS
hist_breaks <- hist(NFL$TDs)$breaks
color_list <- rep('#E9EB8D', length(hist_breaks))
print(color_list)
color_list[hist_breaks <= 30] <- '#FA8C8E'
  print(color_list)
color_list[hist_breaks >= 45] <- '#9FF5A8'
print(color_list)
hist(NFL$TDs, col=color_list)

hist_breaks <- hist(NFL$NetPts)$breaks
color_list <- rep('#E9EB8D', length(hist_breaks))
print(color_list)
color_list[hist_breaks <= 10] <- '#FA8C8E'
  print(color_list)
  color_list[hist_breaks >= 100] <- '#9FF5A8'
    print(color_list)
    hist(NFL$NetPts, col=color_list)


# MEDIA DE TDs POR CONFERENCE
FactorConference <-factor(NFL$Division)
levels(FactorConference)
MeanTDsConference <-tapply(NFL$TDs,FactorConference,mean)
MeanTDsConference
# Media de puntos a favor
MeanTDsConference <-tapply(NFL$PointsFor,FactorConference,mean)
MeanTDsConference
# Media de Yardas a favor
MeanTDsConference <-tapply(NFL$YardsFor,FactorConference,mean)
MeanTDsConference


summary(NFL$Wins)


# VARIABLES CUALITATIVAS
# Cattegorizar los eqipos segun estos 3 niveles
# Good - 10 NetPts, 30 touchdowns
# Decent - 100 NetPts, 40 touchdowns
# Elite - > 100 NetPts, 50 touchdowns

CategoriasTDs = NFL$TDs
for (i in 1:length(NFL$TDs)) {
  if (NFL$TDs[i] <= 30) {
    CategoriasTDs[i] = 1
  } else if (NFL$TDs[i] >= 30 && NFL$TDs[i] <= 45) {
    CategoriasTDs[i] = 2
  } else if (NFL$TDs[i] >= 45) {
    CategoriasTDs[i] = 3
  }
}
CategoriasNetPts = NFL$NetPts
for (i in 1:length(NFL$NetPts)) {
  if (NFL$NetPts[i] <= 10) {
    CategoriasNetPts[i] = 1
  } else if (NFL$NetPts[i] >= 10 && NFL$NetPts[i] <= 100) {
    CategoriasNetPts[i] = 2
  } else if (NFL$NetPts[i] >= 100) {
    CategoriasNetPts[i] = 3
  }
}

Good <- 0
Decent <- 0
Excelent <- 0
Categorias = NFL$NetPts
for (i in 1:length(CategoriasNetPts)) {
  if ((CategoriasTDs[i] == 1 && CategoriasNetPts[i] == 1) || (CategoriasTDs[i] == 2 && CategoriasNetPts[i] == 1) || (CategoriasTDs[i] == 1 && CategoriasNetPts[i] == 2) || (CategoriasTDs[i] == 1 && CategoriasNetPts[i] == 3) || (CategoriasTDs[i] == 3 && CategoriasNetPts[i] == 1)) {
    Good = Good + 1
    Categorias[i] = 1
  } else if ((CategoriasTDs[i] == 2 && CategoriasNetPts[i] == 2) || (CategoriasTDs[i] == 3 && CategoriasNetPts[i] == 2) || (CategoriasTDs[i] == 2 && CategoriasNetPts[i] == 3) || (CategoriasTDs[i] == 2 && CategoriasNetPts[i] == 1)) {
    Decent = Decent + 1
    Categorias[i] = 2
  } else if (CategoriasTDs[i] == 3 && CategoriasNetPts[i] == 3) {
    Excelent = Excelent + 1
    Categorias[i] = 3
  }
}
print(Good)
print(Decent)
print(Excelent)



# Eliminar outliers
box_var=boxplot(NFL$YardsFor)
box_var$out
pos1 <- which(NFL == box_var$out[1], arr.ind = T)
NFL[pos1[1],pos1[2]] = 0
pos2 <- which(NFL == box_var$out[2], arr.ind = T)
NFL[pos2[1],pos2[2]] = 0
MeanTDsConference <-tapply(NFL$YardsFor,FactorConference,mean)
MeanTDsConference






# RELACION ENTRE VARIABLES
cat("Correlacion Var4 y Var1 =", cor(NFL$Wins, NFL$TDs))
cat("Correlacion Var4 y Var1 =", cor(NFL$Wins, NFL$YardsFor))

cat("Correlacion Var4 y Var1 =", cor(NFL$Wins, NFL$YardsAgainst))


cat("Correlacion victorias y TDs =", cor(NFL$Wins, NFL$TDs))
plot(NFL$Wins, NFL$TDs, pch = 16, xlab = "Victorias", ylab = "Touchdowns", col = c("red", "blue"))
abline(lm(NFL$TDs ~ NFL$Wins), col = "black")


plot(NFL$Wins, NFL$YardsAgainst, pch = 16, xlab = "Victorias", ylab = "Yardas en contra", col = c("red", "blue"))
abline(lm(NFL$YardsAgainst ~ NFL$Wins), col = "black")


vars <- data.frame(NFL$Wins, NFL$TDs, NFL$YardsFor, NFL$PointsFor)
pairs(vars)
cor(vars)


# MODELO DE REGRESION
WINS<-NFL$Wins;TDS<-NFL$TDs
model1<-WINS~TDS
reg1<-lm(model1)
summary(reg1)


plot(model1);abline(reg1) 
pred<-predict(reg1,data.frame(TDS<-c(10,40,70))) 
points(cbind(tds,pred),col="red")
print(pred)




#CONTRASTE DE HIPOTESIS
NFCTDs <- rep(0, 16)
SouthYardsFor <- rep(0, 8)
NorthYardsFor <- rep(0, 8)
WestNetPts <- rep(0, 8)
cnt <- 1
cntN <- 1
cntS <- 1
cntW <- 1
for (i in 1:nrow(NFL)) {
  for (j in 1:ncol(NFL)) {
    if (NFL[i,j] == "NFC") {
      NFCTDs[cnt] = NFL[i,12]
      cnt = cnt + 1
    }
    if (NFL[i,j] == "South") {
      SouthYardsFor[cntS] = NFL[i,10]
      cntS = cntS + 1
    }
    if (NFL[i,j] == "North") {
      NorthYardsFor[cntN] = NFL[i,10]
      cntN = cntN + 1
    }
    if (NFL[i,j] == "West") {
      WestNetPts[cntW] = NFL[i,9]
      cntW = cntW + 1
    }
  }
}
print(NFCTDs)
print(SouthYardsFor)
print(NorthYardsFor)
print(WestNetPts)


t.test(SouthYardsFor, NorthYardsFor, paired = TRUE, alternative = "two.side", conf.level = 0,97)

t.test(NFCTDs, mu=40, alt="greater", conf.level = 0.95)

t.test(WestNetPts, mu=100, alt="less", conf.level = 0.95)



