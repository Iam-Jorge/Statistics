sqrt(144)

?plot #ayuda general
??plot #ayuda concreta
128%%6 #Resto de dividir 128 entre 6

##DIAPOSITIVA 13
incid<-c(123,89,142,118,109) #vector con 5 elementos
names(incid)<-c(1:5) #Le decimos que para ese vector le vamos a poner nombre a 5 posiciones
names(incid)<-seq(1,5) #Van a ir de manera secuencial
names(incid)<-c("Mon","Tue","Wed","Thu","Fri")
incid

#DIAPOSITIVA 14
#Calcular la media
sum(incid)/length(incid) #sumatorio del vector entre su longitud
mean(incid)

#Calcular la varianza
var(incid)

#Codigo que calcula la varianza
xbar<-mean(incid)
n<-length(incid)
sum((xbar-incid)^2/(n-1))

#DIAPOSITIVA 15
sort(incid)
cumsum(incid)
diff(incid)
range(incid)
max(incid)

#DIAPOSITIVA 16
resueltos<-c(65,54,67,59,53)
sin_resolver<-incid-resueltos
sin_resolver
porcentaje_resueltos<-(resueltos/incid)*100
porcentaje_resueltos

#DIAPOSITIVA 17
incid[1]
incid[-1]

#DIAPOSITIVA 18
incid>110

sum(incid>110) #Numero de mayores que 110
sum(incid[incid>110]) #Sumatorio de los mayores que 110

#DIAPOSITIVA 19
incid2<-scan() #Meter por teclado. Dar enter dos veces
names(incid2)<-names(incid)
incidT<-c(incid,incid2)
incidT

#DIAPOSITIVA 20
matrix1<-rbind(incid, incid2)
matrix1
matrix2<-cbind(incid,incid2)
matrix2

#DIAPOSITIVA 21
regiones<-c("clm","and","mad","cat","mad","cat",
            "mad","clm","and","mad","cat","clm",
            "and", "mad", "cat", "mad", "cat", "mad"
            , "clm", "and")
FactorRegiones<-factor(regiones) # Factoriza todas las regiones (vector de todos los posibles resultados unicos)
print(FactorRegiones)
FactorRegiones #Comunidades que hay
? levels
levels(FactorRegiones)
ingresos<- c(800, 950, 1500, 1600, 1900, 2100, 1900, 950, 
             1000, 2100, 1950, 750, 800, 2500, 2100, 1950, 
             1900, 1800, 1100, 1400)
MediaIngresos<-tapply(ingresos,FactorRegiones,mean)
MediaIngresos

#DIAPOSITIVA 23
demo("graphics")
