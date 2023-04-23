# EJERCICIOS
# Ejercicio 1. 
# Se han probado dos marcas de automóviles comparando las velocidades máximas que alcanzan. Se han hecho 12 
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
# p-value = 0.1866 < 0.05 significacion. Se acepta H0: Las varianas son iguales


# b). Suponiendo que los datos siguen una distribución normal, ¿existe evidencia estadística de que el vehículo 
# de la marca B es  5km/h mayor que la velocidad máxima media de la marca A con una significancia del 5%?

# H0: mediaA <= mediaB
# H1: mediaA > mediaB
t.test(velocidadA,velocidadB,var.equal = TRUE,alt="less")
# p-value = 0.0338 < 0.05 significacion. Se rechaza H0: mediaA <= mediaB
# Existe evidencia de que la velocidad maxima media del vehiculo marca B es mayor que la del vehiculo marca A

# Ejercicio 2.
# El departamento de calidad de una empresa quiere comprobar si se mantiene el nivel de calidad de un producto de una 
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
