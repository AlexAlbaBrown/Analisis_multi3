rm(list=ls())
setwd("E:/Análisis Multivariado I/Taller 06-23-16")
library(MASS)
library(FactoMineR)

####################
#Primer Ejemplo ####
####################

# Consumición anual en franco de 8 tipo de comida/bebida (variables) por 
# 8 categorias socio-profesionales.

# Variables: 1 Pan común, 2 Otro tipo de pan, 3 Vino común, 4 Otro tipo de 
# vino, 5 Papas, 6 Vegetales, 7 Uva, 8 Plato preparado

# Individus 1 Productor rural, 2 Asalariado rural, 3 Profesional 
# independiente, 4 Ejecutivo superior, 5 Ejecutivo medio, 6 Empleado, 
# 7 Obrero, 8 Desocupado

X=t(matrix(c(167,1,163,23,41,8,6,6,162,2,141,12,40,12,4,15,119,6,69,56,39,5,
             13,41,87,11,63,111,27,3,18,39,103,5,68,77,32,4,11,30,111,4,72,66,
             34,6,10,28,130,3,76,52,43,7,7,16,138,7,117,74,53,8,12,20),
           nrow=8) )
colnames(X)=c("PC", "OP", "VC","OV","P","Veg","Uva", "Platos")
rownames(X)=c("PRodRu", "Asalrur","Prof","Ejsup","Ejmoy","Emp","Obr", "Des")
X

# Cuentas descriptivas a partir de X.
# Promedio por columnas
colMeans(X)
apply(X,2,mean)
# Varianza por columnas
round(apply(X,2,var), 2)
# Desviación estandar por columnas
s=round(apply(X,2,sd), 2)
s
# Matriz de datos centrados y reducidos de X
Z=scale(X, center=TRUE, scale=TRUE)
Z

# Matriz varianza/covarianza
V=cov(X)
round(V, 2)

# Matriz correlaciones
R=cor(X)
round(R, 2); round(cor(Z), 2)
# observar que cor(X)=cor(Z)
# Valores y vectores propios de R
valp=round(eigen(R)$values,3)
vecp=round(eigen(R)$vectors, 3)

# Para ver porcentaje de la variación explicada
cumsum(valp)/sum(valp)

# Matriz de vectores propios
A=vecp
# Veamos las coordenadas de los individuos sobre los nuevos ejes:
Y=Z%*%A
X11(15,15)
plot(Y[,1:2])
abline(h=0, lty=2)
abline(v=0, lty=2)

# Obs:
# 1) La variabilidad es más grande según el eje 1
# 2) Oposición entre "agricultores" y ejecutivos superiores
# 3) el segundo eje es característico de los inactivos

#######
# Ahora vamos hacer lo mismo con la función PCA del paquete FactomineR.

pc=princomp(X,)


X11(15,15)
par(mfrow=c(1,2))
a=PCA(X, graph=TRUE)

X11(15,15)
#par(mfrow=c(1,2))
biplot(pc, arrow.len=0.1)
  abline(h=0, lty=2)
  abline(v=0, lty=2)
#screeplot(pc)

# Los valores propios y porcentajes de variabilidad
# FactoMineR trabaja con la matriz de correlaciones
a$eig
# Las correlaciones entre Z_i y X_j.
a$var$coord
class(a)
# La primer componente mide la repartición de la consumición entre alimentos
# básicos (PC,VC,Veg) y alimentos más refinados (OP, OV, Uva, Platos) 
# La segunda tiene que ver más bien con la consumición de papas, 
# consumición elevada para los inactivos

# Ver que me da bien lo mismo
vecp[,1]/a$var$coord[,1]
vecp[,2]/a$var$coord[,2]
# los vectores construidos son colineales!
# recordar que si la matriz es reducida/estandarizada (media cero/cada col
# tiene desvio 1) entonces cor(z_j,x_i)=cos(z_j,x_1)

#coordendas de los individuos sobre los ejes.
a$ind$coord

# Mapa de factores: correlación de los datos de las viejas variables con 
# los dos primeros ejes. Mapa de individuos: proyección de los individuos 
# sobre los dos primeros ejes.


# Con dimdesc(a) vemos las correlaciones de las nuevas variables con las 
# viejas variables, escritas en orden de significatividad en cuanto a su
# coeficiente de correlación
