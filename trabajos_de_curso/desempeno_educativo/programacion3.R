########################################
#### PRIMER INFORME- MULTIVARIADO I ####
########################################


library(tidyverse)
library(ggthemes)
library(cluster)
library(mclust)
library(nnet)
library(StatMatch)
library(lattice)
library(MVN)
library(MASS)
library(ggdendro)
library(caret)
library(FactoMineR)

load("multivariado.Rdata")
source("C:/Users/dacza/Dropbox/R Functions/standard.R")

      
#############
#### ACP ####
#############

# Estandarizamos
baseredu.s <- standard(baseredu[,2:9])

# Hacemos ACP con las variables de rendimiento
acp1 <- PCA(baseredu.s, graph=FALSE)
plot.PCA(acp1, choix="var")

# Valores propios y porcentaje de varianza en cada componente
acp1$eig
# El primer eje captura un 61,92% de la varianza, el segundo un 14,16%, y el tercero un 6,7877%

# Contribucion de cada variable a cada eje y cosenos cuadrados
acp1$var$contrib
# prop.aprob.exam contribuye en un 49,18% al segundo componente
acp1$var$cos2
# Todas las variables menos prop.aprob.exam estan bien representadas con el primer eje. Tomando los primeros dos ejes, todas las variables quedan bien representadas, la que
# queda peor representada es prop.aprob.exam (76,83% de su inercia representada en ambos)

# INDICE DE RENDIMIENTO con escolaridades y avances
acp.ind <- PCA(baseredu.s[, 1:7])
acp.ind$eig

# El componente 1 captura un 68,26% de la varianza. Ademas, es el unico componente con autovalor mayor a 1 (4,77). 
acp.ind$var$contrib
# Todas las variables contribuyen casi lo mismo

pond <- as.vector(acp.ind$var$contrib[,1])

cred1s <- NULL
for(i in 1:dim(baseredu)[1]){ cred1s[i] = max(baseredu$cred1[i], 90)}
cred2s <- NULL
for(i in 1:dim(baseredu)[1]){ cred2s[i] = max(baseredu$cred2[i], 90)}
cred3s <- NULL
for(i in 1:dim(baseredu)[1]){ cred3s[i] = max(baseredu$cred3[i], 90)}
cred4s <- NULL
for(i in 1:dim(baseredu)[1]){ cred4s[i] = max(baseredu$cred4[i], 90)}


divisor <-cbind(
  rep(12, dim(baseredu)[1]),
  rep(12, dim(baseredu)[1]),
  rep(12, dim(baseredu)[1]),
  cred1s,
  cred2s,
  cred3s,
  cred4s
)

indice <- as.matrix(baseredu[,2:8] / divisor) %*% pond

# Vamos a usar este indice (discretizado) para verificar si se asocia con las variables sociodemograficas

# Discretizamos la variable, en tres niveles, cada uno con la tercera parte del rango haciendo intervalos de igual rango nos queda: 1- de 2.269 a 31.13933 2- de 31.13933 a 60.00966 y
# 3- de 60.00966 a 88.880

baseredu$indicee <- ifelse(indice<31.13933, 1, ifelse(indice>60.00966, 3, 2))
base.acm <- left_join(base.grupos, (dplyr::select(baseredu, id, indicee)), by="id")

#############
#### ACM ####
#############

# Funciones bajadas del EVA
source("C:/Users/dacza/Dropbox/R Functions/acm.r")
source("C:/Users/dacza/Dropbox/R Functions/acs.r")

# Preparamos la base para el ACM
base.acm$gen2 <- as.factor(ifelse(base.acm$gen==2012,1,2))
levels(base.acm$gen2) <- c("2012","2013")

base.acm$cluster <- as.factor(ifelse(base.acm$agnes_wa_3==1,1, ifelse(base.acm$agnes_wa_3==2,2,3)))
levels(base.acm$cluster) <- c("Cl 1", "Cl 2", "Cl 3")

base.acm$edad2 <- as.factor(ifelse(base.acm$edad<19, 1, ifelse(base.acm$edad>23,3,2)))
levels(base.acm$edad2) <- c("17y18", "19-23","24ymas")

base.acm$sexo <- as.factor(ifelse(base.acm$Xfem==1, 1,2))
levels(base.acm$sexo) <- c("Muj","Hom")

base.acm$hijos <- as.factor(ifelse(base.acm$Xhijos==1, 1,2))
levels(base.acm$hijos) <- c("Hijos","NoHijos")

base.acm$ocup <- as.factor(ifelse(base.acm$Xocup==1, 1,2))
levels(base.acm$ocup) <- c("Ocup","NoOcup")

levels(base.acm$XeduM) <- c("BajoM", "MedioM","AltoM")
levels(base.acm$XeduP) <- c("BajoP", "MedioP","AltoP")

base.acm$sexo <- as.factor(ifelse(base.acm$Xfem==1, 1,2))
levels(base.acm$sexo) <- c("Muj","Hom")

base.acm$busca.trab <- as.factor(ifelse(base.acm$busca.trabajo==1, 1,2))
levels(base.acm$busca.trab) <- c("Busca","NoBusca")

base.acm$sexto.mdeo <- as.factor(ifelse(base.acm$sexto.mvd==1, 1,2))
levels(base.acm$sexto.mdeo) <- c("6toMdeo","6toInt")

base.acm$sexto.pri <- as.factor(ifelse(base.acm$sexto.priv==1, 1,2))
levels(base.acm$sexto.pri) <- c("6toPriv","6toPubl")

base.acm$indice <- as.factor(base.acm$indicee)
levels(base.acm$indice) <- c("Ind 1","Ind 2", "Ind 3")

# Nuestra base para analisis de correspondencia, dejamos id aunque esa no la vamos a usar para identificar
base.acm <- dplyr::select(base.acm, id, indice, cluster, gen2, edad2,XeduM,XeduP, sexo, hijos, ocup, busca.trab, sexto.mdeo, sexto.pri)
base.acm <- as.data.frame(base.acm)

# ACS entre indice y clusters
acm1 <- acm(base.acm[,c(2,3)],ByG=T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# Hay una clara asociacion entre las categorias del indice y los cluster. Recordar que indice 1 era el peor desempe?o y cluster 1 el de peor desemp??o. Por lo que estariamos llegando a 
# las mismas conclusiones sobre el rendimiento de un individuo mediante el indice y el cluster

# Primer analisis con todas las variables cualitativas y el indice
acm2 <- acm(base.acm[,c(2,4,5,6,7,8,9,10,11,12,13)],ByG=T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# Puede estar relacionado el estar trabajando con estar en el cluster de peor rendimiento y el indice mas bajo. 
# Puede estar relacionado tener hijos con estar en el tramo de edad mayor y trabajar al ingreso a facultad.

# Segundo ACM, con indices, sextos y nivel educativo de los padres
acm3 <- acm(base.acm[,c(2,6,7,12,13)], ByG=T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# Los niveles de el padre y la madre estan muy asociados entre si
# Parece haber asociacion fuerte entre nivel educativo del padre y la madre bajo, haber hecho sexto en publico e interior, y obtener indice bajo
# Parece haber asociacion entre nivel educativo del padre y la madre alto, y obtener indice alto
# Obtener indice 2 no parece asociado a ninguna otra de las variables incluidas

# Utilizando las mismas variables, pero el indice como suplementaria
acm4 <- acm(base.acm[, c(2,6,7,12,13)], Csup=c(1), ByG=T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# En este caso no es tan marcada la asociacion de los niveles del indica con las otras caracteristicas

# ACM con indice y variables de 6to
acm5 <- acm(base.acm[,c(2,12,13)], ByG = T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# Vemos una asociacion entre obtener indice bajo e ir a 6to publico en el interior
# Tambien entre obtener indice alto e ir a 6to privado en montevideo
# Obtener indice medio no esta asociada a las variables de 6to

# ACM con indice y variables laborales
acm6 <- acm(base.acm[,c(2,10,11)], ByG = T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# No se encuentra mucha asociacion entre niguna de las variables. Problema de que rendimiento es de los 4 a?os
# y las variables laborales son de la situacion del estudiante al momento de su ingreso

# ACS con indice y sexo
acm7 <- acm(base.acm[,c(2,8)], ByG = T)
abline(h=0, col="black", lty=3)
abline(v=0, col="black", lty=3)
# Parece haber cierta asociacion entre ser mujer y obtener indice alto


##########################
#### GUARDA EL GLOBAL ####
##########################

save(list=ls(), file="multivariado.Rdata")

################################
#### FIN DE LA PROGRAMACI?N ####
################################