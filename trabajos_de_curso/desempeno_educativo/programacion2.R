#########################################
#### SEGUNDO INFORME- MULTIVARIADO I ####
#########################################

library(tidyverse)
library(ggthemes)
library(cluster)
library(mclust)
library(nnet)
library(StatMatch)
library(lattice)
# library(MVN)
library(MASS)
library(caret)
library(FactoMineR)

# source("C:/Users/dacza/Dropbox/R Functions/indicadores.R")
# source("C:/Users/Daniel/Dropbox/R Functions/indicadores.R")

#########################
#### CARGA EL GLOBAL ####
#########################

load(file="multivariado.Rdata")

#############
#### ACP ####
#############

# Estandarizamos
baseredu.s <- standard(baseredu[,2:9])

# Hacemos ACP con las variables de rendimiento
acp1=PCA(baseredu.s)

# Valores propios y porcentaje de varianza en cada componente
acp1$eig
# El primer eje captura un 61,92% de la varianza, el segundo un 14,16%, y el tercero 
# un 6,7877%

# Contribucion de cada variable a cada eje y cosenos cuadrados
acp1$var$contrib
# prop.aprob.exam contribuye en un 49,18% al segundo componente
acp1$var$cos2
# Todas las variables menos prop.aprob.exam estan bien representadas con el primer eje.
# Tomando los primeros dos ejes, todas las variables quedan bien representadas, la que
# queda peor representada es prop.aprob.exam (76,83% de su inercia representada en ambos)

# Indice de rendimiento con escolaridades y avances

acp.ind=PCA(baseredu.s[, 1:7])
acp.ind$eig

# El componente 1 captura un 68,26% de la varianza. Ademas, es el unico componente con
# autovalor mayor a 1 (4,77). 
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

# Si hacemos intervalos de igual rango nos queda: 1- de 2.269 a 31.13933
# 2- de 31.13933 a 60.00966 y 3- de 60.00966 a 88.880
baseredu$indicee<-ifelse(indice<31.13933, 1, ifelse(indice>60.00966, 3, 2))

base.acm <- left_join(base.grupos, (dplyr::select(baseredu, id, indicee)), by="id")


##########################
#### GUARDA EL GLOBAL ####
##########################

save(list=ls(), file="multivariado.Rdata")

################################
#### FIN DE LA PROGRAMACI?N ####
################################