####################
#### PRÁCTICO 2 ####
####################


setwd("G:/UdelaR/CCEEA/Semestre 7/Análisis Multivariado I/Prácticos")


#### EJERCICIO 1 ####

rm(list=ls())

x1=c(2,1,2,2)
x2=c(3,5,2,3)
x3=c(-1,-2,1,1)

X=cbind(x1,x2,x3)

# Parte a

means=colMeans(X)
covs=var(X)
vargen=det(covs)
cors=cor(X)

a=cors[2,1]*cors[3,2]*cors[1,3]
b=cors[3,1]*cors[1,2]*cors[2,3]
c=cors[3,1]*cors[1,3]
d=cors[3,2]*cors[2,3]
e=cors[2,1]*cors[2,1]
a+b-c-d-e
c+d+e

eigen(cors)

# Parte b

y1=(1/3)*x1+(1/3)*x2+(1/3)*x3
y2=x1-0.5*x2-0.5*x3
Y=cbind(y1,y2)
means_y=colMeans(Y)

covs_y=var(Y)
cors_y=cor(Y)
vargen_y=det(covs_y)

#### EJERCICIO 2 ####



#### $ ####

################################
#### FIN DE LA PROGRAMACIÓN ####
################################