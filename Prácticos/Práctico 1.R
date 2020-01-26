####################
#### PRÁCTICO 1 ####
####################

rm(list=ls())
setwd("G:/UdelaR/CCEEA/Semestre 7/Análisis Multivariado I/Prácticos")


#### EJERCICIO 1 ####

rm(list=ls())

v1=c(1,0,2)
v2=c(1,1,2)
v3=c(2,1,6)

# Parte a

a=-v2
b=v1+v3
c=v2+4*v3

# Parte b

d=3*sqrt(t(v1)%*%v1)
e=2*t(v2)%*%v2-2*t(v2)%*%(v3)+t(v3)%*%v3

# Parte c

f=t(v1)%*%v2
g=t(v2)%*%v3

# Parte d

Pv2.v1=((t(v1)%*%v2)/(t(v2)%*%v2))*(v2)
Pv1v2.v1=(t(v1)%*%v1)/(t(v1)%*%v1)*(v1) + (t(v1)%*%v2)/(t(v2)%*%v2)*(v2)

# Parte e

U=cbind(v1, v2, v3)
detU=det(U)


#### EJERCICIO 2 ####

rm(list=ls())

v1=c(1,0,0,0,1)
v2=c(1,1,0,0,0)
v3=c(0,0,0,1,1)

V=cbind(v1, v2, v3)


# Parte a



# Parte b



# Parte c


#### EJERCICIO 3 ####

rm(list=ls())

A=matrix(c(1,0,2,1,1,2), 3, 2)

AtA=t(A)%*%A
tr.AtA=AtA[1,1]+AtA[2,2]
det.AtA=det(AtA)
inv.AtA=solve(AtA)

AAt=A%*%t(A)
tr.AAt=AAt[1,1]+AAt[2,2]+AAt[3,3]
det.AAt=det(AAt)
inv.AAt=solve(AAt)


#### EJERCICIO 5 ####

rm(list=ls())

A=matrix(data=c(2,3,5,4), 2, 2)
eigen(A)
eigen(t(A))


#### EJERCICIO 6 ####

rm(list=ls())

# Parte a

A=matrix(data=c(2,1,1,2), 2, 2)
eigen(A)
eigen(solve(A))

# Parte b

B=matrix(data=c(1,0,1,0,1,0), 3, 2)

BtB=t(B)%*%B
eigen(BtB)

BBt=B%*%t(B)
eigen(BBt)

svd(B)

C=matrix(data=c(1,1,0,1,2,2), 2, 3)

CtC=t(C)%*%C
eigen(CtC)

CCt=C%*%t(C)
eigen(CCt)

svd(C)

# Parte c


# Parte d


#### EJERCICIO 7 ####

rm(list=ls())

# Parte a

A=matrix(data=c(2,3,6,4), 2, 2, byrow=TRUE)
D=matrix(data=c(eigen(A)$values[1], 0, 0, eigen(A)$values[2]), 2, 2, byrow=TRUE)
B=eigen(A)$vectors
A=B%*%D%*%solve(B)

# Parte b

A=matrix(data=c(2,1,1,1,2,1,0,0,1), 3, 3, byrow=TRUE)
D=diag(x=eigen(A)$values, 3, 3)
B=eigen(A)$vectors
A=B%*%D%*%solve(B)

b=1/sqrt(2)
B=round(matrix(data=c(b,-b,-b,b,b,0,0,0,b), 3, 3, byrow=TRUE),2)
A=B%*%D%*%solve(B)

# Parte c

A=matrix(data=c(3,2,4,2,0,2,4,2,3), 3, 3, byrow=TRUE)
D=diag(x=eigen(A)$values, 3, 3)
B=eigen(A)$vectors
A=round(B%*%D%*%solve(B), 2)

# Parte d

A=matrix(data=c(1,3,0,0,2,2,0,0,0,0,-1,0,0,0,0,-1), 4, 4, byrow=TRUE)
D=diag(x=eigen(A)$values, 4, 4)
B=eigen(A)$vectors
A=round(B%*%D%*%solve(B), 2)


#### EJERCICIO 8 ####

rm(list=ls())

A=matrix(data=c(3,2,4,2,0,2,4,2,3), 3, 3, byrow=TRUE)
D=diag(x=eigen(A)$values, 3, 3)
B=eigen(A)$vectors
A100=round(B%*%D^100%*%solve(B), 2)


#### EJERCICIO 9 ####





#$##############################
#### FIN DE LA PROGRAMACIÓN ####
################################