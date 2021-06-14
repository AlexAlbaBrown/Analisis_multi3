# creamos tabla de datos para funcion acs

datos=read.table('riqueza312.txt', header=T)

# dimensiones y nombres de las variables

dim(datos)
names(datos)

# elimina la variable 15 que no tiene nada 
datos1 <- datos[,-1]


# solamente con las variables de interes 
datos2 <- datos1[,c(3:6,13:14)]

# control
summary(datos2)

# creacion de lista con las modadlidades (categorias) de las 6 variables
levJ <- list(c('primaria','secundaria','terciaria'), c('casaN','casaS'), c('autoN','autoS'), c('alqN','alqS'), c('menosqB','Basicos','masqB'), c('nbiN','nbiS'))

# forma 'elaborada' de etiquetar modalidades (categorias) en las 6 variables
for (j in 1:ncol(datos2))
    {datos2[,j] <- as.factor(datos2[,j])
     levels(datos2[,j]) <- levJ[[j]] }

summary(datos2)


# creamos tabla de 2*2 para usar en acs, tomamos variable 1 y 5 de datos2

A <- as.matrix(table(datos2[,1],datos2[,5]))
attributes(A)$class <- NULL
B <- as.data.frame(A)

#hacemos acs a la tabla B

source('acs.R')
salidaacs=acs(B)

