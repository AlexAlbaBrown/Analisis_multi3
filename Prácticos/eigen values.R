
# Genero una matriz con 100 valoresde una normal en 10 filas y 10 columnas
x <- matrix(rnorm(100), 10, 10) # asigno la matrix al elemento x
class(x) # inspecciono la clase del elemento x
dim(x) # inspecciono la dimensión del elemento x

# genero la matrix x'x
x <- t(x) %*% x

# genera las normas de los eigen vectors y las imprime en pantalla
for(i in 1:10){
      y <- eigen(x)$vectors[,i] %*% eigen(x)$vectors[,i]
      print(y)
}

# genera las noramas de los eigen vectors y las guarda en el vector normas

normas <- NULL

for(i in 1:10){
      normas[i]=eigen(x)$vectors[,i] %*% eigen(x)$vectors[,i]
      
}



