###OJO!!!!   NA-NaN handling ##bueno x ahora quedo(8/4)!!!!!!!!!##Modificado sobre codigo  de Oscar y Andr'es(2003)
####Ahora los nombres de filas y columnas se conservan!! (1/6


standard<-function(datos) 
     {	m <- datos 
	nfil <- nrow(datos) 
	matr2 <- unlist(datos) 
	matr22 <- matrix(matr2,nrow(datos),ncol(datos),byrow=F) 
	desvios <- sqrt((diag(var(matr22,use='pairwise.complete.obs')))*( nrow(datos)-1)/( nrow(datos))) 
   	medias <- colMeans(matr22, na.rm=T, dims=1)	 
	matmedias <- matrix(1, nfil, 1) %*% t(medias) 
	matstandar <- (matr22 - matmedias)/(matrix(1, nrow(datos) , 1) %*% t(desvios)) 
  if (is.data.frame(datos))
  { matstandar <- as.data.frame(matstandar)
   attr(matstandar,'names') <- attributes(datos)$names
   attr(matstandar,'row.names') <- attributes(datos)$row.names
  }
  invisible(matstandar) #print(matstandar)
      } 
