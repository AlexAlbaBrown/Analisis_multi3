
standard <- function(datos) {
   m <- datos
   nfil <- nrow(datos)
   matr2 <- unlist(datos)
   matr22 <- matrix(matr2, nrow(datos), ncol(datos), byrow = FALSE)
   desvios <- sqrt((diag(var(matr22, use = 'pairwise.complete.obs'))) * (nrow(datos) - 1) / (nrow(datos)))
   medias <- colMeans(matr22, na.rm = TRUE, dims = 1) 
   matmedias <- matrix(1, nfil, 1) %*% t(medias)
   matstandar <- (matr22 - matmedias) / (matrix(1, nrow(datos), 1) %*% t(desvios))
   if (is.data.frame(datos)) {
      matstandar <- as.data.frame(matstandar)
      attr(matstandar, 'names') <- attributes(datos)$names
      attr(matstandar, 'row.names') <- attributes(datos)$row.names
   }
   invisible(matstandar)
} 
