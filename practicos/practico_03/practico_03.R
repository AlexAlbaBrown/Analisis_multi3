####################
#### PR?CTICO 3 ####
####################

#### EJERCICIO 1 ####

for(i in -10:10) {
   d <- i
   S <- matrix(
      data=c(1 + d, 1, 1, 1, 1 + d, 1, 1, 1, 1 + d),
      nrow = 3,
      ncol = 3,
      byrow=FALSE
   )
   print(
      paste(
         "d=", i, "=> valores propios:",
         round(eigen(S)$values[1], 2),
         round(eigen(S)$values[2], 2),
         round(eigen(S)$values[3], 2),
         sep = " "
      )
   )
}


d <- 0
S <- matrix(
   data = c(1 + d, 1, 1, 1, 1 + d, 1, 1, 1, 1 + d),
   nrow = 3,
   ncol = 3,
   byrow=FALSE
)
eigen(S)

