a = matrix(data = c(156, 14, 2, 4,
                    124, 20, 5, 4,
                    77, 11, 7, 13, 
                    82, 36, 15, 7,
                    53, 11, 1, 57,
                    32, 24, 4, 53,
                    33, 23, 9, 55,
                    12, 46, 23, 15,
                    10, 51, 75, 3,
                    13, 13, 21, 66,
                    8, 1, 53, 77,
                    0, 3, 160, 2,
                    0, 1, 6, 153), nrow=13, ncol=4, byrow=TRUE)

b <- c("Lavanderia", "Almuerzo", "Cena", "Desayuno", "Limpieza", "Lavado_vajilla", 
       "Compras", "Tramites", "Conducir", "Finanzas", "Seguros", "Reparaciones",
       "Vacaciones") 
rownames(a) = b
c <- c("Esposa", "Alternado", "Esposo", "Conjuntamente")
colnames(a) = c

source("C:/Users/dacza/Dropbox/R Functions/acs.r")

acs(a)


library(FactoMineR)
acs = CA(a, ncp=10)
