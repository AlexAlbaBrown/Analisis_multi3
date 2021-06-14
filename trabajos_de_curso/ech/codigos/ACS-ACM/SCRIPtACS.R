

tab <- read.table('tareascasa.txt')# ,sep='\t')#,row.names=1
class(tab); dim(tab)

col <- c('Esposa','Alternado','Esposo','Conjuntamente')
fil <- c('Lavanderia', 'Almuerzo', 'Cena', 'Desayuno', 'Limpiar', 'Lavar platos', 'Compras', 'Tramites', 'Conducir', 'Finanzas', 'Seguros', 'Reparaciones', 'Vacaciones')

names(tab) <- col
row.names(tab) <- fil


source('acs.R')
##USO
#acs(xtab,Fsup=NULL,Csup=NULL) {
#  xtab= Tabla de Contingencia
#  Fsup= Filas suplementarias si las hay, en la forma: c(i,j,z)
#  Csup= Columnas suplementarias en la forma : c(...)
 

A <- acs(tab) #Csup=c(4))

