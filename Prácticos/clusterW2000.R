#### CLUSTER JERÁRQUICO AGREGATIVO CON LA BASE W2000 #########

library(cluster) # cargamos la librería
library(help=cluster) # la ayuda de la librería
?agnes

# cargamos otras funciones que vamos a usar (OJO DIRECTORIO)
source('standard.R') # no es necesaria 
source('indicadores.R') # si ponemos edit(indicadores) vemos el código

# LEEMOS DATOS
dat = read.table('w2000.txt',sep='\t',header=TRUE,row.names=1) 
# toma la primera columna de la base (variable país) como vector de nombres de las filas (no como variable)
head(dat) # muestra las primeras 6 filas del conjunto de datos
names(dat) # muestra los nombres de las variables
str(dat) # muestra la estructura del conjunto de datos
dim(dat) # las dimensiones de la base
# región aparece como entero pero debería ser un factor
class(dat$REGION)
dat$REGION = factor(dat$REGION) # lo convertimos en factor
class(dat$REGION)
levels(dat$REGION) # los niveles
levels(dat$REGION) = c('OECD','EuropaEste','Asia','Africa','OrienteMEdio','AmericaLatina') # le cambio los niveles al factor
LISTA <- NULL; for (r in levels(dat$REGION)) {LISTA[[r]] <- row.names(dat[dat$REGION==r,])} # lista los países por región


# DESCRIPTIVA DE LOS DATOS
summary(dat) # el resumen de las variables
dat[dat$LITERACY==100,]

var(dat[,3:15],use='pairwise.complete.obs') # matriz de covarianzas
round(cor(dat[,3:15],use='pairwise.complete.obs') , 2) # matriz de correlaciones

plot(dat[,c(5,9,10)]) # graficos de dispersión dos a dos entre las variables 5, 9, 10
plot(dat[,c("LIFEEXPF","BABYMORT","GDP_CAP")]) # equivalente al anterior

plot(dat$DENSITY) # ejemplo de gráfico de puntos
text(dat$DENSITY,row.names(dat)) # agrego el nombre de los países a los puntos

boxplot(dat$GDP_CAP,horizontal=TRUE,main='Producto Interno Bruto per capita') # ejemplo de diagrama de caja horizontal
boxplot(dat$GDP_CAP~dat$REGION, names=levels(dat$REGION),col="red", main='Producto Interno Bruto per capita') # PBI per cápita por región

#### CLUSTER JERÁRQUICO AGREGATIVO ###############################

#### ALGORITMO
DATOSst = standard(dat[,3:13])  # estandariza los datos
DATOSst = scale(dat[,3:13]) # otra forma

agrupo = agnes(DATOSst, diss= FALSE, metric = "euclidean", stand = FALSE, method = 'ward') # probar con otros métodos
# "single" es vecino más cercano
# "complete" es vecino más lejano
# otra forma de hacer lo mismo (o similar)
agrupo2 = agnes(dat[,3:13], metric = "euclidean", stand = TRUE, method = 'ward') # en vez de escalar con la varianza,
# escala con mean(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)
# OJO!!! si le doy un factor en los datos lo convierte en numérico y sigue...

#### INDICADORES
IND = indicadores(agrupo[4],DATOSst,imprime=20)
#  agrupo[4] o agrupo$merge
# los datos que usé para hacer el cluster
# nos muestra los últimos 20 pasos
plot(rev(IND$Rcuad), xlab='Número de grupos', ylab=expression(R^2)) # solo muestra apartir de que quedan 20 grupos en adelante

plot(agrupo,which=2) # dendrograma
abline(h=15, lty = 2, col = 'blue', lwd = 2) # trazar una linea
rect.hclust(agrupo, k=3) # grafica rectángulos que permiten visualizar los grupos

### OBTENCIÓN DE LOS GRUPOS
k = 3 # 4
cl = cutree(agrupo[4],k) # me dice en qué grupo está cada observación
cl = factor(cl) # lo convierto en factor
gru = cbind(dat,cl) # lo agrego al conjunto de datos

##### CARACTERIZACIÓN
table(gru$cl, gru$REGION); prop.table(table(gru$cl, gru$REGION),2)

resumen <- function(x) c(mediana=median(x), min=min(x), max=max(x))
aggregate(gru[, c('LIFEEXPF','LIFEEXPM','BABYMORT','BIRTH_RT')], list(Grupo = gru$cl), resumen)

boxplot(gru$GDP_CAP~gru$cl, col=c("red","orange","blue"), main='Producto Interno Bruto per capita')
boxplot(gru$BABYMORT~gru$cl, col=rainbow(3), main='Mortalidad infantil')
plot(gru[,c(9,10)],col=heat.colors(3)[gru$cl])
plot(gru[,c(5,10)],col=rainbow(3)[gru$cl], pch="*")
plot(gru[,c(4,13)],col=rainbow(3)[gru$cl], pch=16)

pairs(gru[, c('LIFEEXPF','LIFEEXPM','BABYMORT','BIRTH_RT')],  , panel = function(x,y){ 
			points(x,y, col = as.numeric(gru$cl), cex =2)},cex.labels = 2.5)

# QUEDARSE CON UNA PARTE DEL CONJUNTO DE DATOS QUE CUMPLE CON DETERMINADA CONDICIÓN
gru1 = subset(gru,gru$cl!=3) # los países que no están en el grupo 3
gru2 = subset(gru,(gru$DENSITY<100 & gru$cl!=3)) # países con densidad menor a 100 habitantes por km^2 y que están en el grupo 1 ó 2 

