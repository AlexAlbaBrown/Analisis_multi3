##########################################
	### INTRODUCCIÓN A R  ###
##########################################


#####################   1 CODIGO ABIERTO  #####################

methods(mean)
mean.default

#####################   2 ORDENES  #####################

# es el símbolo para comentarios

# los nombres de los objetos deben empezar con letras y pueden incluir números y otros símbolos como ".", "_"
# R diferencia entre minúscula y mayúscula
a = c(1, 2)
A 
a  
# para asignar también se utiliza "<-"

b <- c(a,100)

# el decimal se indica con "."
5/2

# No usar "T", "F" ,"pi", "Inf", "NaN" ni "NA" porque tienen significados especiales
T; F; pi; Inf; NaN; NA  # ";" permite escribir comandos en una misma linea

##################### 3 DIRECTORIO DE TRABAJO #####################

# directorio en el que se está trabajando 
getwd() 

# cambiar directorio, pongan el que quieran por ejemplo en sus casas setwd("C:/Misdocumentos/Multivariado/Clase de introduccion a R 2012")
#setwd("\\\\164.73.246.107/cursos/Licenciatura/Multivariado/Clase de introduccion a R 2012") 

# para ver los archivos que hay en el directorio
dir() 

#####################  4 PAQUETES  #####################

# ver los paquetes que están cargados (funcionalidad básica de R)
sessionInfo()  

# instalar un paquete por menú o con la siguiente orden
install.packages() # despliega la lista de paquetes disponibles
install.packages("xlsReadWrite")

# OjO en cada nueva sesión se deben cargar los paquetes que se necesiten
library(foreign)
library(xlsReadWrite)
xls.getshlib() # es necesario para que xlsReadWrite funcione

# cargar una función creada por nosotros
source("acm.R") 

# lista los paquetes y los data.frames cargados
search()

##################### 5 AYUDA #####################

help(mean) # equivalente a ?mean 
help.search("mean") # equivalente a ??mean, busca archivos que contengan la palabra deseada en el nombre, concepto o título
help(package = foreign) # me da información sobre el paquete

##################### 6 CARGAR BASES DE DATOS #####################

# cargar una base interna
data() # lista todos las bases disponibles 

library(car)
help(Chile)
data(Chile)
datos = Chile

# importar bases 
casas = read.table(file = "preciocasas.dat")
estud = read.table("estud.txt", header=TRUE, sep="", dec=".") # son las opciones por defecto, no es necesario ponerlas
nombre = read.csv("nombres.csv",header=TRUE,sep="\t",dec=",")
spss = read.spss("b2005.SAV") # lee archivo de SPSS ojo hay que cargar la librería foreign antes
estudxls = read.xls("estud.xls")  # lee archivo de Excel ojo hay que cargar la librería antes

##################### 7 VER Y REMOVER OBJETOS #####################

ls() # lista los objetos existentes en ese momento en el espacio de trabajo
objects() # otra forma de hacer lo mismo
rm(nombre) # elimina el objeto llamado "nombre"
ls()
#rm(list=ls())  # remueve todo los objetos del espacio de trabajo 

##################### 8 TIPO DE OBJETOS #####################

# Vector
x <- c(10.4, 5.6, 3.1, 6.4, 21.7) 

# Matriz
M <- matrix(c(1,4,-3,2,1,5,-2,5,3), 3, 3, byrow=T)

# Lista
L = list(numerico=x, matriz=M) #crea una lista y le asigna nombre a los objetos

# Data Frame

##################### 9 TRABAJAR CON DATA FRAMES #####################

#convertir un objeto en data.frame
class(spss)
spss = as.data.frame(spss)
class(spss)

class(estud)
str(estud) # estructura de los datos
nrow(estud) # número de filas
ncol(estud) # número de columnas
dim(estud) # la dimensión de la base (número de filas   número de columnas)
head(estud) # muestra las primeras filas de la base
tail(estud) # muestra las últimas filas de la base
names(estud) # nombres de las variables
row.names(estud) # nombres de las filas
summary(estud) # resumen de los datos
summary(estud$status) # resumen de una variable Ojo!! no es una variable cuantitativa
class(estud$status)
estud$status = factor(estud$status) # convierto la variable status en un factor
class(estud$status)
summary(estud$status) #tabla de frecuencia de la variable status

class(estud$tipinst)
levels(estud$tipinst) # los niveles de la variable

estud$obs=4 # agrego una variable de nombre "obs" que vale siempre 4
parte = estud[,c("status","tipinst")] # me quedo solo con las variables "status" y "tipinst"

##################### 10 GRAFICOS #####################

# Univariado agregar rótulos y títulos ver la ayuda para la función boxplot, hist y plot
par(mfrow=c(1,2))
boxplot(Chile$age)
hist(Chile$age)
dev.off()
plot(Chile$sex)

# Bivariado agregar rótulos y títulos ver la ayuda para la función boxplot, hist y plot
plot(casas$V6, casas$V1)
plot(casas$V6, casas$V1, type="l") # no tiene sentido unir los puntos
boxplot(casas$V2~casas$V7)
pairs(casas[,-7])
names(casas)<- c('precio','superficie','cuartosbano','dormitorio','garage','antiguedad','vista') # cambiar nombre a las variables

# agregarle histogramas en la diagonal corro la función panel.hist
panel.hist <- function(x, ...)
     {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$counts; y <- y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col="darkgray", ...)
     }

pairs(casas[,-7],	panel = function(x,y){points(x,y)},  diag.panel = panel.hist)

##################### 11 PARA JUGAR EN SUS CASAS #####################

class(x)
w=x<=7
class(w)
z = c("juan","pedro","clara")
class(z)
z[1] # acceder a un elemento
z[-3] # sacar temporariamente un elemento
length(z) # ver tamaño del vector

# acceder a los elementos, filas y columnas de una matriz
M[1,1]
M[,1] # columna
M[1,] # fila
M=M[,-1] # quito la columna uno

# acceder a los elementos de una lista
L$matriz # elemento de la lista
L[[1]] # elemento de la lista
L[[2]][1,2]