####################
#### PR�CTICO 4 ####
####################

rm(list=ls())
setwd("G:/UdelaR/CCEEA/Semestre 7/An�lisis Multivariado I/Pr�cticos")


#### EJERCICIO 6 ####

library(FactoMineR)

prensa <- read.csv("prensa.csv", header=TRUE, row.names=1)

X11(15,15)
par(oma=c(0,1,0,1))
ACS <- CA(prensa, ncp=5, graph=TRUE)


#### Comentario factor map ####

# El eje 1 capta un 75.10% de la variabilidad total, mientras que el segundo un 18.00%. 
# Esto quiere decir que la variabilidad acumulada (captada por los dos ejes) suma un total de
# 93.1%, lo cual es un porcentaje considerablemente alto.

# Las modalidades JUB, y EMEDIO de la variable INFO, y las modalidades LIB, DIA, y TEL de la 
# variable CSP parecer�an ser las que m�s contribuyen a la conformaci�n del eje 1.
# En lo que respecta al eje 2: AGRI, OBR, EMPL, y OTRO son las que mas contribuyen

# No se puede concluir nada sobre las relaciones entre las modalidades ya que ninguna parece 
# estar demasiado cerca de la circunferencia del c�rculo unidad.

#### An�lisis formal ####

# 1. Vemos las coordenadas de cada modalidad en el factor map

ACS$row$coord
ACS$col$coord

# Las mismas nos informan que, la modalidad AGRI de la variable CSP tiene coordenadas 
# (-0.14970236, 0.547799620), no estando por lo tanto cerca de la circunferencia del c�rculo
# unidad. Lo mismo sucede con las dem�s modalidades. Lo mismo sucede con las modalidades de
# la variable INFO.

# Analizando los valores de las coordenadas (y viendo tambi�n el factor map) parecer�a que el
# eje 2 separa entre Ejecutivos superiores y medios, y desocupados. El eje 1 separa entre 
# productores rurales y, obreros y empleados.

# 2. Determinaci�n de qu� mdalidades son las que contribuyen m�s a la coformaci�n de ls ejes 

ACS$row$contrib
ACS$col$contrib

# Vemos aqu� claramente que, para la dimensi�n 1, las modalidades EMEDIO y JUB de la variable
# CSP, y las modalidades TEL y LIB de la variable INFO son las que m�s contribuyen a la 
# conformaci�n del eje 1.

# Para la dimensi�n 2, AGRI y OBR de la variable CSP, y OTRO y RAD de la variable INFO son 
# las modalidades que mas contribuyen a la conformaci�n del eje 2.

### Es importante destacar la concordancia de los resultados anteriores ###
###### con los obtenidos a partir del an�lisis visual del factor map ######

# 3. Por �ltimo, �cu�les son las modalidades que est�n mejor representadas?

ACS$row$cos2
ACS$col$cos2

# Cuando el coseno del �ngulo formado por el vector que va del origen a la coordenada de cada
# punto en el factor map est� cercano a 1, esto quiere decir que los individuos est�n 
# "bien representados". 

# En la dimensi�n 1: EMEDIO, ESUP, JUB, TEL y LIB son los que est�n mejor representados.
# En la dimensi�n 2 lo son AGRI y OTRO.
# En el resto de las dimensiones, los valores de los cosenos se reducen significativamente.

### Importante destacar que el an�lisis fue realizado conjuntamente a lo largo del c�digo
### seg�n modalidades de ambas variables.

################################
#### FIN DE LA PROGRAMACI�N ####
################################