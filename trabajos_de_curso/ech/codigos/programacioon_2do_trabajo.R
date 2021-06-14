########################################################
##### SEGUNDO INFORME AN?LISIS MULTIVARIADO I 2016 #####
########################################################


#### Cargo librerias y funciones ####

library(cluster); library(lattice); library(mclust)
library(fpc);     library(MVN);     library(MASS)
library(dplyr);   library(nnet);    library(Metrics)
library(bitops);  library(ggplot2); library(RCurl)
library(caret);   library(boot);    library(StatMatch)
library(car);     library(ca);      library(FactoMineR)
library(stats);   library(mitools); library(WriteXLS)
library(Rcmdr)

source("panelcor.R")
source("panelhist.R")
source("indicadores.R")
source("shadowtext.R")


#$##############################
#### PREPARACI?N DE LA BASE ####
################################


#### Exploraci?n inicial ####

ech = read.table("ech95-04.txt", header = TRUE, sep = "")

# La base es un pseudo panel de la ech para el per?odo 1995 a 2004.
# Para cada a?o se muestra la variable res?men (nombre) para cada uno de los grupos construidos

# No existen NA en la base
table(complete.cases(ech))

# Primera vista de las variables
str(ech)

# Cambio el nombre de las variables: A?O -> year, TAMA?O -> size, y P?BLICO -> public
ech = rename(ech, year = A?O, size = TAMA?O, publico = P?BLICO)

# Cambio los nombres de las variables en uppercase por lowercase
names(ech) <- tolower(names(ech))


#### Creaci?n de dummies ####

# Creo variables binarias para cada uno de los atributos de la variable "nombre"

ech[, "educ"] <- substr(ech$nombre, 4, 4)
ech <- select(ech, educ, everything())
ech$educ <- as.factor(gsub(",", "", as.character(ech$educ)))

ech[, "edad"] <- substr(ech$nombre, 3, 3)
ech <- select(ech, edad, everything())
ech$edad <- as.factor(gsub(",", "", as.character(ech$edad)))

ech[, "sexo"] <- substr(ech$nombre, 2, 2)
ech <- select(ech, sexo, everything())
ech$sexo <- as.factor(gsub(",", "", as.character(ech$sexo)))

ech[, "loc"] <- substr(ech$nombre, 1, 1)
ech <- select(ech, loc, everything())
ech$loc <- as.factor(gsub(",", "", as.character(ech$loc)))

# Muevo la variable year a la primera posici?n
ech <- select(ech, year, everything())

# Muevo la variable udad_anio a la primera posici?n
ech <- select(ech, udad_anio, everything())

# Muevo la variable unidad a la primera posici?n
ech <- select(ech, unidad, everything())

# Creo la variable nombre2 (=nombre sin localidad)

ech[, "nombre2"] <- substr(ech$nombre, 2, 4)
ech$nombre2 <- as.factor(gsub(",", "", as.character(ech$nombre2)))

# Creo la variable locsex
ech[, "locsex"] <- substr(ech$nombre, 1, 2)
ech$locsex <- as.factor(gsub(",", "", as.character(ech$locsex)))


#### Generaci?n de submatrices e inversas ####


# Genero la base ech2, la cual contiene la variable nombre + vars explicativas

ech2 <- select(ech, (nombre:ingreso))

# Generp la base ech3, la cual contiene ?nicamente las vars explicativas

ech3 <- select(ech2, -nombre)

# Genero la base ech_redu con las variables de situaci?n de empleo

ech_redu <- select(ech3, desemp, tparcial, multiemp, subemp, precario)

# Genero la base ech_res con el resto de las variables

ech_res <- select(ech, -(1:8),-10,-12,-13,-25,-26, -28, -29)

# Inversas de las matrices

echinv <- t.data.frame(ech)
ech2inv <- t.data.frame(ech2)
ech3inv <- t.data.frame(ech3)


#### Paletas de colores ####

colores6=c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f")
sexcolor <- c("lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3", "palegreen3")


#$##############################################
#### An?lisis de las variables explicativas ####
################################################

cor <- cor(ech3)

write.csv(cor, "tablas/cor.csv")


##### Variables referentes al hogar #####

# Estad?sticas descriptivas
select(ech, jefe, ingreso, size) %>% lapply(summary)
select(ech, jefe, ingreso, size) %>% cor()

# Histogramas de las variables
X11(15,15)
layout(matrix(c(1,1,1,1,2,2,2,2,0,0,3,3,3,3,0,0), 2, 8, byrow = TRUE))
par(oma=c(1,1,1,1))
hist(ech$jefe, main = "% de jefes de hogar", xlab=NA)
hist(ech$ingreso, main = "Mediana de remuneraci?n al trabajo", xlab=NA)
hist(ech$size, main = "% de personas que trabajan en establecimientos \n con menos de cinco personas", xlab=NA)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Histogramas.pdf")
dev.off()

# Densidades
djefe <- with(ech, density(jefe))
dingreso <- with(ech, density(ingreso))
dsize <- with(ech, density(size))

X11(15,15)
par(mfrow=c(1,2), mar=c(3,4,1,1), oma=c(1,1,2,1))
plot(djefe, lty=1, col="red", ylim=c(-0.001, 0.04), main="", ylab="Densidad estimada")
      lines(dsize, lty=1, col="green", main="")
      legend("topright", legend=c("jefes", "tama?o"), col=c("red", "green"), lty=c(1,1))
      abline(h=0, lty=1, col="black")
plot(dingreso, lty=1, col="blue", ylab=NA, main="", ylim=c(-0.001, 0.4))
      legend("topright", legend=c("ingreso"), col=c("blue"), lty=1)
      abline(h=0, lty=1, col="black")
mtext("Variables del hogar", side=3, outer=TRUE, line=-0.1, font = 2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Densidades estimadas.pdf")
dev.off()

# Box plots
X11(15,15)
par(mfrow=c(3,1))
boxplot(ech$jefe, main = "% de jefes de hogar", horizontal = TRUE)
boxplot(ech$ingreso, main = "Mediana de remuneraci?n al trabajo", horizontal = TRUE)
boxplot(ech$size, main = "% de personas que trabajan en establecimientos \n con menos de cinco personas", horizontal = TRUE)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Boxplots.pdf")
dev.off()


# Meidana de los ingresos por grupos
X11(15,15)
par(oma=c(1,1,1,1))
plot(ingreso ~ nombre, data=ech, varwidth=TRUE, main="Ingreso por grupo", col=sexcolor, xlab="Grupos", ylab="Mediana de ingresos")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Ingreso - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(ingreso ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(ingreso ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(ingreso ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(ingreso ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Ingreso por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Ingreso - Boxplots por componentes.pdf")
dev.off()


# Jefatura del hogar
X11(15,15)
par(oma=c(1,1,1,1))
plot(jefe ~ nombre, data=ech, varwidth=TRUE, main="Jefatura de los hogares por grupo", col=sexcolor, xlab="Grupos", ylab="% de jefes de hogar")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(jefe ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(jefe ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(jefe ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(jefe ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Jefatura por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe - Boxplots por componente.pdf")
dev.off()


# Tama?o
X11(20,15)
par(oma=c(1,1,1,1))
plot(size ~ nombre, data=ech, varwidth=TRUE, main="Tama?o por grupo", col=sexcolor, xlab="Grupos", ylab="Tama?o")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Tama?o - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(size ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(size ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(size ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(size ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Tama?o por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Tama?o - Boxplots por componente.pdf")
dev.off()

# Scatter plots


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, upper.panel=panel.cor, diag.panel=panel.hist, pch=20)

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots.pdf")
dev.off()


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)
mtext("Grupo", side=3, outer=TRUE, line=0, font=2, cex=2)


dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots por nombre.pdf")
dev.off()


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$loc), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)
mtext("Pairs por loc")


dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots por localida.pdf")
dev.off()


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$sexo), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)
mtext("Pairs por sexo")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots por sexo.pdf")
dev.off()


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$edad), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)
mtext("Pairs por edad")


dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots por edad.pdf")
dev.off()


X11(15,15)
pairs(~ jefe + ingreso + size, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$educ), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)
mtext("Pairs por educ")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Todas - Scatterplots educaci?n.pdf")
dev.off()


# Jefe Vs Mediana de los ingresosssss
X11(15,15)
par(oma=c(1,1,1,1))
plot(jefe ~ ingreso, data=ech, pch=20, col=c("red", "blue")[loc], main="Por localidad", ylab="% de jefes de hogar", xlab="Mediana del ingreso")
legend("bottomright", legend=c("Interior", "Montevideo"), pch=20, col=c("red", "blue"), bty="n")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe Vs Ingreso - Scatterplots por localidad.pdf")
dev.off()


X11(15,15)
par(oma=c(1,1,1,1))
plot(jefe ~ ingreso, data=ech, pch=20, col=c("magenta", "skyblue")[sexo], main="Por sexo", ylab="% de jefes de hogar", xlab="Mediana del ingreso")
legend("bottomright", legend=c("Hombres", "Mujeres"), pch=20, col=c("magenta", "skyblue"), bty="n")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe Vs Ingreso - Scatterplots por sexo.pdf")
dev.off()


X11(15,15)
par(oma=c(1,1,1,1))
plot(jefe ~ ingreso, data=ech, pch=20, col=c("antiquewhite4", "violetred", "green")[edad], main="Por edad", ylab="% de jefes de hogar", xlab="Mediana del ingreso")
legend("bottomright", legend=c("20-29", "30-49", "50 y m?s"), pch=20, col=c("antiquewhite4", "violetred", "green"), bty="n")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe Vs Ingreso - Scatterplots por edad.pdf")
dev.off()


X11(15,15)
par(oma=c(1,1,1,1))
plot(jefe ~ ingreso, data=ech, pch=20, col=c("forestgreen", "darkorchid", "orange")[educ], main="Por educaci?n", ylab="% de jefes de hogar", xlab="Mediana del ingreso")
legend("bottomright", legend=c("Primaria", "Secundaria", "Terciaria"), pch=20, col=c("forestgreen", "darkorchid", "orange"), bty="n")

dev.copy2pdf(file = "graph/Estad. Desc. - Vars. hogar - Jefe Vs Ingreso - Scatterplots por educaci?n.pdf")
dev.off()


##### Variables referentes a la situaci?n de empleo #####

# Estad?sticas descriptivas
select(ech, desemp, tparcial, multiemp, subemp, precario) %>% lapply(summary)
select(ech, desemp, tparcial, multiemp, subemp, precario) %>% cor()

# Histogramas de las variables
X11(15,15)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,0,0,5,5,5,5,0,0), 3, 8, byrow=TRUE))
par(mar=c(2,4,2,2), oma=c(1,1,4,1))
hist(ech$desemp, main = "Desempleo", xlab=NA)
hist(ech$tparcial, main = "Empleo a tiempo parcial", xlab=NA)
hist(ech$multiemp, main = "Multiempleo", xlab=NA)
hist(ech$subemp, main = "Subempleo", xlab=NA)
hist(ech$precario, main = "Empleo precario", xlab=NA)
mtext("% de personas por situaci?n de empleo", side=3, outer=TRUE, line=-0.1, font=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Todas - Histogramas.pdf")
dev.off()


# Densidades
ddesemp <- with(ech, density(desemp))
dtparcial <- with(ech, density(tparcial))
dmultiemp <- with(ech, density(multiemp))
dsubemp <- with(ech, density(subemp))
dprecario <- with(ech, density(precario))


X11(25,15)
plot(ddesemp, lty=1, col="blue", main="% de personas por situaci?n de empleo", ylab="Densidad estimada", ylim=c(-0.01, 0.085), xlim=c(0, 120))
      lines(dtparcial, lty=1, col="magenta", main="")
      lines(dmultiemp, lty=1, col="red", main="")
      lines(dsubemp, lty=1, col="green", main="")
      lines(dprecario, lty=1, col="yellow", main="")
      abline(h=0, lty=1, col="black")
legend("topright", legend=c("Desempleo", "Tiempo parcial", "Multiempleo", "Subempleo", "Precario"), col=c("blue", "magenta", "red", "green", "yellow"), lty=c(1,1,1,1,1))

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Todas - Densidades estimadas.pdf")
dev.off()


# Box plots
X11(10,10)
par(mfrow=c(5,1), mar=c(2,4,1,1), oma=c(1,1,1,1))
boxplot(ech$desemp, ylab="Desempleo", horizontal = TRUE)
boxplot(ech$tparcial, ylab="Empleo a tiempo parcial", horizontal = TRUE)
boxplot(ech$multiemp, ylab="Multiempleo", horizontal = TRUE)
boxplot(ech$subemp, ylab="Subempleo", horizontal = TRUE)
boxplot(ech$precario, ylab="Empleo precario", horizontal = TRUE)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Todas - Boxplots.pdf")
dev.off()


# Desempleo por grupos
X11(20,15)
par(oma=c(1,1,1,1))
plot(desemp ~ nombre, data=ech, varwidth=TRUE, main="Desempleo por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas desempleadas")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Desempleo - Boxplots por nombre.pdf")
dev.off()

X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(desemp ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(desemp ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(desemp ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(desemp ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Desempleo por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Desempleo - Boxplots por componente.pdf")
dev.off()


# Empleo a tiempo parcial
X11(20,15)
par(oma=c(1,1,1,1))
plot(tparcial ~ nombre, data=ech, varwidth=TRUE, main="Empleo a tiempo parcial por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con empleo a tiempo parcial")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - tparcial - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(tparcial ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(tparcial ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(tparcial ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(tparcial ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Empleo a tiempo parcial por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - tparcial - Boxplots por componente.pdf")
dev.off()


# Multiempleo
X11(20,15)
par(oma=c(1,1,1,1))
plot(multiemp ~ nombre, data=ech, varwidth=TRUE, main="Multiempleo por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con multiempleo")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Multiempleo - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(multiemp ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(multiemp ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(multiemp ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(multiemp ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Multiempleo por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Multiempleo - Boxplots por componente.pdf")
dev.off()


# Subempleo
X11(20,15)
par(oma=c(1,1,1,1))
plot(subemp ~ nombre, data=ech, varwidth=TRUE, main="Subempleo por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con subempleo")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Subempleo - Boxplots por nombre.pdf")
dev.off()

X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(subemp ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(subemp ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(subemp ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(subemp ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Subempleo por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Multiempleo - Boxplots por componente.pdf")
dev.off()


# Empleo precario
X11(20,15)
par(oma=c(1,1,1,1))
plot(precario ~ nombre, data=ech, varwidth=TRUE, main="Empleo precario por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con empleo precario")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Precariedad - Boxplots por nombre.pdf")
dev.off()

X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(subemp ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(subemp ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(subemp ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(subemp ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Empleo precario por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Precariedad - Boxplots por componente.pdf")
dev.off()


# Scatter plots
X11(15,15)
pairs(~ desemp + tparcial + multiemp + subemp + precario, data=ech, upper.panel=panel.cor, diag.panel=panel.hist, pch=20)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Todas - Scatterplots.pdf")
dev.off()


X11(15,15)
pairs(~ desemp + tparcial + multiemp + subemp + precario, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)

dev.copy2pdf(file = "graph/Estad. Desc. - Situaci?n empleo - Todas - Scatterplots por nombre.pdf")
dev.off()


##### Variables referentes a la categor?a de ocupaci?n #####

# Estad?sticas descriptivas
select(ech, privado, publico, cpsl, cpcl) %>% lapply(summary)
select(ech, privado, publico, cpsl, cpcl) %>% cor()

# Histogramas de las variables
X11(15,15)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 2, 8, byrow=TRUE))
par(mar=c(4,4,2,2), oma=c(1,1,5,1))
hist(ech$privado, main="Privado", xlab=NA)
hist(ech$publico, main="P?blico", xlab=NA, ylab=NA)
hist(ech$cpsl, main="Trabajadores por cuenta propia sin local", xlab=NA)
hist(ech$cpcl, main="Trabajadores por cuenta propia sin local", xlab=NA, ylab=NA)
mtext("% de personas por categor?a de ocumpaci?n", side=3, outer=TRUE, line=-0.3, font=2, cex=1.5)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Todas - Histogramas.pdf")
dev.off()


# Densidades
dprivado <- with(ech, density(privado))
dpublico <- with(ech, density(publico))
dcpsl <- with(ech, density(cpsl))
dcpcl <- with(ech, density(cpcl))


X11(25,15)
par(mfrow=c(1,2), mar=c(4,4,3,1), oma=c(1,1,3,1))
plot(dprivado, lty=1, col="blue", ylim=c(-0.001, 0.04), main="Trabajadores en relaci?n de dependencia", ylab="Densidad estimada")
      lines(dpublico, lty=1, col="magenta", main="")
      abline(h=0, lty=1, col="black")
legend("topright", legend=c("Privado", "P?blico"), col=c("blue", "magenta"), lty=c(1,1))
plot(dcpsl, lty=1, col="red", main="Trabajadores por cuenta propia", ylab="")
      lines(dcpcl, lty=1, col="green", main="")
      abline(h=0, lty=1, col="black")
legend("topright", legend=c("Sin local", "Con local"), col=c("red", "green"), lty=c(1,1))
mtext("% de personas por categor?a de ocupaci?n", side=3, outer=TRUE, line=-0.3, font=2, cex=1.5)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Todas - Densidades estimadas.pdf")
dev.off()


# Box plots
X11(10,10)
par(mfrow=c(4,1), mar=c(2,4,1,1), oma=c(1,1,1,1))
boxplot(ech$privado, ylab="Privado", horizontal = TRUE)
boxplot(ech$publico, ylab="P?blico", horizontal = TRUE)
boxplot(ech$cpsl, ylab="Sin local", horizontal = TRUE)
boxplot(ech$cpcl, ylab="Con local", horizontal = TRUE)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Todas - Boxplots.pdf")
dev.off()


# Empleo Privado
X11(20,15)
par(oma=c(1,1,1,1))
plot(privado ~ nombre, data=ech, varwidth=TRUE, main="Privado por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas desempleadas")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Privado - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(privado ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(privado ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(privado ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(privado ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Privado por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Privado - Boxplots por componente.pdf")
dev.off()


# Empleo p?blico
X11(20,15)
par(oma=c(1,1,1,1))
plot(publico ~ nombre, data=ech, varwidth=TRUE, main="P?blico por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con empleo a tiempo parcial")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - P?blico - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(publico ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(publico ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(publico ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(publico ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("P?blico por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - P?blico - Boxplots por componente.pdf")
dev.off()


# Cuenta propia sin local
X11(20,15)
par(oma=c(1,1,1,1))
plot(cpsl ~ nombre, data=ech, varwidth=TRUE, main="Cuenta propia sin local por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores por cuenta propia sin local")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - CPSL - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(cpsl ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(cpsl ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(cpsl ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(cpsl ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Cuenta propia sin local por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - CPSL - Boxplots por componente.pdf")
dev.off()


# Cuenta propia por local
X11(20,15)
par(oma=c(1,1,1,1))
plot(cpcl ~ nombre, data=ech, varwidth=TRUE, main="Cuenta propia con local por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores por cuenta propia con local")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - CPCL - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(cpcl ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(cpcl ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(cpcl ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(cpcl ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Cuenta propia con local por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - CPCL - Boxplots por componente.pdf")
dev.off()


# Scatter plots
X11(15,15)
pairs(~ privado + publico + cpsl + cpcl, data=ech, upper.panel=panel.cor, diag.panel=panel.hist, pch=20)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Todas - Scatterplots.pdf")
dev.off()


X11(15,15)
pairs(~ privado + publico + cpsl + cpcl, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)

dev.copy2pdf(file = "graph/Estad. Desc. - Categor?a ocupaci?n - Todas - Scatterplots por nombre.pdf")
dev.off()


##### Variables referentes a la condici?n de ocupaci?n #####

# Estad?sticas descriptivas
select(ech, profytec, oficina, manual) %>% lapply(summary)
select(ech, profytec, oficina, manual) %>% cor()

# Histogramas de las variables
X11(15,15)
layout(matrix(c(1,1,1,1,2,2,2,2,0,0,3,3,3,3,0,0), 2, 8, byrow=TRUE))
par(mar=c(4,4,2,2), oma=c(1,1,5,1))
hist(ech$profytec, main="Profesionales y t?cnicos", xlab=NA)
hist(ech$oficina, main="Trabajadores de oficina", xlab=NA, ylab=NA)
hist(ech$manual, main="Trabajadores manuales", xlab=NA)
mtext("% de personas por condici?n de ocumpaci?n\n", side=3, outer=TRUE, line=-0.3, font=2, cex=1.5)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Todas - Histogramas.pdf")
dev.off()


# Densidades
dprofytec <- with(ech, density(profytec))
doficina <- with(ech, density(oficina))
dmanual <- with(ech, density(manual))


X11(25,15)
par(oma=c(1,1,1,1))
plot(dprofytec, lty=1, col="blue", ylim=c(-0.001, 0.05), main="% de personas por condici?n de ocupaci?n", ylab="Densidad estimada")
      lines(doficina, lty=1, col="magenta", main="")
      lines(dmanual, lty=1, col="green")
      abline(h=0, lty=1, col="black")
legend("topright", legend=c("Profesionales y t?cnicos", "Empleados de oficina", "Empleados manuales"), col=c("blue", "magenta", "green"), lty=c(1,1,1))

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Todas - Densidades estimadas.pdf")
dev.off()


# Box plots
X11(10,10)
par(mfrow=c(3,1), mar=c(2,6,1,1), oma=c(1,1,1,1))
boxplot(ech$profytec, ylab="Profesionales\ny t?cnicos", horizontal=TRUE)
boxplot(ech$oficina, ylab="Empleados\nde oficina", horizontal=TRUE)
boxplot(ech$manual, ylab="Empleados\nmanuales", horizontal=TRUE)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Todas - Boxplots.pdf")
dev.off()


# Profesionales y t?cnicos
X11(20,15)
par(oma=c(1,1,1,1))
plot(profytec ~ nombre, data=ech, varwidth=TRUE, main="Profesionales y t?cnicos por grupo", col=sexcolor, xlab="Grupos", ylab="% de profesonales y t?cnicos")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Profytec - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(profytec ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(profytec ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(profytec ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(profytec ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Profesionales y t?cnicos por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Profytec - Boxplots por componente.pdf")
dev.off()


# Empleos de oficina
X11(20,15)
par(oma=c(1,1,1,1))
plot(oficina ~ nombre, data=ech, varwidth=TRUE, main="Oficina por grupo", col=sexcolor, xlab="Grupos", ylab="% de personas con empleo de oficina")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Oficina - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(oficina ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(oficina ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(oficina ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(oficina ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Oficina por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Oficina - Boxplots por componente.pdf")
dev.off()


# Empleos manuales
X11(20,15)
par(oma=c(1,1,1,1))
plot(manual ~ nombre, data=ech, varwidth=TRUE, main="Empleados manuales grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores en empleos manuales")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Manual - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(manual ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(manual ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(manual ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(manual ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Manual por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Manual - Boxplots por componente.pdf")
dev.off()


# Scatter plots
X11(15,15)
pairs(~ profytec + oficina + manual, data=ech, upper.panel=panel.cor, diag.panel=panel.hist, pch=20)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Todas - Scatterplots.pdf")
dev.off()


X11(15,15)
pairs(~ profytec + oficina + manual, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)

dev.copy2pdf(file = "graph/Estad. Desc. - Condici?n de ocupaci?n - Todas - Scatterplots por nombre.pdf")
dev.off()


##### Variables referentes a la rama del establecimiento #####

# Estad?sticas descriptivas
select(ech, indust, comercio, sfinan, sperson) %>% lapply(summary)
select(ech, indust, comercio, sfinan, sperson) %>% cor()


# Histogramas de las variables
X11(15,15)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 2, 8, byrow=TRUE))
par(mar=c(4,4,2,2), oma=c(1,1,5,1))
hist(ech$indust, main="Industria", xlab=NA)
hist(ech$comercio, main="Comercio", xlab=NA, ylab=NA)
hist(ech$sfinan, main="Servicios financieros y, a empresas", xlab=NA)
hist(ech$sperson, main="Servicios personales", xlab=NA, ylab=NA)
mtext("% de personas por rama del establecimiento\n", side=3, outer=TRUE, line=-0.3, font=2, cex=1.5)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Todas - Histogramas.pdf")
dev.off()


# Densidades
dindust <- with(ech, density(indust))
dcomercio <- with(ech, density(comercio))
dsfinan <- with(ech, density(sfinan))
dsperson <- with(ech, density(sperson))


X11(25,15)
par(mar=c(4,4,3,1), oma=c(1,1,3,1))
plot(dindust, lty=1, ylim=c(-0.001, 0.15), col="blue", main="Trabajadores por rama", ylab="Densidad estimada")
      lines(dcomercio, lty=1, col="magenta", main="")
      lines(dsfinan, lty=1, col="red", main="")
      lines(dsperson, lty=1, col="green", main="")
      abline(h=0, lty=1, col="black")
legend("topright", legend=c("Industria", "Comercio", "Servicios financieros, y a empresas", "Servicios personales"), col=c("blue", "magenta", "red", "green"), lty=c(1,1,1,1))

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Todas - Densidades estimadas.pdf")
dev.off()


# Box plots
X11(10,10)
par(mfrow=c(4,1), mar=c(2,5,1,1), oma=c(1,1,1,1))
boxplot(ech$indust, ylab="Industria", horizontal = TRUE)
boxplot(ech$comercio, ylab="Comercio", horizontal = TRUE)
boxplot(ech$sfinan, ylab="Servicios financieros\n, y a empresas", horizontal = TRUE)
boxplot(ech$sperson, ylab="Servicios personales", horizontal = TRUE)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Todas - Boxplots.pdf")
dev.off()


# Industria
X11(20,15)
par(oma=c(1,1,1,1))
plot(indust ~ nombre, data=ech, varwidth=TRUE, main="Industria por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores en la industria")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Industria - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(indust ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(indust ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(indust ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(indust ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Industria por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Industria - Boxplots por componente.pdf")
dev.off()


# Comercio
X11(20,15)
par(oma=c(1,1,1,1))
plot(comercio ~ nombre, data=ech, varwidth=TRUE, main="Comercio por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores en comercio")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Comercio - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(comercio ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(comercio ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(comercio ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(comercio ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Comercio por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Comercio - Boxplots por componente.pdf")
dev.off()


# Servicios financieros, y a empresas
X11(20,15)
par(oma=c(1,1,1,1))
plot(sfinan ~ nombre, data=ech, varwidth=TRUE, main="Servicios financieros, y a empresas por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores en servicios financieros, y a empresas")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Sfinan - Boxplots por nombre.pdf")
dev.off()


X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,6,1))
plot(sfinan ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(sfinan ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(sfinan ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(sfinan ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Servicios financieros \ny, a empresas, por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Sfinan - Boxplots por componente.pdf")
dev.off()


# Servicios personales
X11(20,15)
par(oma=c(1,1,1,1))
plot(sperson ~ nombre, data=ech, varwidth=TRUE, main="Servicios personales por grupo", col=sexcolor, xlab="Grupos", ylab="% de trabajadores en servicios personales")
      abline(v=3.5, col="red", lty=3)
      abline(v=6.5, col="red", lty=3)
      abline(v=9.5, col="navy", lty=6)
      abline(v=12.5, col="red", lty=3)
      abline(v=15.5, col="red", lty=3)
      abline(v=18.5, col="navy", lty=6)
      abline(v=21.5, col="red", lty=3)
      abline(v=24.5, col="red", lty=3)
      abline(v=27.5, col="navy", lty=6)
      abline(v=30.5, col="red", lty=3)
      abline(v=33.5, col="red", lty=3)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Sperson - Boxplots por nombre.pdf")
dev.off()

X11(15,15)
par(mfrow=c(4,1), mar=c(2, 4, 1, 1), oma=c(1,1,3,1))
plot(sperson ~ loc, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Localidad")
plot(sperson ~ sexo, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Sexo")
plot(sperson ~ edad, data=ech, varwidth=TRUE, horizontal=TRUE, ylab=NA, xlab="Edad")
plot(sperson ~ educ, data=ech, varwidth=TRUE, horizontal=TRUE, xlab="Educaci?n")
mtext("Servicios personales por componente", side=3, outer=TRUE, line=-0.1, font=2, cex=2)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Sperson - Boxplots por componente.pdf")
dev.off()


# Scatter plots
X11(15,15)
pairs(~ indust + comercio + sfinan + sperson, data=ech, upper.panel=panel.cor, diag.panel=panel.hist, pch=20)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Todas - Scatterplots.pdf")
dev.off()


X11(15,15)
pairs(~ indust + comercio + sfinan + sperson, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor, diag.panel=panel.hist)

dev.copy2pdf(file = "graph/Estad. Desc. - Rama establecimiento - Todas - Scatterplots por nombre.pdf")
dev.off()


##### PAIRS de todas las variables #####


X11(15,15)
pairs(~ jefe + desemp + size + tparcial + multiemp + privado + publico + cpsl + cpcl + profytec + oficina + manual + indust + comercio + sfinan + sperson + subemp + precario + ingreso, data=ech, panel=function(x,y){points(x,y, col=as.numeric(ech$nombre), pch=20, cex=1.2)},cex.labels=1.4, upper.panel=panel.cor)


dev.copy2pdf(file = "graph/Estad. Desc. - Todas - Todas - Scatterplots por nombre.pdf")
dev.off()


#$##################################################
#### Clustering por variables (dist: Euclidean) ####
####################################################


#### Matriz de distancias ####


ech3inv_euc <- dist(ech3inv)

x11(15,15)
levelplot(as.matrix(ech3inv_euc), main="Matriz de distancias euclidias",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Matriz de distancias.pdf")
dev.off()


#### Clusters ####


agnes_var_sl <- agnes(ech3inv, metric="euclidean", stand=TRUE, method="single")
agnes_var_cl <- agnes(ech3inv, metric="euclidean", stand=TRUE, method="complete")
agnes_var_al <- agnes(ech3inv, metric="euclidean", stand=TRUE, method="average")
agnes_var_wa <- agnes(ech3inv, metric="euclidean", stand=TRUE, method="ward")


#### Dendogramas con lineas de corte (visual e indicadores) ####

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_sl, which=2, nmax.lab=50, main="Single Linkage (dist: euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Single.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=8.7, col="green", lty=1)
#       abline(h=7.5, col="green", lty=1)
#       abline(h=12, col="magenta", lty=1)
#       abline(h=9.5, col="magenta", lty=1)
legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Complete.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_al, which=2, nmax.lab=50, main="Average Linkage (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=7.37, col="green", lty=1)
#       abline(h=5.31, col="green", lty=1)
#       abline(h=6.3, col="magenta", lty=1)
legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Average.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_wa, which=2, nmax.lab=50, main="Ward (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=50, col="green", lty=1)
#       abline(h=33, col="green", lty=1)
#       abline(h=40, col="magenta", lty=1)
legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Ward.pdf")
dev.off()


#$##################################################
#### Clustering por variables (dist: Manhattan) ####
####################################################


# Matriz de distancias


ech3inv_man <- dist(ech3inv, method="manhattan")

x11(15,15)
levelplot(as.matrix(ech3inv_man), main="Matriz de distancias de Manhattan",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Matriz de distancias.pdf")
dev.off()


# Clusters


agnes_var_man_sl <- agnes(ech3inv, metric="mahattan", stand=TRUE, method="single")
agnes_var_man_cl <- agnes(ech3inv, metric="mahattan", stand=TRUE, method="complete")
agnes_var_man_al <- agnes(ech3inv, metric="mahattan", stand=TRUE, method="average")
agnes_var_man_wa <- agnes(ech3inv, metric="mahattan", stand=TRUE, method="ward")


# Dendogramas con lineas de corte (visual e indicadores)

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_man_sl, which=2, main="Single Linkage (dist: Manhattan)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Single.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_man_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=8.7, col="green", lty=1)
#       abline(h=7.5, col="green", lty=1)
#       abline(h=12, col="magenta", lty=1)
#       abline(h=9.5, col="magenta", lty=1)
legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Complete.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_man_al, which=2, nmax.lab=50, main="Average Linkage (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=7.37, col="green", lty=1)
#       abline(h=5.31, col="green", lty=1)
#       abline(h=6.3, col="magenta", lty=1)
legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Average.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_var_man_wa, which=2, nmax.lab=50, main="Ward (dist: euclidean)", xlab=NA, cex.main=2)
#       abline(h=50, col="green", lty=1)
#       abline(h=33, col="green", lty=1)
#       abline(h=40, col="magenta", lty=1)
legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering variables - Todas las vars - Agnes - Euclidean - Dendograma Ward.pdf")
dev.off()


##########################################################
#### Cluster por variables sin jefe (dist: Euclidean) ####
##########################################################


################################

# Saco la variable jefe

ech3_sjefe <- select(ech3, desemp:ingreso)
ech3inv_sjefe <- t.data.frame(ech3_sjefe)

# Formaci?n de clusters 


agnes_mah_sjefe_sl <- agnes(ech3inv_sjefe, metric="eudlidean", method="single")
agnes_mah_sjefe_cl <- agnes(ech3inv_sjefe, metric="eudlidean", method="complete")
agnes_mah_sjefe_al <- agnes(ech3inv_sjefe, metric="eudlidean", method="average")
agnes_mah_sjefe_wa <- agnes(ech3inv_sjefe, metric="eudlidean", method="ward")


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sjefe_sl, which=2, nmax.lab=50, main="Single Linkage sin jefe (dist: Euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Sin jefe - Agnes - Eulidean - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sjefe_cl, which=2, nmax.lab=50, main="Complete Linkage sin jefe (dist: Euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Sin jefe - Agnes - Euclidean - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sjefe_al, which=2, nmax.lab=50, main="Average Linkage sin jefe (dist: Euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Sin jefe - Agnes - Euclidean - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sjefe_wa, which=2, nmax.lab=50, main="Ward sin jefe (dist: Euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering variables - Sin jefe - Agnes - Euclidean - Dendograma - Ward.pdf")
dev.off()


######################################
#### Clustering (dist: euclidean) ####
######################################


################################

# Matriz de distancias


ech_euc <- dist(ech2[, colnames(ech2) != "nombre"], method="euclidean")

x11(15,15)
levelplot(as.matrix(ech_euc), main="Matriz de distancias euclidias",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Matriz de distancias.pdf")
dev.off()


# Clusters


agnes_sl <- agnes(ech3, metric="euclidean", stand=TRUE, method="single")
agnes_cl <- agnes(ech3, metric="euclidean", stand=TRUE, method="complete")
agnes_al <- agnes(ech3, metric="euclidean", stand=TRUE, method="average")
agnes_wa <- agnes(ech3, metric="euclidean", stand=TRUE, method="ward")


# ?ndices (Rcuad, psF, psT)


indic_agnes_sl = indicadores(agnes_sl[4], ech3, imprime=20)
indic_agnes_cl = indicadores(agnes_cl[4], ech3, imprime=20)
indic_agnes_al = indicadores(agnes_al[4], ech3, imprime=20)
indic_agnes_wa = indicadores(agnes_wa[4], ech3, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=13, col="green", lty=2)
mtext("Single Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=10, col="green", lty=2)
abline(v=17, col="green", lty=2)
plot(rev(indic_agnes_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=10, col="green", lty=2)
abline(v=17, col="green", lty=2)
plot(rev(indic_agnes_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=10, col="green", lty=2)
abline(v=17, col="green", lty=2)
mtext("Complete Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
mtext("Average Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
mtext("Ward (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Indicadores - Ward Linkage.pdf")
dev.off()


# Agrego grupos a la base

# Se agregan a la base, columnas conteniendo la informaci?n de pertenencia a cada 
# uno de los grupos, seg?n cada uno de los criterios y ptos de corte utilizados

grupos_agnes_single_5 <- factor(cutree(agnes_sl, k=5))
grupos_agnes_single_13 <- factor(cutree(agnes_sl, k=13))


grupos_agnes_complete_10 <- factor(cutree(agnes_cl, k=10))
grupos_agnes_complete_17 <- factor(cutree(agnes_cl, k=17))
grupos_agnes_complete_3 <- factor(cutree(agnes_cl, k=3))
grupos_agnes_complete_8 <- factor(cutree(agnes_cl, k=8))


grupos_agnes_average_3 <- factor(cutree(agnes_al, k=3))
grupos_agnes_average_7 <- factor(cutree(agnes_al, k=7))
grupos_agnes_average_13 <- factor(cutree(agnes_al, k=13))


grupos_agnes_ward_3 <- factor(cutree(agnes_wa, k=3))
grupos_agnes_ward_4 <- factor(cutree(agnes_wa, k=4))
grupos_agnes_ward_5 <- factor(cutree(agnes_wa, k=5))


ech <- cbind(ech, grupos_agnes_single_5, grupos_agnes_single_13, grupos_agnes_complete_10, 
             grupos_agnes_complete_17, grupos_agnes_complete_3, grupos_agnes_complete_8, 
             grupos_agnes_average_3, grupos_agnes_average_7, grupos_agnes_average_13, 
             grupos_agnes_ward_3, grupos_agnes_ward_4, grupos_agnes_ward_5)


write.csv(ech, "tablas/ech con grupos.csv")


x11(15,15)
plot(table(ech$year, ech$grupos_agnes_ward_5), col=colores6)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Comp. grupos por a?o.pdf")
dev.off()


x11(15,15)
plot(table(ech$loc, ech$grupos_agnes_ward_5), col=colores6)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Comp. grupos por localidad.pdf")
dev.off()


x11(15,15)
plot(table(ech$educ, ech$grupos_agnes_ward_5), col=colores6)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Comp. grupos por educaci?n.pdf")
dev.off()


x11(15,15)
plot(table(ech$grupos_agnes_ward_5, ech$sexo), col=colores6)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Comp. grupos por sexo.pdf")
dev.off()


# Dendogramas con lineas de corte (visual e indicadores)

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sl, which=2, nmax.lab=50, main="Single Linkage (dist: euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Dendograma Single.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=8.7, col="green", lty=1)
abline(h=7.5, col="green", lty=1)
abline(h=12, col="magenta", lty=1)
abline(h=9.5, col="magenta", lty=1)
legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Dendograma Complete.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_al, which=2, nmax.lab=50, main="Average Linkage (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=7.37, col="green", lty=1)
abline(h=5.31, col="green", lty=1)
abline(h=6.3, col="magenta", lty=1)
legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Dendograma Average.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_wa, which=2, nmax.lab=50, main="Ward (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=50, col="green", lty=1)
abline(h=33, col="green", lty=1)
abline(h=40, col="magenta", lty=1)
legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Euclidean - Dendograma Ward.pdf")
dev.off()


# Selecci?n de m?todo


# Seleccionamos trabajar con los 10 grupos definidos por el m?todo Complete Linkage


########################################################
#### Cluster sin variable ingreso (dist: euclidean) ####
########################################################


################################

ech_sing <- select(ech3, -ingreso)


# Matriz de distancias


dist_euc_sing <- dist(ech_sing[, colnames(ech_sing) != "nombre"], method="euclidean")

x11(15,15)
levelplot(as.matrix(dist_euc_sing), main="Matriz de distancias euclidias sin variable ingreso",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Matriz de distancias.pdf")
dev.off()


# Formaci?n de Clusters


agnes_sing_sl <- agnes(ech_sing, metric="euclidean", stand=TRUE, method="single")
agnes_sing_cl <- agnes(ech_sing, metric="euclidean", stand=TRUE, method="complete")
agnes_sing_al <- agnes(ech_sing, metric="euclidean", stand=TRUE, method="average")
agnes_sing_wa <- agnes(ech_sing, metric="euclidean", stand=TRUE, method="ward")


# Indicadores


indic_agnes_sing_sl = indicadores(agnes_sing_sl[4], ech_sing, imprime=20)
indic_agnes_sing_cl = indicadores(agnes_sing_cl[4], ech_sing, imprime=20)
indic_agnes_sing_al = indicadores(agnes_sing_al[4], ech_sing, imprime=20)
indic_agnes_sing_wa = indicadores(agnes_sing_wa[4], ech_sing, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_sing_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=7, col="green", lty=2)
      abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_sing_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=7, col="green", lty=2)
      abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_sing_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=7, col="green", lty=2)
      abline(v=16, col="green", lty=2)
mtext("Single Linkage sin ingreso\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_sing_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=4, col="green", lty=2)
      abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_sing_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=4, col="green", lty=2)
      abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_sing_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=4, col="green", lty=2)
      abline(v=11, col="green", lty=2)
mtext("Complete Linkage sin ingreso\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_sing_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_sing_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_sing_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=13, col="green", lty=2)
mtext("Average Linkage sin ingreso\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_sing_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_sing_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_sing_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
mtext("Ward sin ingreso\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sing_sl, nmax.lab=50, which=2, main="Single Linkage sin ingreso (dist: euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sing_cl, nmax.lab=50, which=2, main="Complete Linkage sin ingreso (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=11.3, col="green", lty=1)
abline(h=8.455, col="green", lty=1)
abline(h=11.5, col="magenta", lty=1)
legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sing_al, nmax.lab=50, which=2, main="Average Linkage sin ingreso (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=7.28, col="green", lty=1)
abline(h=5.20, col="green", lty=1)
abline(h=6.3, col="magenta", lty=1)
legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sing_wa, nmax.lab=50, which=2, main="Ward sin ingreso (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=50, col="green", lty=1)
abline(h=32.5, col="green", lty=1)
abline(h=24, col="green", lty=1)
abline(h=40, col="magenta", lty=1)
legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Euclidean - Indicadores - Ward.pdf")
dev.off()


# Decisi?n: la selecci?n sigue siendo Complete 10


# Agrego grupos a la base


grupos_agnes_sing_cl_10 <- factor(cutree(agnes_cl, k=10))


ech <- cbind(ech, grupos_agnes_sing_cl_10)


write.csv(ech, "tablas/ech con grupos.csv")


############################################
#### Cluster con distancia de Manhattan ####
############################################


################################

# Matriz de distancias


dist_man <- dist(ech2[, colnames(ech2) != "nombre"], method="manhattan")

x11(15,15)
levelplot(as.matrix(dist_man), main="Matriz de distancias de Manhattan",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Matriz de distancias.pdf")
dev.off()


# Clusters


agnes_msl <- agnes(ech3, metric="manhattan", stand=TRUE, method="single")
agnes_mcl <- agnes(ech3, metric="manhattan", stand=TRUE, method="complete")
agnes_mal <- agnes(ech3, metric="manhattan", stand=TRUE, method="average")
agnes_mwa <- agnes(ech3, metric="manhattan", stand=TRUE, method="ward")


# Indicadores


indic_agnes_msl = indicadores(agnes_msl[4], ech3, imprime=20)
indic_agnes_mcl = indicadores(agnes_mcl[4], ech3, imprime=20)
indic_agnes_mal = indicadores(agnes_mal[4], ech3, imprime=20)
indic_agnes_mwa = indicadores(agnes_mwa[4], ech3, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_msl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_msl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_msl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=8, col="green", lty=2)
mtext("Single Linkage (dist: Manhattan)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mcl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mcl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mcl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=9, col="green", lty=2)
abline(v=11, col="green", lty=2)
mtext("Complete Linkage (dist: Manhattan)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mal$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
plot(rev(indic_agnes_mal$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
plot(rev(indic_agnes_mal$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
mtext("Average Linkage (dist: Manhattan)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mwa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
plot(rev(indic_agnes_mwa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
plot(rev(indic_agnes_mwa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
mtext("Ward (dist: Manhattan)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_msl, which=2, nmax.lab=50, main="Single Linkage (dist: Manhattan)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mcl, which=2, nmax.lab=50, main="Complete Linkage (dist: Manhattan)", xlab=NA, cex.main=2)
abline(h=40, col="green", lty=1)    # 3 grupos
abline(h=35.5, col="green", lty=1)  # 5 grupos
abline(h=30.5, col="green", lty=1)  # 9 grupos 
abline(h=29, col="green", lty=1)    # 11 grupos 
abline(h=41, col="magenta", lty=1)  # 3 grupos
legend(x=119, y=-15, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mal, which=2, nmax.lab=50, main="Average Linkage (dist: Manhattan)", xlab=NA, cex.main=2)
abline(h=30, col="green", lty=1)    # 2 grupos
abline(h=22.5, col="green", lty=1)  # 5 grupos
abline(h=24.5, col="magenta", lty=1)  # 4 gruposs      
legend(x=119, y=-7, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mwa, which=2, nmax.lab=50, main="Ward (dist: Manhattan)", xlab=NA, cex.main=2)
abline(h=180, col="green", lty=1)   # 3 grupos
abline(h=140, col="magenta", lty=1)  # 
legend(x=119, y=-80, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Manhattan - Dendograma - Ward.pdf")
dev.off()


# Decisi?n: complete 9, average 10


# Agrego grupos a la base


grupos_agnes_mcl_9 <- factor(cutree(agnes_mcl, k=9))
grupos_agnes_mal_10 <- factor(cutree(agnes_mal, k=10))


ech <- cbind(ech, grupos_agnes_mcl_9, grupos_agnes_mal_10)


write.csv(ech, "tablas/ech con grupos.csv")


##############################################
#### Cluster con distancia de Mahalanobis ####
##############################################


################################

# Matriz de distancias

dist_mah <- mahalanobis.dist(ech3)

x11(15,15)
levelplot(dist_mah, main="Matriz de distancias de Mahalanobis",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Matriz de distancias.pdf")
dev.off()


# Formaci?n de clusters 


agnes_mah_sl <- agnes(dist_mah, diss=TRUE, method="single")
agnes_mah_cl <- agnes(dist_mah, diss=TRUE, method="complete")
agnes_mah_al <- agnes(dist_mah, diss=TRUE, method="average")
agnes_mah_wa <- agnes(dist_mah, diss=TRUE, method="ward")


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sl, which=2, nmax.lab=50, main="Single Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_al, which=2, nmax.lab=50, main="Average Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_wa, which=2, nmax.lab=50, main="Ward (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Dendograma - Ward.pdf")
dev.off()


# Indicadores

indic_agnes_mah_sl = indicadores(agnes_mah_sl[4], ech3, imprime=20)
indic_agnes_mah_cl = indicadores(agnes_mah_cl[4], ech3, imprime=20)
indic_agnes_mah_al = indicadores(agnes_mah_al[4], ech3, imprime=20)
indic_agnes_mah_wa = indicadores(agnes_mah_wa[4], ech3, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mah_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=16, col="green", lty=2)
mtext("Single Linkage (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mah_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
abline(v=14, col="green", lty=2)
abline(v=19, col="green", lty=2)
plot(rev(indic_agnes_mah_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
abline(v=14, col="green", lty=2)
abline(v=19, col="green", lty=2)
plot(rev(indic_agnes_mah_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
abline(v=14, col="green", lty=2)
abline(v=19, col="green", lty=2)
mtext("Complete Linkage (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mah_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
plot(rev(indic_agnes_mah_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
plot(rev(indic_agnes_mah_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
mtext("Average Linkage (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_mah_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_mah_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_mah_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=5, col="green", lty=2)
mtext("Ward (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis - Indicadores - Ward.pdf")
dev.off()


# Decisi?n:


#########################################################
#### Cluster con distancia de Mahalanobis (datos st) ####
#########################################################


################################

# Formaci?n de clusters con datos estandarizados


agnes_mahst_sl <- agnes(dist_mah, diss=TRUE, stand=TRUE, method="single")
agnes_mahst_cl <- agnes(dist_mah, diss=TRUE, stand=TRUE, method="complete")
agnes_mahst_al <- agnes(dist_mah, diss=TRUE, stand=TRUE, method="average")
agnes_mahst_wa <- agnes(dist_mah, diss=TRUE, stand=TRUE, method="ward")


# Dendogramas con datos estandarizados


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mahst_sl, which=2, nmax.lab=50, main="Single Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mahst_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mahst_al, which=2, nmax.lab=50, main="Average Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mahst_wa, which=2, nmax.lab=50, main="Ward (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Dendograma - Ward.pdf")
dev.off()


# Indicadores


indic_agnes_mahst_sl = indicadores(agnes_mahst_sl[4], ech3, imprime=20)
indic_agnes_mahst_cl = indicadores(agnes_mahst_cl[4], ech3, imprime=20)
indic_agnes_mahst_al = indicadores(agnes_mahst_al[4], ech3, imprime=20)
indic_agnes_mahst_wa = indicadores(agnes_mahst_wa[4], ech3, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,8,1))
plot(rev(indic_agnes_mahst_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_mahst_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_mahst_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=4, col="green", lty=2)
abline(v=8, col="green", lty=2)
mtext("Single Linkage con datos estandarizados\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,8,1))
plot(rev(indic_agnes_mahst_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mahst_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mahst_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=8, col="green", lty=2)
abline(v=11, col="green", lty=2)
mtext("Complete Linkage con datos\nestandarizados (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,8,1))
plot(rev(indic_agnes_mahst_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
abline(v=15, col="green", lty=2)
plot(rev(indic_agnes_mahst_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
abline(v=15, col="green", lty=2)
plot(rev(indic_agnes_mahst_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=5, col="green", lty=2)
abline(v=10, col="green", lty=2)
abline(v=15, col="green", lty=2)
mtext("Average Linkage con datos\nestandarizados (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,8,1))
plot(rev(indic_agnes_mahst_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=3, col="green", lty=2)
plot(rev(indic_agnes_mahst_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=3, col="green", lty=2)
plot(rev(indic_agnes_mahst_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=3, col="green", lty=2)
mtext("Ward con datos estandarizados\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Todas las vars - Agnes - Mahalanobis (st) - Indicadores - Ward.pdf")
dev.off()


# Decisi?n: ?no existe estructura de grupos?


#################################################
#### Cluster sin ingreso (dist: Mahalanobis) ####
#################################################


################################


ech_sing <- select(ech3, -ingreso)


# Matriz de distancias

dist_mah_sing <- mahalanobis.dist(ech_sing)

x11(15,15)
levelplot(dist_mah_sing, main="Matriz de distancias de Mahalanobis (sin ingreso)",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Matriz de distancia.pdf")
dev.off()


# Formaci?n de Clusters


agnes_mah_sing_sl <- agnes(dist_mah_sing, diss=TRUE, method="single")
agnes_mah_sing_cl <- agnes(dist_mah_sing, diss=TRUE, method="complete")
agnes_mah_sing_al <- agnes(dist_mah_sing, diss=TRUE, method="average")
agnes_mah_sing_wa <- agnes(dist_mah_sing, diss=TRUE, method="ward")


# Indicadores

indic_agnes_mah_sing_sl = indicadores(agnes_mah_sing_sl[4], ech_sing, imprime=20)
indic_agnes_mah_sing_cl = indicadores(agnes_mah_sing_cl[4], ech_sing, imprime=20)
indic_agnes_mah_sing_al = indicadores(agnes_mah_sing_al[4], ech_sing, imprime=20)
indic_agnes_mah_sing_wa = indicadores(agnes_mah_sing_wa[4], ech_sing, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
mtext("Single Linkage sin ingreso\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
mtext("Complete Linkage sin ingreso\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=3, col="green", lty=2)
#abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=3, col="green", lty=2)
#abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=3, col="green", lty=2)
#abline(v=13, col="green", lty=2)
mtext("Average Linkage sin ingreso\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_mah_sing_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
mtext("Ward sin ingreso\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sing_sl, which=2, nmax.lab=50, main="Single Linkage sin ingreso (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sing_cl, which=2, nmax.lab=50, main="Complete Linkage sin ingreso (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=11.3, col="green", lty=1)
#       abline(h=8.455, col="green", lty=1)
#       abline(h=11.5, col="magenta", lty=1)
# legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sing_al, which=2, nmax.lab=50, main="Average Linkage sin ingreso (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=7.28, col="green", lty=1)
#       abline(h=5.20, col="green", lty=1)
#       abline(h=6.3, col="magenta", lty=1)
# legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sing_wa, which=2, nmax.lab=50, main="Ward sin ingreso (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=50, col="green", lty=1)
#       abline(h=32.5, col="green", lty=1)
#       abline(h=24, col="green", lty=1)
#       abline(h=40, col="magenta", lty=1)
# legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin ingreso - Agnes - Mahalanobis - Dendograma - Ward.pdf")
dev.off()


# Elecci?n: 


##################################################
#### Cluster sin outliers (dist: Mahalanobis) ####
##################################################


################################


# Se identificaron como outliers a las observaciones: 277, 249, 208, 187, 174, 157, 143, 135, 89, 88, 40, 17, 9

ech[277, c(3,8)]; ech[208, c(3,8)]; ech[174, c(3,8)]; ech[40, c(3,8)]; ech[187, c(3,8)]
ech[9, c(3,8)]; ech[157, c(3,8)]; ech[143, c(3,8)]; ech[17, c(3,8)]; ech[88, c(3,8)]
ech[135, c(3,8)]; ech[249, c(3,8)]; ech[89, c(3,8)]


# Se retiran de la base:

ech_sout <- ech3[-c(277, 249, 208, 187, 174, 157, 143, 135, 89, 88, 40, 17, 9), ]


# Matriz de distancias

dist_mah_sout <- mahalanobis.dist(ech_sout)

x11(15,15)
levelplot(dist_mah_sout, main="Matriz de distancias de Mahalanobis (sin outliers)",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Matriz de distancias.pdf")
dev.off()


# Formaci?n de Clusters


agnes_mah_sout_sl <- agnes(dist_mah_sout, diss=TRUE, method="single")
agnes_mah_sout_cl <- agnes(dist_mah_sout, diss=TRUE, method="complete")
agnes_mah_sout_al <- agnes(dist_mah_sout, diss=TRUE, method="average")
agnes_mah_sout_wa <- agnes(dist_mah_sout, diss=TRUE, method="ward")


# Indicadores

indic_agnes_mah_sout_sl = indicadores(agnes_mah_sout_sl[4], ech_sout, imprime=20)
indic_agnes_mah_sout_cl = indicadores(agnes_mah_sout_cl[4], ech_sout, imprime=20)
indic_agnes_mah_sout_al = indicadores(agnes_mah_sout_al[4], ech_sout, imprime=20)
indic_agnes_mah_sout_wa = indicadores(agnes_mah_sout_wa[4], ech_sout, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sout_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
mtext("Single Linkage sin outliers\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sout_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
mtext("Complete Linkage sin outliers\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_sout_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
mtext("Average Linkage sin outliers\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sing_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_sing_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
plot(rev(indic_agnes_sing_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=5, col="green", lty=2)
#       abline(v=9, col="green", lty=2)
mtext("Ward sin outliers\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sout_sl, which=2, nmax.lab=50, main="Single Linkage sin outliers (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sout_cl, which=2, main="Complete Linkage sin outliers (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=11.3, col="green", lty=1)
#       abline(h=8.455, col="green", lty=1)
#       abline(h=11.5, col="magenta", lty=1)
# legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sout_al, which=2, nmax.lab=50, main="Average Linkage sin outliers (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=7.28, col="green", lty=1)
#       abline(h=5.20, col="green", lty=1)
#       abline(h=6.3, col="magenta", lty=1)
# legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_sout_wa, which=2, nmax.lab=50, main="Ward sin outliers (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=50, col="green", lty=1)
#       abline(h=32.5, col="green", lty=1)
#       abline(h=24, col="green", lty=1)
#       abline(h=40, col="magenta", lty=1)
# legend(x=119, y=-20, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)


dev.copy2pdf(file = "graph/Clustering - Sin outliers - Agnes - Mahalanobis - Dendograma - Ward.pdf")
dev.off()


# Decisi?n: 


########################################
#### Cluster redu (dist: euclidean) ####
########################################


################################

ech_redu <- select(ech3, desemp, tparcial, multiemp, subemp, precario)


# Matriz de distancias

dist_euc_redu <- dist(ech_redu[, colnames(ech_redu) != "nombre"], method="euclidean")

x11(15,15)
levelplot(as.matrix(dist_euc_redu), main="Matriz de distancias euclidias",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Matriz de distancias.pdf")
dev.off()


# Formaci?n de Clusters


agnes_redu_sl <- agnes(ech_redu, metric="euclidean", stand=TRUE, method="single")
agnes_redu_cl <- agnes(ech_redu, metric="euclidean", stand=TRUE, method="complete")
agnes_redu_al <- agnes(ech_redu, metric="euclidean", stand=TRUE, method="average")
agnes_redu_wa <- agnes(ech_redu, metric="euclidean", stand=TRUE, method="ward")


# Indicadores

indic_agnes_redu_sl = indicadores(agnes_redu_sl[4], ech_redu, imprime=20)
indic_agnes_redu_cl = indicadores(agnes_redu_cl[4], ech_redu, imprime=20)
indic_agnes_redu_al = indicadores(agnes_redu_al[4], ech_redu, imprime=20)
indic_agnes_redu_wa = indicadores(agnes_redu_wa[4], ech_redu, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_redu_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_redu_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_redu_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=5, col="green", lty=2)
mtext("Single Linkage\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_redu_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_redu_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
plot(rev(indic_agnes_redu_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=2, col="green", lty=2)
abline(v=5, col="green", lty=2)
mtext("Complete Linkage\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Indicadores - Single Complete.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_redu_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_redu_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=8, col="green", lty=2)
plot(rev(indic_agnes_redu_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=8, col="green", lty=2)
mtext("Average Linkage\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_redu_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=2, col="green", lty=2)
plot(rev(indic_agnes_redu_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=2, col="green", lty=2)
plot(rev(indic_agnes_redu_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=2, col="green", lty=2)
mtext("Ward\n(dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_redu_sl, which=2, nmax.lab=50, main="Single Linkage (dist: euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_redu_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=10, col="green", lty=1)
abline(h=6.5, col="green", lty=1)
abline(h=5.8, col="magenta", lty=1)
legend(x=119, y=-3.5, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_redu_al, which=2, nmax.lab=50, main="Average Linkage (dist: euclidean)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_redu_wa, which=2, nmax.lab=50, main="Ward (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=40, col="green", lty=1)
abline(h=27, col="magenta", lty=1)
abline(h=20, col="magenta", lty=1)
legend(x=119, y=-14, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Dendograma - Ward.pdf")
dev.off()


# Decisi?n: Complete 5 o Complete 6


# Agrego grupos a la base

grupos_agnes_redu_cl_5 <- factor(cutree(agnes_redu_cl, k=5))
table(grupos_agnes_redu_cl_5)


grupos_agnes_redu_cl_6 <- factor(cutree(agnes_redu_cl, k=6))
table(grupos_agnes_redu_cl_6)


ech <- cbind(ech, grupos_agnes_redu_cl_5, grupos_agnes_redu_cl_6)
write.csv(ech, "tablas/ech con grupos.csv")


# Composici?n de los grupos

colores6=c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f")


table(ech$nombre, grupos_agnes_redu_cl_5)

X11(15,15)
plot(table(ech$nombre, grupos_agnes_redu_cl_5), main="Grupos Complete 5 por nombre", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por nombre - Complete 5.pdf")
dev.off()


X11(15,15)
plot(table(ech$loc , grupos_agnes_redu_cl_5), main="Grupos Complete 5 por localidad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por localidad - Complete 5.pdf")
dev.off()


X11(15,15)
plot(table(ech$sexo , grupos_agnes_redu_cl_5), main="Grupos Complete 5 por sexo", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por sexo - Complete 5.pdf")
dev.off()


X11(15,15)
plot(table(ech$edad , grupos_agnes_redu_cl_5), main="Grupos Complete 5 por edad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por edad - Complete 5.pdf")
dev.off()


X11(15,15)
plot(table(ech$educ , grupos_agnes_redu_cl_5), main="Grupos Complete 5 por educaci?n", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por educaci?n - Complete 5.pdf")
dev.off()


table(ech$nombre, grupos_agnes_redu_cl_6)

X11(15,15)
plot(table(ech$nombre, grupos_agnes_redu_cl_6), main="Grupos Complete 6 por nombre", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por nombre - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$loc , grupos_agnes_redu_cl_6), main="Grupos Complete 6 por localidad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por localidad - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$sexo , grupos_agnes_redu_cl_6), main="Grupos Complete 6 por sexo", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por sexo - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$edad , grupos_agnes_redu_cl_6), main="Grupos Complete 6 por edad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por edad - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$educ , grupos_agnes_redu_cl_6), main="Grupos Complete 6 por educaci?n", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Euclidean - Comp. grupos por educaci?n - Complete 6.pdf")
dev.off()


##########################################
#### Cluster redu (dist: Mahalanobis) ####
##########################################


################################

ech_redu <- select(ech3, desemp, tparcial, multiemp, subemp, precario)


# Matriz de distancias


dist_mah <- mahalanobis.dist(ech_redu)

x11(15,15)
levelplot(dist_mah, main="Matriz de distancias de Mahalanobis",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Matriz de distancias.pdf")
dev.off()


# Formaci?n de Clusters


agnes_mah_redu_sl <- agnes(dist_mah, diss=TRUE, method="single")
agnes_mah_redu_cl <- agnes(dist_mah, diss=TRUE, method="complete")
agnes_mah_redu_al <- agnes(dist_mah, diss=TRUE, method="average")
agnes_mah_redu_wa <- agnes(dist_mah, diss=TRUE, method="ward")


# Indicadores

indic_agnes_mah_sl = indicadores(agnes_mah_redu_sl[4], ech_redu, imprime=20)
indic_agnes_mah_cl = indicadores(agnes_mah_redu_cl[4], ech_redu, imprime=20)
indic_agnes_mah_al = indicadores(agnes_mah_redu_al[4], ech_redu, imprime=20)
indic_agnes_mah_wa = indicadores(agnes_mah_redu_wa[4], ech_redu, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
plot(rev(indic_agnes_mah_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=7, col="green", lty=2)
#abline(v=16, col="green", lty=2)
mtext("Single Linkage\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Indicadores - Single Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
plot(rev(indic_agnes_mah_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#abline(v=4, col="green", lty=2)
#abline(v=11, col="green", lty=2)
mtext("Complete Linkage\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Indicadores - Complete Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
plot(rev(indic_agnes_mah_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
#       abline(v=3, col="green", lty=2)
#       abline(v=13, col="green", lty=2)
mtext("Average Linkage\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Indicadores - Average Linkage.pdf")
dev.off()


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,7,1))
plot(rev(indic_agnes_mah_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=6, col="green", lty=2)
plot(rev(indic_agnes_mah_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=6, col="green", lty=2)
plot(rev(indic_agnes_mah_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=4, col="green", lty=2)
abline(v=6, col="green", lty=2)
mtext("Ward\n(dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Indicadores - Ward.pdf")
dev.off()


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_redu_sl, which=2, nmax.lab=50, main="Single Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Dendograma - Single Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_redu_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=11.3, col="green", lty=1)
#       abline(h=8.455, col="green", lty=1)
#       abline(h=11.5, col="magenta", lty=1)
# legend(x=119, y=-4, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Dendograma - Complete Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_redu_al, which=2, nmax.lab=50, main="Average Linkage (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=7.28, col="green", lty=1)
#       abline(h=5.20, col="green", lty=1)
#       abline(h=6.3, col="magenta", lty=1)
# legend(x=119, y=-2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Dendograma - Average Linkage.pdf")
dev.off()


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_redu_wa, which=2, nmax.lab=50, main="Ward (dist: Mahalanobis)", xlab=NA, cex.main=2)
abline(h=19, col="green", lty=1)
abline(h=15, col="green", lty=1)
abline(h=14, col="magenta", lty=1)
legend(x=119, y=-5, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Dendograma - Ward.pdf")
dev.off()


# Decisi?n: Complete 6, Complete 12, Ward 6


# Agrego grupos a la base

grupos_agnes_mah_redu_cl_6 <- factor(cutree(agnes_mah_redu_cl, k=6))
table(grupos_agnes_mah_redu_cl_6)

grupos_agnes_mah_redu_cl_12 <- factor(cutree(agnes_mah_redu_cl, k=12))
table(grupos_agnes_mah_redu_cl_12)

grupos_agnes_mah_redu_wa_6 <- factor(cutree(agnes_mah_redu_wa, k=6))
table(grupos_agnes_mah_redu_wa_6)


ech <- cbind(ech, grupos_agnes_mah_redu_cl_6, grupos_agnes_mah_redu_cl_12, grupos_agnes_mah_redu_wa_6)
write.csv(ech, "tablas/ech con grupos.csv")


# Composici?n de los grupos


# Complete 6
table(ech$nombre, grupos_agnes_mah_redu_cl_6)

X11(30,15)
plot(table(ech$nombre , grupos_agnes_mah_redu_cl_6), main="Grupos Complete 6 por nombre", ylab=NA, col=colores6)

dev.copy2pdf(height=8.27, width=11.7, file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por nombre - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$loc , grupos_agnes_mah_redu_cl_6), main="Grupos Complete 6 por localidad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por localidad - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$sexo , grupos_agnes_mah_redu_cl_6), main="Grupos Complete 6 por sexo", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por sexo - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$edad , grupos_agnes_mah_redu_cl_6), main="Grupos Complete 6 por edad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por edad - Complete 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$educ , grupos_agnes_mah_redu_cl_6), main="Grupos Complete 6 por educaci?n", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por educaci?n - Complete 6.pdf")
dev.off()


# Complete 12
table(ech$nombre, grupos_agnes_mah_redu_cl_12)

X11(15,15)
plot(table(ech$nombre, grupos_agnes_mah_redu_cl_12), main="Grupos Complete 12 por nombre", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por nombre - Complete 12.pdf")
dev.off()


X11(15,15)
plot(table(ech$loc , grupos_agnes_mah_redu_cl_12), main="Grupos Complete 12 por localidad", ylab=NA,col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por localidad - Complete 12.pdf")
dev.off()


X11(15,15)
plot(table(ech$sexo , grupos_agnes_mah_redu_cl_12), main="Grupos Complete 12 por sexo", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por sexo - Complete 12.pdf")
dev.off()


X11(15,15)
plot(table(ech$edad , grupos_agnes_mah_redu_cl_12), main="Grupos Complete 12 por edad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por edad - Complete 12.pdf")
dev.off()


X11(15,15)
plot(table(ech$educ , grupos_agnes_mah_redu_cl_12), main="Grupos Complete 12 por educaci?n", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por educaci?n - Complete 12.pdf")
dev.off()


# Ward 6

table(ech$nombre, grupos_agnes_mah_redu_wa_6)
table(grupos_agnes_mah_redu_wa_6)

X11(15,15)
plot(table(ech$nombre, grupos_agnes_mah_redu_wa_6), main="Composici?n de los grupos", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por nombre - Ward 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$loc, grupos_agnes_mah_redu_wa_6), main="Grupos Ward 6 por localidad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por localidad - Ward 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$sexo, grupos_agnes_mah_redu_wa_6), main="Grupos Ward 6 por sexo", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por sexo - Ward 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$edad, grupos_agnes_mah_redu_wa_6), main="Grupos Ward 6 por edad", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por edad - Ward 6.pdf")
dev.off()


X11(15,15)
plot(table(ech$educ, grupos_agnes_mah_redu_wa_6), main="Grupos Ward 6 por educaci?n", ylab=NA, col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Comp. grupos por educaci?n - Ward 6.pdf")
dev.off()


# Decisi?n: Ward 6


################
#### Ward 6 ####
################

################################

# Dendograma con grupos


x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_mah_redu_wa, which=2, main="Dendograma Ward (dist: Mahalanobis)", xlab=NA, cex.main=2)
abline(h=15, col="green", lty=1)
rect.hclust(agnes_mah_redu_wa, k=6, border="red")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Dendograma con grupos.pdf")
dev.off()


# Estad?sticas descriptivas por grupo


# Grupo 1
filter(ech, grupos_agnes_mah_redu_wa_6 == 1) %>% select(-1,-2, -(28:46)) %>% lapply(summary)

# Grupo 2
filter(ech, grupos_agnes_mah_redu_wa_6 == 2) %>% select(-1,-2, -(28:46)) %>% lapply(summary)

# Grupo 3
filter(ech, grupos_agnes_mah_redu_wa_6 == 3) %>% select(-1,-2, -(28:46)) %>% lapply(summary)

# Grupo 4
filter(ech, grupos_agnes_mah_redu_wa_6 == 4) %>% select(-1,-2, -(28:46)) %>% lapply(summary)

# Grupo 5
filter(ech, grupos_agnes_mah_redu_wa_6 == 5) %>% select(-1,-2, -(28:46)) %>% lapply(summary)

# Grupo 6
filter(ech, grupos_agnes_mah_redu_wa_6 == 6) %>% select(-1,-2, -(28:46)) %>% lapply(summary)


# Boxplots por grupos


k = 6
cl = cutree(agnes_mah_redu_wa[4],k)
cl = factor(cl)
gru = cbind(ech,cl)


# Tabla de contingencia
table(gru$nombre, gru$cl)


X11(15,15)
boxplot(gru$desemp ~ gru$cl, col=colores6, main="Desempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot Desempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(gru$tparcial ~ gru$cl, col=colores6, main="Trabajo a tiempo parcial por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot tparcial por grupos.pdf")
dev.off()


X11(15,15)
boxplot(gru$multiemp ~ gru$cl, col=colores6, main="Multiempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot Multiempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(gru$subemp ~ gru$cl, col=colores6, main="Subempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot Subempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(gru$precario ~ gru$cl, col=colores6, main="Precariedad por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot Precariedad por grupos.pdf")
dev.off()


X11(15,15)
boxplot(gru$ingreso ~ gru$cl, col=colores6, main="Ingreso por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Boxplot Ingreso por grupos.pdf")
dev.off()


# Medias y Desvios por grupos


stats <- select(ech, -(1:2), grupos_agnes_mah_redu_wa_6)

medias <- group_by(stats, grupos_agnes_mah_redu_wa_6) %>% summarize(jefe=mean(jefe), 
                                                                    desemp=mean(desemp), 
                                                                    size=mean(size), 
                                                                    tparcial=mean(tparcial), 
                                                                    multiemp=mean(multiemp), 
                                                                    privado=mean(privado), 
                                                                    publico=mean(publico), 
                                                                    cpsl=mean(cpsl), cpcl=mean(cpcl), 
                                                                    profytec=mean(profytec), 
                                                                    oficina=mean(oficina), 
                                                                    manual=mean(manual), 
                                                                    indust=mean(indust), 
                                                                    comercio=mean(comercio), 
                                                                    sfinan=mean(sfinan), 
                                                                    sperson=mean(sperson), 
                                                                    subemp=mean(subemp), 
                                                                    precario=mean(precario), 
                                                                    ingreso=mean(ingreso))

write.csv(medias, "tablas/medias.csv")

desvios <- group_by(stats, grupos_agnes_mah_redu_wa_6) %>% summarize(jefe=sd(jefe), 
                                                                     desemp=sd(desemp), 
                                                                     size=sd(size), 
                                                                     tparcial=sd(tparcial), 
                                                                     multiemp=sd(multiemp), 
                                                                     privado=sd(privado), 
                                                                     publico=sd(publico), 
                                                                     cpsl=sd(cpsl), cpcl=sd(cpcl), 
                                                                     profytec=sd(profytec), 
                                                                     oficina=sd(oficina), 
                                                                     manual=sd(manual), 
                                                                     indust=sd(indust), 
                                                                     comercio=sd(comercio), 
                                                                     sfinan=sd(sfinan), 
                                                                     sperson=sd(sperson), 
                                                                     subemp=sd(subemp), 
                                                                     precario=sd(precario), 
                                                                     ingreso=sd(ingreso))


write.csv(desvios, "tablas/desvios.csv")


# Seguimiento de las unidades en el tiempo


ech$grupos_agnes <- as.numeric(grupos_agnes_mah_redu_wa_6)


ranking <- NULL

for(i in 1:360){
      ranking[i] <- if(ech[i,47]==1){
            3
      } else if(ech[i,47]==2){
            1
      } else if(ech[i,47]==3){
            2
      } else if(ech[i,47]==4){
            4
      } else if(ech[i,47]==5){
            5
      } else if(ech[i,47]==6){
            6
      }
      
}


ech <- cbind(ech, ranking)

X11(15,15)
par(mfrow=c(3,3), mar=c(2,4,1,1), oma=c(1,1,7,1))
plot(x=ech$year[ech$nombre== "IH11"], y=ech$ranking[ech$nombre== "IH11"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH11", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH12"], y=ech$ranking[ech$nombre== "IH12"], pch=21, bg="red" ,xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH12", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH13"], y=ech$ranking[ech$nombre== "IH13"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH13", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "IH21"], y=ech$ranking[ech$nombre== "IH21"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH21", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH22"], y=ech$ranking[ech$nombre== "IH22"], pch=21, bg="red", xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH22", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH23"], y=ech$ranking[ech$nombre== "IH23"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH23", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "IH31"], y=ech$ranking[ech$nombre== "IH31"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH31", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH32"], y=ech$ranking[ech$nombre== "IH32"], pch=21, bg="red" ,xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH32", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IH33"], y=ech$ranking[ech$nombre== "IH33"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IH33", col="green", lty=1, horiz=TRUE, bty="n")

mtext("Seguimiento de las unidades en el tiempo\nInterior-Hombres", side=3, line=1, outer=TRUE, cex=1.5, font=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Seg. Tiempo - Interior Hombres.pdf")
dev.off()


X11(15,15)
par(mfrow=c(3,3), mar=c(2,4,1,1), oma=c(1,1,7,1))
plot(x=ech$year[ech$nombre== "IM11"], y=ech$ranking[ech$nombre== "IM11"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM11", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM12"], y=ech$ranking[ech$nombre== "IM12"], pch=21, bg="red" ,xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM12", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM13"], y=ech$ranking[ech$nombre== "IM13"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM13", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "IM21"], y=ech$ranking[ech$nombre== "IM21"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM21", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM22"], y=ech$ranking[ech$nombre== "IM22"], pch=21, bg="red", xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM22", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM23"], y=ech$ranking[ech$nombre== "IM23"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM23", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "IM31"], y=ech$ranking[ech$nombre== "IM31"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM31", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM32"], y=ech$ranking[ech$nombre== "IM32"], pch=21, bg="red" ,xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM32", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "IM33"], y=ech$ranking[ech$nombre== "IM33"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="IM33", col="green", lty=1, horiz=TRUE, bty="n")

mtext("Seguimiento de las unidades en el tiempo\nInterior-Mujeres", side=3, line=1, outer=TRUE, cex=1.5, font=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Seg. Tiempo - Interior Mujeres.pdf")
dev.off()


X11(15,15)
par(mfrow=c(3,3), mar=c(2,4,1,1), oma=c(1,1,7,1))
plot(x=ech$year[ech$nombre== "MH11"], y=ech$ranking[ech$nombre== "MH11"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH11", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH12"], y=ech$ranking[ech$nombre== "MH12"], pch=21, bg="red" ,xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH12", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH13"], y=ech$ranking[ech$nombre== "MH13"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH13", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "MH21"], y=ech$ranking[ech$nombre== "MH21"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH21", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH22"], y=ech$ranking[ech$nombre== "MH22"], pch=21, bg="red", xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH22", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH23"], y=ech$ranking[ech$nombre== "MH23"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH23", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "MH31"], y=ech$ranking[ech$nombre== "MH31"], pch=21, bg="blue", xlab="A?o", ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH31", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH32"], y=ech$ranking[ech$nombre== "MH32"], pch=21, bg="red" ,xlab="A?o", ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH32", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MH33"], y=ech$ranking[ech$nombre== "MH33"], pch=21, bg="green", xlab="A?o", ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MH33", col="green", lty=1, horiz=TRUE, bty="n")

mtext("Seguimiento de las unidades en el tiempo\nMontevideo-Hombres", side=3, line=1, outer=TRUE, cex=1.5, font=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Seg. Tiempo - Montevideo Hombres.pdf")
dev.off()


X11(15,15)
par(mfrow=c(3,3), mar=c(2,4,1,1), oma=c(1,1,7,1))
plot(x=ech$year[ech$nombre== "MM11"], y=ech$ranking[ech$nombre== "MM11"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM11", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM12"], y=ech$ranking[ech$nombre== "MM12"], pch=21, bg="red" ,xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM12", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM13"], y=ech$ranking[ech$nombre== "MM13"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM13", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "MM21"], y=ech$ranking[ech$nombre== "MM21"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM21", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM22"], y=ech$ranking[ech$nombre== "MM22"], pch=21, bg="red", xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM22", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM23"], y=ech$ranking[ech$nombre== "MM23"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM23", col="green", lty=1, horiz=TRUE, bty="n")

plot(x=ech$year[ech$nombre== "MM31"], y=ech$ranking[ech$nombre== "MM31"], pch=21, bg="blue", xlab=NA, ylab="Ranking", type="b", col="blue", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM31", col="blue", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM32"], y=ech$ranking[ech$nombre== "MM32"], pch=21, bg="red" ,xlab=NA, ylab=NA, type="b", col="red", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM32", col="red", lty=1, horiz=TRUE, bty="n")
plot(x=ech$year[ech$nombre== "MM33"], y=ech$ranking[ech$nombre== "MM33"], pch=21, bg="green", xlab=NA, ylab=NA, type="b", col="green", ylim=c(0,6), font.lab=2)
legend("bottomright", legend="MM33", col="green", lty=1, horiz=TRUE, bty="n")

mtext("Seguimiento de las unidades en el tiempo\nMontevideo-Mujeres", side=3, line=1, outer=TRUE, cex=1.5, font=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Agnes - Mahalanobis - Ward 6 - Seg. Tiempo - Montevideo Mujeres.pdf")
dev.off()


####################################################
#### Cluster redu divisivos (dist: Mahalanobis) ####
####################################################


################################

# Matriz de distancias


dist_mah <- mahalanobis.dist(ech_redu)

x11(15,15)
levelplot(dist_mah, main="Matriz de distancias de Mahalanobis",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Diana - Mahalanobis - Matriz de distancias.pdf")
dev.off()


# Formaci?n de clusters 


diana_mah <- diana(dist_mah, diss=TRUE)


# Dendogramas


x11(20,15)
par(oma=c(1,1,1,1))
plot(diana_mah, which=2, main="Cluster divisivo (dist: Mahalanobis)", xlab=NA, cex.main=2)
#       abline(h=7, col="green", lty=1)
#       abline(h=11.1, col="magenta", lty=1)
# legend(x=119, y=-2.2, legend=c("Criterio de indicadores", "An?lisis visual"), col=c("green", "magenta"), lty=c(1,1), xpd=NA)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Diana - Mahalanobis - Dendograma.pdf")
dev.off()


# Indicadores

indic_diana_mah = indicadores(diana_mah[4], ech3, imprime=20)


x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_diana_mah$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
abline(v=4, col="green", lty=2)
plot(rev(indic_diana_mah$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
abline(v=4, col="green", lty=2)
plot(rev(indic_diana_mah$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
abline(v=4, col="green", lty=2)
mtext("Cluster divisivo (dist: Mahalanobis)", side=3, outer=TRUE, line=1, font=2, cex=2)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Diana - Mahalanobis - Indicadores.pdf")
dev.off()

# Decisi?n: 


#############################################
#### k-medoides redu (dist: Mahalanobis) ####
#############################################

################################


# Matriz de distancias


dist_mah <- mahalanobis.dist(ech_redu)

x11(15,15)
levelplot(dist_mah, main="Matriz de distancias de Mahalanobis",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Pam - Mahalanobis - Matriz de distancias.pdf")
dev.off()


# Formaci?n de clusters


pam_mah_6 <- pam(dist_mah, k=6, diss=TRUE, stand=FALSE)


# Silhouettes


X11(15,15)
par(oma=c(1,0,0,1))
plot(pam_mah_6, main="k-medoides, k=6 (dist: Mahalanobis)")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Pam - Mahalanobis - Silhouettes.pdf")
dev.off()


####################################
#### k-medias (dist: Euclidean) ####
####################################


################################


# Formaci?n de clusters

clara_mah_6 <- clara(ech_redu, k=6, metric="eculidean", stand=FALSE, samples=10)

# Clara no permite utilizar distancia de Mahalanobis


##################
#### k medias ####
##################


################################

rango <- apply(ech_redu, 2, max) - apply(ech_redu, 2, min)
ech.dat <- sweep(ech_redu, 2, rango, FUN="/")
n <- nrow(ech.dat)
was <- rep(0, 10)
was[1] <- (n-1) * sum(apply(ech.dat, 2, var))
for(i in 2:10){
      was[i] <- sum(kmeans(ech.dat, centers=i)$withinss)
}


X11(15,15)
par(oma=c(1,1,1,1))
plot(1:10, was, type="b", pch=21, bg="red", col="red", xlab="N?mero de grupos", ylab="Suma de cuadrados en el grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - kmeans - Mahalanobis - Suma de cuadrados por grupo.pdf")
dev.off()


kmeans6 <- kmeans(ech.dat, center=6)

kmeans6$size


# Agrego la columna con la informaci?n de los grupos

grupos_kmeans <- as.factor(kmeans6[[1]])
ech <- cbind(ech, grupos_kmeans)

# Boxplot por grupos

X11(15,15)
par(mfrow=c(2,2), mar=c(3,3,3,1), oma=c(1,1,4,1))
boxplot(ech$desemp ~ ech$grupos_kmeans, col=colores6, main="Desempleo por grupo")
boxplot(ech$subemp ~ ech$grupos_kmeans, col=colores6, main="Subempleo por grupo")
boxplot(ech$precario ~ ech$grupos_kmeans, col=colores6, main="Precariedad por grupo")
boxplot(ech$ingreso ~ ech$grupos_kmeans, col=colores6, main="Ingreso por grupo")
mtext("Kmeans (k=6)", side=3, font=2, outer=TRUE, cex=1.5, line=1)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - kmeans - Mahalanobis - Boxplots.pdf")
dev.off()


####################
#### Fuzzy sets ####
####################

################################


fanny <- fanny(dist_mah, diss=TRUE, k=6, stand=FALSE, memb.exp=1.4, maxit=2000)

# Silhouette 

X11(15,15)
plot(fanny, main="Fuzzy sets, k=6 (dist: Mahalanobis")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Silhouette.pdf")
dev.off()


# Agrego los coeficientes a la base

ech <- cbind(ech, fanny[[1]])

names(ech)[names(ech)=="1"] <- "fanny_g1"
names(ech)[names(ech)=="2"] <- "fanny_g2"
names(ech)[names(ech)=="3"] <- "fanny_g3"
names(ech)[names(ech)=="4"] <- "fanny_g4"
names(ech)[names(ech)=="5"] <- "fanny_g5"
names(ech)[names(ech)=="6"] <- "fanny_g6"

# Agrego los grupos a la base 
# (se agregan los coeficientes de pertenencia y una columna, "grupos_fanny", que contiene el 
# n?mero del grupo con mayor coef. de pertenencia)

grupos_fanny <- NULL

for(i in 1:360){ 
      grupos_fanny[i] <- ech[i, ] %>% select(fanny_g1:fanny_g6) %>% apply(1, function(x) which(x == max(x))) %>% as.numeric()
}

grupos_fanny <- as.factor(grupos_fanny)
ech <- cbind(ech, grupos_fanny)


#### Comparo los grupos Ward 6 y Fanny 6 ####

table(ech$grupos_fanny)
table(ech$grupos_agnes_mah_redu_wa_6)


a <- as.numeric(ech$grupos_agnes_mah_redu_wa_6) - as.numeric(ech$grupos_fanny)
a <- a != 0
sum(a)


write.csv(ech, "tablas/ech con grupos.csv")


# 239 observaciones cambian de grupo


#### Caracterizaci?n fanny 6 ####


# Tabla de contingencia
table(ech$nombre, ech$grupos_fanny)

X11(15,15)
plot(table(ech$nombre, ech$grupos_fanny), col=colores6)

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Nombre por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$desemp ~ ech$grupos_fanny, col=colores6, main="Desempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Desempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$tparcial ~ ech$grupos_fanny, col=colores6, main="Trabajo a tiempo parcial por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - tparcial por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$multiemp ~ ech$grupos_fanny, col=colores6, main="Multiempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Multiempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$subemp ~ ech$grupos_fanny, col=colores6, main="Subempleo por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Subempleo por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$precario ~ ech$grupos_fanny, col=colores6, main="Precariedad por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Precariedad por grupos.pdf")
dev.off()


X11(15,15)
boxplot(ech$ingreso ~ ech$grupos_fanny, col=colores6, main="Ingreso por grupo")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Fanny - Mahalanobis - Ingreso por grupos.pdf")
dev.off()


########################################
##### Clusters basados en modelos ######
########################################


################################


ech_mclust <- Mclust(ech.dat)

X11(15,15)
par(oma=c(1,1,1,1))
plot(ech_mclust, ech.dat, what="BIC", col=1:10, ylab="-BIC")

dev.copy2pdf(file = "graph/Clustering - Situaci?n de empleo - Clusters basados en modelos - BIC por grupos.pdf")
dev.off()


# Best model: elipsoidal, equal orientation (VVE) with 9 components

print(ech_mclust)

table(ech_mclust$classification)

# Centros de los grupos

ccent <- function(cl){
      f <- function(i) colMeans(ech_redu[cl==i,])
      x <- sapply(sort(unique(cl)), f)
      colnames(x) <- sort(unique(cl))
      return(x)
}

ccent(ech_mclust$classification)


################################
#### An?lisis Discriminante ####
################################


##### Testeo Multinormalidad #####

## Para todas las variable no utilizadas en la formaci?n de clusters

select(ech, jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, 
       sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()

# Se rechaza la existencia de multinormalidad para toda la poblaci?n


## Por niveles del factor nombre

for(i in 1:36){
      try(filter(ech, nombre==levels(ech$nombre)[i]) %>% 
                select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% 
                mardiaTest(qqplot=FALSE) %>% 
                print(), silent=FALSE)
}

# Problemas de singularidad


## Por componentes del factor nombre

filter(ech, sexo=="H") %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()
filter(ech, sexo=="M") %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()

# La distribuci?n no es normal multivariada


filter(ech, educ==1) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()
filter(ech, educ==2) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()
filter(ech, educ==3) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()

# La distribuci?n no es normal multivaraida


filter(ech, edad==1) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()
filter(ech, edad==2) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()
filter(ech, edad==3) %>% select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% mardiaTest(qqplot=FALSE) %>% print()

# La distribuci?n no es normal multivaraida


## Por grupos Ward 6

for(i in 1:6){
      try(filter(ech, grupos_agnes_mah_redu_wa_6==levels(ech$grupos_agnes_mah_redu_wa_6)[i]) %>% 
                select(jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso) %>% 
                mardiaTest(qqplot=FALSE) %>% 
                print(), silent=FALSE)
}


##### Discriminante Log?stico #####

# Esta opci?n no aplica dado que la variable de respuesta no sigue una distribuci?n binomial, sino que 
# sigue una distribuci?n multihipergeom?trica, la cual aproximaremos por una distribuci?n multinomail


##############################################################
##### Discriminante Multinomial (conformaci?n de grupos) #####
##############################################################


##### Construcci?n del modelo #####

rmlog <- multinom(ech$grupos_agnes_mah_redu_wa_6 ~ ., data=ech_redu, maxit=1000, Hess=TRUE, model=TRUE, Wald=TRUE)

# Prueba stepwise de significaci?n global (criterios BIC y AIC)

stepwise(rmlog, criterion="AIC")
stepwise(rmlog, criterion="BIC")

# Devuelve los coeficientes estimados por ecuacci?n, los errores estandard, y
# los valores del estad?stico de Ward para cada regresor en cada ecuaci?n
w1 <- summary(rmlog, Wald=TRUE)

# Armo una matrix con el valor absoluto de los estad?sticos de Ward para cada
# regresor (por filas), en cada ecuaci?n (por columnas)
w1_abs <- abs(w1$Wald.ratios)

# Calculo p valores para la prueba de significaci?n individual a dos colas

p1 <- round(pnorm(w1_abs, lower.tail=FALSE), 3)

write.csv(p1, "tablas/pvalores modelo 1.csv")

# Las vatiables no significativas para los grupos ind?can que esa variable no
# contribuye a explicar la formaci?n de ese grupo

# Devuelve los valores ajustados para la muestra
fitted(rmlog)

# C?lculo de los valores predichos
probspredict <- predict(rmlog, newdata=ech_redu, "probs")
grupospredict <- predict(rmlog, newdata=ech_redu, "class")

# Predicciones correctas

postResample(obs=ech$grupos_agnes_mah_redu_wa_6, pred=grupospredict)

x1 <- table(ech$grupos_agnes_mah_redu_wa_6, grupospredict)

write.csv(x1, "tablas/grupospredict.csv")

# Agrego los grupos a la base

ech <- cbind(ech, grupospredict)


##### Cross Validation #####

# El siguiente c?digo fue escrito por Manuel Amunategui, y puede encontrarse 
# su p?gina de github: (http://amunategui.github.io/multinomial-neuralnetworks-walkthrough/)

ech_redu_grupos <- cbind(ech_redu, grupospredict)
totalAccuracy <- c()
cv <- 360

for (cv in seq(1:cv)) {
      # assign chunk to data test
      dataTestIndex <- cv
      dataTest <- ech_redu_grupos[dataTestIndex,]
      
      # everything else to train
      dataTrain <- ech_redu_grupos[-dataTestIndex, ]
      crossval <- multinom(dataTrain$grupospredict ~ ., data=dataTrain, maxit=500, trace=F) 
      pred <- predict(crossval, newdata=dataTest, type="class")
      
      # classification error
      cv_ac <- postResample(dataTest$grupospredict, pred)[[1]]
      print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
      totalAccuracy <- c(totalAccuracy, cv_ac)
}

mean(totalAccuracy)

# Con cv=360 (leave one out), mean(totalAccuracy) = 0.9555556


###########################################################
#### Discriminante multilogistico para los componentes ####
###########################################################


##### Construcci?n del modelo #####

ech$interior <- recode(ech$loc, "'I'=1; else=0")
ech$montevideo <- recode(ech$loc, "'M'=1; else=0")

ech$male <- recode(ech$sexo, "'H'=1; else=0")
ech$female <- recode(ech$sexo, "'M'=1; else=0")

ech$edad1 <- recode(ech$edad, "'1'=1; else=0")
ech$edad2 <- recode(ech$edad, "'2'=1; else=0")
ech$edad3 <- recode(ech$edad, "'3'=1; else=0")

ech$educ1 <- recode(ech$educ, "'1'=1; else=0")
ech$educ2 <- recode(ech$educ, "'2'=1; else=0")
ech$educ3 <- recode(ech$educ, "'3'=1; else=0")

# Grupo de referencia: Mujeres de Montevideo con edad 1 y educ 1 (MM11)

ech_dummies <- select(ech, grupospredict, interior, male, edad2, edad3, educ2, educ3)

# Estimaci?n del modelo

rmlog_comp <- multinom(ech_dummies$grupospredict ~ ., data=ech_dummies, maxit=1000)

stepwise(rmlog_comp, criterion="AIC")
stepwise(rmlog_comp, criterion="BIC")

# Inferencia

w2 <- summary(rmlog_comp, Wald=TRUE)
w2_abs <- abs(w2$Wald.ratios)
p2 <- round(pnorm(w2_abs, lower.tail=FALSE), 3)

# Predicci?n

predict_comp_probs <- predict(rmlog_comp, newdata=ech_dummies, "probs")
predict_comp_class <- predict(rmlog_comp, newdata=ech_dummies, "class")

# Predicciones correctas

postResample(obs=ech_dummies$grupospredict, pred=predict_comp_class) # Apenas un 60.55%

table(ech_dummies$grupospredict, predict_comp_class)


#########################################################
##### Discriminante Multinomial (vars explicativas) #####
#########################################################


##### Construcci?n del modelo #####

ech_wa <- select(ech, grupospredict, jefe, size, privado, publico, cpsl, cpcl, profytec, oficina, manual, indust, comercio, sfinan, sperson, ingreso)      

# Estimamos el modelo multi log?stico

rmlog2 <- multinom(grupospredict ~ ., data=ech_wa, maxit=1000)

# Inferencia

stepwise(rmlog2, criterion="AIC") # Seg?n criterio AIC se debe estimar rmlog3

rmlog3 <- multinom(formula = grupospredict ~ jefe + size + privado + cpsl + cpcl + profytec + oficina + manual + comercio + sfinan + sperson + ingreso, data = ech_wa, maxit = 1000)

stepwise(rmlog3, criterion="AIC")

w3 <- summary(rmlog3, Wald=TRUE)
w3_abs <- abs(w3$Wald.ratios)
p3 <- round(pnorm(w3_abs, lower.tail=FALSE), 3)

p3<0.05

# Devuelve los coeficientes estimados del modelo tomando como referencia al grupo IH11
coef(rmlog3)

# Devuelve los valores ajustados para la muestra
fitted(rmlog3)

# C?lculo de los valores predichos
predict3 <- predict(rmlog3, newdata=ech3, "probs")
predict4 <- predict(rmlog3, newdata=ech3, "class")

a <- NULL

for(i in 1:360){a[i] <- max(predict3[i,])}

print(a)

# Predicciones correctas

postResample(obs=ech_wa$grupospredict, pred=predict4)

table(ech_wa$grupospredict, predict4)

ech_predict4 <- cbind(ech_wa, predict4)


##### Cross Validation #####

# El siguiente c?digo fue escrito por Manuel Amunategui, y puede encontrarse 
# su p?gina de github: (http://amunategui.github.io/multinomial-neuralnetworks-walkthrough/)

totalAccuracy <- c()
cv <- 360

for (cv in seq(1:cv)) {
      # assign chunk to data test
      dataTestIndex <- cv
      dataTest <- ech_predict4[dataTestIndex,]
      
      # everything else to train
      dataTrain <- ech_predict4[-dataTestIndex, ]
      crossval <- multinom(dataTrain$predict4 ~ ., data=dataTrain, maxit=500, trace=F) 
      pred <- predict(crossval, newdata=dataTest, type="class")
      
      # classification error
      cv_ac <- postResample(dataTest$predict4, pred)[[1]]
      print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
      totalAccuracy <- c(totalAccuracy, cv_ac)
}

mean(totalAccuracy)

# Con cv=360 (leave one out), mean(totalAccuracy) = 0.8444444



















#$###############################
#### Componentes principales ####
#################################


#### ACP ####


for(i in 1:360){row.names(ech)[i] <- as.character(ech$udad_anio[[i]])}

# X11(15,15)
# par(mfrow=c(1,2), oma=c(0,1,0,1))
a <- PCA(ech3, graph=FALSE, ncp=2)

summary(a)


#### Gr?ficos con individuos y grupos ####


x1 <- a$ind$coord[,1];  x2 <- a$var$coord[,1]
y1 <- a$ind$coord[,2];  y2 <- a$var$coord[,2]

normas <- NULL
for(i in 1:length(x1)){normas[i]=sqrt((x1[i])^2+(y1[i])^2)}

wx <- NULL; wy <- NULL; t=0.2; z=max(normas); theta <- seq(0, 2*pi, length=200)


# Factor map solo variables
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg="white", ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nSolo variables"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)



# Factor map seg?n localidad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$loc), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Localidades", c("Interior","Montevideo"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map seg?n sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$sexo), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Sexo", c("Hombres","Mujeres"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map seg?n educ
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$educ), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por nivel educativo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Nivel\nEducativo", c("Primaria","Secundaria", "Terciaria"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map seg?n edad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$edad), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por tramo de edad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Tramo\nde Edad", c("20-29","30-49", "50 o m?s"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map seg?n localidad y sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$locsex), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad y sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Individuos", c("IH","IM", "MM", "MH"), pch=c(21,21,21,21), col=c(1,2,4,3), pt.bg=c(1,2,4,3), pt.lwd=0, bty="n")


# Factor map seg?n grupos
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$grupospredict), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por grpos"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Clusters", c("1","2","3","4","5","6"), pch=c(21,21,21,21,21,21), col=c(1,2,3,4,5,6), pt.bg=c(1,2,3,4,5,6), pt.lwd=0, ncol=2, bty="n")


# Factor map seg?n a?o
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=(as.numeric(ech$year)-1994), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por a?o"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="A?os", c("95'","96'","97'","98'","99'","00'","01'","02'","03'","04'"), pch=c(21,21,21,21,21,21,21,21,21,21), col=c(1,2,3,4,5,6,7,8,9,10), pt.bg=c(1,2,3,4,5,6,7,8,9,10), ncol=2, pt.lwd=0, bty="n")


# Factor map seg?n nombre2
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$nombre2), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos sin localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
nombres=unique(as.character(ech$nombre2))
legend("bottomleft", title="Individuos", nombres, col=as.numeric(unique(ech$nombre2)), pch=21, pt.bg=as.numeric(unique(ech$nombre2)), ncol=2, pt.lwd=0, bg="white", bty="n")


#$####################
#### ACP reducido ####
######################


#### ACP reducido ####


for(i in 1:360){row.names(ech_redu)[i] <- as.character(ech$udad_anio[[i]])}

# X11(15,15)
# par(mfrow=c(1,2), oma=c(0,1,0,1))
a <- PCA(ech_redu, graph=FALSE, ncp=2)

summary(a)


#### Gr?ficos con individuos y grupos ####


x1 <- a$ind$coord[,1];  x2 <- a$var$coord[,1]
y1 <- a$ind$coord[,2];  y2 <- a$var$coord[,2]

normas <- NULL
for(i in 1:length(x1)){normas[i]=sqrt((x1[i])^2+(y1[i])^2)}

wx <- NULL; wy <- NULL; t=0.2; z=max(normas); theta <- seq(0, 2*pi, length=200)


# Factor map solo variables
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg="white", ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nSolo variables"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)


# Factor map reducido seg?n localidad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$loc), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Localidades", c("Interior","Montevideo"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map reducido seg?n sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$sexo), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Sexo", c("Hombres","Mujeres"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map reducido seg?n educ
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$educ), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por nivel educativo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Nivel\nEducativo", c("Primaria","Secundaria", "Terciaria"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map reducido seg?n edad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$edad), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por tramo de edad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Tramo\nde Edad", c("20-29","30-49", "50 o m?s"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map reducido seg?n localidad y sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$locsex), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad y sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Individuos", c("IH","IM", "MM", "MH"), pch=c(21,21,21,21), col=c(1,2,4,3), pt.bg=c(1,2,4,3), pt.lwd=0, bty="n")


# Factor map reducido seg?n grupos
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$grupospredict), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por grpos"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Clusters", c("1","2","3","4","5","6"), pch=c(21,21,21,21,21,21), col=c(1,2,3,4,5,6), pt.bg=c(1,2,3,4,5,6), pt.lwd=0, ncol=2, bty="n")


# Factor map reducido seg?n a?o
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=(as.numeric(ech$year)-1994), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos por a?o"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="A?os", c("95'","96'","97'","98'","99'","00'","01'","02'","03'","04'"), pch=c(21,21,21,21,21,21,21,21,21,21), col=c(1,2,3,4,5,6,7,8,9,10), pt.bg=c(1,2,3,4,5,6,7,8,9,10), ncol=2, pt.lwd=0, bty="n")


# Factor map reducido seg?n nombre2
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$nombre2), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Reducido - ", round(a$eig[2,3],2), "%", "\nIndividuos sin localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
nombres=unique(as.character(ech$nombre2))
legend("bottomleft", title="Individuos", nombres, col=as.numeric(unique(ech$nombre2)), pch=21, pt.bg=as.numeric(unique(ech$nombre2)), ncol=2, pt.lwd=0, bg="white", bty="n")


##$##################################
#### ACP resto de las variables #####
#####################################


### ACP resto de las variables ####

for(i in 1:360){row.names(ech_res)[i] <- as.character(ech$udad_anio[[i]])}

# X11(15,15)
# par(mfrow=c(1,2), oma=c(0,1,0,1))
a <- PCA(ech_res, graph=FALSE, ncp=2)

summary(a)


#### Gr?ficos con individuos y grupos ####


x1 <- a$ind$coord[,1];  x2 <- a$var$coord[,1]
y1 <- a$ind$coord[,2];  y2 <- a$var$coord[,2]

normas <- NULL
for(i in 1:length(x1)){normas[i]=sqrt((x1[i])^2+(y1[i])^2)}

wx <- NULL; wy <- NULL; t=0.2; z=max(normas); theta <- seq(0, 2*pi, length=200)


# Factor map Resto solo variables
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg="white", ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nSolo variables"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)


# Factor map Resto seg?n localidad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$loc), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Localidades", c("Interior","Montevideo"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map resto seg?n sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$sexo), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Sexo", c("Hombres","Mujeres"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0, bty="n")


# Factor map resto seg?n educ
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$educ), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por nivel educativo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Nivel\nEducativo", c("Primaria","Secundaria", "Terciaria"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map resto seg?n edad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$edad), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por tramo de edad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Tramo\nde Edad", c("20-29","30-49", "50 o m?s"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0, bty="n")


# Factor map resto seg?n localidad y sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$locsex), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad y sexo"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Individuos", c("IH","IM", "MM", "MH"), pch=c(21,21,21,21), col=c(1,2,4,3), pt.bg=c(1,2,4,3), pt.lwd=0, bty="n")


# Factor map resto seg?n grupos
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$grupospredict), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por grpos"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Clusters", c("1","2","3","4","5","6"), pch=c(21,21,21,21,21,21), col=c(1,2,3,4,5,6), pt.bg=c(1,2,3,4,5,6), pt.lwd=0, ncol=2, bty="n")


# Factor map resto seg?n a?o
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=(as.numeric(ech$year)-1994), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos por a?o"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="A?os", c("95'","96'","97'","98'","99'","00'","01'","02'","03'","04'"), pch=c(21,21,21,21,21,21,21,21,21,21), col=c(1,2,3,4,5,6,7,8,9,10), pt.bg=c(1,2,3,4,5,6,7,8,9,10), ncol=2, pt.lwd=0, bty="n")


# Factor map resto seg?n nombre2
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$nombre2), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map Resto - ", round(a$eig[2,3],2), "%", "\nIndividuos sin localidad"), axes=FALSE)
      axis(side=1, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      axis(side=2, at=c(-z,-(z/2),0,(z/2),z), labels=c("-1","-0.5","0","0.5","1"))
      abline(h=0, lty=2)
      abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)){shadowtext(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue", bg="white", cex=1.3, r=0.1)}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
nombres=unique(as.character(ech$nombre2))
legend("bottomleft", title="Individuos", nombres, col=as.numeric(unique(ech$nombre2)), pch=21, pt.bg=as.numeric(unique(ech$nombre2)), ncol=2, pt.lwd=0, bg="white", bty="n")


#$################################
#### Correspondencias Simples ####
##################################


#### Independencia ####

chisq.test(ech$grupospredict, ech$loc) # rechazo independencia
chisq.test(ech$grupospredict, ech$sexo) # rechazo independencia
chisq.test(ech$grupospredict, ech$educ) # rechazo independencia
chisq.test(ech$grupospredict, ech$edad) # rechazo independencia
chisq.test(ech$grupospredict, ech$locsex) # rechazo independencia


#### Tablas de contingencia ####

table(ech$grupospredict, ech$locsex)
table(ech$grupospredict, ech$educ)
table(ech$grupospredict, ech$edad)


#### ACS por localidad y sexo ####


conti1 <- with(ech, table(ech$grupospredict,ech$locsex))

round(prop.table(conti1, 1),4)
round(prop.table(conti1, 2),4)

acs1 <- ca(conti1) 
acs1
summary(acs1)


X11(15,15)
par(oma=c(0,1,0,1))
plot(acs1, mass=TRUE, map="symbiplot", contrib="absolute", arrows=c(FALSE, TRUE), main="Factor map\nIndividuos por localidad y sexo")


#### ACS por nivel educativo ####


conti2 <- with(ech, table(ech$grupospredict,ech$educ))

round(prop.table(conti2, 1),4)
round(prop.table(conti2, 2),4)

acs2 <- ca(conti2)
acs2
summary(acs2)


X11(15,15)
par(oma=c(0,1,0,1))
plot(acs2, mass=TRUE, map="symbiplot", contrib="absolute", arrows=c(FALSE, TRUE), main="Factor map\nIndividuos por nivel educativo")


#### ACS por tramo de edad ####


conti3 <- with(ech, table(ech$grupospredict,ech$edad))

round(prop.table(conti3, 1),4)
round(prop.table(conti3, 2),4)

acs3 <- ca(conti3)
acs3
summary(acs3)


X11(15,15)
par(oma=c(0,1,0,1))
plot(acs3, mass=TRUE, map="symbiplot", contrib="absolute", arrows=c(FALSE, TRUE), main="Factor map\nIndividuos por tramo de edad")


#$############################
### FIN DE LA PROGRAMACI?N ###
##############################