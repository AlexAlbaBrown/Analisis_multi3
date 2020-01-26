######### LABORATORIO DE ANÁLISIS DISCRIMINANTE ###########

library("FactoMineR")
library("MVN")

### 1º Ejemplo: conjunto de datos IRIS

library(foreign)
iris = read.dbf("iris.dbf") # cargo los datos 

str(iris)
summary(iris)

X11(15,15)
par(mfrow = c(4,4))
plot(iris[,1:4],col=c("red","blue","green")[iris[,5]],pch="*")

# Realizo las pruebas de igualdad de medias, Homogeneidad de Variancias Box M y de Mardia para Multinormalidad por grupos

source("testes.R") # cargo la función testes()
testes(iris[,1:4],iris[,5]) 

# No Rechazamos hipotesis de multinormalidad
# Rechazo la homogeneidad de varianzas

#### DISCRIMINANTE CUADRATICO

disc <- qda(Sp ~ Sepal_L_ + Sepal_W_ + Petal_L_ + Petal_W_ , data=iris, prior=c(1/3,1/3,1/3)) 
pred = predict(disc)
attributes(pred)
pred$posterior
pred$class

# Error aparente

A = table(iris$Sp,pred$class)  
B = prop.table(A, 1) # proporciones por filas
diag(B) # me quedo con la diagonal (proporción de acierto en cada grupo)
C = prop.table(A) # proporción sobre el total de observaciones
sum(diag(C)) # proporción global de acierto

### Estimando por Validación Cruzada (leaving one out)

disc2 <- qda(Sp ~ Sepal_L_ + Sepal_W_ + Petal_L_ + Petal_W_ , data=iris, prior=c(1/3,1/3,1/3), CV=TRUE) 
# si no le doy las probabilidades a priori las estima con las proporciones globales de la base
pred2 = disc2$posterior
A2 = table(iris$Sp,disc2$class)  
B2 = prop.table(A2, 1) # proporciones por filas
diag(B2) # me quedo con la diagonal (proporción de acierto en cada grupo)
C2 = prop.table(A2) #proporción sobre el total de observaciones
sum(diag(C2)) #proporción global de acierto

### Usando muestra de entrenamiento

muestra = sample(1:nrow(iris),100) # saco una muestra de entrenamiento
disc3 <- qda(Sp ~ Sepal_L_ + Sepal_W_ + Petal_L_ + Petal_W_ , data=iris, prior=c(1/3,1/3,1/3), subset=muestra) 
pred3m = predict(disc3)
A3m = table(iris$Sp[muestra],pred3m$class)  
pred3c = predict(disc3,iris[-muestra,])
A3c = table(iris$Sp[-muestra],pred3c$class)  


### 2º Ejemplo: conjunto de datos DBP

library(foreign)
base <- read.spss("datos_dbp.sav", to.data.frame = TRUE) # leo la base
dim(base)
str(base)

# Genero dos nuevas variables a partir de las que están en la base
base$avmdias <- base$diasdeavm+base$horasdeavm/24 
base$conteg <- base$ncontroles/base$eg
names(base)

# Me quedo con las variables que me interesan
d <- cbind(base[,2:16],base[,19:21],base[,27:28])
dim(d)
summary(d)

table(d$dbp)

#### Algunos gráficos para las variables cuantitativas

cuanti <- c("edadmademb","ngestas","ncontroles","conteg","eg","peso","talla","pc","avmdias")
nombrescuanti <- c("Edad de la madre al embarazo","Número de gestaciones","Número de controles","Número de controles en relación a la edad gestacional", "Edad gestacional en semanas","Peso al nacer","Talla","Perímetro craneano","Días de AVM")

# Diagramas de caja
X11(15,15)
par(mfrow = c(3,3), mex=0.8)
for(i in 1:length(cuanti)) boxplot(d[,cuanti[i]]~d$dbp, main = nombrescuanti[i], xlab="Broncodisplasia", varwidth=T)

# Matriz de dispersión	
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
     {   usr <- par("usr"); on.exit(par(usr))
         par(usr = c(0, 1, 0, 1))
         r <- cor(x, y, use= "complete.obs")
         txt <- format(c(r, 0.123456789), digits=digits)[1]
         txt <- paste(prefix, txt, sep="")
         if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
         text(0.5, 0.5, txt, cex = cex.cor)
     }

X11(15,15)
pairs(d[,cuanti[1:4]], col=(3:4)[d$dbp], labels=nombrescuanti[1:4], cex.labels=0.8,
   		main="Matriz de dispersión", lower.panel=panel.cor)

X11(15,15)
pairs(d[,cuanti[4:9]], col=(3:4)[d$dbp], labels=nombrescuanti[4:9], cex.labels=0.8,
   		main="Matriz de dispersión", lower.panel=panel.cor)
# parece q las q discriminan mejor son las variables que corresponden al niño

# Histogramas
X11(15,15)
par(mfrow = c(3,3), mex=0.8)
for (i in 1:length(cuanti))hist(d[,cuanti[i]],main='',xlab=nombrescuanti[i])

### Realizo las pruebas de igualdad de medias, Homogeneidad de Variancias Box M y de Mardia para Multinormalidad por grupos

source("testes.R") # cargo la función testes()

# Tomo las observaciones sin NA sino testes no corre
comp <- apply(is.na(d[,cuanti]),1,sum)==0
datos <- d[comp,]
dim(datos)

testes(datos[,cuanti[-3]],datos$dbp)

# rechazo todas las hipótesis nulas

#### DISCRIMINANTE LINEAL (aunque no se cumplan los supuestos) 

library(MASS)
disc <- lda(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias, data=datos) 

# Error aparente

A = table(datos$dbp,predict(disc)$class)  
B = prop.table(A, 1) # proporciones por filas
diag(B) # me quedo con la diagonal (proporción de acierto en cada grupo)
C = prop.table(A) # proporción sobre el total de observaciones
sum(diag(C)) # proporción global de acierto

# Estimando por Validación Cruzada (leaving one out)

disc1 <- lda(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias, data=datos, CV=TRUE) 
A1 = table(datos$dbp,disc1$class)  
B1 = prop.table(A1, 1)
diag(B1) # proporción de acierto en cada grupo
C1 = prop.table(A1) 
sum(diag(C1)) # proporción global de acierto

# Usando muestra de entrenamiento

muestra = sample(1:nrow(datos),80) # saco una muestra de entrenamiento
disc2 <- lda(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias, data=datos, subset=muestra) # lo indico en argumento subset
pred2.e = predict(disc2)
(A2.e = table(datos$dbp[muestra],pred2.e$class))  
prop.table(A2.e,1)
pred2.t = predict(disc2,datos[-muestra,])
(A2.t = table(datos$dbp[-muestra],pred2.t$class)) 
prop.table(A2.t,1)

#### DISCRIMINANTE LOGISTICO
log1 <- glm(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias, family=binomial, data=datos)
log2 <- glm(dbp ~ 1, data=datos, family=binomial)
anova(log2,log1)
qchisq(0.95,anova(log2,log1)$Df[2])
anova(log2,log1)$Deviance[2]
summary(log1)
probs = predict(log1,type="response") # o probs=log1$fitted.values, es la probabilidad de que y sea 1 estimada
# por defecto está type="link" que devuelve el logaritmo de los odds

# Error
Al = table(datos$dbp,as.numeric(probs>0.5))
Bl = prop.table(Al, 1)
diag(Bl) # proporción de acierto en cada grupo
Cl = prop.table(Al) 
sum(diag(Cl)) # proporción global de acierto 

# Agrego algunas variablesde tipo categóricas que pueden ayudar a separar los dos grupos
comp1 <- apply(is.na(d[,c(cuanti[-3],'corticoideprenat','atb')]),1,sum)==0
datos1 <- d[comp1,] # cambio la base porque agrego dos variables que tienen NA
log1 <- glm(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias + corticoideprenat + atb, family=binomial, data=datos1)
log2 <- glm(dbp ~ 1, data=datos1, family=binomial)
anova(log2,log1)
qchisq(0.95,anova(log2,log1)$Df[2])
anova(log2,log1)$Deviance[2]
summary(log1)
probs1 = predict(log1,type="response") 

# Error
A1l = table(datos1$dbp,as.numeric(probs1>0.5))
B1l = prop.table(A1l, 1) 
diag(B1l) # proporción de acierto en cada grupo
C1l = prop.table(A1l)
sum(diag(C1l)) # proporción global de acierto 
# mejoro respecto al anterior

# Usando muestra de entrenamiento
muestra = sample(1:nrow(datos1),80) # saco otra muestra de entrenamiento
log3 <- glm(dbp ~ edadmademb + ngestas + conteg + eg + peso + talla + pc + avmdias + corticoideprenat + atb, family=binomial, data=datos1, subset=muestra)
summary(log3)

probs2.e = predict(log3, type="response")
Al.e = table(datos1$dbp[muestra],as.numeric(probs2.e >0.5))
Bl.e = prop.table(Al.e, 1)
diag(Bl.e) # proporción de acierto en cada grupo
Cl.e = prop.table(Al.e) 
sum(diag(Cl.e)) # proporción global de acierto 

probs2.t = predict(log3, newdata=datos1[-muestra,], type="response") 
Al.t = table(datos1$dbp[-muestra],as.numeric(probs2.t >0.5))
Bl.t = prop.table(Al.t, 1)
diag(Bl.t) # proporción de acierto en cada grupo
Cl.t = prop.table(Al.t) 
sum(diag(Cl.t)) # proporción global de acierto
