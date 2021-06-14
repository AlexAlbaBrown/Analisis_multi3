# lee los datos del ejemplo
riqueza312 <- read.table('riqueza312.txt', header=T)

# dimensiones y nombres de las variables

dim(riqueza312)
names(riqueza312)

# elimina la variable 15 que no tiene nada 
riqueza312 <- riqueza312[,-1]


# solamente con las variables de interes para ACM
riqueza <- riqueza312[,c(2:5,12:13)]

# control
summary(riqueza)

# creacion de lista con las modadlidades de las 6 variables
levJ <- list(c('primaria','secundaria','terciaria'), c('casaN','casaS'), c('autoN','autoS'), c('alqN','alqS'), c('menosqB','Basicos','masqB'), c('nbiN','nbiS'))

# forma 'elaborada' de etiquetar modalidades en las 6 variables
for (j in 1:ncol(riqueza))
    {riqueza[,j] <- as.factor(riqueza[,j])
     levels(riqueza[,j]) <- levJ[[j]] }


# carga la funcion y la deja dsponible en el espacio de trabajo
source('acm.R')

# realiza el ACM y lo guarda en ss  despues del '#' variantes posibles...
ss <- acm(riqueza,NF=4, Csup=c(4,6), ByG=T)

#agrega lineas al grafico
#abline(h = -1:5, v = -2:3, col = "lightgray", lty=3)
abline(h =0, col = "lightgray", lty=3)
abline(v =0, col = "lightgray", lty=3)

#vemos inercia
ss[1]

# guarda las coordenadas de las modalidades en 'a'( solamente para graficar)
a <- ss[[6]]


# grafica el factor1(a[,1]) contra el factor3(a[,3]) de la salida anterior
#grafica  factor12 (a[,2]) contra el factor3 (a[,3])
# cambiar lo que corresponda para adecuarlo a lo que quieran
   
par(fg='red',bg='antiquewhite1')
plot(a[,1],a[,3],xlab='Factor 1',ylab='Factor 3', main='Modalidades plano factorial 1 y 3',axes=T,col=12,pch=19)
abline(h=0, lty=3)
abline(v=0, lty=3)
text(a[,1],a[,3],row.names(a),cex=0.7)
points(ss[[8]][,1],ss[[8]][,2],pch=23,col=1)
text(ss[[8]][,1],ss[[8]][,2],row.names(ss[[8]]),cex=0.7) #,pos=1, #pch='+'
  

par(fg='purple',bg='white')
plot(a[,2],a[,3],xlab='Factor 2',ylab='Factor 3', main='Modalidades plano factorial 2 y 3',axes=T,col=12,pch=19)
abline(h=0, lty=3)
abline(v=0, lty=3)
text(a[,2],a[,3],row.names(a),cex=0.7)
points(ss[[8]][,1],ss[[8]][,2],pch=23,col=1)
text(ss[[8]][,1],ss[[8]][,2],row.names(ss[[8]]),cex=0.7) #,pos=1, #pch='+'
  















