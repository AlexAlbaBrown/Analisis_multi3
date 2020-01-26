
rm(list=ls())

setwd("I:/Análisis Multivariado I/Taller 04-26-16")
credit <- read.table("credit.txt", header=T, dec=',')


#HACEMOS DISCRIMINANTE PROBABILISTICO
source("testes.R")

credit1=credit[,-1]

#hace tests
library(MASS)
testes(credit1[,c(2,3,5,6)], credit1[,1])

# Moroso: con pvalor=0.69 no rechazo la hipótesis nula de existencia de multinorm
# No moroso con pvalor=0.16 no rechazo la hitóptesis nula de existencia de multinorm


#hace funcion discriminante lineal 
 
dl<-lda(BAD~ LOAN + MORTDUE + CLAGE + CLNO, data=credit1) 
dl 
predict(dl)

#guarda funcion, posterior y predichos 
 
zscore1 <- predict(dl)$x 
post1 <- predict(dl)$posterior 
pred1 <- predict(dl)$class

#agrega a los datos originales los zscores las probabilidades a posteriori y las predichas
junta1 <- cbind(credit1,zscore1,post1,pred1)

#clasificacion 
table(credit1[,1],pred1) 
 
#usando CV

dlC<-lda(BAD~ LOAN + MORTDUE + CLAGE + CLNO, CV=TRUE,data=credit1) 
dlC 
#guarda funcion, posterior y predichos 
 

post1C <-dlC$posterior 
pred1C <-dlC$class

#agrega a los datos originales los zscores las probabilidades a posteriori y las predichas
junta1 <- cbind(credit1,post1C,pred1C)

#clasificacion 
table(credit1[,1],pred1C) 



#hace funcion discriminante cuadratica

dq <- qda(BAD~ LOAN + MORTDUE + CLAGE + CLNO, data=credit1) 
dq 
predict(dq) 
 
#guarda funcion, posterior y predichos de cuadratica 
post2<-predict(dq)$posterior 
pred2<-predict(dq)$class 
junta2<-cbind(credit1,post2,pred2) 

#clasificacion cuadratica 
table(credit1[,1],pred2) 
 

                                              





###OTRO EJEMPLO

#lee datos

library(foreign)
loan <- read.spss("bank loan default.sav", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
loan=loan[1:700,]
apred=loan[701:850,]
loan1=loan[,-2]

#HACEMOS DISCRIMINANTE PROBABILISTICO

#hace tests

testes(loan1[,1:7], loan1[,8])



#hacemos discriminante logistico
rlog=glm(loan1[,8]~age+employ+address+income+debtinc+creddebt+othdebt, family=binomial,data=loan1)
summary(rlog)

step(rlog)
rlog1=glm(loan1[,8]~age+employ+address+debtinc+creddebt, family=binomial,data=loan1)
summary(rlog1)

rlog2=glm(loan1[,8]~employ+address+debtinc+creddebt, family=binomial,data=loan1)
summary(rlog2)


#calculo valores predichos

scores1<-predict.glm(rlog,type='response')
SCOR1<-vector('character',length=length(loan1))

for (i in 1:nrow(loan1))
	if (scores1[i]>0.5) SCOR1[i]<-'YES' else SCOR1[i]<-'NO'

#creo tabla con errores de clasificación

M=table( loan1[,8],SCOR1 )
M
prop.table(M, m=1)
	

scores1<-predict.glm(rlog,type='response')
SCOR2<-vector('character',length=length(loan1))

for (i in 1:nrow(loan1))
	if (scores1[i]>0.3) SCOR2[i]<-'YES' else SCOR2[i]<-'NO'

#creo tabla con errores de clasificación

M1=table( loan1[,8],SCOR2)
M1
prop.table(M1, m=1)


##usando rlog2

scores2<-predict.glm(rlog2,type='response')
SCOR12<-vector('character',length=length(loan1))

for (i in 1:nrow(loan1))
	if (scores2[i]>0.5) SCOR12[i]<-'YES' else SCOR12[i]<-'NO'

#creo tabla con errores de clasificación

M12=table( loan1[,8],SCOR12 )
M12
prop.table(M12, m=1)
	

scores2<-predict.glm(rlog,type='response')
SCOR22<-vector('character',length=length(loan1))

for (i in 1:nrow(loan1))
	if (scores2[i]>0.3) SCOR22[i]<-'YES' else SCOR22[i]<-'NO'

#creo tabla con errores de clasificación

M22=table( loan1[,8],SCOR22)
M22
prop.table(M22, m=1)





