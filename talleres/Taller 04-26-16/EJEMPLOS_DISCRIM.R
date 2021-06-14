

credit <- read.table("credit.txt", header=T, dec=',')


#HACEMOS DISCRIMINANTE PROBABILISTICO
source("testes.R")

credit1=credit[,-1]

#hace tests
library(MASS)
testes(credit1[,c(2,3,5,6)], credit1[,1])


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
 

ggll <- glm(BAD~ LOAN + MORTDUE + CLAGE + CLNO, family=logit, data=credit1) 
summary(ggll)

pr=ggll$fitted.values

pred3=pr*0
for (i in 1:nrow(credit1))
if (pr[i]>0.5) pred3[i]=1

table(credit1[,1],pred3) 
                          

####EJEMPLO INVEST


#lee datos

invest<-read.table("invest.txt", header=T)
attach(invest)


testes(invest[,c(3,4,5,6,7,8,9)], invest[,2])
 

#ajusta modelo LOGISTICO

salida1<-  glm(InvGrade ~  IndicePais + DPBI+ PBIpercapita+ Inflacion + CrecimPBI, family=binomial, data=invest)

summary(salida1)

salida1<-  glm(InvGrade ~  IndicePais +PBIpercapita, family=binomial, data=invest)

summary(salida1)


#calculo valores predichos

scores1<-predict.glm(salida1,type='response')
SCOR1<-vector('character',length=length(InvGrade))

for (i in 1:nrow(invest))
	if (scores1[i]>0.5) SCOR1[i]<-'1' else SCOR1[i]<-'0'

#creo tabla con errores de clasificación

a=table( SCOR1, InvGrade)

prop.table(a, m=1)


#EJEMPLO PESO


#lee datos

peso<-read.table("bajopeso.txt", header=T)
attach(peso)

#ajusta modelo
peso$race=as.factor(peso$race)
salida1<-  glm(low~ age +lwt +race +smoke +ptl +ht +ui +ftv, family= binomial,data=peso)

summary(salida1)

#calculo valores predichos

scores1<-predict.glm(salida1,type='response')
SCOR1<-vector('character',length=length(low))

for (i in 1:nrow(peso))
	if (scores1[i]>0.5) SCOR1[i]<-'1' else SCOR1[i]<-'0'

#creo tabla con errores de clasificación

a=table(low, SCOR1)

prop.table(a, m=1)

step(salida1)
#ajusta nuevo modelo
salida2<-  glm(low~lwt +race +smoke+ht +ui, family= binomial, data=peso)

summary(salida2)

#calculo valores predichos

scores2<-predict(salida2,type='response')
SCOR2<-vector('character',length=length(low))

for (i in 1:nrow(peso))
	if (scores2[i]>0.5) SCOR2[i]<-'1' else SCOR2[i]<-'0'

#creo tabla con errores de clasificación

b=table(low, SCOR2)
prop.table(b,m=1)


scores3<-predict(salida2,type='response')
SCOR3<-vector('character',length=length(low))

for (i in 1:nrow(peso))
	if (scores3[i]>0.33) SCOR3[i]<-'1' else SCOR3[i]<-'0'

#creo tabla con errores de clasificación

c=table(low, SCOR3)
prop.table(c,m=1)
	                   
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



#OTRO EJEMPLO VINOS

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
testes(wine[,2:14], wine[,1])


dlw<-lda(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14, data=wine) 
dlw 

predict(dlw)

#guarda funcion, posterior y predichos 
 
zscore1w <- predict(dlw)$x 
post1w <- predict(dlw)$posterior 
pred1w <- predict(dlw)$class

#agrega a los datos originales los zscores las probabilidades a posteriori y las predichas
junta1w <- cbind(wine,zscore1w,post1w,pred1w)

#clasificacion 
table(wine[,1],pred1w) 
 
#usando CV

dlwC<-lda(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14,CV=TRUE, data=wine) 
dlwC

#guarda funcion, posterior y predichos 
 

post1wC <-dlwC$posterior 
pred1wC <-dlwC$class

#agrega a los datos originales los zscores las probabilidades a posteriori y las predichas
junta1wC <- cbind(wine,post1wC,pred1wC)

#clasificacion 
table(wine[,1],pred1wC) 



#hace funcion discriminante cuadratica

dqw <- qda(V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14, data=wine) 
dqw 
predict(dqw) 
 
#guarda funcion, posterior y predichos de cuadratica 
post2w<-predict(dqw)$posterior 
pred2w<-predict(dqw)$class 
junta2w<-cbind(wine,post2w,pred2w) 

#clasificacion cuadratica 
table(wine[,1],pred2w) 
 
