rm(list=ls())
base=read.table("vacaciones.csv",sep=",",header=TRUE,row.names=1)
library(FactoMineR)
#Test chi2
k=chisq.test(base)
k$observed
k$expected

#Perfil filas
sumfila=apply(base,1,sum)
tperfilfila=base/sumfila
round(tperfilfila,digit=5)
apply(tperfilfila,1,sum)
barplot(t(as.matrix(tperfilfila)),legend.text=T)
pro=t(as.matrix(tperfilfila))
barplot(pro,legend.text=T,cex.names=0.3,cex.axis=0.3)


AFC=CA(base,ncp=5,graph=TRUE)