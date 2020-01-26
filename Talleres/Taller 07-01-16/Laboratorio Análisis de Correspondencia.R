rm(list=ls())
library(ade4)
data(mariages)
mariages

#Distribución condicional por filas
	#Tabla normalizada (f_{ij})
	T=mariages/sum(mariages)	 
	round(T,4)
	
	#Distribución condicional por filas (f_{ij} / f_{i.})
	sumfila=apply(T,1,sum)
	sumfila
		#en 32% de los casos, la señora es una empleada
	sumcol=apply(T,2,sum)
	sumcol
		#En 38% de los casos el hombre es un obrero.

	tperfilfila=T/sumfila
	round(tperfilfila,digit=3)
	apply(tperfilfila,1,sum)
		#Cuando la señora es ejecutiva superior, el hombre también los es en 56%
	barplot(t(as.matrix(tperfilfila)),legend.text=T)
	pro=t(as.matrix(tperfilfila))
	barplot(pro,legend.text=T,cex.names=0.3,cex.axis=0.3)

	
	#Distribución condicional por columna (f_{ij} / f_{.j})
	sumcol=apply(T,2,sum)
	sumcol
	tperfilcolumna=t(T)/sumcol
	round(t(tperfilcolumna),digit=3)
	apply(t(tperfilcolumna),2,sum)
		#cuando el hombre es ejecutivo superior, tb lo es su señora en 26% de los cassos

	#Analisis de correspondencias
	aco=dudi.coa(mariages,scann=F,nf=3)
		#para ver las coordenadas sobre los ejes de las filas
		aco$li
		#para ver las coordenadas sobre los ejes de las columnas 
		aco$co

		#grafico
		par(mfrow=c(1,2))
		s.label(aco$li)
		s.label(aco$co)

	################
	#Mosaicos
	###############
		#observados
		ssmariages=mariages[4:6,4:6]
		ks=chisq.test(ssmariages)
		ks$expected
		ks$observed
		#teóricos
		mosaicplot(ssmariages,main="Diagrama en mosaico", las=2)



library(FactoMineR)
aco1=CA(mariages)
aco1$eig
aco1$col$coord
aco1$row$coord
