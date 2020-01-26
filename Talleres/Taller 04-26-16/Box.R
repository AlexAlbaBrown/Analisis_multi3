Box<-function(datos,grupo) 
 {       
 N<-nrow(datos)
 grupo <- as.factor(grupo)
 ng<-length(levels(grupo))
 v<-N-ng 
 p<-length(datos) 
 df <- ((ng - 1) * p * (p + 1))/2 
 
 
 Si<-by(datos,grupo,var) 
 ni<-by(datos,grupo,nrow) 
 ni<-as.vector(ni) 
 ni<-c(unlist(ni)) 
 
 S<-matrix(0,p,p) 
 
 for(j in 1:(length(ni))) 
	 
   {S<-S+ (ni[j]-1)*matrix(unlist(Si[j]),p,p)} 
 
 S<-S/v 
 
 suma<-0 
 
 for(j in 1:(length(ni))) 
	 
   {suma<-suma+ (ni[j]-1)*log(det(matrix(unlist(Si[j]),p,p)))} 
 
 
 Q<- log(det(S))*v-suma 
 
# ajuste: 
 
c1 <- (p * (2 * p + 3) - 1)/(6 * (ng - 1) * (p + 1)) * (sum(1/(ni-1)) - 1/v) 
Q <- c(Box.M = Q, adj.M = (1 -c1) * Q) 
df <- rep(df, 2) 
chi <- data.frame(Statistic = Q,df = df, Pr = 1 -pchisq(Q, df, ncp = 0),row.names = c("Box.M","adj.M")) 
 
print(chi) 
} 
