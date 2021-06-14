testM<-function(y) 
{n<-nrow(y) 
p<-ncol(y) 
dfchi<-p*(p+1)*(p+2)/6 
q<-diag(n)-(1/n)*matrix(1,n,n) 
y<-as.matrix(y) 
 
s<- t(y) %*% q %*% y 
s<- (1/n)*s 
 
sinv<-ginv(s)    ##sinv<-ginverse(s) ginv existe en MASS 
 
 
gmatriz<-q%*%y%*%sinv%*%t(y)%*%q                   
beta1hat<-(sum(gmatriz*gmatriz*gmatriz))/(n*n) 
beta2hat<-sum(diag(gmatriz*gmatriz))/n 
kappa1<-n*beta1hat/6                                              
kappa2<-(beta2hat-p*(p+2))/sqrt(8*p*(p+2)/n)        
pvalsim<-1-pchisq(kappa1,dfchi) 
pvalkurt<-2*(1-pnorm(abs(kappa2))) 
res<-c("beta1hat"=beta1hat, "kappa1"=kappa1, "pvalsim"=pvalsim, "beta2hat"=beta2hat, "kappa2"=kappa2, "pvalkurt"=pvalkurt,"n"=n) 
res 
} 
 
 
 
                
