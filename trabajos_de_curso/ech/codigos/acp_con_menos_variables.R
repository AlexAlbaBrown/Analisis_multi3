##$###################
#### ACP Reducido ####
######################

for(i in 1:360){row.names(ech)[i] <- as.character(ech$udad_anio[[i]])}
ech_red <- select(ech, 10,12,13,25,26)

X11(15,15)
#par(mfrow=c(1,2), oma=c(0,1,0,1))
a <- PCA(ech_red, graph=TRUE, ncp=2)

summary(a)


x1 <- a$ind$coord[,1]
y1 <- a$ind$coord[,2]

x2 <- a$var$coord[,1]
y2 <- a$var$coord[,2]

wx <- NULL; wy <- NULL; t=0.2; z=6; theta <- seq(0, 2*pi, length=200)

# Factor map según localidad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$loc), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Localidades", c("Interior","Montevideo"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0)


# Factor map según sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$sexo), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por sexo"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Sexo", c("Hombres","Mujeres"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0)


# Factor map según educ
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$educ), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por nivel educativo"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Educación", c("Primaria","Secundaria", "Terciaria"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0)


# Factor map según edad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$edad), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por tramo de edad"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Edad", c("20-29","30-49", "50 o más"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0)


# Factor map según grupos
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$grupospredict), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por clusters"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Clusters", c("1","2","3","4","5","6"), pch=c(21,21,21,21,21,21), col=c(1,2,3,4,5,6), pt.bg=c(1,2,3,4,5,6), pt.lwd=0)


# Factor map según año
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$year), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por año"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Años", c("95","96","97","98","99","00","01","02","03","04"), pch=c(21,21,21,21,21,21,21,21,21,21), col=c(1,2,3,4,5,6,7,8,9,10), pt.bg=c(1,2,3,4,5,6,7,8,9,10), ncol=2, pt.lwd=0)


# Factor map según nombre2
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$nombre2), ylim=c(-6,6), xlim=c(-6,6), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por nombre"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
nombres=unique(as.character(ech$nombre2))
legend("bottomleft", title="Individuos", nombres, col=as.numeric(unique(ech$nombre2)), pch=21, pt.bg=as.numeric(unique(ech$nombre2)), ncol=2, pt.lwd=0, bg="white")

##$#################################
#### ACP resto de las variables#####
####################################

for(i in 1:360){row.names(ech)[i] <- as.character(ech$udad_anio[[i]])}
ech_res <- select(ech, -(1:8),-10,-12,-13,-25,-26,-28,-29,-30)

X11(15,15)
#par(mfrow=c(1,2), oma=c(0,1,0,1))
a <- PCA(ech_res, graph=TRUE, ncp=2)

summary(a)

a$var$coord
a$var$contrib
a$var$cos2


x1 <- a$ind$coord[,1]
y1 <- a$ind$coord[,2]

x2 <- a$var$coord[,1]
y2 <- a$var$coord[,2]

wx <- NULL; wy <- NULL; t=0.2; z=6; theta <- seq(0, 2*pi, length=200)

# Factor map según localidad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$loc), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por localidad"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Localidades", c("Interior","Montevideo"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0)


# Factor map según sexo
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$sexo), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por sexo"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Sexo", c("Hombres","Mujeres"), pch=c(21,21), col=c(1,2), pt.bg=c(1,2), pt.lwd=0)


# Factor map según educ
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$educ), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por nivel educativo"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Educación", c("Primaria","Secundaria", "Terciaria"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0)


# Factor map según edad
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$edad), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por tramo de edad"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Edad", c("20-29","30-49", "50 o más"), pch=c(21,21, 21), col=c(1,2,3), pt.bg=c(1,2,3), pt.lwd=0)


# Factor map según grupos
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$grupospredict), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por clusters"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Clusters", c("1","2","3","4","5","6"), pch=c(21,21,21,21,21,21), col=c(1,2,3,4,5,6), pt.bg=c(1,2,3,4,5,6), pt.lwd=0)


# Factor map según año
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$year), ylim=c(-6.5,6.5), xlim=c(-6.5,6.5), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por año"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
legend("bottomleft", title="Años", c("95","96","97","98","99","00","01","02","03","04"), pch=c(21,21,21,21,21,21,21,21,21,21), col=c(1,2,3,4,5,6,7,8,9,10), pt.bg=c(1,2,3,4,5,6,7,8,9,10), ncol=2, pt.lwd=0)


# Factor map según nombre2
X11(15,15)
par(oma=c(0,1,0,1))
plot(a$ind$coord, pch=21, col=NULL, bg=as.numeric(ech$nombre2), ylim=c(-6,6), xlim=c(-6,6), xlab=paste0("Dim 1 - ", round(a$eig[1,2],2), "%"), ylab=paste0("Dim 1 - ", round(a$eig[2,2],2), "%"), main=paste0("Factor Map - ", round(a$eig[2,3],2), "%", "\nIndividuos por nombre"))
abline(h=0, lty=2)
abline(v=0, lty=2)
for(h in 1:length(x2)){arrows(x0=0, y0=0, x1=x2[h]*z, y1=y2[h]*z, lwd=2, col="blue")}
for(i in 1:length(x2)){if(x2[i]<0){wx[i]=x2[i]*z-t} else{wx[i]=x2[i]*z+t}}
for(j in 1:length(y2)){if(y2[j]<0){wy[j]=y2[j]*z-t} else{wy[j]=y2[j]*z+t}}
for(k in 1:length(wx)) {text(x=wx[k], y=wy[k], labels=row.names(a$var$coord)[k], col="blue")}
lines(x=z*cos(theta), y=z*sin(theta), lty=2)
nombres=unique(as.character(ech$nombre2))
legend("bottomleft", title="Individuos", nombres, col=as.numeric(unique(ech$nombre2)), pch=21, pt.bg=as.numeric(unique(ech$nombre2)), ncol=2, pt.lwd=0, bg="white")














