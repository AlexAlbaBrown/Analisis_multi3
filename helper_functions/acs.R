#ANALISIS DE CORRESPONDENCIAS SIMPLES#         
##Modificado sobre codigo de Fionn Murtagh.(12/2003)

#  xtab= Tabla de Contingencia
#  Fsup= Filas suplementarias si las hay, en la forma: c(i,j,z)
#  Csup= Columnas suplementarias en la forma : c(...)

acs <- function(xtab,Fsup=NULL,Csup=NULL) {
  
  DATOS <- as.data.frame(xtab)       ###pa rescatar los names y row names ORIGINALES
  
  if (is.null(Csup)==F)  xtab <- DATOS[,-Csup]
  if (is.null(Fsup)==F)  xtab <- DATOS[-Fsup,]
  if (is.null(Csup)==F & is.null(Fsup)==F)  xtab <- DATOS[-Fsup,-Csup]
  
  
  tot <- sum(xtab)
  fIJ <- xtab/tot                              #Frecuencia Relativa Conjunta
  fI <- apply(fIJ, 1, sum)                     #Frecuencia Marginal de las FILAS
  fJ <- apply(fIJ, 2, sum)                     #Frecuencia Marginal de las COLUMNAS
  fJsupI <- sweep(fIJ, 1, fI, FUN="/")         #Perfiles FILAS
  fIsupJ <- sweep(fIJ, 2, fJ, FUN="/")         #Perfiles Columnas
  
  s <- as.matrix(t(fJsupI)) %*% as.matrix(fIJ)      #esta es X'DX
  s1 <- sweep(s, 1, sqrt(fJ), FUN="/")              #esta es M^(1/2)X'DX
  s2 <- sweep(s1, 2, sqrt(fJ), FUN="/")             #esta es  M^(1/2)X'DXM^(1/2)
  
  #s2 es simetrica pero por las dudas symmetr=T ( x precision y que tiene algoritmos !=) 
  
  autoval <- eigen(s2,symmetric=T)$values
  autovect <- eigen(s2,symmetric=T)$vectors
  autoval[autoval < 1.0e-8] <- 0.0
  
  tot <- sum(autoval[-1])    ######INERCIA
  
  evectors <- sweep(autovect, 1, sqrt(fJ), FUN="/") # Eigenvectors divided rowwise by sqrt(fJ)
  
  # PROYECCION SOBRE LOS FACTORES DE Filas y Columnas
  
  Rcoord <- as.matrix(fJsupI) %*% evectors
  temp  <- as.matrix(s2) %*%autovect        #  M^(1/2)X'DXM^(1/2) * autovect
  
  # Esto divide 'rowwise' by sqrt(fJ) and 'columnwise' by sqrt(eigenvalues):
  Ccoord <- sweep(sweep(temp,1,sqrt(fJ),FUN="/"),2,sqrt(autoval),FUN="/")
  # Nota: La primera columna de Ccoord es 1.
  
  # CONTRIBUCION A LOS FACTORES DE Filas y Columnas
  # Contributions: mass times projection distance squared.
  temp <- sweep( Rcoord^2, 1, fI, FUN="*")
  # Normalize such that sum of contributions for a factor equals 1.
  sumCtrF <- apply(temp, 2, sum)
  # Note: Obs. x factors. Read cntrs. with factors 1,2,... from cols. 2,3,...
  Rcontr <- sweep(temp, 2, sumCtrF, FUN="/")
  temp <- sweep( Ccoord^2, 1, fJ, FUN="*")
  sumCtrF <- apply(temp, 2, sum)
  # Note: Vbs. x factors. Read cntrs. with factors 1,2,... from cols. 2,3,...
  Ccontr <- sweep(temp, 2, sumCtrF, FUN="/")
  
  # CORRELATIONS WITH FACTORS BY ROWS AND COLUMNS
  # dstsq(i) = sum_j 1/fj (fj^i - fj)^2
  temp <- sweep(fJsupI, 2, fJ, "-")
  dstsq <- apply( sweep( temp^2, 2, fJ, "/"), 1, sum)
  # NOTE: Obs. x factors. Read corrs. with factors 1,2,... from cols. 2,3,...
  Rcos2 <- sweep(Rcoord^2, 1, dstsq, FUN="/")
  temp <- sweep(fIsupJ, 1, fI, "-")
  dstsq <- apply( sweep( temp^2, 1, fI, "/"), 2, sum)
  # NOTE: Vbs. x factors. Read corrs. with factors 1,2,... from cols. 2,3,...
  Ccos2 <- sweep(Ccoord^2, 1, dstsq, "/")
  
  
  ##########################   IMPRESION  ##############################
  
  
  #tabla
  chiTEST <- chisq.test(xtab)
  
  porcentaje <- autoval[-1]/tot             #PORCENTAJE
  
  ACUMporcentaje <- cumsum(porcentaje)      #% Acumulado
  
  chicuad <- porcentaje*chiTEST$statistic    #chi^2    #$stat o  chicuad[1]
  
  #attr(chiTEST$parameter,'names') <- NULL
  #attr(chiTEST$statistic,'names') <- NULL
  
  
  Rcoord <- as.data.frame(Rcoord)
  attr(Rcoord,'names') <- c(paste('dim ',0:(ncol(Rcoord)-1),sep=''))
  
  Ccoord <- as.data.frame(Ccoord)
  attr(Ccoord,'names') <- c(paste('dim ',0:(ncol(Ccoord)-1),sep=''))
  
  
  Rcontr <- as.data.frame(Rcontr)
  attr(Rcontr,'names') <- c(paste('contr ',0:(ncol(Rcontr)-1),sep=''))
  
  Ccontr <- as.data.frame(Ccontr)
  attr(Ccontr,'names') <- c(paste('contr ',0:(ncol(Ccontr)-1),sep=''))
  
  
  Rcos2 <- as.data.frame(Rcos2)
  attr(Rcos2,'names') <- c(paste('cos2 ',0:(ncol(Rcos2)-1),sep=''))
  
  Ccos2 <- as.data.frame(Ccos2)
  attr(Ccos2,'names') <- c(paste('cos2 ',0:(ncol(Ccos2)-1),sep=''))
  
  
  
  #QUALITY es la suma de los cos del plano principal (*cos2[,1:2])
  ##y MASS son los perfiles(fI y fJ) 
  
  Rsumm <- as.data.frame(cbind(apply(Rcos2[,2:3],1,sum),fI))  
  Csumm<- as.data.frame(cbind(apply(Ccos2[,2:3],1,sum),fJ))    
  
  ###INERCIA DE LAS modalidadesFILA
  Raux <- matrix((rep(fJ,nrow(fJsupI))),nrow(fJsupI),ncol(fJsupI),byrow=T)  ## MTZ con filas todas iguales a fJ
  Raux1 <- (fJsupI-Raux )^2    
  Raux <-apply((Raux1/Raux),1,sum)*fI
  RInercia <- Raux/sum(Raux)  ###proporcionees de inercia de cada modalidad FILA
  
  ###INERCIA DE LAS modalidadesCOLUMNAS
  Caux <- matrix((rep(fI,ncol(fIsupJ))),nrow(fIsupJ),ncol(fIsupJ)) #,byrow=T)  ## MTZ con filas todas iguales a fJ
  Caux1 <- (fIsupJ-Caux)^2
  Caux <-apply((Caux1/Caux),2,sum)*fJ
  CInercia <- Caux/sum(Caux)  ###proporcionees de inercia de cada modalidad FILA
  
  #TABLAS
  
  tabla <- as.data.frame(cbind(c(round(autoval[-1],digits=6),c('----'),round(tot,digits=6)),c(round(chicuad,digits=2),c('----'),round(chiTEST$statistic,digits=2)),c(round(porcentaje*100,digits=4),c('----'),100),c(round(ACUMporcentaje*100,digits=4),c('----') ,NaN)))
  attr(tabla,'names') <- c('Inercia' , ' Chi^2 ' , ' Porcentaje  ' , ' Acumulado')
  attr(tabla,'row.names') <- c(1:(dim(tabla)[1]-2),'--','Total')
  
  Rsumm <- cbind(paste('modalidadFILA ',1:nrow(Rsumm),sep=''),attributes(xtab)$row.names,round(Rsumm,digits=6),round(RInercia,digits=6))
  Csumm <- cbind(paste('modalidadCOLUMNA ',1:nrow(Csumm),sep=''),attributes(xtab)$names,round(Csumm,digits=6),round(CInercia,digits=6))
  attr(Rsumm,'names') <- c('Tipo', 'Nombre' ,'Calidad ' , ' Masa  ' ,'Inercia') 
  attr(Csumm,'names') <- c('Tipo', 'Nombre' ,'Calidad ' , ' Masa  ' ,'Inercia')
  TABLAFIN <- rbind(Rsumm,Csumm)
  
  
  disponibles <- c('Tabla de Contingencia','Perfiles FILA','Perfiles COLUMNA','Test',
                   'Inercia y Descomposicion Chi^2','Coordenadas de las FILAS',
                   ' Contribuciones parciales a la Inercia: FILAS  ',
                   'Cosenos cuadrados: FILAS  ','Coordenadas de las COLUMNAS',
                   'Contribuciones parciales a la Inercia: COLUMNAS',
                   'Cosenos cuadrados: COLUMNAS ', 'Resumen para FILAS y COLUMNAS ')
  
  
  
  ###### SUPLEMENTARIAS ########
  # pal grafico
  rsuppproj <- NULL
  csuppproj <- NULL
  
  if (is.null(Fsup)!=1)     ###la preg es con negacion pa q la respuesta sea TRUE
  {
    rsupp <- DATOS[Fsup,]
    # Note: we must coerce rsupp to matrix type, which  propagates to rsuppIJ
    rsuppIJ <- as.matrix(rsupp)/tot
    if (nrow(rsuppIJ) > 1) rsuppI <- apply(rsuppIJ, 1, sum)
    if (nrow(rsuppIJ) == 1) rsuppI <- sum(rsuppIJ)
    
    rsuppproj <- rsuppIJ %*% as.matrix(Ccoord )
    temp <- rsuppproj
    
    # Divide cols. by mass; and then rows. by sqrt of evals.
    rsuppproj <- sweep ( sweep(temp,1,rsuppI,FUN="/"),2, sqrt(autoval),FUN="/")
    
    # Value of this function on return: table of projections,
    # rows = set of supplementary rows; columns = set of factors.
    # (More than 1 supplementary rows => labels will be retained.)
    # Adjust for trivial factor.
  }
  
  
  if (is.null(Csup)!=1)    ###la preg es con negacion pa q la respuesta sea TRUE
  {
    csupp <- DATOS[,Csup]
    # Note: we must coerce csupp to matrix type, which  propagates to csuppIJ
    csuppIJ <- as.matrix(csupp)/tot
    if (ncol(csuppIJ) > 1) csuppJ <- apply(csuppIJ, 2, sum)
    if (ncol(csuppIJ) == 1) csuppJ <- sum(csuppIJ)
    csuppproj <- t(csuppIJ) %*% as.matrix(Rcoord)
    temp <- csuppproj
    # Divide rows by mass; and then cols. by sqrt of evals.
    csuppproj <- sweep ( sweep(temp,1,csuppJ,FUN="/"),2,sqrt(autoval),FUN="/")
    
    # Value of this function on return: table of projections,
    # rows = set of supplementary columns; columns = set of factors.
    # (More than 1 supplementary column => labels will be retained.)
    # Adjust for trivial factor.
    
  }
  
  #########ESCRIBO EL ARCHIVO DE SALIDA A .txt
  
  write.table((rbind(cbind(Rsumm,Rcoord[,-1],Rcontr[,-1],Rcos2[-1]),(cbind(Csumm,Ccoord[,-1],Ccontr[,-1],Ccos2[-1])))),file='ACS_salida.txt', append = FALSE, quote = TRUE, sep = "\t ",eol = "\n ", na = "NA", dec = ",", row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"))
  
  
  
  cat('                   ','ANALISIS DE CORRESPONDENCIAS SIMPLES', date(), sep='                ','\n')
  
  cat('    ','\n');cat('       ',' Componentes Disponibles ','\n'); cat('    ','\n')   
  
  if ((is.null(Fsup)!=F) & (is.null(Csup)!=F))
    outpuT <- list(xtab,fJsupI,fIsupJ,chiTEST,tabla,Rcoord[,-1],Rcontr[,-1],Rcos2[,-1],Ccoord[,-1],Ccontr[,-1],Ccos2[,-1],TABLAFIN)
  
  
  if ((is.null(Fsup)==F) & (is.null(Csup)==F))
  {
    disponibles <- c(disponibles,'MODALIDADES SUPLEMENTARIAS: FILA','MODALIDADES SUPLEMENTARIAS: COLUMNA')
    outpuT <- list(xtab,fJsupI,fIsupJ,chiTEST,tabla,Rcoord[,-1],Rcontr[,-1],Rcos2[,-1],Ccoord[,-1],Ccontr[,-1],Ccos2[,-1],TABLAFIN,rsuppproj[,-1],csuppproj[,-1])
  }
  
  else
  {
    if (is.null(Fsup)!=1)
    {
      disponibles <- c(disponibles,'MODALIDADES SUPLEMENTARIAS: FILA')
      outpuT <- list(xtab,fJsupI,fIsupJ,chiTEST,tabla,Rcoord[,-1],Rcontr[,-1],Rcos2[,-1],Ccoord[,-1],Ccontr[,-1],Ccos2[,-1],TABLAFIN,rsuppproj[,-1])
    }
    if (is.null(Csup)!=1)
    {
      disponibles <- c(disponibles,'MODALIDADES SUPLEMENTARIAS: COLUMNA')
      outpuT <- list(xtab,fJsupI,fIsupJ,chiTEST,tabla,Rcoord[,-1],Rcontr[,-1],Rcos2[,-1],Ccoord[,-1],Ccontr[,-1],Ccos2[,-1],TABLAFIN,csuppproj[,-1])
    }
    
  }
  
  
  
  ## PLOT
  A<-c(Rcoord[,2],Ccoord[,2]) ; B<-c(Rcoord[,3],Ccoord[,3]) 
  
  AA <- cbind(A,B)
  NOMER <- attributes(xtab)$row.names; NOMEC<-attributes(xtab)$names
  NOME <- c(NOMER,NOMEC)
  AA <- as.data.frame(AA);attr(AA,'row.names')<-NOME
  
  windows()#postscript()
  plot(A,B,xlab='Factor 1',ylab='Factor 2', main="Proyeccion de Modalidades - Plano Principal",axes=T,col=12,pch=19)
  #text(AA,row.names(AA),cex=1,pos=1);abline(h=0);abline(v=0)  
  text(AA[1:nrow(xtab),],NOMER,cex=1,pos=1,col='darkred')
  text(AA[(nrow(xtab)+1):nrow(AA),],NOMEC,cex=1,pos=1,col='blue2')
  abline(h=0);abline(v=0)  
  #dev.off()
  
  print(disponibles)
  
  return(outpuT)
  
  
} # FIN DE LA FUNCION
