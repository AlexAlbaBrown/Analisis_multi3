########################################
#### PRIMER INFORME- MULTIVARIADO I ####
########################################

library(tidyverse)
library(ggthemes)
library(cluster)
library(mclust)
library(nnet)
library(StatMatch)
library(lattice)
library(MVN)
library(MASS)
library(ggdendro)
library(caret)

source("C:/Users/dacza/Dropbox/R Functions/indicadores.R")
source("C:/Users/Daniel/Dropbox/R Functions/indicadores.R")

# Base de actividades de los alumnos de la carrera de econom?a
acti <- read.csv("base-actividades.csv", sep=";", header=T) %>% as_tibble()

# Eliminamos a los estudiantes que se inscribieron en la facultad y en la carrera de econom?a, pero no se inscribieron a ninguna materia
acti <- acti %>% filter(mat != 0)

# Base de materias de la carrera de econom?a
materias <- read.csv("Materias, codigos, areas del conocimiento.csv", header=TRUE) %>% as_tibble() %>% dplyr::select(-las.hizo.alguien.de.plan.nuevo.) %>% dplyr::rename(area = ?rea.del.conocimiento, cred = Cr?ditos)

# Merging actividades con ?reas
acti <- left_join(acti, materias, by=c("mat" = "C?digo"))
acti <- acti %>% mutate(cursozero = ifelse(acti$tact == "C" & acti$nota == 0, T, F))
acti <- filter(acti, cursozero == FALSE) %>% dplyr::select(-cursozero)
acti <- mutate(acti, oblig.apro = if_else(Car?cter == "Obligatoria" & nota > 2, 1, 0))

# Calculamos las escolaridades: global, y por ?reas EC, MC y SOC
escolaridad <- acti %>% filter(nota <= 12) %>% group_by(id) %>% summarise(esco = mean(nota))
escolaridad.ec <- acti %>% filter(nota <= 12, area == "Econom?a") %>% group_by(id) %>% summarise(esco.ec = mean(nota))
escolaridad.mc <- acti %>% filter(nota <= 12, area == "MMCC") %>% group_by(id) %>% summarise(esco.mc = mean(nota))
escolaridad.soc <- acti %>% filter(nota <= 12, area == "Social") %>% group_by(id) %>% summarise(esco.soc = mean(nota))
escolaridad <- left_join(escolaridad, escolaridad.ec, by="id") %>% left_join(., escolaridad.mc, by="id")%>% left_join(., escolaridad.soc, by="id")
escolaridad$esco.ec <- ifelse(is.na(escolaridad$esco.ec), 0, escolaridad$esco.ec)
escolaridad$esco.mc <- ifelse(is.na(escolaridad$esco.mc), 0, escolaridad$esco.mc)
escolaridad$esco.soc <- ifelse(is.na(escolaridad$esco.soc), 0, escolaridad$esco.soc)

# % de aprobs. por examen
total.aprob <- acti %>% filter(nota >= 3, tgen != "R") %>% group_by(id) %>% tally()
total.aprob.exam <- acti %>% filter(nota >= 3, tgen != "R", tact == "E") %>% group_by(id) %>% tally()
total.aprob <- full_join(total.aprob, total.aprob.exam, by="id") %>% rename(tot.aprob = n.x, tot.aprob.exam = n.y)
total.aprob$tot.aprob.exam <- ifelse(is.na(total.aprob$tot.aprob.exam) == TRUE, 0, total.aprob$tot.aprob.exam)
total.aprob <- mutate(total.aprob, prop.aprob.exam = tot.aprob.exam/tot.aprob)

# Base formulario estad?stico
multivar <- read.csv("base-estudiantes.csv", sep=";", header=T) %>% as_tibble() %>% dplyr::select(-plan, -carr04, -matches("fecing"), -tratamiento, -carr12, -EDA, -MC.EDA) %>% filter(gen %in% c(2012, 2013)) %>% 
      dplyr::select(id, gen, matches("cred"), sit_labNT, horas, sexto, edad, Xfem, Xhijos, Xocup, XeduP, XeduM, carr11, carr14, EDAoMCEDA) %>%
      dplyr::select(-matches("prop"), -matches("_2012"))

multivar$XeduM <- as.factor(multivar$XeduM)
levels(multivar$XeduM)[1] <- "Bajo"
levels(multivar$XeduM)[2] <- "Medio"
levels(multivar$XeduM)[3] <- "Alto"

multivar$XeduP <- as.factor(multivar$XeduP)
levels(multivar$XeduP)[1] <- "Bajo"
levels(multivar$XeduP)[2] <- "Medio"
levels(multivar$XeduP)[3] <- "Alto"

multivar$horas <- as.numeric(multivar$horas)
multivar$horas <- ifelse(multivar$horas == 1, 0, multivar$horas)
multivar$horas <- ifelse(multivar$horas == 6, 1, multivar$horas)
multivar$horas <- as.factor(multivar$horas)
levels(multivar$horas)[1] <- "No corresponde"
levels(multivar$horas)[2] <- "Menos de 10 horas"
levels(multivar$horas)[3] <- "Entre 10 y 20 horas"
levels(multivar$horas)[4] <- "Entre 20 y 30 horas"
levels(multivar$horas)[5] <- "Entre 30 y 40 horas"
levels(multivar$horas)[6] <- "M?s de 40 horas"

# Definmos la variable busca trabajo
multivar$busca.trabajo <- ifelse(grepl("[Nn]o", multivar$sit_labNT) == TRUE, 0, 1)

# Dicotomizar la variable sexto
multivar$sexto.mvd <- ifelse(grepl("Montevideo", multivar$sexto) == TRUE, 1, 0)
multivar$sexto.priv <- ifelse(grepl("Privada", multivar$sexto) == TRUE, 1, 0)

# Variable otros estudios universitarios
# multivar$otros_univ <- ifelse(multivar$otros_univ == "Si posee", 1, 0)

# Unimos UTU a p?blica
multivar$sexto <- as.character(multivar$sexto)
multivar$sexto <- ifelse(as.character(multivar$sexto) == "UTU Interior", "Interior Publica", multivar$sexto)
multivar$sexto <- ifelse(multivar$sexto == "UTU Montevideo", "Montevideo Publica", multivar$sexto)

# Borramos sit_labNT
multivar <- dplyr::select(multivar, -sit_labNT)

# Juntamos la informaci?n del formulario estad?stico y las escolaridades
base <- left_join(multivar, escolaridad, by="id") %>% dplyr::select(id, matches("esco"), everything())

# 8 o menos materias obligatorias de la carrera de economia + est? inscripto en contabobolog?a + 40 cr?ditos de contabobismo
acti <- left_join(acti, (multivar %>% dplyr::select(id, carr11, carr14, EDAoMCEDA)), by="id")
acti <- left_join(acti, (acti %>% group_by(id) %>% summarise(obligstotales = sum(oblig.apro))), by="id")
acti <- acti %>% mutate(conta = if_else(area == "Contabilidad" & nota >2, 1, 0))
acti <- left_join(acti, (acti %>% group_by(id) %>% summarise(contatot = sum(conta)) ), by="id")
contabobos <- filter(acti, carr11 == 1 & obligstotales <= 9 & contatot >= 4) %>% dplyr::select(id)
contabobos <- unique(as.vector(contabobos[[1]]))
# acti <- acti %>% filter(carr11 != 1 | obligstotales > 9 | contatot < 4)

# 8 o menos materias obligatorias de la carrera de economia + est? inscripto en admin + 40 cr?ditos de ?rea admin
acti <- acti %>% mutate(adm = if_else(area == "Administraci?n" & nota > 2, 1, 0))
acti <- left_join(acti, (acti %>% group_by(id) %>% summarise(admtot = sum(adm)) ), by="id")
amindoes <- acti %>% filter(carr14 == 1 & obligstotales <= 12 & admtot > 5)  %>% dplyr::select(id)
amindoes <- unique(unclass(amindoes)[[1]])
# acti <- acti %>% filter(carr14 != 1 | obligstotales > 12 | admtot < 6)

eliminar <- c(contabobos, amindoes)

# Definimos los totales para las ?reas de SOC, MMCC, y ECO
#acti <- acti %>% mutate(adm = if_else(area == "Administraci?n" & nota != 0, 1, 0))
#acti <- left_join(acti, (acti %>% group_by(id) %>% summarise(admtot = sum(adm)) ), by="id")

# Elimino del objeto "base" las observaciones cuyos id est?n en el vector "eliminar"
base <- filter(base, !(id %in% eliminar))
base <- left_join(base, (dplyr::select(total.aprob, id, prop.aprob.exam)), by="id")

# Creamos los cr?ditos por a?o
base <- base %>% mutate(cred2 = cred1y2 - cred1, cred3 = cred1.2y3 - cred1y2, cred4 = cred1234 - cred1.2y3) %>% dplyr::select(id, matches("esco"), matches("cred"), prop.aprob.exam, everything(), -cred1y2, -cred1.2y3, -cred1234)

write.csv(base, file="base.csv", row.names=FALSE)

############################
#### ESTAD. DESCRIPTIVA ####
############################

# Histograma de escolaridad global por sexo
base %>% dplyr::select(id, esco, Xfem) %>% mutate( Xfem = as.factor(ifelse(Xfem == 1, "Mujeres", "Hombres"))) %>%
      ggplot() +
      geom_histogram(aes(esco, y=..density.., fill=Xfem), position="dodge") +
      labs(x="Escolaridad", y="") +
      ggthemes::theme_economist() +
      theme(legend.title=element_blank(),
            legend.position="bottom",
            axis.title=element_text(face="bold", size=12),
            axis.ticks=element_blank())

# Generaci?n seg?n Sexto que hicieron
xtabs(~ gen + sexto, data=base)

# Gen por sexo
xtabs(~ gen + Xfem, data=base)

# Cr?dits al cabo de 4 a?os seg?n sexto
# ggplot(base) +
#       geom_histogram(aes(cred1234, y=..density.., fill=sexto), position="dodge") + 
#       ggthemes::theme_economist()
      
# base %>% mutate(montevideo = if_else(grepl("Montevideo", base$sexto) == TRUE, 1, 0)) %>%
#       filter(montevideo == 1) %>%
#       ggplot() +
#       geom_histogram(aes(cred1234, y=..density.., fill=sexto), position="dodge") + 
#       ggthemes::theme_economist()

# Edad al ingreso seg?n Xocup
ggplot(base) +
      geom_histogram(aes(edad, fill=as.factor(Xocup)), position="dodge") +
      ggthemes::theme_economist()

# table((base %>% filter(cred1234 >= 270))$Xocup)

# Educaci?n de los padres
# round(xtabs(~ XeduP + XeduM, data=(base %>% filter(cred1234 >= 270))) / xtabs(~ XeduP + XeduM, data=base) * 100, 2)

# Avance y escolardad
ggplot(base) +
      geom_point(aes(x=(cred1 + cred2 + cred3 + cred4), y=esco)) +
      labs(x="Cr?ditos acumulados al final del cuarto a?o de carrera", y="Escolaridad global") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_text(face="bold"))

# Prop exam y escolardad
ggplot(base) +
      geom_point(aes(x=(cred1 + cred2 + cred3 + cred4), y=prop.aprob.exam)) +
      labs(x="Cr?ditos acumulados al final del cuarto a?o de carrera", y="Proporci?n de materias aprobadas por examen") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_text(face="bold"))

# sexto
base %>% dplyr::select(gen, edad, Xfem, Xhijos, Xocup, XeduP, XeduM, carr11, carr14, busca.trabajo, sexto.mvd, sexto.priv)

xtabs(~Horas + Edad, data=(rename(base, Horas = horas, Edad = edad)))

# boxplot de escolaridad seg?n nivel educ de los padres 
base %>% dplyr::select(esco, XeduP, XeduM) %>% gather(key=key, value=value, -esco) %>% mutate(key = ifelse(key == "XeduP", "Padre", "Madre")) %>% filter(is.na(value) == FALSE) %>%
      ggplot() +
      geom_boxplot(aes(x=value, y=esco, fill=(key)), na.rm=TRUE, show.legend=FALSE) +
      facet_wrap(~ key, scales="free", ncol=2, nrow=1) +
      labs(x="Nivel educativo", y="Escolaridad", fill="") +
      # scale_fill_manual(values=c("Bajo", "Medio", "Alto"), name="value") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_text(face="bold"),
            legend.position="bottom")


# boxplot de avance seg?n nivel educ de los padres 
base %>% dplyr::select(cred1, cred2, cred3, cred4, XeduP, XeduM) %>% mutate(avance = cred1+cred2+cred3+cred4) %>% dplyr::select(-matches("cred")) %>% gather(key=key, value=value, -avance) %>% 
      mutate(key = ifelse(key == "XeduP", "Padre", "Madre")) %>% filter(is.na(value) == FALSE) %>%
      ggplot() +
      geom_boxplot(aes(x=value, y=avance, fill=(key)), show.legend=FALSE) +
      facet_wrap(~ key, scales="free", ncol=2, nrow=1) +
      labs(x="Nivel educativo", y="Cr?ditos acumulados", fill="") +
      # scale_fill_manual(values=c("Bajo", "Medio", "Alto"), name="value") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_text(face="bold"),
            legend.position="bottom")


##############################
#### AN?LISIS DE CLUSTERS ####
##############################

mygreen <- "darkgreen"

baseredu <- base %>% dplyr::select(id, matches("esco"), -esco.soc, matches("cred"), prop.aprob.exam)
baseredu <- baseredu[complete.cases(baseredu), ]

agnes_sl <- agnes((dplyr::select(baseredu, -id)), metric="euclidean", stand=TRUE, method="single")
agnes_cl <- agnes((dplyr::select(baseredu, -id)), metric="euclidean", stand=TRUE, method="complete")
agnes_al <- agnes((dplyr::select(baseredu, -id)), metric="euclidean", stand=TRUE, method="average")
agnes_wa <- agnes((dplyr::select(baseredu, -id)), metric="euclidean", stand=TRUE, method="ward")

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_sl, which=2, nmax.lab=50, main="Single Linkage (dist: euclidean)", xlab=NA, cex.main=2)

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_cl, which=2, nmax.lab=50, main="Complete Linkage (dist: euclidean)", xlab=NA, cex.main=2)

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_al, which=2, nmax.lab=50, main="Average Linkage (dist: euclidean)", xlab=NA, cex.main=2)

x11(20,15)
par(oma=c(1,1,1,1))
plot(agnes_wa, which=2, nmax.lab=50, main="Ward (dist: euclidean)", xlab=NA, cex.main=2)
abline(h=20, col=mygreen, lty=2)

# ?ndices (Rcuad, psF, psT)

indic_agnes_sl <- indicadores(agnes_sl[4], (dplyr::select(baseredu, -id)), imprime=20)
indic_agnes_cl <- indicadores(agnes_cl[4], (dplyr::select(baseredu, -id)), imprime=20)
indic_agnes_al <- indicadores(agnes_al[4], (dplyr::select(baseredu, -id)), imprime=20)
indic_agnes_wa <- indicadores(agnes_wa[4], (dplyr::select(baseredu, -id)), imprime=20)

x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_sl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=6, col=mygreen, lty=2)
plot(rev(indic_agnes_sl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=6, col=mygreen, lty=2)
plot(rev(indic_agnes_sl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=6, col=mygreen, lty=2)
mtext("Single Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_cl$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=2, col=mygreen, lty=2)
plot(rev(indic_agnes_cl$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=2, col=mygreen, lty=2)
plot(rev(indic_agnes_cl$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=2, col=mygreen, lty=2)
mtext("Complete Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_al$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
plot(rev(indic_agnes_al$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
plot(rev(indic_agnes_al$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
mtext("Average Linkage (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

x11(15,15)
par(mfrow=c(3,1), mar=c(3,4,1,1), oma=c(1,1,5,1))
plot(rev(indic_agnes_wa$Rcuad), xlab="N?mero de grupos", ylab="Rcuad", col="orange", type="b", lty=1, pch=21, bg="orange", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
plot(rev(indic_agnes_wa$psF), xlab="N?mero de grupos", ylab="psF", col="blue", type="b", lty=1, pch=21, bg="blue", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
plot(rev(indic_agnes_wa$psT), xlab="N?mero de grupos", ylab="psT", col="deeppink", type="b", lty=1, pch=21, bg="deeppink", font.lab=2)
      abline(v=3, col=mygreen, lty=2)
mtext("Ward (dist: euclidean)", side=3, outer=TRUE, line=1, font=2, cex=2)

# Creo una base de grupos
grupos <- mutate(dplyr::select(baseredu, id), 
                 agnes_sl_6 = factor(cutree(agnes_sl, k=6)), 
                 agnes_cl_2 = factor(cutree(agnes_cl, k=2)), 
                 agnes_av_3 = factor(cutree(agnes_al, k=3)), 
                 agnes_wa_3 = factor(cutree(agnes_wa, k=3)))

####################
#### FUZZY SETS ####
####################

fanny <- fanny(baseredu, diss=FALSE, k=3, stand=TRUE, memb.exp=1.4, maxit=2000)

X11(15,15)
plot(fanny, main="Fuzzy sets, k=3 (dist: Euclidean", which.plots=2)

grupos <- cbind(grupos, fanny[[1]]) %>% as_tibble()

for(i in 1:3){
      names(grupos)[names(grupos)==as.character(i)] <- paste0("fanny_g",i)
}

# Agrego los grupos a la base 
# (se agregan los coeficientes de pertenencia y una columna, "grupos_fanny", que contiene el 
# n?mero del grupo con mayor coef. de pertenencia)

grupos_fanny <- NULL

for(i in 1:dim(grupos)[1]){
      grupos_fanny[i] <- grupos[i, ] %>% dplyr::select(fanny_g1, fanny_g2, fanny_g3) %>% apply(1, function(x) which(x == max(x))) %>% as.numeric()
}

grupos_fanny <- as.factor(grupos_fanny)
grupos <- cbind(grupos, grupos_fanny) %>% as_tibble()
grupos <- dplyr::select(grupos, -matches("fanny_g"))

#############################################
#### k-medoides redu (dist: Euclidean) ####
#############################################

# Matriz de distancias

#dist_mah <- mahalanobis.dist((dplyr::select(baseredu, -grupos_fanny)))

# x11(15,15)
# levelplot(dist_mah, main="Matriz de distancias de Mahalanobis",xlab="Nombre", ylab="Nombre", scales=list(x=list(at=c(100, 200, 300)), y=list(at=c(100, 200, 300))))

# Formaci?n de clusters

pam_euc <- pam(baseredu, metric= "euclidean", k=3, diss=FALSE, stand=TRUE)

# Silhouettes

X11(15,15)
par(oma=c(1,0,0,1))
plot(pam_euc, main="k-medoides, k=3 (dist: Euclidean)", which.plots=2)

grupos <- grupos %>% mutate(k_medoides = pam_euc[[3]]) %>% as_tibble()

#######################################
#### CARACTERIZACI?N DE LOS GRUPOS ####
#######################################

# Avance en los 4 a?os, escolaridad y grupos Ward
left_join(baseredu, grupos, by="id") %>%
      ggplot() +
      geom_point(aes(x=(cred1+cred2+cred3+cred4), y=esco, color=agnes_wa_3))

# Avance en los 4 a?os, escolaridad y grupos Fanny
left_join(baseredu, grupos, by="id") %>%
      ggplot() +
      geom_point(aes(x=(cred1+cred2+cred3+cred4), y=esco, color=grupos_fanny))

# Avance en los 4 a?os, escolaridad y grupos K medoides
left_join(baseredu, grupos, by="id") %>%
      ggplot() +
      geom_point(aes(x=(cred1+cred2+cred3+cred4), y=esco, color=as.factor(k_medoides)))

# Avance en los 4 a?os, prop aprob por examen y grupos Ward
left_join(baseredu, grupos, by="id") %>%
      ggplot() +
      geom_point(aes(x=(cred1+cred2+cred3+cred4), y=prop.aprob.exam, color=agnes_wa_3))

# Avance en los 4 a?os, escolaridad, sexto y grupos Ward
left_join(base, grupos, by="id") %>%
      ggplot() +
      geom_point(aes(x=(cred1+cred2+cred3+cred4), y=esco, shape=agnes_wa_3, color=sexto)) +
      ggthemes::theme_economist()

# left_join(base, grupos, by="id") %>%
#       ggplot() +
#       geom_point(aes(x=(cred1+cred2+cred3+cred4), y=prop.aprob.exam, shape=agnes_wa_3, color=sexto))

# Tabla con medias de avance, sexo, hijos, etc.
left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id") %>% dplyr::select(id,agnes_wa_3, everything()) %>% group_by(agnes_wa_3) %>%
      summarise(fem = mean(Xfem), hijos = mean(Xhijos), Xocup = mean(Xocup), carr11 = mean(carr11), carr14 = mean(carr14), sexto.mvd = mean(sexto.mvd), sexto.priv = mean(sexto.priv))

# Tabla con medianas de avance, y escolaridad
left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id") %>% dplyr::select(id,agnes_wa_3, everything()) %>% group_by(agnes_wa_3) %>%
      summarise(esco = median(esco), avance = median(cred1+cred2+cred3+cred4))

# Tabla con grupos y educaci?n del padre
round((xtabs(~ agnes_wa_3 + XeduP, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE)/
            matrix(data=(rep(xtabs(~ agnes_wa_3, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE), 4)), nrow=4, ncol=4, byrow=FALSE)), 3) * 100

# Tabla con grupos y educaci?n de la madre
round((xtabs(~ agnes_wa_3 + XeduM, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE)/
             matrix(data=(rep(xtabs(~ agnes_wa_3, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE), 4)), nrow=4, ncol=4, byrow=FALSE)), 3) * 100

# # Tabla con grupos y horas que trabajaba cuando entr?
# round((xtabs(~ agnes_wa_3 + horas, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE)/
#              matrix(data=(rep(xtabs(~ agnes_wa_3, data=(left_join(base, (dplyr::select(grupos, id, agnes_wa_3)), by="id")), addNA=TRUE), 6)), nrow=4, ncol=6, byrow=FALSE)), 3) * 100

# Escolaridad MC y EC seg?n grupo
left_join(baseredu, grupos, by="id") %>% dplyr::select(matches("esco"), cred1, cred2, cred3, cred4, agnes_wa_3) %>%
      ggplot() + 
      geom_point(aes((cred1+cred2+cred3+cred4), esco.ec, color=agnes_wa_3)) +
      ggthemes::theme_economist()

left_join(base, grupos, by="id") %>% filter(sexto == "Montevideo Privada") %>% group_by(agnes_wa_3) %>% tally()
xtabs(~agnes_wa_3 + sexto, data=left_join(base, grupos, by="id") )

################################
#### AN?LISIS DISCRIMINANTE ####
################################

#### RECLASIFICACI?N ####

# Prueba de multinormalidad (rechaza)
dplyr::select(baseredu, matches("esco"), matches("cred"), prop.aprob.exam) %>% mvn(mvnTest="mardia") %>% print()
rmlog1 <- multinom(grupos$agnes_wa_3 ~ esco + esco.mc + esco.ec + cred1 + cred2 + cred3 + cred4 + prop.aprob.exam, data=baseredu[,-1], maxit=1000, Hess=TRUE, model=TRUE, Wald=TRUE)
step1 <- stepAIC(rmlog1)
coef(step1) # Sac? esco

rmlog2 <- multinom(grupos$agnes_wa_3 ~ esco.mc + esco.ec + cred1 + cred2 + cred3 + cred4 + prop.aprob.exam, data=baseredu[,-1], maxit=1000, Hess=TRUE, model=TRUE, Wald=TRUE)

# p valores para la significaci?n individual
(w1 <- summary(rmlog2, Wald=TRUE))
(w1_abs <- abs(w1$Wald.ratios))
(p1 <- round(pnorm(w1_abs, lower.tail=FALSE), 3)) # Todas las variables resultaron significativas
 
# # Devuelve los valores ajustados para la muestra
# fitted(rmlog1)

# C?lculo de los valores predichos
probspredict1 <- predict(rmlog1, newdata=baseredu, "probs")
grupospredict1 <- predict(rmlog1, newdata=baseredu, "class")

xtabs(~grupos$agnes_wa_3 + grupospredict1)

# Conclusi?n: reclasificame esta!!!

#### EXPLICAR LA CLASIFICACI?N ####

# Prueba de multinormalidad (rechaza)
dplyr::select(base, gen, edad, Xfem, Xhijos, Xocup, carr11, carr14, busca.trabajo, sexto.mvd, sexto.priv) %>% mvn(mvnTest="mardia") %>% print()

# Discriminante log?stico multinomial
base.grupos <- left_join((dplyr::select(base, -c(esco, esco.ec, esco.mc, esco.soc, cred1, cred2, cred3, cred4, prop.aprob.exam))), (dplyr::select(grupos, id, agnes_wa_3)), by="id") %>% 
      dplyr::select(id, agnes_wa_3, everything()) %>% filter(is.na(agnes_wa_3) == FALSE)
base.grupos <- base.grupos[complete.cases(base.grupos),]

# Estimaci?n del modelo multinomial
rmlog3 <- multinom(base.grupos$agnes_wa_3 ~ gen + edad + Xfem + Xhijos + Xocup + XeduP + XeduM + carr11 + carr14 + busca.trabajo + sexto.mvd + sexto.priv, data=base.grupos[,-1], maxit=1000, Hess=TRUE, model=TRUE, Wald=TRUE)

# Prueba stepwise de significaci?n global utilizando criterio AIC
step3 <- stepAIC(rmlog3)
step3
# Re-estimo el modelo sacando las variables sugeridas porla prueba de stepwise
rmlog4 <- multinom(base.grupos$agnes_wa_3 ~ gen + edad + Xfem + Xocup + carr11 + carr14 + busca.trabajo + sexto.priv, data=base.grupos[,-1], maxit=1000, Hess=TRUE, model=TRUE, Wald=TRUE)

(w2 <- summary(rmlog4, Wald=TRUE))
(w2_abs <- abs(w1$Wald.ratios))
(p2 <- round(pnorm(w1_abs, lower.tail=FALSE), 3))

# Las vatiables no significativas para los grupos ind?can que esa variable no
# contribuye a explicar la formaci?n de ese grupo

# Devuelve los valores ajustados para la muestra
fitted(rmlog)

# C?lculo de los valores predichos
probspredict <- predict(rmlog, newdata=base.grupos, "probs")
grupospredict <- predict(rmlog, newdata=base.grupos, "class")

x1 <- table(base.grupos$agnes_wa_3, grupospredict)


##########################
#### GUARDA EL GLOBAL ####
##########################

save(list=ls(), file="multivariado.Rdata")

################################
#### FIN DE LA PROGRAMACI?N ####
################################