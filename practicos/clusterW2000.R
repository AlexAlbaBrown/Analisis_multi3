#### CLUSTER JERÁRQUICO AGREGATIVO CON LA BASE W2000 ####

library(cluster)

# cargamos otras funciones que vamos a usar (OJO DIRECTORIO)
source(file = here::here("helper_functions", "standard.R"))
source(file = here::here("helper_functions", "indicadores.R"))

# LEEMOS DATOS
dat = read.table('w2000.txt', sep = '\t', header = TRUE, row.names = 1)
head(dat)
names(dat)
str(dat)
dim(dat)

class(dat$REGION)
dat$REGION = factor(dat$REGION)
class(dat$REGION)
levels(dat$REGION)
levels(dat$REGION) <- c('OECD','EuropaEste','Asia','Africa','OrienteMEdio','AmericaLatina')
LISTA <- NULL
for (r in levels(dat$REGION)) {
   LISTA[[r]] <- row.names(dat[dat$REGION==r, ])
}

# DESCRIPTIVA DE LOS DATOS
summary(dat)
dat[dat$LITERACY==100, ]

var(dat[,3:15], use = 'pairwise.complete.obs')
round(cor(dat[,3:15], use = 'pairwise.complete.obs'), 2)

plot(dat[,c(5,9,10)])
plot(dat[,c("LIFEEXPF", "BABYMORT", "GDP_CAP")])

plot(dat$DENSITY)
text(dat$DENSITY, row.names(dat))

boxplot(
   dat$GDP_CAP,
   horizontal = TRUE,
   main = 'Producto Interno Bruto per capita'
)
boxplot(
   dat$GDP_CAP ~ dat$REGION,
   names = levels(dat$REGION),
   col = "red",
   main = 'Producto Interno Bruto per capita'
)


# CLUSTER JERÁRQUICO AGREGATIVO -------------------------------------------


#### ALGORITMO ####

DATOSst = standard(dat[,3:13])
DATOSst = scale(dat[,3:13])

agrupo = agnes(
   DATOSst,
   diss = FALSE,
   metric = "euclidean",
   stand = FALSE,
   method = 'ward'
)
# "single" es vecino más cercano
# "complete" es vecino más lejano
# otra forma de hacer lo mismo (o similar)
agrupo2 = agnes(
   dat[,3:13],
   metric = "euclidean",
   stand = TRUE,
   method = 'ward'
)


#### INDICADORES ####

IND = indicadores(agrupo[4], DATOSst, imprime = 20)

plot(
   rev(IND$Rcuad),
   xlab = 'Número de grupos',
   ylab = expression(R^2)
)

plot(agrupo, which = 2)
abline(h = 15, lty = 2, col = 'blue', lwd = 2)
rect.hclust(agrupo, k = 3)


#### OBTENCIÓN DE LOS GRUPOS ####

k <- 3 # 4
cl <- cutree(agrupo[4], k)
cl <- factor(cl)
gru <- cbind(dat, cl)


#### CARACTERIZACIÓN ####

table(gru$cl, gru$REGION)
prop.table(table(gru$cl, gru$REGION), 2)

resumen <- function(x) c(mediana = median(x), min = min(x), max = max(x))
aggregate(gru[, c('LIFEEXPF', 'LIFEEXPM', 'BABYMORT', 'BIRTH_RT')], list(Grupo = gru$cl), resumen)

boxplot(
   gru$GDP_CAP ~ gru$cl,
   col = c("red", "orange", "blue"),
   main = "Producto Interno Bruto per capita"
)
boxplot(
   gru$BABYMORT ~ gru$cl,
   col = rainbow(3),
   main = "Mortalidad infantil"
)
plot(gru[,c(9,10)], col = heat.colors(3)[gru$cl])
plot(gru[,c(5,10)], col = rainbow(3)[gru$cl], pch = "*")
plot(gru[,c(4,13)], col = rainbow(3)[gru$cl], pch = 16)

pairs(
   gru[, c('LIFEEXPF','LIFEEXPM','BABYMORT','BIRTH_RT')],
   panel = function(x,y) { points(x, y, col = as.numeric(gru$cl), cex = 2)},
   cex.labels = 2.5
)


# QUEDARSE CON UNA PARTE DEL CONJUNTO DE DATOS QUE CUMPLE CON DETERMINADA CONDICIÓN
gru1 <- subset(gru, gru$cl != 3)
gru2 <- subset(gru, (gru$DENSITY < 100 & gru$cl != 3))
