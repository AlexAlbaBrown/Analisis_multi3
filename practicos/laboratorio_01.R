#### EJERCICIO 1 ####

rm(list = ls())

M <- matrix(
   data = c(4, 3, -2, 1, 2, -1),
   nrow = 2,
   ncol = 3,
   byrow = TRUE
)
Mt <- t(M)
A <- M %*% Mt
B <- Mt %*% M

#### Parte a ####

sumcolM <- colSums(M)
meanrowM <- rowMeans(M)
meancolM <- colMeans(M)

N <- cbind(M, c(5, 10))


#### Parte b ####

M[, 1:2]


#### Parte c ####

A[ ,1]
B[ ,1]
A[2, ]
B[2, ]
A[2, 2]
B[2, 2]
detA <- det(A)
detB <- det(B)
invA <- solve(A)
invB <- solve(B)
eigenA <- eigen(A)
eigenB <- eigen(B)
svd(A)
svd(B)

listA <- list(A, detA, eigenA[1])
listB <- list(B, detB, eigenB[1])


#### EJERCICIO 2 ####

rm(list = ls())

x1 <- rnorm(6)
x2 <- rgamma(6, shape = 1)
x3 <- runif(6)
x4 <- rweibull(6, shape = 2)
x5 <- rpois(6, lambda = 1)

miejemplo <- as.data.frame(cbind(x1,x2,x3,x4,x5))


#### Parte a ####

for(i in 1:dim(miejemplo)[1]) {
   row.names(miejemplo)[i] <- paste0("obs", i)
}

library(dplyr)
miejemplo <- rename(miejemplo, var1 = x1, var2 = x2, var3 = x3, var4 = x4, var5 = x5)

class(miejemplo)


#### Parte b ####

miejemplo <- cbind(miejemplo, var6 = miejemplo[ ,1] + miejemplo[ ,2] + miejemplo[ ,3])
miejemplo <- cbind(miejemplo, var7 = as.factor(as.numeric(miejemplo$var2 > 5)))


#### Parte c ####

miotroejemplo <- slice(miejemplo, -1) %>% select(1:6)


#### Parte d ####

summary(miejemplo)


#### Parte e ####

write.table(miejemplo, file = "miejemplo.txt")

rm(list = "miejemplo")

miejemplo2 <- read.table("miejemplo.txt", header = TRUE, sep = "")


#### Parte f ####

write.table(iris3, file = "iris.txt")

IRIS <- read.table("iris.txt", header = TRUE, sep = "")


#### EJERCICIO 3 ####

rm(list=ls())

#### Parte a ####

data <- read.table("miejemplo.txt", header = TRUE )
data <- data[,1:5]
class(data)
names(data)
row.names(data)
dim(data)


#### Parte b ####

for(i in 1:dim(data)[1]) {
   row.names(data)[i] <- paste0(i)
}


#### Parte c ####

data$var6 <- as.numeric(data$var2 > 6)


#### Parte d ####

write.table(data, file = "data.txt", col.names = FALSE, row.names = FALSE)
data2 <- read.table("data.txt")


#### EJERCICIO 4 ####

rm(list = ls())

set.seed(10)
a <- floor(runif(1000, min=1, max=100))

#### Parte a ####

b <- cut(a, breaks = seq(0, 100, by = 25), labels = c("D", "R", "B", "MB"))


#### Parte b ####

for(i in 1:length(a)) {
   print(paste(a[i], b[i]))
}


#### Parte c ####

table(b)


#### EJERCICIO 5 ####

rm(list = ls())

f <- function(x) {
   (1 / ((x^2) + 1)) + sqrt(abs(x + 2))
}


#### Parte a ####

V <- seq(from = 0, to = 10, by = 0.5)
A <- f(V)
max(A)
which.max(A)


#### Parte b ####

f


#### Parte c ####

x <- seq(-4, 3, 0.0001)
y <- f(x)

X11(15,15)
plot(x, y, xlim = c(-4, 3), col = "red", pch = ".")


#### Parte d ####

min(y)
max(y)


#### EJERCICIO 6 ####

rm(list = ls())


#### Parte a ####

library(tigerstats)

pnormGC(1.2, region = "below", mean = 0, sd = 2, graph = TRUE)
pnormGC(2.1, region = "above", mean = 0, sd = 2, graph = TRUE)
pnormGC(c(0.4, 1), region = "between", mean = 0, sd = 2, graph = TRUE)


#### Parte b ####

pnormGC(qnorm(0.75, mean = 0, sd = 2), region = "below", mean = 0, sd = 2, graph = TRUE)


#### Parte c ####

par(oma=c(0,1,0,1))
plot(
   seq(-7, 7, 0.0001),
   dnorm(seq(-7, 7, 0.0001), sd = 2),
   col = 'red',
   pch=".",
   ylab = NA,
   xlab = NA,
   main="Densidad normal"
)
abline(v = 0, col = "grey", lty = 1)
abline(h = 0, col = "grey", lty = 1)


par(oma = c(0, 1, 0, 1))
plot(
   seq(-7, 7, 0.0001),
   pnorm(seq(-7, 7, 0.0001), sd = 2),
   col = 'red',
   pch = ".",
   ylab = NA,
   xlab = NA,
   main = "Densidad acumulada"
)
abline(v = 0, col = "grey", lty = 1)
abline(h = 0, col = "grey", lty = 1)


#### Parte d ####

pchisqGC(1.2, region = "below", df = 3, graph = TRUE)
pchisqGC(2.1, region = "above", df = 3, graph = TRUE)
pchisqGC(1, region = "below", df = 3) - pchisqGC(0.4, region = "below", df = 3)
pchisqGC(qchisq(0.75, df = 3), region = "below", df = 3, graph = TRUE)


par(oma=c(0,1,0,1))
plot(
   seq(0, 15, 0.0001),
   dchisq(seq(0, 15, 0.0001), df = 3),
   col = 'red',
   pch = ".",
   ylab = NA,
   xlab = NA,
   main = "Densidad Chi-cuadrado"
)
abline(h = 0, col = "grey", lty = 1)

par(oma = c(0, 1, 0, 1))
plot(
   seq(0, 15, 0.0001),
   pchisq(seq(0, 15, 0.0001), df = 3),
   col = 'red',
   pch=".",
   ylab = NA,
   xlab = NA,
   main = "Densidad acumulada"
)
abline(v = 0, col = "grey", lty = 1)
abline(h = 0, col = "grey", lty = 1)


#### EJERCICIO 7 ####

rm(list = ls())

library(HSAUR3)

#### Parte a ####

data("Forbes2000", package = "HSAUR3")
class(Forbes2000)
dim(Forbes2000)
str(Forbes2000)


#### Parte b ####

summary(Forbes2000)


#### Parte c ####

table(complete.cases(Forbes2000))
F2000 <- na.exclude(Forbes2000)
table(complete.cases(F2000))


#### Parte d ####

mean(F2000$sales)
sd(F2000$sales)


#### Parte e ####

library(dplyr)

select(F2000, sales, profits) %>% cor()

par(oma = c(0, 1, 0, 1))
plot(
   Forbes2000$sales,
   Forbes2000$profits,
   col = rgb(0, 0, 1, 0.1),
   pch = 16,
   ylab = "Profits",
   xlab = "Sales",
   main = "Forbes2000"
)
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")


#### Parte f ####

activos <- NULL
empresa <- NULL

for(i in 1:3) {
   activos[i] <- Forbes2000$assets[order(Forbes2000$assets, decreasing = TRUE)[i]]
   empresa[i] <- Forbes2000$name[order(Forbes2000$assets, decreasing = TRUE)[i]]
}

mayores <- as.data.frame(cbind(empresa, activos))


### Parte g ####

hist(Forbes2000$marketvalue)


#### Parte h ####

tapply(Forbes2000$sales, Forbes2000$category, mean, na.rm=TRUE)
