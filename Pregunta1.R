#Pregunta 1

x <- c(0,1)
f <-c(0.68, 0.32)

plot(x, f, type="h", ylim=c(0,1), col="red")

n <- 43
Y <- function(i){sum(sample(x, n, f, replace=TRUE))}

encuestas <- sapply(1:400000, Y)
fr <- table(encuestas)/400000
barplot(fr)
fr["13"]

dbinom(13, 43, 0.32)

y<- 0:43
plot(y,dbinom(y, 43, 0.32),type="h", col="red")

dbinom(17, 44, 0.32)
plot(y,dbinom(y, 44, 0.32),type="h", col="red")
pbinom(16, 44, 0.32)
#mediana 
qbinom(0.5, 44, 0.32)
qbinom(0.25, 44, 0.32)

####

24*0.68 #media
24*0.68*0.32 #varianza
qbinom(0.25, 24, 0.68)  #1er quartil

y <- 0:43
plot(y, pbinom(y, 24, 0.68), type="s")










###########################################
# ensayo de bernoulli

x <- c(0,1)
f <- c(0.68, 0.32)

plot(x, f, type="h", ylim=c(0,1), col="red")
points(x, f, pch=16, col="red")

n <- 43
muestra <- sample(x, n, f, replace=TRUE)

table(muestra)/n
mean(muestra)

bar <- barplot(table(muestra)/n, ylim=c(0,1))
lines(bar, f, type="h", col="red")
points(bar, f, pch=16, col="red")

muestra <- sample(x, n, f, replace=TRUE)
muestra

Y <- function(i){sum(sample(x, n, f, replace=TRUE))}

set.seed(123)
m <- 400000
encuestas <- sapply(1:m, Y)

fr <- table(encuestas)/m
fr["13"]

dbinom(13, 43, 0.32)

xx <- names(fr)
xx

br <- barplot(table(encuestas)/m)
lines(br, dbinom(2:29, 43, 0.32), type="h", col="red")

dbinom(17, 44, 0.32)
plot(0:43, dbinom(0:43, 44, 0.32), type="h", col="red")

pbinom(16, 44, 0.32)

#####
n <- 24
x <- c(0,1)
f <- c(0.32, 0.68)

Xstar <- function(i){sum(sample(x, n, f, replace=TRUE))}
set.seed(123)
m <- 400000
encuestas <- sapply(1:m, Xstar)
mean(encuestas)
n*0.68
var(encuestas)
n*0.68*0.32
qbinom(0.25, 24, 0.68)

plot(0:24, dbinom(0:24, 24, 0.68), type="h", col="red")

46*0.32
