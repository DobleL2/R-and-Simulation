# Simulación de variables discretas X

# Como partida una U(0,1), la idea general consiste en asociar a cada
# posible valor x_i de X un subintervalo de (0,1) de longitud igual a 
# la correspondiente probabilidad

# Simular Bernoulli(p)
p <- 0.5
x <- runif(nsim) < p

# Algoritmo 5.1 distribucion uniforme discreta
#   1. Generar U ~ U(0,1)
#   2. Devolver X = floor(nU) + 1

# Funcion por defecto de R para generar variables discretas con
# dominio finito en R
valores <- c(1,5,6)
nsim <- 10000
prob <- c(1/4,1/2,1/4)

hist(sample(x = valores, size = nsim, replace = TRUE, prob))

# Definicion de la funcion quantil o inversa generalizada
# Q(u) = inf{x in R: F(x)>= u} , for all u in (0,1)

# Algoritmo 5.2 (de transformacion cuantil)
#   1.- Generar U ~ U(0,1)
#   2.- Devolver X = Q(U)

# Algoritmo 5.3 (de transformacion cuantil con busqueda secuencial)
#   1.- Generar U ~ U(0,1)
#   2.- Hacer I = 1 y S = p_I
#   3.- Mientras U > S hacer I = I + 1 y S = S + p_I
#   4.- Devolver X = x_I

# El algoritmo anterior es valido independientemente de que los valores
# que toma la variable esten ordenados

# -------------- IMPLEMENTACION ----------------------
rpmf0 <- function(x, prob = 1/length(x), n = 1000) {
  X <- numeric(n)
  U <- runif(n)
  for(j in 1:n) {
    i <- 1
    Fx <- prob[1]
    while (Fx < U[j]) {
      i <- i + 1
      Fx <- Fx + prob[i]
    }
    X[j] <- x[i]
  }
  return(X)
}

# Este algoritmo esta implementado de manera optima en la libreri simres
library(simres)
rpmf


# Ejemplo 5.1 (Simulacion de una binomial mediante el metodo de la transformaicon quantil)
set.seed(1)
n <- 10
p <- 0.5
nsim <- 10^5
x <- 0:n
pmf <- dbinom(x, n, p)
plot(x,pmf)
system.time( rx <- rpmf(x, pmf, nsim) )
hist(rx)

# Aproximacion de la media
mean(rx)

# Numero medio de observaciones para generar cada observacion
ncomp <- attr(rx, "ncomp")
ncomp/nsim

# Representamos la aproximacion por simulacion de la funcion de masa
# de probabilidad y la comparamos con la teorica
res <- table(rx)/nsim
# res <- table(factor(rx, levels = x))/nsim
plot(res, ylab = "frecuencia relativa", xlab = "valor")
points(x, pmf, pch = 4, col = "blue")  # Comparación teórica

# Comparaciones numericas
res <- as.data.frame(res)
names(res) <- c("x", "psim")
res$pteor <- pmf
print(res, digits = 2)

# Máximo error absoluto 
max(abs(res$psim - res$pteor))

# Máximo error absoluto porcentual 
100*max(abs(res$psim - res$pteor) / res$pteor)

# Nota:. Puede ocurrir que no todos los valores sean generados en la simulación. En el código anterior si length(x) > length(psim), la sentencia res$pteor <- pmf generará un error. Una posible solución sería trabajar con factores (llamar a la función rpmf() con as.factor = TRUE o emplear res <- table(factor(rx, levels = x))/nsim).


# ALGORITMO DE LA TABLA GUIA
rpmf.table
## function(x, prob = 1/length(x), m, n = 1000, as.factor = FALSE) {
##   # Inicializar tabla y FD
##   Fx <- cumsum(prob)
##   g <- rep(1,m)
##   i <- 1
##   for(j in 2:m) {
##     while (Fx[i] < (j-1)/m) i <- i + 1
##     g[j] <- i
##   }
##   ncomp <- i - 1
##   # Generar valores
##   X <- numeric(n)
##   U <- runif(n)
##   for(j in 1:n) {
##     i <- i0 <- g[floor(U[j] * m) + 1]
##     while (Fx[i] < U[j]) i <- i + 1
##     ncomp <- ncomp + i - i0
##     X[j] <- x[i]
##   }
##   if(as.factor) X <- factor(X, levels = x)
##   attr(X, "ncomp") <- ncomp
##   return(X)
## }
## <bytecode: 0x000001c2be34fe00>
## <environment: namespace:simres>


# Ejemplo 5.3 (Simulacion de una binomial mediante tabla guia)
set.seed(1)
system.time( rx <- rpmf.table(x, pmf, n-1, nsim) )

# Numero medio de comparaciones
ncomp <- attr(rx, "ncomp")
ncomp/nsim

sum((1:length(x))*pmf) # Numero esperado con búsqueda secuencial

# Analisis de los resultados
res <- table(rx)/nsim
plot(res, ylab = "frecuencia relativa", xlab = "valores")
points(x, pmf, pch = 4, col = "blue")  # Comparación teórica


# ALGORITMO METODO ALIAS DE SIMULACION
rpmf.alias
## function(x, prob = 1/length(x), n = 1000, as.factor = FALSE) {
##   # Inicializar tablas
##   a <- numeric(length(x))
##   q <- prob*length(x)
##   low <- q < 1
##   high <- which(!low)
##   low <- which(low)
##   while (length(high) && length(low)) {
##     l <- low[1]
##     h <- high[1]
##     a[l] <- h
##     q[h] <- q[h] - (1 - q[l])
##     if (q[h] < 1) {
##       high <- high[-1]
##       low[1] <- h
##     } else low <- low[-1]
##   } # while
##   # Generar valores
##   V <- runif(n)
##   i <- floor(runif(n)*length(x)) + 1
##   X <- x[ ifelse( V < q[i], i, a[i]) ]
##   if(as.factor) X <- factor(X, levels = x)
##   return(X)
## }
## <bytecode: 0x000001c2bb3f42b0>
## <environment: namespace:simres>

# Ejemplo 5.4 (Simulacion de una binomial mediante el metodo Alias)
set.seed(1)
system.time( rx <- rpmf.alias(x, pmf, nsim) )

# Analizamos los resultados
res <- table(rx)/nsim
plot(res, ylab = "frecuencia relativa", xlab = "valores")
points(x, pmf, pch = 4, col = "blue")  # Comparación teórica

# -------- VARIABLE DISCRETA CON DOMINIO INFINITO ----------
# Ejemplo 5.5 (Distribucion de Poisson)

# Probabilidades: p_j = P(X=x_j) = P(X=j-1)= (e^lambda)(lambda^{j-1})/(j-1)! ,  j=1,2,3,...

# ALGORITMO Poisson
#   1. Generar U ~ U(0,1)
#   2. Hacer I = 0, p = e^lambda  y S=p
#   3. Mientras U > S hacer I = I + 1, p = lambda/I * p y S = S+p
#   4. Devolver X = I

# ALGORITMO Geometrica
#   1.- Hacer lambda = -ln(1-p)
#   2.- Generar U ~ U(0,1)
#   3.- Hacer T = - ln(U)/ lambda
#   4.- Devolver X = floor(T)

# Algoritmo 5.8 (Binomial negativa o tambien conocida como Gamma-Poisson)
#   1.- Simular L ~ Gamma (r, p/(1-p))
#   2.- Simular X ~ Poiss(L)
#   3.- Devolver X

# Empleando una aproximacion similar podriamos generar otras distribuciones
# como la Beta-Binomial, empleadas en inferencia Bayesiana

