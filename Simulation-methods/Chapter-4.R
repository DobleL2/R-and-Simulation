# Simulación de variables continuas

# Si X es una variable aleatoria con función de distribución F continua y estrictamente
# monótona (invertible), entonces

#   U = F(X) ~ U(0,1)

# El recíproco también es cierto, si U ~ U(0,1) entonces F^-1(U) ~ X

# Algoritmo genérico para simular una variable continua 

# Algoritmo 4.1 (Método de inversión)
#   1. Generar U ~ U(0,1)
#   2. Devolver X = F^-1(U)

# Simulación de una distribución exponencial
tini <- proc.time()

lambda <- 2
nsim <- 10^2
set.seed(1)
u <- runif(nsim)
x <- -log(u)/lambda # -log(1-u)/lambda

tiempo <- proc.time() - tini
tiempo

hist(x, breaks = "FD", freq = FALSE, 
     main = "", xlim = c(0, 5), ylim = c(0, 2.5))
curve(dexp(x, lambda), lwd = 2, add = TRUE)

# Ejercicio: Distribución doble exponencial
# a. Escribir una función que permita generar, por el método de inversión, una muestra de n
# observaciones de esta distribución
ddexp <- function(x, lambda = 1){
  # Densidad doble exponencial
  lambda*exp(-lambda*abs(x))/2
}

rdexp <- function(lambda = 1){
  # Simulación por inversión
  # Doble exponencial
  u <- runif(1)
  if (u<0.5) {
    return(log(2*u)/lambda)
  } else {
    return(-log(2*(1-u))/lambda)
  }
}

rdexpn <- function(n = 1000, lambda = 1) {
  # Simulación n valores de doble exponencial
  x <- numeric(n)
  for(i in 1:n) x[i]<-rdexp(lambda)
  return(x)
}

# b. Generar 10^4 valores de la distribución doble exponencial de parámetro lambda = 2 y 
# obtener el tiempo de CPU que tarda en generar la secuencia
set.seed(54321)
system.time(x <- rdexpn(10^4, 2))

# c. Representar el histograma y compararlo con la densidad teórica
hist(x, breaks = "FD", freq = FALSE, main="")
# lines(density(x), col = 'blue')
curve(ddexp(x, 2), add = TRUE)

# Algoritmo 4.2 (de Odeh y Evans)
#   1. Generar U ~ U(0,1)
#   2. Si U < 10^-20 ó U > 1-10^-20 entonces volver a 1
#   3. Si U < 0.5 entonces hacer X = g(1-U) en caso contrario hacer X = -g(U)
#   4. Devolver X.