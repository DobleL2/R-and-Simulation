# Método de aceptación rechazo
#   Se trata de un método universal alternativo al de inversión para el caso de que no
#   se pueda emplear la función cuartil, pero se dispone de una expresión para la función
#   de densidad objetivo

# La idea es simular una variable aleatoria bidimensional (X,Y) con distribución uniforme 
# en el hipografo de f 

# Para simular una distribución uniforme en el hipografo A_f lo que se hace es utilizar
# una variable aleatoria auxiliar T con función de densidad g, similar a f y fácil de simular,
# y una constante c > 0 que verifica

#       f(x) <= c*g(x) forall x \in R

# ----------ALGORITMO -----------
#   f es la densidad objetivo y g es una densidad auxiliar (fácil de simular y similar a f) tq
#       f(x) <= c*g(x) forall x \in R

# Algrotimo 4.3 (Método de aceptación-rechazo; Von Neuman, 1951)
#   1.- Generar U ~ U(0,1)
#   2.- Generar T ~ g
#   3.- Si c * U * g(T) <= f(T) devolver X = T en caso contrario volver al paso 1

# Para densidades acotada en un intervalo cerrado, con f acotada superiormente
# c = M(b-a) y g(x) = 1/(b-a) 

# Algoritmo:
# 1. Generar U,V ~ U(0,1)
# 2. Hacer T = a + (b-a)V
# 3. Si M * U <= f(T) devolver X = T sino regresar a 1


# Ejemplo 4.2 (Simulación de una beta a partir de una uniforme)

# Densidad objetivo beta (línea continua) y densidad auxiliar uniforme reescalada
# (linea descontinua)

# densidad objetivo: dbeta
# densidad auxiliar: dunif
s1 <- 2
s2 <- 4
curve(dbeta(x, s1, s2), -0.1, 1.1, lwd = 2)
m <- dbeta((s1 - 1)/(s1 + s2 - 2), s1, s2) #Cota
# abline(h = m, lty = 2)
segments(0, m, 1, m, lty = 2, lwd = 2)
abline(v = 0, lty = 3)
abline(v = 1, lty = 3)
abline(h = 0, lty = 3)

# Método aceptación-rechazo para simular valores de la densidad objetivo
ngen <- 0 

rbeta2 <- function(s1 = 2, s2 = 2) {
  # Simulación por aceptación-rechazo
  # Beta a partir de uniforme
  m <- dbeta((s1 - 1)/(s1 + s2 - 2), s1, s2)
  while (TRUE) {
    u <- runif(1)
    x <- runif(1)
    ngen <<- ngen+1
    if (m*u <= dbeta(x, s1, s2)) return(x)
  }
}

rbeta2n <- function(n = 1000, s1 = 2, s2 = 2) {
  # Simulación n valores Beta(s1, s2)
  x <- numeric(n)
  for(i in 1:n) x[i]<-rbeta2(s1, s2)
  return(x)
}

# Generamos una muestra de 1000 observaciones de una Beta(2,4)
set.seed(1)
nsim <- 1000
ngen <- 0
system.time(x <- rbeta2n(nsim, s1, s2))

# Para analizar la eficiencia se puede emplear el número de generaciones
# de la distribucion auxiliar
{cat("Número de generaciones = ", ngen)
  cat("\nNúmero medio de generaciones = ", ngen/nsim)
  cat("\nProporción de rechazos = ", 1-nsim/ngen, "\n")}

# Representamos la distribucion de los valores generados y comparamos con
# la densidad teórica
hist(x, breaks = "FD", freq = FALSE, main = "")
curve(dbeta(x, s1, s2), col = 2, lwd = 2, add = TRUE)

# Ejemplo funcion cuadratica
# Definir la función
f <- function(x) {
  return((1/16) * (3 * x^2 + 2 * x + 2))
}

# Crear un vector de valores de x de 0 a 2
x <- seq(0, 2, by = 0.01)

# Calcular los valores de y correspondientes
y <- f(x)

# Graficar la función
plot(x, y, type = 'l', col = 'blue', lwd = 2,
     main = expression(f(x) == frac(1, 16) * (3 * x^2 + 2 * x + 2)),
     xlab = 'x', ylab = 'f(x)',
     xlim = c(0, 2))
grid()

ngen <- 0 

rf2 <- function() {
  # Simulación por aceptación-rechazo
  m <- f(2)
  while (TRUE) {
    u <- runif(1)
    x <- runif(1,0,2)
    ngen <<- ngen+1
    if (m*u <= f(x)) return(x)
  }
}

rf2n <- function(n = 1000) {
  # Simulación n valores Beta(s1, s2)
  x <- numeric(n)
  for(i in 1:n) x[i]<-rf2()
  return(x)
}

set.seed(1)
nsim <- 100000
ngen <- 0
system.time(x <- rf2n(nsim))

{cat("Número de generaciones = ", ngen)
  cat("\nNúmero medio de generaciones = ", ngen/nsim)
  cat("\nProporción de rechazos = ", 1-nsim/ngen, "\n")}

hist(x, breaks = "FD", freq = FALSE, main = "")
curve(f(x), col = 2, lwd = 2, add = TRUE)


# EJEMPLO: Simulación de una normal estandar

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

# Densidad objetivo (normal estándar, linea continua) y densidad auxiliar (doble exponencial,
# línea discontinua) reescalada
# densidad objetivo: dnorm
# densidad auxiliar: ddexp
c.opt <- sqrt(2*exp(1)/pi)
lambda.opt <- 1
curve(c.opt * ddexp(x), xlim = c(-4, 4), lty = 2)
curve(dnorm, add = TRUE)

# Aproximando cota optima de manera numérica en lugar de analítica
# NOTA: Cuidado con los límites
# optimize(f = function(x) dnorm(x)/ddexp(x), maximum = TRUE, interval = c(-0.5,0.5))
optimize(f = function(x) dnorm(x)/ddexp(x), maximum = TRUE, interval = c(0, 2))

# Recordemos que para establecer la condicion de rechazo tenemos
# c * U * g(T) <= f(T)

ngen <- 0

rnormAR <- function() {
  # Simulación por aceptación-rechazo
  # Normal estandar a partir de doble exponencial
  c.opt <- sqrt(2*exp(1)/pi)
  lambda.opt <- 1
  while (TRUE) {
    u <- runif(1)
    x <- rdexp(lambda.opt) # rdexpn(1, lambda.opt)
    ngen <<- ngen + 1 # Comentar esta línea para uso normal
    # if (u*exp((x^2+1)*0.5-abs(x)) <= 1) return(x)
    if (c.opt * u * ddexp(x, lambda.opt) <= dnorm(x)) return(x)
  }
}

rnormARn <- function(n = 1000) {
  # Simulación n valores N(0,1)
  x <- numeric(n)
  for(i in 1:n) x[i] <- rnormAR()
  return(x)
}

# Generamos 10000 observaciones
set.seed(1)
nsim <- 10^4
ngen <- 0
system.time(x <- rnormARn(nsim))

# Evaluamos la eficiencia
{cat("Número de generaciones = ", ngen)
  cat("\nNúmero medio de generaciones = ", ngen/nsim)
  cat("\nProporción de rechazos = ", 1-nsim/ngen, "\n")}

hist(x, breaks = "FD", freq = FALSE, main = "")
curve(dnorm, add = TRUE)