# Simulacion de una distribucion mixta mediante el metodo de inversion generalizado

# Consideramos la variable aleatoria con funcion de distribucion dada por

fdistr <- function(x) {
  ifelse(x < 0, 0,
         ifelse(x < 1/5, x/2 + 1/10,
                ifelse(x <= 9/10, x + 1/10, 1) ) )
}

# Representacion
curve(fdistr, from = -0.1, to = 1.1, type = 's', main = '')
abline(h = c(1/10, 2/10, 3/10), lty = 2) # Discontinuidades

# NOTA: Esta variable toma los valores 0 y 1/5 con probabilidades 1/10

# a. Diseñar un algoritmo basado en el metodo de inversion generalizado
# para generar observaciones de esta variable

# Recordar el algoritmo general
#   1. Generar U ~ U(0,1)
#   2. Devolver X = Q(U)

# EN ESTE CASO
#   1. Generar U ~ U(0,1)
#   2. Si U < 1/10 devolver X = 0
#   3. En caso contrario, si U < 2/10 devolver X = 2(U-1/10)
#   4. En caso contrario, si U < 3/10 devolver X = 2/10
#   5. Caso contrario devolver X = U - 1/10


# b. Implementar el algoritmo en una función que permita generar nsim valores de esta variable

# IMPLEMENTACION
# Función cuantil:
fquant <- function(u) 
  ifelse(u < 1/10, 0,
         ifelse(u < 2/10, 2*(u - 1/10),
                ifelse(u < 3/10, 1/5, u - 1/10) ) )
# Función para generar nsim valores:
rx <- function(nsim) fquant(runif(nsim))

# Ejemplo
set.seed(1)
nsim <- 10^4
system.time(simx <- rx(nsim))

hist(simx, breaks = "FD", freq = FALSE)

# Comparacion de funciones de distribucion
curve(ecdf(simx)(x), from= -0.1, to = 1.1, type = "s")
curve(fdistr(x), type = "s", lty = 2, add = TRUE)