# EJEMPLO 1.1.1

# Supongamos que nos regalan un álbum con n = 75 cromos, que se venden sobres con 
# m = 6 cromos por 0.8$, y que estamos interesados en el número de sobres que hay 
# que comprar para completar la colección, por ejemplo en su valor medio.

# Podemos aproximar la distribución de número de sobres para completar la colección
# a partir de nsim = 1000 simulaciones de colecciones de cromos.


# Parámetros
n <- 75 # Número total de cromos
m <- 6  # Número de cromos en cada sobre
repe <- TRUE # Repetición de cromos en cada sobre
# Número de simulaciones
nsim <- 1000
# Resultados simulación
nsobres <- numeric(nsim)
# evol <- vector("list", nsim)
# Fijar semilla
set.seed(1)
# Bucle simulación
for (isim in 1:nsim) {
  # seed <- .Random.seed    # .Random.seed <- seed
  album <- logical(n)
  i <- 0 # Número de sobres
  while(sum(album) < n) {
    i <- i + 1
    album[sample(n,m, replace = repe)] <- TRUE
  }
  nsobres[isim] <- i
}

# Distribución del número de sobres para completar la colección (aproximada por simulación):
hist(nsobres, breaks = "FD", freq = FALSE,
     main = "", xlab = "Número de sobres")
lines(density(nsobres))

# Aproximación por simulación del número medio de sobres para completar la colección:
sol <- mean(nsobres)
sol

# Número mínimo de sobres para asegurar de que se completa la colección con una probabilidad del 95%:
nmin <- quantile(nsobres, probs = 0.95)
ceiling(nmin)

# Reserva de dinero para poder completar la colección el 95% de las veces:
ceiling(nmin)*0.8

# Aproximaciones por simulación de la distribución del número de sobres para completar la colección, 
# de su valor esperado (línea vertical continua) y del cuantil 0.95 (línea vertical discontinua).
hist(nsobres, breaks = "FD", freq = FALSE,
     main = "", xlab = "Número de sobres")
lines(density(nsobres))
abline(v = sol)
abline(v = nmin, lty = 2)

# Por supuesto, la distribución del gasto necesario para completar la colección es esta misma reescalada.
# Gráficos exploratorios de las simulaciones del gasto para completar la colección obtenidos con la función simres::mc.plot().
res <- simres::mc.plot(nsobres*0.8)

# Aproximación del gasto medio:
res$approx  # sol*0.8



# ------- SECUENCIAS CUASI-ALEATORIAS ---------------
library(randtoolbox)
n <- 2000
par.old <- par(mfrow = c(1, 3))
plot(halton(n, dim = 2), xlab = 'x1', ylab = 'x2')
plot(sobol(n, dim = 2), xlab = 'x1', ylab = 'x2')
plot(torus(n, dim = 2), xlab = 'x1', ylab = 'x2')



# ------ GENERADORES PSEUDO-ALEATORIOS ---------------
# kind:
#  * Wichmann-Hill
#  * Marsaglia-Multicarry
#  * Super-Duper
#  * Mersenne-Twister
#  * Knuth-TAOCP-2002
#  * Knuth-TAOCP

# normal.kind
#  * Kinderman-Ramage
#  * Buggy Kinderman-Ramage
#  * Ahrens-Dieter
#  * Box-Muller
#  * Inversion

# sample.kind
#  * Rounding
#  * Rejection


# ----- TIEMPO CPU ---------
tini <- proc.time()
# Código a evaluar
tiempo <- proc.time() - tini

# Tiempos de CPU durante una simulación
CPUtimeini <- function() {
  .tiempo.ini <<- proc.time()
  .tiempo.last <<- .tiempo.ini
}

CPUtimeprint <- function() {
  tmp <- proc.time()
  cat("Tiempo última operación:\n")
  print(tmp-.tiempo.last)
  cat("Tiempo total operación:\n")
  print(tmp-.tiempo.ini)
  .tiempo.last <<- tmp
}

# EJEMPLO
# Llamando a CPUtimeini() donde se quiere empezar a contar, y a CPUtimeprint() para imprimir el tiempo
# total y el tiempo desde la última llamada a una de estas funciones. Ejemplo:
funtest <- function(n) mad(runif(n)) 
CPUtimeini()
result1 <- funtest(10^6)
CPUtimeprint()

result2 <- funtest(10^3)
CPUtimeprint()

