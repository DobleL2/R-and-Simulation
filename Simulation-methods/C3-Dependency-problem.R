# Ejemplo 3.3 (aproximación de una proporción bajo dependencia cadena de Markov)
#   Supongamos que en A Coruña llueve de media uno de cada tres días al año, y que la 
#   probabilidad de que un día llueva solo depende de lo que ocurrió el día anterior
#   siendo 0.94 si el día anterior llovío y 0.03 si no

# Valores de la variable indicadora de día lluvioso con el siguiente código
# Variable dicotómica 0/1 (FALSE/TRUE)  
set.seed(1)
nsim <- 10000
alpha <- 0.03 # prob de cambio si seco
beta <- 0.06  # prob de cambio si lluvia
rx <- logical(nsim) # x == "llueve"
rx[1] <- FALSE # El primer día no llueve
for (i in 2:nsim)
  rx[i] <- if (rx[i-1]) runif(1) > beta else runif(1) < alpha

# Gráfico de convergencia asumiendo independencia
n <- 1:nsim
est <- cumsum(rx)/n
esterr <- sqrt(est*(1-est)/(n-1)) # OJO! Supone independencia
plot(est, type="l", lwd=2, ylab="Probabilidad", 
     xlab="Número de simulaciones", ylim=c(0,0.6))
abline(h = est[nsim], lty=2)
lines(est + 2*esterr, lty=2) 
lines(est - 2*esterr, lty=2)
abline(h = 1/3, col="darkgray") # Probabilidad teórica

# Probabilidad teórica, empleando resultados de cadenas de Markov, es p = 1/3 y la 
# aproximación de la propoción sería correcta (es consistente)
est[nsim]

# Al ser datos dependientes, la aproximación anterior del error estándar no es adecuada
esterr[nsim]

# En este caso al haber dependencia positiva se produce una subestimación del verdadero
# error estándar.
# Correlograma de la secuencia indicadora de días de lluvia
acf(as.numeric(rx))

# El gráfico anterior sugiere que si solo tomamos 1 de cada 25 valores podría ser razonable asumir independencia

# Correlograma de la subsecuencia de días de lluvia obtenida seleccionando uno de cada 25 valores
lag <- 24
xlag <- c(rep(FALSE, lag), TRUE)
rxi <- rx[xlag]
acf(as.numeric(rxi))

# Gráfico de convergencia de la aproximación de la probabilidad a partir de la subsecuencia de días de lluvia
# (Calculando el error de aproximación asumiendo independencia)
nrxi <- length(rxi)
n <- 1:nrxi
est <- cumsum(rxi)/n
esterr <- sqrt(est*(1-est)/(n-1))
plot(est, type="l", lwd=2, ylab="Probabilidad", 
     xlab=paste("Número de simulaciones /", lag + 1), ylim=c(0,0.6))
abline(h = est[length(rxi)], lty=2)
lines(est + 2*esterr, lty=2) # Supone independencia

# Esta forma de proceder podría ser adecuada para tratar de aproximar la precisión:
esterr[nrxi]

# pero no sería la más eficiente para aproximar la media. (Sería mejor usar todas las aproximaciones)
lines(est - 2*esterr, lty=2)
abline(h = 1/3, col="darkgray")     # Prob. teor. cadenas Markov


# Se podría considerar las medias de grupos de 25 valores consecutivos y suponer independencia
# Gráfico de convergencia de las medias por lotes
rxm <- rowMeans(matrix(rx, ncol = lag + 1, byrow = TRUE))
nrxm <- length(rxm)
n <- 1:nrxm
est <- cumsum(rxm)/n
esterr <- sqrt((cumsum(rxm^2)/n - est^2)/(n-1)) # Errores estándar
plot(est, type="l", lwd=2, ylab="Probabilidad", 
     xlab=paste("Número de simulaciones /", lag + 1), ylim=c(0,0.6))
abline(h = est[length(rxm)], lty=2)
lines(est + 2*esterr, lty=2) # OJO! Supone independencia
lines(est - 2*esterr, lty=2)
abline(h = 1/3, col="darkgray")     # Prob. teor. cadenas Markov


# Alternativamente
# Se podría recurrir a la generación de múltiples secuencias independientes entre sí

# Variable dicotómica 0/1 (FALSE/TRUE)  
set.seed(1)
nsim <- 1000
nsec <- 10
alpha <- 0.03 # prob de cambio si seco
beta <- 0.06  # prob de cambio si lluvia
rxm <- matrix(FALSE, nrow = nsec, ncol= nsim)
for (i in 1:nsec) {
  # rxm[i, 1] <- FALSE # El primer día no llueve
  # rxm[i, 1] <- runif(1) < 1/2 # El primer día llueve con probabilidad 1/2
  rxm[i, 1] <- runif(1) < 1/3 # El primer día llueve con probabilidad 1/3 (ideal)
  for (j in 2:nsim)
    rxm[i, j] <- if (rxm[i, j-1]) runif(1) > beta else runif(1) < alpha
}

# La idea sería considerar las medias de las series como una muestra independiente de una nueva variable 
# y estimar su varianza de forma habitual

# Media de cada secuencia
n <- 1:nsim
est <- apply(rxm, 1, function(x) cumsum(x)/n)
matplot(n, est, type = 'l', lty = 3, col = "lightgray",
        ylab="Probabilidad", xlab="Número de simulaciones")
# Aproximación
mest <- apply(est, 1, mean)
lines(mest, lwd = 2)
abline(h = mest[nsim], lty = 2)
# Precisión
mesterr <- apply(est, 1, sd)/sqrt(nsec)
lines(mest + 2*mesterr, lty = 2)
lines(mest - 2*mesterr, lty = 2)
# Prob. teor. cadenas Markov
abline(h = 1/3, col="darkgray")     

# Aproximación final
mest[nsim] # mean(rxm)

# Error estándar
mesterr[nsim]


# ---- PERIODO DE CALENTAMIENTO ----------------
# En el caso de datos dependientes (simulación dinámica) pueden aparecer probelmas de  estabilización
# Puede existir una evolución lenta hasta alcanzar una distribución estacionaria, siendo sensible
# a condiciones iniciales

# Ignorar los resultados obtenidos durante un cierto periodo inicial de tiempo (periodo de
# calentamiento o estabilización), tiene como obetivo conseguir que se establice la distribución
# de probabilidad

set.seed(2)
nsim <- 10000
rx2 <- logical(nsim)
rx2[1] <- TRUE # El primer día llueve
for (i in 2:nsim)
  rx2[i] <- if (rx2[i-1]) runif(1) > beta else runif(1) < alpha
n <- 1:nsim
est <- cumsum(rx)/n
est2 <- cumsum(rx2)/n
plot(est, type="l", ylab="Probabilidad", 
     xlab="Número de simulaciones", ylim=c(0,0.6))
lines(est2, lty = 2)
# Ejemplo periodo calentamiento nburn = 2000
abline(v = 2000, lty = 3)
# Prob. teor. cadenas Markov
abline(h = 1/3, col="darkgray")     



# Ejemplo 3.4 (simulación de un proceso autorregresivo serie de tiempo)
nsim <- 200   # Numero de simulaciones
xmed <- 0     # Media
rho <- 0.5    # Coeficiente AR
nburn <- 10   # Periodo de calentamiento (burn-in)

# Fijando la varianza del error
evar <- 1
# Varianza de la respuesta
xvar <- evar / (1 - rho^2)

# La recomendación sería fijar la varianza de la respuesta
xvar <- 1     
# Varianza del error
evar <- xvar*(1 - rho^2)

# Para simular la serie, al ser un AR(1) normalmente simularíamos el primer valor
#x[1] <- rnorm(1, mean = xmed, sd = sqrt(xvar))

# Como ejemplo nos alejamos un poco de la distribución estacionaria, para que el 
# "periodo de calentamiento" sea mayor:
set.seed(1)
x <- numeric(nsim + nburn)
# Establecer el primer valor 
x[1] <- -10
# Simular el resto de la secuencia
for (i in 2:length(x))
  x[i] <- xmed + rho*(x[i-1] - xmed) + rnorm(1, sd=sqrt(evar))
x <- as.ts(x)
plot(x)
abline(v = nburn, lty = 2)

# Eliminamos el periodo de calentamiento
rx <- x[-seq_len(nburn)]