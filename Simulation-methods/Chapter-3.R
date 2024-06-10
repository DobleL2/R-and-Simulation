# CONVERGENCIA
#   Estamos interesados en aproximar la media teorica mu = E[X] a partir de una secuencia obtenida 
#   en una simulación, utilizando para ello la media muestral. Una justificación teórica de la
#   validez de esta aproximación es la ley débil de los grandes números Xn -> u

# Ejemplo 3.1 (Aproximación de una probabilidad)
#   Simulamos una variable aleatoria X con distribución de Bernoulli de parámetro P=0.5
p <- 0.5
set.seed(1)
nsim <- 10000 # nsim <- 100
rx <- runif(nsim) < p # rbinom(nsim, size = 1, prob = p)

# La aproximación por simulación de E[X]=p será
mean(rx) 

# Gráfico de la evolución de la aproximación
plot(cumsum(rx)/1:nsim, type = "l", lwd = 2, xlab = "Número de generaciones", 
     ylab = "Proporción muestral", ylim = c(0, 1))
abline(h = mean(rx), lty = 2)
# valor teórico
abline(h = p) 

# Detección de problemas de convergencia
#   En la ley débil se requiere como condición suficiente que E[|X_i|] < infty,
#   en caso contrario la media muestral puede no converger a una constante.
#   Un ejemplo es la distribución de Cauchy
set.seed(1)
nsim <- 10000
rx <- rcauchy(nsim) # rx <- rt(nsim, df = 2)
plot(cumsum(rx)/1:nsim, type = "l", lwd = 2, 
     xlab = "Número de generaciones", ylab = "Media muestral")

# CONCLUSION
#   Para detectar problemas de convergencia es especialmente recomendable representar la
#   evolución de la aproximación de la característica de interés sobre el número de generacioness

boxplot(rx)

# -------- Precisión ------------

# Ejemplo 3.2
#   Aproximación de la media de una distribución normal
xsd <- 1
xmed <- 0
set.seed(1)
nsim <- 1000
rx <- rnorm(nsim, xmed, xsd)

# La aproximación por simulación de la media será:
mean(rx)

# Como medida de la presición de la aproximación podemos utilizar el error máximo
2*sd(rx)/sqrt(nsim)

# Gráfico de convergencia incluyendo el error de la aproximación
n <- 1:nsim
est <- cumsum(rx)/n
# (cumsum(rx^2) - n*est^2)/(n-1) # Cuasi-varianzas muestrales
esterr <- sqrt((cumsum(rx^2)/n - est^2)/(n-1)) # Errores estándar de la media
plot(est, type = "l", lwd = 2, xlab = "Número de generaciones", 
     ylab = "Media y rango de error", ylim = c(-1, 1))
abline(h = est[nsim], lty=2)
lines(est + 2*esterr, lty=3)
lines(est - 2*esterr, lty=3)
abline(h = xmed)


