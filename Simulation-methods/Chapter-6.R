# Simulacion de datos funciones o temporales
nsim <- 20
n <- 100
t <- seq(0, 1, length = n)
# Media
mu <- sin(2*pi*t)
# Covarianzas
t.dist <- as.matrix(dist(t))
x.cov <- exp(-t.dist)

# Factorizacion
U <- chol(x.cov)
L <- t(U)

# Simular una realizacion
set.seed(1)
head(mu + L %*% rnorm(n))

# Simular
z <- matrix(rnorm(nsim * n), nrow = n)
x <- mu + L %*% z

matplot(t, x, type = "l", ylim = c(-3.5, 3.5))
lines(t, mu, lwd = 2)

# Usando la libreria
library(MASS)
mvrnorm


x <- mvrnorm(nsim, mu, x.cov)

matplot(t, t(x), type = "l")
lines(t, mu, lwd = 