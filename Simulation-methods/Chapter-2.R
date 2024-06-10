library(simres)

simres::rlcg

# Teorema de generadores congruenciales (2.1)
#   Un generador congruencial tiene periodo máximo (p=m) si y solo si:
#     1.- c y m son primos relativos (i.e.  m.c.d.(c,m)=1)
#     2.- a-1 es múltiplo de todos los factores primos de m (i.e. a = 1 mod q, para todo q factor primo de m)
#     3.- Si m es múltiplo de 4, entonces a-1 también lo ha de ser (i.e. m = 0 mod 4 entonces a = 1 mod 4)

# CONSECUENCIAS
# * Si m primo, p = m si y solo si a = 1
# * Generador multiplicativo no cumple la condición 1 (m.c.d.(0,m)=m)

# Teorema de periodo máximo (p=m-1) (2.2)
#   Un generador multiplicativo tiene periodo máximo (p=m-1) si:
#     1.- m es primo
#     2.- a es una raiz primitiva de m (i.e. el menor entero tal que a^q=1 mod m es q=m-1)

library(simres)
system.time(u <- rlcg(n = 999, 
                      seed = 543210, a = 2^16 + 3, c = 0, m = 2^31))

# xyz <- matrix(u, ncol = 3, byrow = TRUE)
xyz <- stats::embed(u, 3)
library(plot3D)
# points3D(xyz[,1], xyz[,2], xyz[,3], colvar = NULL, phi = 60, 
#          theta = -50, pch = 21, cex = 0.2)
points3D(xyz[,3], xyz[,2], xyz[,1], colvar = NULL, phi = 60, 
         theta = -50, pch = 21, cex = 0.2)


# EJEMPLO 2.1
# Dado el generador congruencial, de ciclo máximo con parametros a = 5, c =1, m =512
# a. Generar 500 valores de este generador, obtener el tiempo de CPU, representar su distribución mediante un histograma 
# (en escala de densidades) y compararla con la densidad teórica

set.rng(321, "lcg", a = 5, c = 1, m = 512)  # Establecer semilla y parámetros
nsim <- 500
system.time(u <- rng(nsim)) 
hist(u, freq = FALSE)
abline(h = 1)                   # Densidad uniforme

set.rng(321, "lcg", a = 5, c = 1, m = 512)  # Establecer semilla y parámetros
nsim <- 500
system.time(u <- rng(nsim)) 

# b. Calcular la media de las simulaciones (mean) y compararla con la teórica
mean(u)

# c. Aproximar (mediante simulación) la probabilidad del intervalo (0.4;0.8) y compararla con la teórica
# La probabilidad teórica es 0.8 - 0.4 = 0.4
sum((0.4 < u) & (u < 0.8))/nsim
mean((0.4 < u) & (u < 0.8))     # Alternativa


# El generador Mersenne-Twister es empleado por defecto en R
