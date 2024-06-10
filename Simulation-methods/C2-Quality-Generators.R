# Se trata principalmente de contrastar si las muestras generadas son iid

# Para evaluar generadores de números pseudoaleatorios se recomienda emplezar baterías de contraste

# Uno de los contrastes más conocidos es el test chi-cuadrado de bondad de ajuste para el caso discreto

simres::chisq.cont.test


# Ejemplo 2.2 (Análisis de un generador congruencial continuación)
set.rng(321, "lcg", a = 5, c = 1, m = 512)  # Establecer semilla y parámetros
nsim <- 500
u <- rng(nsim)

# Gráfico resultante de aplicar la función chisq.cont.test() comparando el histograma de los valores generados con la densidad uniforme
chisq.cont.test(u, distribution = "unif", 
                nclass = 10, nestpar = 0, min = 0, max = 1)

simres::freq.test(u)

# Si el p valor del contraste chi-cuadrado es practicamente 1, esto indica que el generador no reproduce adecuadamente la 
# variabilidad de una distrubicón uniforme

# ----- TEST KOLMOGOROV-SMIRNOV ----------------
# Este contraste de hipótesis compara la función de distribución bajo la hipótesis nula con la función des distribución empírica

# Distribución empírica
curve(ecdf(u)(x), type = "s", lwd = 2)
curve(punif(x, 0, 1), add = TRUE)

# Test de Kolmogorov-Smirnov
ks.test(u, "punif", 0, 1)

# Gráfico secuencial de los valores generados
plot(as.ts(u))

# Gráfico de dispersión retardado de los valores generados
plot(u[-nsim],u[-1]) # ----> Si se observa algún tipo de patrón indicaría dependencia

# Autocorrelaciones de los valores generados
acf(u)

# Para contrastar si las diez autocorrelaciones son nulas podríamos emplear el test de Ljung-Box
Box.test(u, lag = 10, type = "Ljung")