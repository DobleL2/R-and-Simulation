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
