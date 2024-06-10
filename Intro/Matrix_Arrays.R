
# Sampling data 
valores <- c(0,1,2,3) 
probabilidades <- c(0.3,0.2,0.4,0.5)
n <- 100
sample(valores,n,T,probabilidades)[1:3]


# ----------- Valores y vectores propios ------------------------------

#
C <- matrix(sample(seq(1,20), size=9), nrow = 3, byrow = FALSE)

matrix(eigen(C)$vectors[2,],nrow=3,byrow=TRUE)

diag(c(1,2,3))

creador_matrices <- function(num){
  return(diag(1:num))
}

c(creador_matrices(7),creador_matrices(7))

c1 <- matrix(sample(seq(1,20), size=6), nrow = 2, byrow = FALSE)
c1
c2 <- matrix(sample(seq(1,20), size=6), nrow = 2, byrow = FALSE)
c2

my_array <- array(c(c1,c2),c(2,3,2))
my_array
my_array[, , 1]

vec_y <- sample(seq(1,20), size=12)
array(vec_y,c(2,3,2))

# El dolar es equivalente al doble corchete

lista <- list(c(2L, 5L, 8L), diag(4), c("a", "b", "c", "d", "e"))
attr(lista,"names") <- c("lista1","matriz1","lista2")
lista
attributes(lista)


# ---------------------------- Data Table con una columna de matrices ---------

# Importamos la libreria data.table
library(data.table)

# Creamos el data table con una columna que representa los ids
dt <- data.table(id = 1:3)

# Agregramos la columna que tiene como elementos matrices 
dt$matrices <- list(matrix(1:4,nrow=2),matrix(5:8,nrow=2),matrix(9:12,nrow=2))

# Accediendo a la primera matriz
dt$matrices[[1]]

# Accediendo al elemento 1,1 de la matriz 1
dt$matrices[[1]][1,1]



# ----------- Crear una dataframe con una columna de matrices --------------------

# Creamos una lista de matrices
list_of_matrices <- list(matrix(c(1,0,1,0),nrow=2),matrix(c(1,0,1,0),nrow=2),matrix(c(1,0,1,0),nrow=2),matrix(c(1,0,1,0),nrow=2))

# Creamos el dataframe y agregamos una columna cualquiera con tres elementos
dt1 <- data.frame(col1 = 1:4)

dt1
# Agregamos la columna de matrices
dt1$col2 <- list_of_matrices

# Agregamos la columna 3
dt1$col3 <- c("Hola","Hola","Hola","Hola")

# Leemos la primera matrz almacenada
dt1$col2[[1]]

# Leemos el elemento 1,1 de la primera matriz
dt1$col2[[1]][1,1]