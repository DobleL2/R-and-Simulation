

# ----------------- Entorno de trabajo ---------------------------

# Te indica el nombre de las variables que se encuentran en el espacio de trabajo
ls()
typeof(ls())

# Eliminar algun dato guardado en memoria
remove(x)

# Eliminar todos los objetos existentes
rm(list=ls())

# Eliminar todos los objetos existentes dado algun patron
rm(list=ls(pattern="new"))

# Asignar el valor a una variable
x <- 5
5 -> y
z <- list(a=1,b=2)

# Verificar los objetos almacenados en el workspace
objects()
typeof(objects())

# Verificar la ubicacion donde esta ubicado el archivo
getwd()

# Abre el cuadro de ayda respecto a la funcion de la que necesitas ayuda
help(options)

# -------------- Creacion de vectores ---------------------------

# Valores logicos
logi_var <- c(TRUE,FALSE,TRUE,TRUE) 
logi_var

# Valores enteros
int_var <- c(2L,4L,7L,5L)
int_var

# Valores decimales
dbl_var <- c(2.3, 6.8, 4.1)
dbl_var

# Valores complejos
cpl_var <- c(3+6i,5-2i,-8i)
cpl_var

# Valores caracteres
chr_var <- c("statistical","model","test")
chr_var


# --------------- Metodos de vectores ------------------------------

mode(logi_var) # Ver el tipo de dato en el vector
length(logi_var) # Ver la longitud del vector
typeof(int_var) # Ver la raiz del tipo de dato por ejemplo en numeric hay (integer o double)

# --------------- Vectores vacios ----------------------------------

vacio_numeric <- vector("numeric",10) # 0
vacio_numeric

vacio_logical <- vector("logical",5) # FALSE
vacio_logical

vector_complex <- vector("complex",4) # 0+0i
vector_complex 

vector_character <- vector("character") # ""
vector_character 

# ---------------- Revisar NAs ------------------------------------

x_vec <- c(2,3,NA,8)

# Devuelve un vector de TRUE y FALSE
is.na(x_vec)

# Devuelve la posicion donde el na
which(is.na(x_vec))

# Devuelve la posicion donde no hay na
which(!is.na(x_vec))

# Filtrar dada una condicion en un nuevo vector
new_x_vec <- x_vec[which(!is.na(x_vec))]


# ------------------- Verificar tipo de datos y cambiar ----------

# Comprobaciòn
is.logical(x_vec)
is.numeric(x_vec)
is.complex(x_vec)
is.double(x_vec)
is.integer(x_vec)
is.character(x_vec)

# Cambio
as.logical(x_vec)
as.numeric(x_vec)
as.complex(x_vec)
as.double(x_vec)
as.integer(x_vec)
as.character(x_vec)

# ------------------- Diferentes tipos de NA ---------------------

NA_real_
NA_integer_
NA_character_
NA_complex_

# ---------------- Generar sequencias y repeticiones ------------------

1:5
seq()
seq(1,5)
seq(1,10,2)
seq(1,10,length.out=100)
seq(from=-2,to=3,length=10)

rep(3,8)
rep(c(3,6,9),times=2)
rep(c(2,4,6),each=2,times=2)
rep(c('A','B'),times=2)

# ----------------------- Operaciones basicas -----------------------

3 + 2 
3 - 2
3 * 2
5 / 3
3 ^ 2
sqrt(16)
abs(-4)
exp(1)
log(9,3)
factorial(4)

# Modulo
5 %% 2

# Division entera
5 %/% 3

# Operadores de comparacion
4 == 3
4 != 3
4 <= 3
4 >= 3

# Operadores logicos
T && F
T || F

# Operadores logicos vectores
v1 <- c(TRUE, FALSE, TRUE)
v2 <- c(TRUE, TRUE, FALSE)

v1 & v2
v1 | v2

# Negacion
!v1

v3 <- c(3,4,2,5)
v4 <- c(1,2,3,4)
sum(v3)
prod(v3)

# Operacoines vectores
v3+v4
exp(v4)
log(v4,2)

# ----------------- Dar nombre a los elementos de un vector -------

y <- 1:5
names(y) <- c("uno","dos","tres","cuatro","cinco")
attr(y,"names") <- c("uno","dos","tres","cuatro","cinco")


# ----------------- Modificando vectores --------------------------

# Crear un vector
vec <- c(2,4,6,8)

# Accedemos al elemento de la posicion 2
vec[2]

# Agregar elementos al final
vec <- c(vec,c(15,18))

# Agregar valores al inicio
vec <- c(-1,vec)

# Agregar un elemento en una posicion intermedia
vec <- c(vec[1:4],-4,vec[5:7])

# Eliminamos el valor alguna posicion 
vec <- vec[-2]

# Eliminamos varias posiciones
vec <- vec[-c(4,7)]

# Modificar elementos
vec[1] <- 0

# ------------------------- Ordenar Vectores ----------------------

v5 <- c(5, -3.2, 6.7, 0.21, 4.6, -1.23, 0, 9.2, -5.34)

# Ordena en orden descendente por defecto se encuentra como orden ascendente
sort(v5,decreasing = TRUE)

# Devuelve los indices del vector en orden
order(v5,decreasing = TRUE)

# ----------------- FACTORES --------------------------------------

# Vector de valores
vec_gen <- c("m", "f", "f", "m", "m", "f")

# Revisar la clase del vector
class(vec_gen)

# Genera una tabla de frecuencias de los valores en el vector
table(vec_gen)

# Creacion del factor
genero <- factor(vec_gen,
                 levels = c("m", "f", "o"),
                 labels = c("Masculino", "Femenino", "Otro"), ordered = FALSE)

# Imprime la tabla ya con los labels asignados
table(genero)


# ------------------------ Matrices y Arrays ------------------------------

# Crear una matriz a partir de un vector
matriz <- matrix(1:9, nrow=3, ncol =3)
matriz

# Crear un arreglo a partir de un vector
arreglo <- array(1:24, dim = c(2,3,4))
arreglo

# Crear una matriz utilizando vectores como filas
vec1 <- c(1,3,5)
vec2 <- c(1,3,5) + 2

Mat1 <- rbind(vec1,vec2)
Mat1

# Crear una matriz utilizando vectores como columnas
Mat2 <- cbind(vec1,vec2)
Mat2

# Acceder a los valores de una matriz
Mat2[2,2]

# Acceder a una fila de la matriz
Mat2[2,]

# Acceder a una columna de la matriz
Mat2[,2]

# Modificar un valor 
Mat2[2,2] <- 3

# Modificar una fila 
Mat2[2,] <- c(1,2)

# Modificar una columna 
Mat2[,2] <- c(1,2,2)

# ------------------------------- Operaciones de Matrices ---------------------

# Transpuesta
t(Mat2)

# Suma (deben tener las mismas dimensiones)
Mat1 + t(Mat2)

# Multiplicacion matrices
Mat1 %*% Mat2

# Creamos una matriz cuadrada
matriz_cuadrada <- matrix(sample(seq(1,20),size=9),nrow=3)
matriz_cuadrada

# Inversa
solve(matriz_cuadrada)

# Determinante
det(matriz_cuadrada)

# Obtener la diagonal de una matriz cuadraada
diag(matriz_cuadrada)

# Crear una matriz diagonal
diag(c(1,2,3))

# Crear la matriz identidad
diag(4)

# Obtener los valores y vectores propios
eigen(matriz_cuadrada)


# --------------------------- Listas -------------------------------------

# Ejemplo de una lista de distintos tipos
lista <- list(vec1=c(2L, 5L, 8L), mat=diag(4), vec2=c("a", "b", "c", "d", "e"))
lista

# Lista de vectores
lista1 <- list(impar=c(1,3,5,7,9), par=c(2,4,6,8,10))
str(lista1)

# Lista de matrices
lista2 <- list(I2=diag(2), I3=diag(3))
str(lista2)

# Lista de listas
lista3 <- list(LST1=list(a=1, b=c(2,2)), LST2=list(txt="Ecuador", dia="Lunes"))
str(lista3)

# Concatenacioón de listas
lista_completa <- c(lista1, lista2, lista3)
str(lista_completa)


# ------------------------------- DataFrames -----------------------------

# Creando un dataframe
datos <- data.frame(vec1=seq(1,4), vec2=c(T,F,T,F), vec3=LETTERS[1:4])
datos

# Creando un dataframe en el caso de que no se tenga la misma longitud
datos1 <- data.frame(vec1=seq(1,4), vec2=c(T,F,T,F), vec3=LETTERS[1:4],
                     vec4=c(-1,-3), vec5=c("EC"))
datos1
str(datos1)

rownames(datos1) <- paste0(datos1$vec1,datos$vec3)
datos1
set.seed(123)
datos1$norm <- rnorm(4,4,2)
subset(datos1,norm>4)

# Modificamos la cuarta variable por los valores -2, 5, 8, -9
datos1[,4] <- c(-2, 5, 8, -9)
datos1

# Modificamos la primera observación
datos1[1,c(1,4)] <- c(0, -3)
datos1[1,2] <- FALSE
datos1[1,c(3,5)] <- c("B", "PR")

# Añadimos la variable vec6 con los valores 0.5, 1.2, 3.4, 5.6
datos1$vec6 <- c(0.5, 1.2, 3.4, 5.6)
datos1

# Otra forma de agregar una variable (columna) al dataframe
# Añadimos la variable vec7 con los valores TRUE, FALSE, FALSE, TRUE
datos1 <- data.frame(datos1, vec7=c(TRUE, FALSE, FALSE, TRUE))

# Añadimos la quinta observación
datos1[5,c(1,4,6)] <- c(4, -1, 2.8)
datos1[5,c(2,7)] <- c(TRUE, TRUE)
datos1[5,c(3,5)] <- c("A", "EC")
datos1

# Seleccionamos la primera, segunda y cuarta variable
datos1[,c(1,2,4)]

# Filtrar un dataframe dada alguna condicion
subset(datos1, vec2==F)

# Filtrar un dataframe dada alguna condicion y seleccionamos algunas columnas
subset(datos1, vec2==F & vec1 >=2, select = c(vec1,vec2,vec4)) 

# Creamos vec8 como suma de vec4 y vec6
datos1$vec8 <- datos1$vec4 + datos1$vec6
datos1

# Creamos vec9 como el doble producto de vec8 menos 5
datos1$vec9 <- 2*datos1$vec8 - abs(datos1$vec4) + 5
datos1

# Eliminacion de variables
datos1[,2] <- NULL
datos1$vec4 <- NULL
datos1