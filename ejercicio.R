############################
#Introducción a R	         #
############################


###################
# Instalar paquetes

# Paquete base

##Llamando paquetes.

library(vegan)




##############
##Obteniendo ayuda de las funciones de los paquetes.  

?sum
help(sum)

############
##Obteniendo ayuda de temas 

help.search("sum")


###################
###Tipos de objetos


##Double

x<- 12.5
is.double(x)

##Integer

y<- 7

is.integer(y)
y<- as.integer(y)
is.integer(y)

##Caracteres

a<- "Pedro"
is.character(a)



####Vectores

vn<- c(2,5,6,7,8,9) ###Vector numérico

vc<- c("pedro", "juan", "luis", "antonio", 
       "jose", "karla")  ###Vector de caracter

vl<- vn>5&vn<9 ###Vector Lógico

###veamos los vectores

vn; vc; vl


#####Operadores lógicos y matemáticos


###R puede ser utilizada como una calculadora


5+4*(3+5)^2/pi


##Los vectores pueden ser modificados con estos operadores


vn[c(1,3:5)]+4*(3+vn[c(1,3:5)])^2/pi


##podemos obtener las caracteristicas de este vector

length(vn)
sum(vn)
sd(vn)
max(vn)
min(vn)


###############
# Construyendo Objetos
###############


### Secuencias


#Secuencias numéricas

a<- seq(from=5, to=50) 

b<- seq(from=5, to=50, by=5)

c<- seq(5,50, 5)

d<- rep(3, 14)

e<- c(rep(3, 14), rep(4, 6), seq(3,15,3))

f<- rnorm(10, mean=5, sd=3)

#Secuencias logicas

A<- paste(c("X","Y"), 1:10, sep="_")

B<- c(rep("p3", 14), rep("p4", 6))

C<- gl(2, 6, label=c("Macho", "Hembra"))

levels(C)


###Ordenar los vectores

###Funciones sort y order

sort(f)
rev(sort(f))
order(f)


###Trabajo 1. ########################################################################
# 1. Construir un 3 vectores que contengan la siguiente información de 22 individuos:	#
#	edad: 4 personas de 18 años, 3 personas de 24 años, 6 personas de 30 años		#
#	y 9 personas de 35 años (ej. 18, 18, 18 ........)                              #                	#
#	sexo: deben estar ordenados masculino, femenino las 22 personas                #    	
#	peso: un vector que tenga un peso medio de 150 y una desviación de 25          #
#                                                                                    #
# 2. Realizar algunas operaciones con los vectores                                   #
#     - Obtener la cantidad de libras/año de cada persona                            #
#     - La suma, valor máximo y mínimo, el promedio de los pesos de las mujeres y de #
#       los hombres                                                                  #
#     - Obtener un vector con los pesos de las mujeres y uno con el de hombres       #
#     - Obtener un vector con los pesos mayores de 115 y menores de 123                                                                                #                                   	#
######################################################################################



sexo<- rep(gl(2, 1, label=c("macho", "hembra")), 11)
edad<- c(rep(18,4), rep(24,3), rep(30,6), rep(35,9))
peso<- rnorm(22, mean=150, sd=25)


################

#Manipulando Objetos

################


##Vamos a unir los 3 vectores en un data frame


trab1<- data.frame(edad, sexo, peso)


## Observemos nuestra matriz

str(trab1)


#Como acceder a los datos. Indexación

trab1[1,]

trab1[,1]

trab1[1,1]

# en un vector

x<-1:10

x[4:7]

x[x > 3 & x < 8]

###queremos ver algunas carcterísticas de nuestros datos


summary(trab1[,c(1,3)])

sd(trab1[,c(1,3)])


###Queremos una matriz con los datos de las hembras


hembra<- trab1[trab1$sexo=="hembra",]
macho<- trab1[trab1$sexo=="macho",]

##Podemos graficar para ver la diferencia entre sexos

boxplot(hembra$peso, macho$peso)


###Listas

#Una lista es una colección ordenada de elementos de distinto 
#tipo. Una lista puede contener otra lista, y de este modo puede 
#utilizarse para construir estructuras de datos arbitrarias.

x <- 1:7
y <- c("Ana", "Marcos", "Juan", "Pedro", "Ramón")
z <- list(secuencia = x, nombres = y)


##Muchos resultados son mostrados como listas

mod<- lm(trab1$peso~trab1$sexo)





###Haciendo tablas bidimensionales
##Queremos ver cuantas hembras y machos por cada edad

tab<- table(trab1$sexo, trab1$edad)

##podemos sacar la proporción en de hembras y machos por cada edad


tab.p<- prop.table(tab, 1) ##por filas
tab.p1<- prop.table(tab, 2) ##por columnas




### Vamos a crear una tabla de datos nueva
#### Estamos haciendo una encuesta de ingresos por persona dependiendo de la provincia

provincia <- c("loj", "azu", "zam", "mor", "mor", "oro", "gua", "gua",
               "zam", "can", "mor", "can", "zam", "zam", "azu", "loj", "azu", "oro", 
               "gua", "can", "zam", "mor", "mor", "gua", "azu", "rios", "mor", "can", "can", "rios")

ingresos <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 
              56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

#Ahora necesitamos convertir a este vector en niveles

FactorProv <- factor(provincia)

levels(FactorProv)

###Nos interesa obtener datos a partir de los niveles.  (Lo que 
##haríamos con una tabla dinámica por ejemplo)

MediaIngresos <- tapply(ingresos, FactorProv, mean) 

##Podriamos sacar suma (sum), desviación estandar (sd), 
#valores máximos (max) o mínimos (min), 
#conteo (length)


##Vamos hacer una tabla en la cual coloquemos todos los datos descriptivos
por niveles

tabla <- matrix(0,4,8)
colnames(tabla)<-levels(FactorProv)
rownames(tabla)<- c("Media", "Max", "Min", "SD")

tabla[1,]<- tapply(ingresos, FactorProv, mean)
tabla[2,]<- tapply(ingresos, FactorProv, max)
tabla[3,]<- tapply(ingresos, FactorProv, min)
tabla[4,]<- tapply(ingresos, FactorProv, sd)


##Analisar como funciona el apply y el lapply



####Trabajo incluya una fila nueva para poner la frecuencia (Conteo) 
###por cada provincia


############################################



#########################

#Leer y grabar datos

#########################


###Cual es el directorio que estoy usando?

setwd("C:/Users/UTPL/Documents/R")

dta <-read.table("nombre del archivo.txt", header=T)


datos<- read.table("clipboard", header=T)


write.table(tabla, file="tabla.txt")





########################################################
#	Trabajo 2.														#
#	Nos interesa tener datos de los estudiantes de la UTPL con el fin de evaluar la	#
#	proveniencia de los estudiantes, el tipo de colegio del que vienen y relacionarlo	#
#	con la cantidad de materias reprobadas.                                                            	#
#	Realizar una encuesta en el aula y:									#
#	1. Construir un data frame con estos datos								#
#	2. Obtener 2 tablas con los datos generales por tipo de colegio y por cantón		# 
#	3. Sacar una proporción de materias perdídas por tipo de colegio y por cantón	#
#																#
########################################################

