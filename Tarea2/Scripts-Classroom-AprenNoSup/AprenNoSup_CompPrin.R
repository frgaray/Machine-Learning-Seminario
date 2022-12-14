# Ejemplo del uso
# de la t?cnica de componentes principales en R

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2023-1/ApreEstAut")
library(tidyverse)
# Datos
# Los datos corresponden a los scores que se han dado a 329 comunidades
# de acuerdo con los siguientes aspectos

#Climate and Terrain
#Housing
#Health Care & the Environment
#Crime
#Transportation
#Education
#The Arts
#Recreation
#Economics

#Excepto para Housing y Crime, todos los scores est?n en el sentido
# a mayor valor mejor calificaci?n
#Para Housing y Crime el sentido es el contrario
#es decir, a menor valor, mejor calificaci?n

Datos=read.delim("places.txt",
                  header = FALSE, sep='')
names(Datos)=c("Clim", "Housing", "HealthEnv", "Crime", "Transp",  "Educ", "Arts", "Recre",  "Econ", "Id")
summary(Datos)

# Las escalas de los datos no son iguales
X11()
library(GGally)
ggpairs(data=Datos[,-10], title="Datos")

# El objetivo es tratar de encontrar uno o varios ?ndices
# para resumir la informaci?n

# Se podr?an usar los datos en la escala original, otra opci?n es
# usar una escala logar?tmica para trata de estandarizar
X11()
ggpairs(data=log10(Datos[,-10]), title="Datos")


# Una de las funciones para encontrar los componentes principales en R base
# es prcomp(), la otra es princomp()
# esta ?ltima permite dar la estimaci?n de matriz de varianzas
# y covarianzas a usar

# scale es el argumento que permite escalar a varianza 1
# (por default se centran los datos a media cero)

# Los datos se pueden usar en formato dataframe con las columnas de X
# o bien f?rmula sin variable dependiente

# Ejemplo usando la escala original
R.CP=prcomp(Datos[,-10], scale = FALSE)
R.CPb=prcomp(~.,Datos[,-10],  scale = FALSE)

X11()
par(mfrow=c(1,2))
biplot(R.CP)
biplot(R.CPb)


# Para visualizar los resultados se puede usar el siguiente paquete
library(factoextra)
X11()
fviz_eig(R.CP)
fviz_pca_var(R.CP,
             col.var = "contrib")
fviz_pca_ind(R.CP)
X11()
fviz_eig(R.CPb)
fviz_pca_var(R.CPb,
             col.var = "contrib")

# M?s detalles en

print(summary(R.CP), digits=3)
round(R.CP$sdev^2, 2) #Varianzas de los CP, eigenvalores
round(R.CP$rotation, 2) #Matriz con eigenvectores
#Informaci?n usada en caso de estandarizar
round(R.CP$center, 2) #medias de las variables originales usadas para centrar los datos
round(R.CP$scale, 2)  #desviacion estandar de las variables originales, 0 si scale=FALSE
round(R.CP$x, 2) # Componentes principales de cada observaci?n
# Se pueden calcular las CP para nuevas observaciones
# usando la funci?n predict
predict(R.CP, newdata=Datos[2,-10])


# Ejemplo usando la escala logar?tmica
R.CPlog=prcomp(log10(Datos[,-10]), scale = FALSE)
print(summary(R.CPlog), digits=3)
round(R.CPlog$sdev^2, 2)
round(R.CPlog$rotation, 3)
round(R.CPlog$x[1:1,], 3)
round(R.CPlog$center, 2)
predict(R.CPlog, newdata=log10(Datos[1,-10]))

R.CPlog$rotation[,1]%*%(t(log10(Datos[1,-10]))-R.CPlog$center)
t(R.CPlog$rotation[,1])%*%R.CPlog$rotation[,1]



cor(cbind(R.CPlog$x[,1:3],log10(Datos[,-10])))

# Uso del paquete psych
library(psych)
p=9
pca <- principal(log10(Datos[,-10]), cor="cov", covar = TRUE, nfactor = p, rotate = "none",scores=TRUE)
pca
#standarized loadings son las correlaciones entre los componentes y las
#variables originales

head(pca$scores)
#Los scores o CP que se guardan en $scores
#se calculan con los standarized loadings
#para tener varianza 1, es decir, no son los que se
#obtienen directamente con los eigenvectores
#Estos se puede obtener realizando la siguiente modificaci?n:
round(pca$scores[1,]*sqrt(pca$values), 2)

#los eigenvectores originales, tambi?n se pueden obtener como:
pca$loadings%*%diag(1/sqrt(pca$values))

X11()
biplot(pca,choose=c(1,2,3))

# Nota, los CP que se obtienen con la funci?n predict de psych
# no son los mismos que los que se obtendr?an usando los eigenvectores
# originales, sin embargo, la direcci?n de ?stos casi es la misma
# pues usa una simulaci?n en la predicci?n

CPpsych=factor.scores(x = as.matrix(log10(Datos[,-10])), f = pca, method="components" )
CPpsych$scores[1,]

CPpsych2=predict.psych(pca, log10(Datos[,-10]), log10(Datos[,-10]))

pairs(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))

summary(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))
cor(cbind(CPpsfs=CPpsych$scores[,1], CPpspred=CPpsych2[,1], CPpssc=pca$scores[,1], CPor=R.CPlog$x[,1]))


# Tambi?n, sin ning?n dato es posible obtener
eigen(cov(log10(Datos[,-10])))

# Para datos binarios u ordinales, 
# se puede usar con el paquete psych, cor="mixed" que usa la funci?n mixedCor
# Otra opci?n es usar hetcor en el paquete polycor para calcular una matrix
# de correlaci?n.

pcam <- principal(cov(log10(Datos[,-10])), cor="cov", covar = TRUE, nfactor = p, rotate = "none",scores=TRUE)
pcam

# Componentes principales usando la matriz de correlaci?n
pcamcor <- principal(cov(log10(Datos[,-10])), covar = FALSE, nfactor = p, rotate = "none",scores=TRUE)
pcamcor

pcamcorb <- principal(cor(log10(Datos[,-10])), covar = FALSE, nfactor = p, rotate = "none",scores=TRUE)
pcamcorb



X11()
fviz_eig(R.CPlog)
fviz_pca_var(R.CPlog,
             col.var = "contrib")
