# Ejemplo del uso
# de la técnica de Análisis Factorial Exploratorio en R

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

# Más detalles
#Climate & Terrain: very hot and very cold months, seasonal temperature variation, heating- and cooling-degree days, freezing days, zero-degree days, ninety-degree days.
#Housing: utility bills, property taxes, mortgage payments.
#Health Care & Environment: per capita physicians, teaching hospitals, medical schools, cardiac rehabilitation centers, comprehensive cancer treatment centers, hospices, insurance/hospitalization costs index, flouridation of drinking water, air pollution.
#Crime: violent crime rate, property crime rate.
#Transportation: daily commute, public transportation, Interstate highways, air service, passenger rail service.
#Education: pupil/teacher ratio in the public K-12 system, effort index in K-12, accademic options in higher education.
#The Arts: museums, fine arts and public radio stations, public television stations, universities offering a degree or degrees in the arts, symphony orchestras, theatres, opera companies, dance companies, public libraries.
#Recreation: good restaurants, public golf courses, certified lanes for tenpin bowling, movie theatres, zoos, aquariums, family theme parks, sanctioned automobile race tracks, pari-mutuel betting attractions, major- and minor- league professional sports teams, NCAA Division I football and basketball teams, miles of ocean or Great Lakes coastline, inland water, national forests, national parks, or national wildlife refuges, Consolidated Metropolitan Statistical Area access.
#Economics: average household income adjusted for taxes and living costs, income growth, job growth.

#Excepto para Housing y Crime, todos los scores están en el sentido
# a mayor valor mejor calificación
#Para Housing y Crime el sentido es el contrario
#es decir, a menor valor, mejor calificación

Datos=read.delim("places.txt",
                  header = FALSE, sep='')
names(Datos)=c("Clim", "Housing", "HealthEnv", "Crime", "Transp",  "Educ", "Arts", "Recre",  "Econ", "Id")
summary(Datos)

Datos$Arts[which.max(Datos$Arts)]
Datos=Datos[-which.max(Datos$Arts),]
summary(Datos)

# Las escalas de los datos no son iguales
X11()
library(GGally)
ggpairs(data=Datos[,-10], title="Datos")

# El objetivo es tratar de encontrar uno o varios índices
# para resumir la información

# Se podrían usar los datos en la escala original, otra opción es
# usar una escala logarítmica para trata de estandarizar
X11()
ggpairs(data=log10(Datos[,-10]), title="Datos")

# Uso del paquete psych
library(psych)

Efa1 <- fa(log10(Datos[,-10]),nfactors=3) 
#Factores correlacionados rotate="oblimin", method="minres"
#Usa la función de correlación (estandariza los datos)
Efa1 
#standarized loadings son las correlaciones entre los factores y las
#variables originales, muestran el peso de cada variable

# De la varianza total, estos 3 factores sólo explican el 49%

# communalities:
# Estos porcentajes reflejan que tan bien están modeladas las variables 
# a través de los factores (tipo coef det, R^2 en reg):
Efa1$communalities #similar a h2 de la salida principal
# Un modelo muy pobre para Crime y Econ

# El resultado se puede visualizar con las siguientes 
# herramientas
X11()
biplot(Efa1,choose=c(1,2,3))
fa.diagram(Efa1, cut=.5) # por default cut=.3

# Para imprimir una salida más fácil
# de interpretar
print(Efa1, cut = .5, digits=2, sort=TRUE)

# Algunas herramientas de diagnóstico
# aunque lo más importante es encontrar alguna
# interpretación útil

summary(Efa1) #también al final de la salida anterior
# Las pruebas Chi-square consideran
# H0: el modelo parece plausible
# Ha: parece que se requiere un modelo más complejo (más factores)

# RMSEA deseable que sea pequeño, e.g. menor a .01

#Tucker Lewis Index, al menos de .9

# BIC para comparar entre modelos

# Una herramienta para tener una idea del número de factores es
X11() 
# fm="minres", se puede modificar el método
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100)


Efa1b <- fa(log10(Datos[,-10]),nfactors=5) 
Efa1b 
fa.diagram(Efa1b)



Efa2 <- fa(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
Efa2 
fa.diagram(Efa2)



Efa3 <- fa(log10(Datos[,-10]),nfactors=4, rotate="varimax") 
Efa3
fa.diagram(Efa3)

X11() 
# si se usa ml
set.seed(123)
parallel <- fa.parallel(log10(Datos[,-10]), fa="fa", n.iter=100, fm="ml")


Efa4 <- fa(log10(Datos[,-10]),nfactors=5, rotate="oblimin", fm="ml") 
Efa4
fa.diagram(Efa4)

# Quizás estos tiene mejor interpretación
Efa5 <- fa(log10(Datos[,-10]),nfactors=5, rotate="biquartimin", fm="ml") 
Efa5
fa.diagram(Efa5)


# la función principal también permite rotar
CP5 <- principal(log10(Datos[,-10]),nfactors=5, rotate="varimax") 
CP5
fa.diagram(CP5)
