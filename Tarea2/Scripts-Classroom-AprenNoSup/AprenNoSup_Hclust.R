# Ejemplo del uso
# de la técnica para conglomeración
# jerárquica aglomerativa en R
rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2023-1/ApreEstAut")
library(tidyverse)
datos=mtcars
help("mtcars")

# 32 autos de 1974
#	mpg	Miles/(US) gallon    -- Millas por galón
#	cyl	Number of cylinders     -- Num. de cilindros
#	disp	Displacement (cu.in.) -- Desplazamiento del motor
#	hp	Gross horsepower -- Caballos de fuerza
#	drat	Rear axle ratio  -- Relación de eje trasero 
#	wt	Weight (1000 lbs)  -- Peso
#	qsec	1/4 mile time    -- velocidad en segundos para recorrer 402 metros
#	vs	Engine (0 = V-shaped, 1 = straight)  -- Tipo de motor 
#	am	Transmission (0 = automatic, 1 = manual) -- Tipo de transmisión
#	gear	Number of forward gears  -- Num. de cambios de velocidades
#	carb	Number of carburetors  -- Num. de carburadores

summary(datos)


# distancias clásicas
dis_datos <- dist(x = datos, method="canberra")
dis_datos2 <- dist(x = datos, method="euclidian")

clust.jer <- hclust(dis_datos, method="complete")

clust.jer2 <- hclust(dis_datos2, method="ward.D")
X11()
par(mfrow=c(1,2))
plot(clust.jer)

# Se puede agregar la identificación de los clusters
plot(clust.jer2)
rect.hclust(clust.jer2 , k = 3, border = 2:6)

# Para graficar sólo parte del dendograma
# por ejemplo a la altura 250 del eje vertical
X11()
par(mfrow=c(1,2))
dend.clust.jer2 <- cut( as.dendrogram(clust.jer2), h = 250)
plot(dend.clust.jer2$upper, center=TRUE)

plot(clust.jer2)

# Se puede obtener una conglomeración en particular
datosv2=datos
datosv2$c3= cutree(clust.jer2, k = 3)
table(datosv2$c3)



# Por ejemplo, para interpretar usando CP
library(psych)
p=2
pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca,group=datosv2$c3, pch=c(0,21,4)[datosv2$c3])

#Se pueden calcular índices para evaluar la conglomeración

library(clusterCrit)
intCriteria(as.matrix(datos),as.integer(datosv2$c3),c("Silhouette"))

library(NbClust)
k_clus <- NbClust(diss = dis_datos, distance = NULL, min.nc = 2,
                  max.nc = 5, method = "complete", index = "silhouette")
# Todos los índices calculados por valor de k evaludado
k_clus$All.index
# El valor de k seleccionado
k_clus$Best.nc


k_clus2 <- NbClust(data = datos, distance = "canberra", min.nc = 2,
                  max.nc = 5, method = "complete", index = "silhouette")
# Todos los índices calculados por valor de k evaludado
k_clus2$All.index
# El valor de k seleccionado
k_clus2$Best.nc


# También se puede usar lo comentado 
# para k-means sobre preprocesamiento y 
# uso de la disimilaridad "gower"


# Otras opciones en R son
# diana y agnes