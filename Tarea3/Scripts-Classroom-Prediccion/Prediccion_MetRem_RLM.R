#Ejemplo del c?lculo del poder predictivo de un
#modelo de regresi?n lineal m?ltiple

rm(list = ls(all.names = TRUE))
gc()

library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)

# Supongamos que queremos investigar o atinarle al salario de algunos de los
# jugadores y para esto tenemos la informaci?n hist?rica 
# (del a?o pasado aunque aqu? es de 1986)
# Estamos asumiendo que tendremos la informaci?n asociada a los jugadores a los 
# que aplicaremos este modelo

##############################
### Primer modelo a explorar
##############################

# Por simplicidad, por ahora no realizaremos nada de preprocesamiento
# nos centraremos en obtener algunas m?tricas de predicci?n
# Modelo con todas las variables, s?lo efectos principales
mod1=lm(Salary ~ ., Datos)
summary(mod1)

# La regla ser?a el modelo ajustado, usando como 
# predicci?n el valor esperado estimado para valores de x
# Para usar la regla dar?amos la expresi?n con coeficientes estimados 
# al usuario o bien se usar?a la funci?n predict

####
## Calculo del poder predictivo del modelo 1

###
# Supongamos que s?lo queremos dividir la muestra
# en dos: train (80%) y test (20%)
# y calcular el MSE como medida de poder predictivo

# una opci?n es usar sample() para dividir la muestra
set.seed (1)
n=dim(Datos)[1]
train <- sample(1:n, n*.8) #muestra aleatoria simple sin reemplazo
head(train) #resultado es conjunto de ?ndices correspondiente al train
length(train)
test = (-train)

# El resto es replicar el esquema de entrenamiento con
# train y calcular el MSE usando la regla en test
mod1t=lm(Salary ~ ., Datos[train,]) #entrenar s?lo con train
predm1t=predict(mod1t, Datos[test,]) #evaluar s?lo con test
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2)
MSE.mod1

# Por lo general, la medida por s? misma no tiene mucho valor,
# aunque se prefiere un valor peque?o
# Estas medidas se usan m?s para comparar posibles esquemas 
# de entrenamiento (Redes Neuronales vs Regresi?n vs Regresi?n polinomial vs Regresi?n lasso)


# Antes de pasar a otra alternativa para medir 
# poder predictivo veamos algunos conceptos 
# adicionales que antes eran relevantes, aunque ya sin mucho uso

# Medida de error aparente
# Se calcula usando las n observaciones, 
# considerando la m?trica definida
predm1=predict(mod1, Datos)
MSE.mod1.ap=mean((Datos$Salary-predm1)^2)
MSE.mod1.ap
# Es una MUY MALA medida de aproximaci?n del error de predicci?n:
# subestima el verdadero error de predicci?n

# Medida de error de entrenamiento
# Se calcula s?lo con el conjunto de entrenamiento
predm1t=predict(mod1t, Datos[train,])
MSE.mod1.tr=mean((Datos$Salary[train]-predm1t)^2)
MSE.mod1.tr
# Esta ?ltima s?lo aproximar?a al error aparente (muy mala aproximaci?n)
# Ninguna de ?stas refleja el escenario de 
# NUEVAS observaciones


# Notar la variabilidad en la estimaci?n del poder predictivo
set.seed (2) #correr con varias semillas
n=dim(Datos)[1]
train <- sample (1:n, n*.8)
test = (-train)
mod1t=lm(Salary ~ ., Datos[train,])
predm1t=predict(mod1t, Datos[test,])
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2)
MSE.mod1

###
# Supongamos que queremos aplicar el m?todo
# repeated holdout method
# B=100, train (80%) y test (20%)
# y calcular el MSE como medida de poder predictivo

# Podr?amos usar un ciclo for

# Pero tambi?n podemos aprovechar algunas funciones de R
# Por ejemplo, con una sola instrucci?n en R obtener 
# los ?ndices de los B conjuntos train
set.seed(321)
B=100
IndexTrain = replicate(B, sample(1:n, n*.8))
# Se obtiene matriz donde cada columna corresponde 
# a los ?ndices del train

# Definamos lo que se har?a en cada repetici?n

mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  mod1t=lm(Salary ~ ., Dat[train,])
  predm1t=predict(mod1t, Dat[test,])
  MSE=mean((Dat$Salary[test]-predm1t)^2)
  return(MSE)
}

MSE.B.mod1= sapply(1:B,mod1RHM, IndTrain=IndexTrain, Dat=Datos)
summary(MSE.B.mod1)

# Estimaci?n del poder predictivo usando
# MSE y Repeated holdout method es
(MSE.RHM.mod1=mean(MSE.B.mod1))

# La variabilidad de la estimaci?n es menor, 
# e.g. correr todo el proceso de nuevo

###
# Supongamos que queremos aplicar el m?todo
# K-Cross Validation
# K=5, train (aprox 80%) y test (aprox 20%)
# y calcular el MSE como medida de poder predictivo

# usamos un vector con valores del 1 a K
K=5
labK=rep(1:K, length.out = n)
table(labK)

# realizamos una permutaci?n aleatoria de los pliegues
set.seed(123)
Pliegues <- sample(labK)  #seleccionados n de n
mod1KCV=function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1t=lm(Salary ~ ., Dat[train,])
  predm1t=predict(mod1t, Dat[test,])
  MSE=mean((Dat$Salary[test]-predm1t)^2)
  return(MSE)
}

MSE.K.mod1= sapply(1:K,mod1KCV, Plie=Pliegues, Dat=Datos)
summary(MSE.K.mod1)

# Estimaci?n del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod1=mean(MSE.K.mod1))


###
# Supongamos que queremos aplicar el m?todo
# Repeated K-Cross Validation
# B=20, K=5, train (aprox 80%) y test (aprox 20%)
# y calcular el MSE como medida de poder predictivo

BKCV=function(x, labK, Dat, K){
Pliegues <- sample(labK)
MSE.K.mod1= sapply(1:K,mod1KCV, Plie=Pliegues, Dat=Dat)
MSE.KCV.mod1=mean(MSE.K.mod1)
return(MSE.KCV.mod1)
}

set.seed(1)
B=20
MSE.BK.mod1= sapply(1:B,BKCV, labK=labK, Dat=Datos, K=K)
summary(MSE.BK.mod1)
(MSE.BKCV.mod1=mean(MSE.BK.mod1))

MSE.RHM.mod1


# Los dos m?todos Repeated tienen menor variabilidad
# al ser promedios de varias repeticiones

# Notar que a falta de poder de c?mputo, K-Cross Validation
# no es tan variable, as? que muchas veces se toma como 
# una moderada aproximaci?n del poder predictivo,
# aunque es mucho m?s variable cuando los m?todos de 
# entrenamiento son m?s complejos (muchos par?metros e hiperpar?metros)


### Nota 1.
# Cuando hay varias reglas (m?todos de entrenamiento) a comparar
# se debe aplicar el mismo m?todo de remuestreo para calcular
# el poder predictivo que tambi?n debe calcularse de la misma forma.

### Nota 2.
# En los paquetes, para las reglas m?s com?nes hay funciones que
# calculan medidas de predicci?n
# para algunos de los m?todos de remuestreo de forma "eficiente"

# Para modelos lineales generalizados, cv.glm en boot para K-CV
library(boot)
set.seed(123)
K=5
B=20 #repetiremos el K-CV

mod1glm=glm(Salary ~ ., data=Datos)

cv=rep(NA,B)
for (i in 1:B){
  cv[i]=cv.glm(Datos, mod1glm, K=K)$delta[1]
}
(mean(cv))


library(cvTools) #Repeated K-CV
set.seed(123)
K=5
B=20
folds <- cvFolds(nrow(Datos), K = K, R = B)
repCV(mod1, cost = mspe, folds = folds)

### Nota 3.
# Algunos paquete usan estos m?todos de remuestreo
# pero con m?s ?nfasis en el entrenamiento y tuneo de hiperpar?metros
# Los m?s populares en R son 
# caret (muchos m?todos, pero ya no se mantiene para los actuales)
# https://topepo.github.io/caret/index.html
# tidymodels (se podr?a decir que es el reemplazo de caret, 
# poca documentaci?n a?n y en proceso de desarrollo)
# https://www.tidymodels.org/
# https://www.tmwr.org/
# e1071 con su funci?n tune para los m?todos m?s com?nes, eg. svm y nnet
# https://cran.r-project.org/web/packages/e1071/

# Algunos aspectos de inter?s de caret
library(caret)
# tiene su propia funci?n para particionar la muestra
# supongamos que s?lo la dividiremos en 2
set.seed(1234)
Partition <- createDataPartition(Datos$Salary,p = .80, groups =1, list = FALSE)
TrainDatos <- Datos[ Partition,]
TestDatos  <- Datos[-Partition,]
# Por ejemplo, aqu? no se tune? nada
tc <- trainControl(method = "none") # aqu? permite indicar m?todo de remuestreo para tunear
# por ejemplo, para un modelo de regresi?n se podr?a tunear el intercepto (define una malla)
# Ajustamos el modelo de regresi?n con el train
lm1_tr_caret = caret::train(Salary ~ ., data = TrainDatos, method = "lm",
                trControl = tc)
lm1_tr_caret
summary(lm1_tr_caret)
# la ventaja es que tiene medidas interesantes para poder predictivo
# al aplicar en el test
lm1_test_caret = predict(lm1_tr_caret, TestDatos)
(MedPred=postResample(pred = lm1_test_caret, obs = TestDatos$Salary))
# MAE-Mean absolute error, media de la diferencia en valor absoluto |y-\hat{y}|
# Rsquared, correlaci?n al cuadrado entre y y \hat{y}
#Notar que RMSE es la ra?z de MSE
MedPred[1]^2

# replica de las otras medidas de poder predictivo:
mean(abs(lm1_test_caret-TestDatos$Salary))
cor(lm1_test_caret,TestDatos$Salary)^2

#Otra ventaja de caret es que tiene funciones que realizan la divisi?n
# considerando muestreo aleatorio simple con estratificaci?n.
# La estratificaci?n la realizan usando los valores de la variable
# dependiente y (usa los percentiles en el caso continuo 
# y los niveles en caso categ?rico)

Partition2 <- createDataPartition(Datos$Salary,p = .80, groups =5, list = FALSE)

# Por ejemplo
# Se puede replicar lo hecho en R base
# con dos divisiones, pero otras m?tricas
set.seed (1)
train <- sample(1:n, n*.8) 
test = (-train)
TrainDatos <- Datos[train,]
TestDatos  <- Datos[test,]
lm1_tr_caret = caret::train(Salary ~ ., data = TrainDatos, method = "lm",
                            trControl = tc)
lm1_test_caret = predict(lm1_tr_caret, TestDatos)
(MedPred=postResample(pred = lm1_test_caret, obs = TestDatos$Salary))
#Notar que RMSE es la ra?z de MSE
MedPred[1]^2


# En la mayor?a de las aplicaciones o ejemplos de los paquetes
# caret o tidymodels se pone m?s atenci?n en el entrenamiento
# y el tuneo. Para la evaluaci?n del poder predictivo muestran
# s?lo la divisi?n en dos grupos, pero como ya lo vimos
# la estimaci?n por ese m?todo tiene mucha variabilidad.

# Se recomienda al menos usar un K-CV para estimar el poder predictivo
# Algunas referencias sobre el tema son
# i) Cap?tulo 5 en Data Mining:Practical Machine Learning Tools and Techniques
# ii) Performance-Estimation Properties of Cross-Validation-Based Protocols with Simultaneous Hyper-Parameter Optimization
#    Tsamardinos et. al. (2015)
# iii) A bias correction for the minimum error rate in cross-validation
#    Tibshirani y Tibshirani (2009)
# iv) Cross-validation pitfalls when selecting and assessing regression and classification models
#    Krstajic et. al. (2014), 


# Aqu? agregar ejemplo con tidymodels (pr?ximos semestres...)

