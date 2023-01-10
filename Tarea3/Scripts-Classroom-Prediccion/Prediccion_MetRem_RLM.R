#Ejemplo del cálculo del poder predictivo de un
#modelo de regresión lineal múltiple

rm(list = ls(all.names = TRUE))
gc()

library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)

# Supongamos que queremos investigar o atinarle al salario de algunos de los
# jugadores y para esto tenemos la información histórica 
# (del año pasado aunque aquí es de 1986)
# Estamos asumiendo que tendremos la información asociada a los jugadores a los 
# que aplicaremos este modelo

##############################
### Primer modelo a explorar
##############################

# Por simplicidad, por ahora no realizaremos nada de preprocesamiento
# nos centraremos en obtener algunas métricas de predicción
# Modelo con todas las variables, sólo efectos principales
mod1=lm(Salary ~ ., Datos)
summary(mod1)

# La regla sería el modelo ajustado, usando como 
# predicción el valor esperado estimado para valores de x
# Para usar la regla daríamos la expresión con coeficientes estimados 
# al usuario o bien se usaría la función predict

####
## Calculo del poder predictivo del modelo 1

###
# Supongamos que sólo queremos dividir la muestra
# en dos: train (80%) y test (20%)
# y calcular el MSE como medida de poder predictivo

# una opción es usar sample() para dividir la muestra
set.seed (1)
n=dim(Datos)[1]
train <- sample(1:n, n*.8) #muestra aleatoria simple sin reemplazo
head(train) #resultado es conjunto de índices correspondiente al train
length(train)
test = (-train)

# El resto es replicar el esquema de entrenamiento con
# train y calcular el MSE usando la regla en test
mod1t=lm(Salary ~ ., Datos[train,]) #entrenar sólo con train
predm1t=predict(mod1t, Datos[test,]) #evaluar sólo con test
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2)
MSE.mod1

# Por lo general, la medida por sí misma no tiene mucho valor,
# aunque se prefiere un valor pequeño
# Estas medidas se usan más para comparar posibles esquemas 
# de entrenamiento (Redes Neuronales vs Regresión vs Regresión polinomial vs Regresión lasso)


# Antes de pasar a otra alternativa para medir 
# poder predictivo veamos algunos conceptos 
# adicionales que antes eran relevantes, aunque ya sin mucho uso

# Medida de error aparente
# Se calcula usando las n observaciones, 
# considerando la métrica definida
predm1=predict(mod1, Datos)
MSE.mod1.ap=mean((Datos$Salary-predm1)^2)
MSE.mod1.ap
# Es una MUY MALA medida de aproximación del error de predicción:
# subestima el verdadero error de predicción

# Medida de error de entrenamiento
# Se calcula sólo con el conjunto de entrenamiento
predm1t=predict(mod1t, Datos[train,])
MSE.mod1.tr=mean((Datos$Salary[train]-predm1t)^2)
MSE.mod1.tr
# Esta última sólo aproximaría al error aparente (muy mala aproximación)
# Ninguna de éstas refleja el escenario de 
# NUEVAS observaciones


# Notar la variabilidad en la estimación del poder predictivo
set.seed (2) #correr con varias semillas
n=dim(Datos)[1]
train <- sample (1:n, n*.8)
test = (-train)
mod1t=lm(Salary ~ ., Datos[train,])
predm1t=predict(mod1t, Datos[test,])
MSE.mod1=mean((Datos$Salary[test]-predm1t)^2)
MSE.mod1

###
# Supongamos que queremos aplicar el método
# repeated holdout method
# B=100, train (80%) y test (20%)
# y calcular el MSE como medida de poder predictivo

# Podríamos usar un ciclo for

# Pero también podemos aprovechar algunas funciones de R
# Por ejemplo, con una sola instrucción en R obtener 
# los índices de los B conjuntos train
set.seed(321)
B=100
IndexTrain = replicate(B, sample(1:n, n*.8))
# Se obtiene matriz donde cada columna corresponde 
# a los índices del train

# Definamos lo que se haría en cada repetición

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

# Estimación del poder predictivo usando
# MSE y Repeated holdout method es
(MSE.RHM.mod1=mean(MSE.B.mod1))

# La variabilidad de la estimación es menor, 
# e.g. correr todo el proceso de nuevo

###
# Supongamos que queremos aplicar el método
# K-Cross Validation
# K=5, train (aprox 80%) y test (aprox 20%)
# y calcular el MSE como medida de poder predictivo

# usamos un vector con valores del 1 a K
K=5
labK=rep(1:K, length.out = n)
table(labK)

# realizamos una permutación aleatoria de los pliegues
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

# Estimación del poder predictivo usando
# MSE y K-Cross Validation es
(MSE.KCV.mod1=mean(MSE.K.mod1))


###
# Supongamos que queremos aplicar el método
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


# Los dos métodos Repeated tienen menor variabilidad
# al ser promedios de varias repeticiones

# Notar que a falta de poder de cómputo, K-Cross Validation
# no es tan variable, así que muchas veces se toma como 
# una moderada aproximación del poder predictivo,
# aunque es mucho más variable cuando los métodos de 
# entrenamiento son más complejos (muchos parámetros e hiperparámetros)


### Nota 1.
# Cuando hay varias reglas (métodos de entrenamiento) a comparar
# se debe aplicar el mismo método de remuestreo para calcular
# el poder predictivo que también debe calcularse de la misma forma.

### Nota 2.
# En los paquetes, para las reglas más comúnes hay funciones que
# calculan medidas de predicción
# para algunos de los métodos de remuestreo de forma "eficiente"

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
# Algunos paquete usan estos métodos de remuestreo
# pero con más énfasis en el entrenamiento y tuneo de hiperparámetros
# Los más populares en R son 
# caret (muchos métodos, pero ya no se mantiene para los actuales)
# https://topepo.github.io/caret/index.html
# tidymodels (se podría decir que es el reemplazo de caret, 
# poca documentación aún y en proceso de desarrollo)
# https://www.tidymodels.org/
# https://www.tmwr.org/
# e1071 con su función tune para los métodos más comúnes, eg. svm y nnet
# https://cran.r-project.org/web/packages/e1071/

# Algunos aspectos de interés de caret
library(caret)
# tiene su propia función para particionar la muestra
# supongamos que sólo la dividiremos en 2
set.seed(1234)
Partition <- createDataPartition(Datos$Salary,p = .80, groups =1, list = FALSE)
TrainDatos <- Datos[ Partition,]
TestDatos  <- Datos[-Partition,]
# Por ejemplo, aquí no se tuneó nada
tc <- trainControl(method = "none") # aquí permite indicar método de remuestreo para tunear
# por ejemplo, para un modelo de regresión se podría tunear el intercepto (define una malla)
# Ajustamos el modelo de regresión con el train
lm1_tr_caret = caret::train(Salary ~ ., data = TrainDatos, method = "lm",
                trControl = tc)
lm1_tr_caret
summary(lm1_tr_caret)
# la ventaja es que tiene medidas interesantes para poder predictivo
# al aplicar en el test
lm1_test_caret = predict(lm1_tr_caret, TestDatos)
(MedPred=postResample(pred = lm1_test_caret, obs = TestDatos$Salary))
# MAE-Mean absolute error, media de la diferencia en valor absoluto |y-\hat{y}|
# Rsquared, correlación al cuadrado entre y y \hat{y}
#Notar que RMSE es la raíz de MSE
MedPred[1]^2

# replica de las otras medidas de poder predictivo:
mean(abs(lm1_test_caret-TestDatos$Salary))
cor(lm1_test_caret,TestDatos$Salary)^2

#Otra ventaja de caret es que tiene funciones que realizan la división
# considerando muestreo aleatorio simple con estratificación.
# La estratificación la realizan usando los valores de la variable
# dependiente y (usa los percentiles en el caso continuo 
# y los niveles en caso categórico)

Partition2 <- createDataPartition(Datos$Salary,p = .80, groups =5, list = FALSE)

# Por ejemplo
# Se puede replicar lo hecho en R base
# con dos divisiones, pero otras métricas
set.seed (1)
train <- sample(1:n, n*.8) 
test = (-train)
TrainDatos <- Datos[train,]
TestDatos  <- Datos[test,]
lm1_tr_caret = caret::train(Salary ~ ., data = TrainDatos, method = "lm",
                            trControl = tc)
lm1_test_caret = predict(lm1_tr_caret, TestDatos)
(MedPred=postResample(pred = lm1_test_caret, obs = TestDatos$Salary))
#Notar que RMSE es la raíz de MSE
MedPred[1]^2


# En la mayoría de las aplicaciones o ejemplos de los paquetes
# caret o tidymodels se pone más atención en el entrenamiento
# y el tuneo. Para la evaluación del poder predictivo muestran
# sólo la división en dos grupos, pero como ya lo vimos
# la estimación por ese método tiene mucha variabilidad.

# Se recomienda al menos usar un K-CV para estimar el poder predictivo
# Algunas referencias sobre el tema son
# i) Capítulo 5 en Data Mining:Practical Machine Learning Tools and Techniques
# ii) Performance-Estimation Properties of Cross-Validation-Based Protocols with Simultaneous Hyper-Parameter Optimization
#    Tsamardinos et. al. (2015)
# iii) A bias correction for the minimum error rate in cross-validation
#    Tibshirani y Tibshirani (2009)
# iv) Cross-validation pitfalls when selecting and assessing regression and classification models
#    Krstajic et. al. (2014), 


# Aquí agregar ejemplo con tidymodels (próximos semestres...)

