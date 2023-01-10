#Ejemplo de la comparación del poder predictivo
# para varias reglas basadas en regresión lineal múltiple:

# 1. Sólo efectos principales
# 2. Incluyendo las variables continuas al cuadrado
# 3. Incluyendo las variables continuas al cuadrado y selección por
#    pasos usando criterio BIC
# 4. Incluyendo las variables continuas al cuadrado y selección usando
#    lasso con K-CV y MSE para tunear parámetro lambda

rm(list = ls(all.names = TRUE))
gc()


# Datos a usar
library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)


# Por simplicidad se usará:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# y cálculo del MSE como medida de poder predictivo

# La partición se realizará con caret y 
# será la misma para todos los modelos
library(caret)
set.seed(1)
B=50
Partition<- createDataPartition(Datos$Salary, p = .80, groups =4, list = FALSE, times = B)
# Notar que la partición considera que el test tendrá en la medida de lo
# posible observaciones de 4 grupos


##############################
### Primer modelo a explorar
### Sólo efectos principales
##############################
##
# Descripción del método de entrenamiento y regla final
mod1=lm(Salary ~ ., Datos)
summary(mod1)

##
# Medición del poder predictivo

mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  modtr=lm(Salary ~ ., Dat[train,])
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

MSE.B.mod1= sapply(1:B,mod1RHM, IndTrain=Partition, Dat=Datos)
(MSE.RHM.mod1=mean(MSE.B.mod1))
#[1] 118964.9

##############################
### Segundo modelo a explorar
### Incluyendo las variables continuas al cuadrado
##############################

##
# Descripción del método de entrenamiento y regla final

# Analicemos cuales son las variables continuas
str(Datos)

# r base con sapply
(continuas = names(which(sapply(Datos, is.numeric))) )
# también se puede usar tidyverse
library(tidyverse)
(categoricas = names(Datos %>% select_if(~!is.numeric(.x))) )

# definir variables predictoras continuas
xnames=continuas[!continuas %in%  c("Salary")]
forexp=as.formula(  paste('Salary ~.',"+", paste(paste('I(',xnames,'^2)',collapse = ' + ')  ) )) 
forexp
mod2=lm(forexp, Datos)
summary(mod2)

# Notar que la fórmula que se definió 
# no depende para nada de las observaciones,
# así que aunque es parte del preprocesamiento
# no cambiaría en la división train-test

##
# Medición del poder predictivo

mod2RHM=function(x, IndTrain, Dat, forme){
  train= IndTrain[,x]
  test = (-train)
  modtr=lm(forme, Dat[train,])
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

MSE.B.mod2= sapply(1:B,mod2RHM, IndTrain=Partition, Dat=Datos, forme=forexp)
(MSE.RHM.mod2=mean(MSE.B.mod2))
# [1] 124168.7

##############################
### Tercer modelo a explorar
### Incluyendo las variables continuas al cuadrado  
###  y selección por pasos usando criterio BIC
##############################

##
# Descripción del método de entrenamiento y regla final

# se requiere una fórmula para definir el modelo más complejo
upperfor=as.formula(  paste('~.',"+", paste('I(',xnames,'^2)',collapse = ' + ') ) ) 
upperfor
# se requiere definir la penalización para BIC
pen=log(dim(Datos)[1])

# Adicional al ajuste mod2 se realiza la selección
library(MASS)
mod3 <- stepAIC(mod2, scope =list(upper = upperfor, lower = ~1), trace = FALSE,direction ="both", k=pen)
summary(mod3)

# Notar que la fórmula que se definió 
# no depende para nada de las observaciones,
# Pero hay que tomar en cuenta la definición de mod2,
# así como la penalización sí dependen de n,
# además la selección de variables siempre es
# parte del entrenamiento cuando el criterio
# se basa en las observaciones, como el AIC o BIC


##
# Medición del poder predictivo

mod3RHM=function(x, IndTrain, Dat, forme, upform){
  train= IndTrain[,x]
  test = (-train)
  assign("DatosAux", Dat[train,], envir = .GlobalEnv) #Cuidado stepAIC busca la base de datos en el environment global 
  modAux=lm(forme, data=DatosAux)
  penAux=log(dim(DatosAux)[1])
  modtr=stepAIC(modAux, scope =list(upper = upform, lower = ~1), trace = FALSE,direction ="both", k=penAux)
  predte=predict(modtr, Dat[test,])
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

#MSE.B.mod3= sapply(1:B,mod3RHM, IndTrain=Partition, Dat=Datos, forme=forexp, upform=upperfor)
#Mejor usar un for en orden, pues DatosAux estará en el environment global
MSE.B.mod3=NA
for(ik in 1:B){
   MSE.B.mod3[ik]=mod3RHM(ik,IndTrain=Partition, Dat=Datos, forme=forexp, upform=upperfor)
  }
(MSE.RHM.mod3=mean(MSE.B.mod3))
# [1] 108758.1

# Se podría hacer todo con caret. Sólo en muy pocos casos se puede
library(caret)
set.seed(1)
B=50
PartitionList<- createDataPartition(Datos$Salary, p = .80, groups =4, list = TRUE, times = B)
ntr=length(PartitionList$Resample01)

MSEcaret=function(data, lev = NULL, model = NULL){
  out <- c(mean((data$pred - data$obs)^2))
  names(out) <- c("MSE")
  return(out)
}
trControl <- trainControl(index=PartitionList, summaryFunction=MSEcaret)

caret_model <- train(forexp, 
                     data=Datos, 
                     method="glmStepAIC", 
                     family = "gaussian",
                     direction ="both", k=log(ntr), trace=FALSE, scope =list(upper = upperfor, lower = ~1),
                     trControl=trControl, metric="MSE")
caret_model$results
caret_model$bestTune  # esto es equivalente al error predictivo SÓLO cuando
                      # parameter es NONE, de otra forma es tuneo
# El modelo final podría diferir, pues la penalización del calculado con el completo
# es diferente. En este caso dió lo mismo
caret_model$finalModel$coefficients
mod3$coefficients

#Recomendación. Para el cálculo del poder predictivo.
#               Sólo usar caret cuando se esté seguro
#               de que no hay hiperparámetros involucrados

##############################
### Cuarto modelo a explorar
### Incluyendo las variables continuas al cuadrado  
###  y selección usando lasso con glmnet
##############################

##
# Descripción del método de entrenamiento y regla final

# Usaremos la fórmula con variables al cuadrado para crear matrix X 
forexp
Xmod4 <- model.matrix(forexp, data=Datos)[,-1]
Ymod4 <- Datos[,"Salary"] 

#recordar que para lasso alpha=1 (está por default)
#por otro lado hay dos opciones de estimadores, 
# relax = FALSE está por default (lo usaremos por ahora)
# recordar que esos no son EMV
library(glmnet)
mod4.lasso = glmnet(Xmod4, Ymod4, family = gaussian("identity"), nlambda = 200)
#Faltaría tunear (definir valor) de lambda

#glmnet tiene una opción para tunear usando K-CV
set.seed(1)
mod4.lasso.tun=cv.glmnet(Xmod4, Ymod4, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
plot(mod4.lasso.tun)
print.cv.glmnet(mod4.lasso.tun)
mod4.lasso.tun$lambda.min
coef(mod4.lasso.tun, s = "lambda.min")

# Regla final es aquella que usa el lambda seleccionado
# por ejemplo
predict(mod4.lasso.tun, newx = Xmod4[1:5,], type = "response", s = "lambda.min")


# Nota sobre el preprocesamiento para el usuario final
# Se usa model.matrix, así que para nuevos datos y
# cuando hay variables categóricas
# se deben guardar 
# los niveles de todas las variables categóricas y su orden
# así se puede construir de forma adecuada la matrix

xlevs <- lapply(Datos[,sapply(Datos, is.factor), drop = F], function(j){
  levels(j)
})

# Así, para datos nuevos, por ejemplo Datos[2:2,]
datosnew=Datos[2:2,]
str(datosnew) #Aquí se guarda la información
# un usuario nuevo debería indicar de la misma forma
# el dataframe, incluyendo los niveles para las variables factor
# si sólo hay un valor podría incluirlo como categórica
datosnew$Division=factor(as.character(datosnew$Division))
datosnew$League=factor(as.character(datosnew$League))
datosnew$NewLeague=factor(as.character(datosnew$NewLeague))
str(datosnew)
X_new <- model.matrix(forexp, data=datosnew, xlev = xlevs)[,-1]

Xmod4[2:2,]
X_new

# Error sin los niveles
model.matrix(forexp, data=datosnew)[,-1]



##
# Medición del poder predictivo
# aquí si es importante incluir el tuneo dentro del entrenamiento

mod4RHM=function(x, IndTrain, Dat, forme){
  train= IndTrain[,x]
  test = (-train)
  Xmod4ttotal = model.matrix(forme, data=Dat)[,-1]
  Xmod4t = Xmod4ttotal[train, ]
  Ymod4t = Dat[train,"Salary"] 
  mod4t.lasso.tun=cv.glmnet(Xmod4t, Ymod4t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
  predte=predict(mod4t.lasso.tun, newx = Xmod4ttotal[test,], type = "response", s = "lambda.min")
  MSE=mean((Dat$Salary[test]-predte)^2)
  return(MSE)
}

set.seed(1)
MSE.B.mod4= sapply(1:B,mod4RHM, IndTrain=Partition, Dat=Datos, forme=forexp)
(MSE.RHM.mod4=mean(MSE.B.mod4))
# [1] 101029.9


# Comparación
MSE.RHM.mod1
MSE.RHM.mod2
MSE.RHM.mod3
MSE.RHM.mod4

# Para este ejemplo, la regla con mejor poder predictivo es la 4, es decir
# la que usa lasso sobre los efectos principales y variables al cuadrado

# Notar la diferencia de la estimación en el proceso de entrenamiento
# usado para tunear MSE 88637 vs 
# la estimación del poder predictivo MSE 101029.9

