#Ejemplo de la comparaci?n del poder predictivo
# para varias reglas basadas en regresi?n lineal m?ltiple:

# 1. S?lo efectos principales
# 2. Incluyendo las variables continuas al cuadrado
# 3. Incluyendo las variables continuas al cuadrado y selecci?n por
#    pasos usando criterio BIC
# 4. Incluyendo las variables continuas al cuadrado y selecci?n usando
#    lasso con K-CV y MSE para tunear par?metro lambda

rm(list = ls(all.names = TRUE))
gc()


# Datos a usar
library (ISLR)
help(Hitters)

Datos=Hitters
Datos = na.omit(Datos)
str(Datos)


# Por simplicidad se usar?:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# y c?lculo del MSE como medida de poder predictivo

# La partici?n se realizar? con caret y 
# ser? la misma para todos los modelos
library(caret)
set.seed(1)
B=50
Partition<- createDataPartition(Datos$Salary, p = .80, groups =4, list = FALSE, times = B)
# Notar que la partici?n considera que el test tendr? en la medida de lo
# posible observaciones de 4 grupos


##############################
### Primer modelo a explorar
### S?lo efectos principales
##############################
##
# Descripci?n del m?todo de entrenamiento y regla final
mod1=lm(Salary ~ ., Datos)
summary(mod1)

##
# Medici?n del poder predictivo

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
# Descripci?n del m?todo de entrenamiento y regla final

# Analicemos cuales son las variables continuas
str(Datos)

# r base con sapply
(continuas = names(which(sapply(Datos, is.numeric))) )
# tambi?n se puede usar tidyverse
library(tidyverse)
(categoricas = names(Datos %>% select_if(~!is.numeric(.x))) )

# definir variables predictoras continuas
xnames=continuas[!continuas %in%  c("Salary")]
forexp=as.formula(  paste('Salary ~.',"+", paste(paste('I(',xnames,'^2)',collapse = ' + ')  ) )) 
forexp
mod2=lm(forexp, Datos)
summary(mod2)

# Notar que la f?rmula que se defini? 
# no depende para nada de las observaciones,
# as? que aunque es parte del preprocesamiento
# no cambiar?a en la divisi?n train-test

##
# Medici?n del poder predictivo

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
###  y selecci?n por pasos usando criterio BIC
##############################

##
# Descripci?n del m?todo de entrenamiento y regla final

# se requiere una f?rmula para definir el modelo m?s complejo
upperfor=as.formula(  paste('~.',"+", paste('I(',xnames,'^2)',collapse = ' + ') ) ) 
upperfor
# se requiere definir la penalizaci?n para BIC
pen=log(dim(Datos)[1])

# Adicional al ajuste mod2 se realiza la selecci?n
library(MASS)
mod3 <- stepAIC(mod2, scope =list(upper = upperfor, lower = ~1), trace = FALSE,direction ="both", k=pen)
summary(mod3)

# Notar que la f?rmula que se defini? 
# no depende para nada de las observaciones,
# Pero hay que tomar en cuenta la definici?n de mod2,
# as? como la penalizaci?n s? dependen de n,
# adem?s la selecci?n de variables siempre es
# parte del entrenamiento cuando el criterio
# se basa en las observaciones, como el AIC o BIC


##
# Medici?n del poder predictivo

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
#Mejor usar un for en orden, pues DatosAux estar? en el environment global
MSE.B.mod3=NA
for(ik in 1:B){
   MSE.B.mod3[ik]=mod3RHM(ik,IndTrain=Partition, Dat=Datos, forme=forexp, upform=upperfor)
  }
(MSE.RHM.mod3=mean(MSE.B.mod3))
# [1] 108758.1

# Se podr?a hacer todo con caret. S?lo en muy pocos casos se puede
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
caret_model$bestTune  # esto es equivalente al error predictivo S?LO cuando
                      # parameter es NONE, de otra forma es tuneo
# El modelo final podr?a diferir, pues la penalizaci?n del calculado con el completo
# es diferente. En este caso di? lo mismo
caret_model$finalModel$coefficients
mod3$coefficients

#Recomendaci?n. Para el c?lculo del poder predictivo.
#               S?lo usar caret cuando se est? seguro
#               de que no hay hiperpar?metros involucrados

##############################
### Cuarto modelo a explorar
### Incluyendo las variables continuas al cuadrado  
###  y selecci?n usando lasso con glmnet
##############################

##
# Descripci?n del m?todo de entrenamiento y regla final

# Usaremos la f?rmula con variables al cuadrado para crear matrix X 
forexp
Xmod4 <- model.matrix(forexp, data=Datos)[,-1]
Ymod4 <- Datos[,"Salary"] 

#recordar que para lasso alpha=1 (est? por default)
#por otro lado hay dos opciones de estimadores, 
# relax = FALSE est? por default (lo usaremos por ahora)
# recordar que esos no son EMV
library(glmnet)
mod4.lasso = glmnet(Xmod4, Ymod4, family = gaussian("identity"), nlambda = 200)
#Faltar?a tunear (definir valor) de lambda

#glmnet tiene una opci?n para tunear usando K-CV
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
# Se usa model.matrix, as? que para nuevos datos y
# cuando hay variables categ?ricas
# se deben guardar 
# los niveles de todas las variables categ?ricas y su orden
# as? se puede construir de forma adecuada la matrix

xlevs <- lapply(Datos[,sapply(Datos, is.factor), drop = F], function(j){
  levels(j)
})

# As?, para datos nuevos, por ejemplo Datos[2:2,]
datosnew=Datos[2:2,]
str(datosnew) #Aqu? se guarda la informaci?n
# un usuario nuevo deber?a indicar de la misma forma
# el dataframe, incluyendo los niveles para las variables factor
# si s?lo hay un valor podr?a incluirlo como categ?rica
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
# Medici?n del poder predictivo
# aqu? si es importante incluir el tuneo dentro del entrenamiento

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


# Comparaci?n
MSE.RHM.mod1
MSE.RHM.mod2
MSE.RHM.mod3
MSE.RHM.mod4

# Para este ejemplo, la regla con mejor poder predictivo es la 4, es decir
# la que usa lasso sobre los efectos principales y variables al cuadrado

# Notar la diferencia de la estimaci?n en el proceso de entrenamiento
# usado para tunear MSE 88637 vs 
# la estimaci?n del poder predictivo MSE 101029.9

