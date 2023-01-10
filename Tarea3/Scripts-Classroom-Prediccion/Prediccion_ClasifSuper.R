#Ejemplo de la comparación del poder predictivo
# para varias reglas basadas en regresión lineal múltiple
# y otro métodos clásicos:

# 1. Sólo efectos principales
# 2. Efectos principales y selección por pasos usando criterio BIC
# 3. Incluyendo interacciones con 
#    selección por pasos usando criterio BIC
# 4. Incluyendo interacciones  con
#    selección usando lasso con K-CV y Error Rate para tunear parámetro lambda
# 5. LDA continuo, considerando variables binarias
# 6. QDA continuo, considerando variables binarias
# 7. Método Naive
# 8. K vecinos más cercanos

rm(list = ls(all.names = TRUE))
gc()


# Datos 
# Coronary artery disease data
# with 236 observations on  14 variables.
# 13 binary vars and one terniary. 
# Contingency table of dimension 2^13*3=24,576 cells

#Sex: Female Male
#AngPec: Atypical None Typical
#AMI: Definite NotCertain
#QWave: No Yes
#QWavecode: Nonusable Usable
#STcode: Nonusable Usable
#STchange: No Yes
#SuffHeartF a factor with levels No Yes
#Hypertrophi: No Yes
#Hyperchola: No Yes
#Smoker: No Yes
#Inherit: No Yes
#Heartfail: No Yes
#CAD: No Yes
library(gRbase) 
# El interés es predecir la condición de tener 
# Enfermedad de las arterias coronarias (CAD)
data(cad1) #Datos
str(cad1)
head(cad1, n=1)
summary(cad1) 
?cad1


# Por simplicidad se usará:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# y cálculo de la tasa de clasificación correcta global como
# medida de poder predictivo


# La partición se realizará con caret y 
# será la misma para todos los modelos
library(caret)
set.seed(1)
B=50
Partition<- createDataPartition(cad1$CAD, p = .80, list = FALSE, times = B)
# Notar que la partición considera 
# que la variable CAD es tipo factor y 
# genera particiones estratificando por los grupos de CAD


##############################
### Primer modelo a explorar
### Sólo efectos principales
##############################
##
# Descripción del método de entrenamiento y regla final
mod1 <- glm(CAD ~ ., data=cad1,   family=binomial(link="logit"))
summary(mod1)
# uso de la regla
# como ejemplo, con los mismos datos con los que se entrena
# y usando la regla de mayor probabilidad
logit=predict(mod1, newdata = cad1, type = "response")
res=ifelse(logit>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])

# Medidas aparentes de poder predictivo  
(ConMatAp=table(cad1$CAD, res))
(ConMatAp[1,1]+ConMatAp[2,2])/sum(ConMatAp)  #Tasa Clas correcta global/accuracy
(ConMatAp[1,1])/sum(ConMatAp[1,]) #TCC grupo 0/ especificidad / specificity
(ConMatAp[2,2])/sum(ConMatAp[2,]) #TCC grupo 1/ sensibilidad / recall
library(metrica)
metrics_summary(obs = cad1$CAD, pred = res, type = "classification")
metrics_summary(obs = cad1$CAD, pred = res, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
# quizás hay un error en el cálculo de AUC

# Este otro paquete sirve para analizar diferentes puntos de corte
# El punto de corte se podría tunear
library(ROCR)
pred <- prediction( logit, cad1$CAD)
pred
perf <- performance(pred,"tpr","fpr")
perf
plot(perf) #y:sensibilidad, x: 1-especificidad

Acc <- performance(pred,measure="acc")

Acc@x.values[[1]][which.max(Acc@y.values[[1]])]
Acc@y.values[[1]][which.max(Acc@y.values[[1]])]

# para tunear se podría usar una malla pero sobre
# un conjunto test


##
# Medición del poder predictivo

mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  modtr=glm(CAD ~ ., data=Dat[train,],   family=binomial(link="logit"))
  preda=predict(modtr, newdata = Dat[test,], type = "response")
  predb=ifelse(preda>=.5,levels( Dat$CAD)[2],levels( Dat$CAD)[1])
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod1= sapply(1:B,mod1RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod1=rowMeans(TCC.B.mod1))


##############################
### Segundo modelo a explorar
### Efectos principales y selección por pasos usando criterio BIC
##############################

# se requiere definir la penalización para BIC
pen=log(dim(cad1)[1])

# Realizamos la selección
library(MASS)
mod2 <- stepAIC(mod1, scope =list(upper = ~., lower = ~1), trace =FALSE,direction="both", k=pen)
summary(mod2)


# uso de la regla
# como ejemplo, con los mismos datos con los que se entrena
# y usando la regla de mayor probabilidad
logit2=predict(mod2, newdata = cad1, type = "response")
res2=ifelse(logit2>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res2, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')


mod2RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  assign("DatosAux", Dat[train,], envir = .GlobalEnv) #Cuidado stepAIC busca la base de datos en el environment global 
  modAux=glm(CAD ~ ., data=DatosAux,   family=binomial(link="logit"))
  penAux=log(dim(DatosAux)[1])
  modtr=stepAIC(modAux, scope =list(upper = ~., lower = ~1), trace =FALSE,direction="both", k=penAux)
  preda=predict(modtr, newdata = Dat[test,], type = "response")
  predb=ifelse(preda>=.5,levels( Dat$CAD)[2],levels( Dat$CAD)[1])
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod2=matrix(NA,ncol=B,nrow=3)
for(ik in 1:B){
  TCC.B.mod2[,ik]=mod2RHM(ik,IndTrain=Partition, Dat=cad1)
}
(TCC.RHM.mod2=rowMeans(TCC.B.mod2))




##############################
### Tercer modelo a explorar
### Incluyendo interacciones de segundo orden
### entre variables y selección por pasos usando criterio BIC
##############################

# modelo auxiliar (nulo, para empezar con selección forward)
mod3auxN <- glm(CAD ~ 1, data=cad1,   family=binomial(link="logit"))
summary(mod3auxN)

# modelo auxiliar (el más completo a explorar)
mod3auxF <- glm(CAD ~ .^2, data=cad1,   family=binomial(link="logit"))
summary(mod3auxF)

# se requiere definir la penalización para BIC
pen=log(dim(cad1)[1])

# Realizamos la selección
library(MASS)
mod3 <- stepAIC(mod3auxN, scope =list(upper = mod3auxF, lower = mod3auxN), trace =FALSE,direction="forward", k=pen)
summary(mod3)


#mod3b <- stepAIC(mod1, scope =list(upper = ~.^2, lower = ~1), trace =FALSE,direction="both", k=pen)
#summary(mod3b)


# uso de la regla
# como ejemplo, con los mismos datos con los que se entrena
# y usando la regla de mayor probabilidad
logit3=predict(mod3, newdata = cad1, type = "response")
res3=ifelse(logit3>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res3, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')


mod3RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  assign("DatosAux", Dat[train,], envir = .GlobalEnv) #Cuidado stepAIC busca la base de datos en el environment global 
  modAuxN=glm(CAD ~ 1, data=DatosAux,   family=binomial(link="logit"))
  modAuxF=glm(CAD ~ .^2, data=DatosAux,   family=binomial(link="logit"))
  penAux=log(dim(DatosAux)[1])
  modtr=stepAIC(modAuxN, scope =list(upper = modAuxF, lower = modAuxN), trace =FALSE,direction="forward", k=penAux)
  preda=predict(modtr, newdata = Dat[test,], type = "response")
  predb=ifelse(preda>=.5,levels( Dat$CAD)[2],levels( Dat$CAD)[1])
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod3=matrix(NA,ncol=B,nrow=3)
for(ik in 1:B){
  TCC.B.mod3[,ik]=mod3RHM(ik,IndTrain=Partition, Dat=cad1)
}
(TCC.RHM.mod3=rowMeans(TCC.B.mod3))



##############################
### Cuarto modelo a explorar
### Incluyendo interacciones 
###  y selección usando lasso con glmnet
##############################

##
# Descripción del método de entrenamiento y regla final

Xmod4 <- model.matrix(CAD~ .^2, data=cad1)[,-1]
Ymod4 <- cad1[,"CAD"] 

#recordar que para lasso alpha=1 (está por default)
#por otro lado hay dos opciones de estimadores, 
# relax = FALSE está por default (lo usaremos por ahora)
# recordar que esos no son EMV
library(glmnet)
mod4.lasso = glmnet(Xmod4, Ymod4, family = binomial(link="logit"), nlambda = 200)
#Faltaría tunear (definir valor) de lambda

#glmnet tiene una opción para tunear usando K-CV
# sólo tiene algunas mediciones, por ejemplo deviance y mse
# para los modelos glm() en general
#Pero agrega dos para familiy=binomial i.e. regresión logística
set.seed(1)
mod4.lasso.tun=cv.glmnet(Xmod4, Ymod4, nfolds = 5, type.measure ="class", gamma = 0, relax = FALSE, family = "binomial", nlambda = 50)
plot(mod4.lasso.tun)
print.cv.glmnet(mod4.lasso.tun)
mod4.lasso.tun$lambda.min
coef(mod4.lasso.tun, s = "lambda.min")

# Regla final es aquella que usa el lambda seleccionado
# por ejemplo

logit4=predict(mod4.lasso.tun, newx = Xmod4, type = "response", s = "lambda.min")
res4=ifelse(logit4>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res4, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

##
# Medición del poder predictivo
# aquí si es importante incluir el tuneo dentro del entrenamiento

mod4RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  Xmod4ttotal = model.matrix(CAD~ .^2, data=Dat)[,-1]
  Xmod4t = Xmod4ttotal[train, ]
  Ymod4t = Dat[train,"CAD"] 
  mod4t.lasso.tun=cv.glmnet(Xmod4t, Ymod4t, nfolds = 5, type.measure ="class", gamma = 0, relax = FALSE, family = "binomial", nlambda = 50)
  preda=predict(mod4t.lasso.tun, newx = Xmod4ttotal[test,], type = "response", s = "lambda.min")
  predb=ifelse(preda>=.5,levels( Dat$CAD)[2],levels( Dat$CAD)[1])
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(1)
TCC.B.mod4= sapply(1:B,mod4RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod4=rowMeans(TCC.B.mod4))


##############################
### Quinto modelo a explorar
### LDA continuo, considerando variables binarias
##############################

# lda trabaja con la versión binaria de variables categóricas
# aunque la teoría corresponde al caso continuo
# funciona para más de dos clases.
library(MASS)
mod5 <- lda(CAD ~ ., cad1)
mod5p=predict(mod5, cad1)

logit5=mod5p$posterior[,2]
res5=mod5p$class  # con punto de corte .5
res5b=ifelse(logit5>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
sum(res5!=res5b)

# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res5, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# Medición del poder predictivo

mod5RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  mod5t <- lda(CAD ~ ., Dat[train,])
  mod5pt=predict(mod5t, Dat[test,])
  predb=mod5pt$class
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod5= sapply(1:B,mod5RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod5=rowMeans(TCC.B.mod5))



##############################
### Sexto modelo a explorar
### QDA continuo, considerando variables binarias
##############################

# qda trabaja con la versión binaria de variables categóricas
# aunque la teoría corresponde al caso continuo
# funciona para más de dos clases.
library(MASS)
mod6 <- qda(CAD ~ ., cad1)
mod6p=predict(mod6, cad1)

logit6=mod6p$posterior[,2]
res6=mod6p$class  # con punto de corte .5
res6b=ifelse(logit6>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
sum(res6!=res6b)

# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res6, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# Medición del poder predictivo

mod6RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  mod6t <- qda(CAD ~ ., Dat[train,])
  mod6pt=predict(mod6t, Dat[test,])
  predb=mod6pt$class
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod6= sapply(1:B,mod6RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod6=rowMeans(TCC.B.mod6))


##############################
### Séptimo modelo a explorar
### Naive classifier o método naive
##############################

library(e1071) 
#naiveBayes considera normales para continuas
#           y multinomiales para categóricas

mod7 <- naiveBayes(CAD ~ ., cad1)

logit7=predict(mod7, cad1, type="raw")[,2]
res7=predict(mod7, cad1) # clases con punto de corte .5
res7b=ifelse(logit7>=.5,levels(cad1$CAD)[2],levels(cad1$CAD)[1])
sum(res7!=res7b)

# Medidas aparentes de poder predictivo  
metrics_summary(obs = cad1$CAD, pred = res7, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# Medición del poder predictivo

mod7RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  modt <-  naiveBayes(CAD ~ ., Dat[train,])
  predb=predict(modt, Dat[test,])
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod7= sapply(1:B,mod7RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod7=rowMeans(TCC.B.mod7))

#Notar que error aparente vs poder predictivo es similar 
#en este modelo. Esto se debe a su simplicidad y robustez

##############################
### Octavo modelo a explorar
### a. KNN con variables binarias
### b. KKK con componentes principales
##############################


# función en paquete class considera distancia euclidiana
# es decir, variables continuas
# a. podemos usar transformación como en glmnet
# b. alguna versión de componentes principales

# la función considera que se da la información del train todo el tiempo
# es decir, incluye la predicción

#knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

# opción a.

Xmod8 <- model.matrix(CAD~ ., data=cad1)[,-1]
Ymod8 <- cad1[,"CAD"] 

head(Xmod8)

# con k fijo, eg 10
# predicción sobre los datos de entrenamiento
library(class)
res8k10=knn(train=Xmod8, test=Xmod8, Ymod8, k = 10, use.all = TRUE)
# medidad de error aparente
metrics_summary(obs = cad1$CAD, pred = res8k10, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# en la práctica debemos tunear k

library(e1071) #contiene herramientas para tunear con base en Error rate
# otra opción para tunear puede ser caret
set.seed(123)
knn.cross <- tune.knn(x = Xmod8, y = Ymod8, k = 1:20,tunecontrol=tune.control(sampling = "cross"), cross=5)
summary(knn.cross)
plot(knn.cross)

# Mejor valor de k a usar 
knn.cross$best.parameters[[1]]

# regla a usar para nuevos datos
res8kb=knn(train=Xmod8, test=Xmod8, Ymod8, k = knn.cross$best.parameters[[1]], use.all = TRUE)
metrics_summary(obs = cad1$CAD, pred = res8kb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')


# Medición del poder predictivo

mod8aRHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  Xmod8ttotal = model.matrix(CAD~ ., data=Dat)[,-1]
  Xmod8t = Xmod8ttotal[train, ]
  Xmod8test = Xmod8ttotal[test, ]
  Ymod8t = Dat[train,"CAD"] 
  knn.crosst <- tune.knn(x = Xmod8t, y = Ymod8t, k = 1:20,tunecontrol=tune.control(sampling = "cross"), cross=5)
  predb=knn(train=Xmod8t, test=Xmod8test, Ymod8t, k = knn.crosst$best.parameters[[1]], use.all = TRUE)
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(123)
TCC.B.mod8a= sapply(1:B,mod8aRHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod8a=rowMeans(TCC.B.mod8a))


# opción B, usamos una versión de CP para datos mixtos
# lo mismo se podría aplicar a todos los otros métodos como preprocesamiento


library(tidyverse)
library(FactoMineR)
cad1X=cad1 %>% dplyr::select(!(CAD))
CP_cad1X <- FAMD(cad1X, ncp=dim(cad1X)[2], graph=FALSE)
summary(CP_cad1X)
datosCPX=CP_cad1X$ind$coord  #podríamos quedarnos con menos dimensiones
                             #quizás usar una regla sobre la varianza
summary(datosCPX)
datosCPwC=data.frame(datosCPX)
datosCPwC$CAD=cad1$CAD
library(GGally)
X11()
datosCPwC %>%
  ggpairs(aes(color=CAD))

# Nota. Este preprocesamiento observa a todo los datos
# se debe incluir en la parte del train en cada iteración 
# del cálculo del poder predictivo

knn.cross8b <- tune.knn(x = datosCPX, y = cad1$CAD, k = 1:20,tunecontrol=tune.control(sampling = "cross"), cross=5)
plot(knn.cross8b)

res8bkb=knn(train=datosCPX, test=datosCPX, cad1$CAD, k = knn.cross8b$best.parameters[[1]], use.all = TRUE)
metrics_summary(obs = cad1$CAD, pred = res8bkb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')



# Medición del poder predictivo

mod8bRHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  DatX=Dat %>% dplyr::select(!(CAD))
  CP_DatXt <- FAMD(DatX[train,], ncp=dim(DatX)[2], graph=FALSE)
  datosCPXt=CP_DatXt$ind$coord
  datosCPXtest=predict(CP_DatXt,DatX[test,])$coord
  Ymod8t = Dat[train,"CAD"] 
  knn.crosst <- tune.knn(x = datosCPXt, y = Ymod8t, k = 1:20,tunecontrol=tune.control(sampling = "cross"), cross=5)
  predb=knn(train=datosCPXt, test=datosCPXtest, Ymod8t, k = knn.crosst$best.parameters[[1]], use.all = TRUE)
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(123)
TCC.B.mod8b= sapply(1:B,mod8bRHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod8b=rowMeans(TCC.B.mod8b))


#Comparación de modelos


ResGlob=as.data.frame(rbind(TCC.RHM.mod1,
TCC.RHM.mod2,
TCC.RHM.mod3,
TCC.RHM.mod4,
TCC.RHM.mod5,
TCC.RHM.mod6,
TCC.RHM.mod7,  
TCC.RHM.mod8a,
TCC.RHM.mod8b))
names(ResGlob)=c("accuracy", "recall", "specificity")

ResGlob
#mejor modelo es el NAIVE!
