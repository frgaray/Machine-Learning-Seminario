# Continuaci?n del Ejemplo sobre la comparaci?n del poder predictivo
# considerando ?rboles de clasificaci?n y random forest

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
# El inter?s es predecir la condici?n de tener 
# Enfermedad de las arterias coronarias (CAD)
data(cad1) #Datos
str(cad1)
head(cad1, n=1)
summary(cad1) 
?cad1

# Por simplicidad se usar?:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# y c?lculo de la tasa de clasificaci?n correcta global como
# medida de poder predictivo


# La partici?n se realizar? con caret y 
# ser? la misma para todos los modelos
library(caret)
set.seed(1)
B=50
Partition<- createDataPartition(cad1$CAD, p = .80, list = FALSE, times = B)
# Notar que la partici?n considera 
# que la variable CAD es tipo factor y 
# genera particiones estratificando por los grupos de CAD


##############################
### Primer modelo a explorar
### ?rbol de clasificaci?n (CART)
##############################
##
# Descripci?n del m?todo de entrenamiento y regla final

library(rpart)

cart1=rpart(CAD ~ ., data=cad1, maxsurrogate=0)
cart1

# Los hiperpar?metros que se usan por default en rpart se pueden
# consultar usando 
# help(rpart.control)

# Por ejemplo, ya no divide un nodo si hay menos de 20 observaciones
# o si al realizar la divisi?n un nodo hijo tiene menos de 20/3 observaciones
# El m?ximo de profundidad es 30, contando como 1 la primera divisi?n
# del espacio generado por las p variables

# rpart tiene tambi?n un par?metro de complejidad, que com?nmente
# es el usado en el tuneo. Se llama cp y por default vale .01
# ?ste resume si la ganancia al dividir un subespacio es 
# relevante, valores grandes prodecen ?rboles peque?os, mientras
# que valores peque?os pueden producir ?rboles muy grandes

# En general, los cuatro hiperpar?metros antes mencionados
# sirven para definir la complejidad de los ?rboles y se 
# podr?an tunear


printcp(cart1)
summary(cart1, cp = 0.057)

library(rpart.plot)
X11()
prp(cart1,
    type = 4, # etiquetas de la regla de decisi?n en ambos lados
    clip.right.labs = FALSE, # poner etiquetas completas en ambos lados
    extra = 101, # n?mero de observaciones 
    under = TRUE, # posici?n de los indicado en extra
    under.cex = 1, # tama?o del texto en extra
    fallen.leaves = TRUE, # nivelas los nodos terminales
    box.palette = "GnYlRd", # agregar colores
    cex=.8)

# Si se quisiera usar el ?rbol obtenido por default
res <- predict(cart1,newdata=cad1, type="class")

# Las medidas aparentes de poder predictivo
library(metrica)
metrics_summary(obs = cad1$CAD, pred = res, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# para analizar los pasos del algoritmo se puede usar la funci?n summary
summary(cart1)

# Por ejemplo al iniciar el criterio Gini es
1-(129/236)^2-(107/236)^2
# considerando la separaci?n por AngPec la ganancia es
236*(1-(129/236)^2-(107/236)^2)-115*(1-(96/115)^2-(19/115)^2)-121*(1-(33/121)^2-(88/121)^2)
# se presentan las otras mejores separaciones de otras variables

# Esto mismo se presenta para otros nodos donde se realiz? una separaci?n
# e.g nodos 14 con 35 observaciones. El criterio Gini es
1-(14/35)^2-(21/35)^2
# considerando la separaci?n por AMI la ganancia es
35*(1-(14/35)^2-(21/35)^2)-15*(1-(10/15)^2-(5/15)^2)-20*(1-(4/20)^2-(16/20)^2)

# tuneo de hiperpar?metros
# se podr?a usar caret
# usaremos e1071

library(e1071) #contiene herramientas para tunear con base en Error rate
set.seed(123) #dar mallas, no tan finas, pues se podr?a tardar, depende de poder de c?mputo
cart1tun <- tune.rpart(CAD~., data = cad1, minsplit = c(5,10,15, 20),cp = c(.001,.005,.01,.015,.02,.03,.04))
summary(cart1tun)

# Mejores valores a usar 
cart1tun$best.parameters


cart2=rpart(CAD ~ ., data=cad1, maxsurrogate=0, minsplit =cart1tun$best.parameters[[1]], cp =cart1tun$best.parameters[[2]])
cart2

X11()
prp(cart2,
    type = 4, # etiquetas de la regla de decisi?n en ambos lados
    clip.right.labs = FALSE, # poner etiquetas completas en ambos lados
    extra = 101, # n?mero de observaciones 
    under = TRUE, # posici?n de los indicado en extra
    under.cex = 1, # tama?o del texto en extra
    fallen.leaves = TRUE, # nivelas los nodos terminales
    box.palette = "GnYlRd", # agregar colores
    cex=.8)

# Si se quisiera usar el ?rbol tuneado
res2 <- predict(cart2,newdata=cad1, type="class")

# Las medidas aparentes de poder predictivo
metrics_summary(obs = cad1$CAD, pred = res2, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')


# Medici?n del poder predictivo

#sin tunear
mod1RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  cartt=rpart(CAD ~ ., data=Dat[train,], maxsurrogate=0)
  predb=predict(cartt,newdata=Dat[test,], type="class")
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

TCC.B.mod1= sapply(1:B,mod1RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod1=rowMeans(TCC.B.mod1))
# [1] 0.8565217 0.8123810 0.8936000

# Medici?n del poder predictivo

#con tuneo
mod1tunRHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  cartttun <- tune.rpart(CAD~., data = Dat[train,], minsplit = c(5,10,15, 20),cp = c(.001,.005,.01,.015,.02,.03,.04))
  cartt=rpart(CAD ~ ., data=Dat[train,], maxsurrogate=0, minsplit =cartttun$best.parameters[[1]], cp =cartttun$best.parameters[[2]])
  predb=predict(cartt,newdata=Dat[test,], type="class")
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(123)
TCC.B.mod1tun= sapply(1:B,mod1tunRHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod1tun=rowMeans(TCC.B.mod1tun))
# [1] 0.8573913 0.8257143 0.8840000


##############################
### Segundo modelo a explorar
### Random Forest
##############################
##
# Descripci?n del m?todo de entrenamiento y regla final

library(randomForest)

# tiene los siguientes hiperpar?metros por default
# mtry: p/3 para y continua, sqrt(p) para y categ?rica
# ntree: 500
# nodesize: 5 para y continua, 1 para y categ?rica (?rboles muy profundos)
set.seed(123)
RF1=randomForest(CAD ~ ., data = cad1, importance=TRUE) 
print(RF1) #no confundir oob error rate con poder predictivo
           #por lo general es menor, aunque no es tan malo como el aparente
           #sirve m?s para tunear

# ya no se puede visualizar en un ?rbol, pero se puede 
# tener un resumen de las variables m?s importantes (mayores valores)
round(importance(RF1), 2)


# Varias estrategias para tunear
# caret siempre es una opci?n

# Otras alternativas
# Usando errores oob
oobRFi = function(i, mallaj, k) {
  j=max(mallaj)
  forest = randomForest(CAD ~ ., data = cad1, mtry = i, importance = F,
                        ntree = j, nodesize=k) 
  return(forest$err.rate[mallaj])  #da los errores oob al considerar de 1 a ntree
}


mallamtry=seq(1,13,2)
mallantree=c(50,100,500,1000)
mallanodesize=c(1,10,15)

tunoob=function(mallamtry, mallantree,mallanodesize){
nmtry=length(mallamtry)
nntree=length(mallantree)
nnodesize=length(mallanodesize)
iterk=0
tunRF1=matrix(NA, 4, nrow=nmtry*nntree*nnodesize)
for(jmt in 1:nmtry){
    for(jns in 1:nnodesize){
      iterk=iterk+1
      rowinf=(nntree)*(iterk-1)+1
      rowsup=(nntree)*(iterk-1)+nntree
      tunRF1[rowinf:rowsup,4]=oobRFi(mallamtry[jmt], mallantree, mallanodesize[jns])
      tunRF1[rowinf:rowsup,1]=mallamtry[jmt]
      tunRF1[rowinf:rowsup,2]=mallantree
      tunRF1[rowinf:rowsup,3]=mallanodesize[jns]
    }
}
return(tunRF1[which.min(tunRF1[,4]),])
}

set.seed(1)
tunoob(mallamtry, mallantree,mallanodesize)


# usando tune.randomForest
set.seed(123)
tunRF15CV=tune.randomForest(CAD ~ .,data=cad1,importance = F, mtry=mallamtry, ntree=mallantree,nodesize=mallanodesize, tunecontrol = tune.control(sampling = "cross", cross = 5))
# con mayor poder de c?mputo se puede realizar repetead k-CV usando en tune.control nrepeat = 10 o 100
tunRF15CV$best.parameters
tunRF15CV$best.performance

RF2=randomForest(CAD ~ ., data = cad1, mtry = tunRF15CV$best.parameters[[2]], importance = F,
                 ntree = tunRF15CV$best.parameters[[3]], nodesize=tunRF15CV$best.parameters[[1]])


# Si se quisiera usar el RF tuneado
res3 <- predict(RF2,newdata=cad1, type="class")

# Las medidas aparentes de poder predictivo
metrics_summary(obs = cad1$CAD, pred = res3, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')

# c?lculo del poder predictivo
# sin tuneo, hiperpar?metros por default
mod2RHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  RFt=randomForest(CAD ~ ., data = Dat[train,], importance = F)
  predb=predict(RFt,newdata=Dat[test,], type="class")
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(123)
TCC.B.mod2= sapply(1:B,mod2RHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod2=rowMeans(TCC.B.mod2))
# [1] 0.8582609 0.8257143 0.8856000

# con tuneo usando tune.randomForest y 5-CV
mod2tunRHM=function(x, IndTrain, Dat){
  train= IndTrain[,x]
  test = (-train)
  tunRFt5CV=tune.randomForest(CAD ~ .,data=Dat[train,],importance = F, mtry=mallamtry, ntree=mallantree,nodesize=mallanodesize, tunecontrol = tune.control(sampling = "cross", cross = 5))
  RFt=randomForest(CAD ~ ., data = Dat[train,], mtry = tunRFt5CV$best.parameters[[2]], importance = F,
                       ntree = tunRFt5CV$best.parameters[[3]], nodesize=tunRFt5CV$best.parameters[[1]])
  predb=predict(RFt,newdata=Dat[test,], type="class")
  resPod=metrics_summary(obs = Dat[test,"CAD"], pred = predb, metrics_list=c("accuracy", "recall", "specificity"),type = 'classification')
  return(resPod[,2])
}

set.seed(1234)
TCC.B.mod2tun= sapply(1:B,mod2tunRHM, IndTrain=Partition, Dat=cad1)
(TCC.RHM.mod2tun=rowMeans(TCC.B.mod2tun))
# [1] 0.8495652 0.8057143 0.8864000

# tunear requiere m?s poder de c?mputo, m?s cuando hay muchos hiperpar?metros
# lo que tiene tambi?n una implicaci?n directa en el poder de c?mputo
# que ser? necesario para evaluar el poder predictivo

# en algunos ejemplos, como este, usar los valores por default podr?a
# resultar en reglas m?s estables y con mejor poder predictivo,
# pero siempre queda la duda sobre si el tuneo se puede mejorar