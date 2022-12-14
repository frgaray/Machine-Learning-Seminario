# Ejemplo para selecci?n de variables
# como un problema de optimizaci?n con penalizaciones

# Uso del paquete glmnet

rm(list = ls(all.names = TRUE))
gc()

# Datos (mismos que los usados para optimizaci?n discreta)
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
# Preprocesamiento
bwtMod <- with(birthwt, {
  race <- factor(race, levels=c(1,2,3),labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0, levels=c(TRUE,FALSE),labels=c("TRUE", "FALSE"))
  ftv <- factor(ftv) # por default los niveles son 0,1,2,3,4,5,6
  levels(ftv)[-(1:2)] <- "2+"        #convertir todo los niveles con 2, 3, 4, 5 y 6 en 2+. Forma f?cil para recodificar
  data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke > 0),
             ptd, ht = factor(ht > 0), ui = factor(ui > 0), ftv)
})

summary(bwtMod)
str(bwtMod)


library(glmnet)

# Se necesita ingresar la matriz de dise?o X sin 
# columna asociada al intercepto
# Por simplicidad, modelo con efectos principales
XbwtMod <- model.matrix(low ~ ., data=bwtMod)[,-1]
Y <- bwtMod[,"low"] 

# estimaci?n equivalente a glm, es decir, sin penalizaciones
# por default alpha=1, es decir, m?todo Lasso
# tambi?n relax = FALSE, es decir, realizando estimaci?n directa usando gamma=1
#
fit0.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), standardize = FALSE, 
              intercept = TRUE, lambda = 0)
coef(fit0.glmnet)

fit0.glm <- glm(low ~ ., family = binomial("logit"), data = bwtMod)
summary(fit0.glm)

# Internamente estandariza, pero en la salida los coeficientes se regresan
# a la escala original. En este caso relax = TRUE (gamma=0) tambi?n da lo mismo
fit0b.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), standardize = TRUE, 
                      intercept = TRUE, lambda = 0, relax = TRUE)
coef(fit0b.glmnet)



#Uso de penalizaciones
# No se incluye argumento lambda, de manera que se ajusta para una malla de valores
# Se puede indicar la malla (sucesi?n de valores a evaluar) 
# bien el n?mero de valores que incluya la malla 
# (s?lo ajusta hasta que ya no ve cambios importantes)

#recordar que alpha=1 y relax = FALSE 
fit1.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), nlambda = 200)
print(fit1.glmnet)
coef(fit1.glmnet)
# C?lculo de valores proporcionales al BIC o AIC
# Se calculan a mano

AICfit1=-((fit1.glmnet$nulldev)-fit1.glmnet$nulldev * (1 - fit1.glmnet$dev.ratio))+2*fit1.glmnet$df
MinAICfit1=which.min(AICfit1)
coef(fit1.glmnet)[,MinAICfit1]

# Para el enfoque de estimaci?n o inferencia
# quiz?s es m?s f?cil y conveniente usar la opci?n
# relax=TRUE

fit2.glmnet <- glmnet(XbwtMod, Y, family = binomial("logit"), nlambda = 200, relax = TRUE)
print(fit2.glmnet)
coef(fit2.glmnet$relaxed)

AICfit2=-((fit2.glmnet$relaxed$nulldev)-fit2.glmnet$relaxed$nulldev * (1 - fit2.glmnet$relaxed$dev.ratio))+2*fit2.glmnet$relaxed$df
MinAICfit2=which.min(AICfit2)
coef(fit2.glmnet$relaxed)[,MinAICfit2]

# Este an?lisis se puede hacer directamente con glm
# para relax=TRUE


ModelList=list(NA)
AICList=list(NA)

nlam=length(fit2.glmnet$lambda)
XbwtModcint <- model.matrix(low ~ ., data=bwtMod)

for(jk in 1:nlam){
  Xjk=XbwtModcint[, coef(fit2.glmnet)[,jk]!=0]
  modeljk=glm.fit(Xjk, Y, family = binomial("logit"))
  ModelList[[jk]]=modeljk
  AICList[[jk]]=modeljk$aic
}

MinAIC=which.min(unlist(AICList))
ModMinAIC=ModelList[[MinAIC]]
coefficients(ModMinAIC)


# Con el BIC
BICfit2=-((fit2.glmnet$relaxed$nulldev)-fit2.glmnet$relaxed$nulldev * (1 - fit2.glmnet$relaxed$dev.ratio))+log(dim(bwtMod)[1])*fit2.glmnet$relaxed$df
MinBICfit2=which.min(BICfit2)
coef(fit2.glmnet$relaxed)[,MinBICfit2]

fitBIC.glm <- glm(low ~ ptd+lwt+ht, family = binomial("logit"), data = bwtMod)
summary(fitBIC.glm)
BIC(fitBIC.glm) #mismo modelo que con step y BIC

# Nota. glmnet tiene la funci?n cv.glmnet que ser? muy util para
# definir valor de lambda con base en m?tricas asociadas al poder predictivo

# Dado que la matriz dise?o se construye posiblemente con una f?rmula
# se pueden incluir modelos m?s complejos, e.g. con interacciones

XbwtModc <- model.matrix(low ~ .^2 + I(age^2)
                         + I(lwt^2), data=bwtMod)[,-1]
summary(XbwtModc)
# Hay una columna con puros ceros, de una interacci?n. Se debe eliminar
XbwtModc=XbwtModc[, !(colnames(XbwtModc) %in% c("htTRUE:uiTRUE"))]
# Hay una columna con puros ceros, de una interacci?n. Se debe eliminar
fit3.glmnet <- glmnet(XbwtModc, Y, family = binomial("logit"), nlambda=200  , relax = TRUE)
print(fit3.glmnet)


# Con el BIC
BICfit3=-((fit3.glmnet$relaxed$nulldev)-fit3.glmnet$relaxed$nulldev * (1 - fit3.glmnet$relaxed$dev.ratio))+log(dim(bwtMod)[1])*fit3.glmnet$relaxed$df
MinBICfit3=which.min(BICfit3)
coef(fit3.glmnet$relaxed)[,MinBICfit3]

fitBICc.glm <- glm(low ~ I(lwt*(ptd==FALSE)), family = binomial("logit"), data = bwtMod)
summary(fitBICc.glm)
BIC(fitBICc.glm)
