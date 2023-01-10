# Ejemplo para selección de variables
# como un problema de optimización con penalizaciones

# Uso del paquete smurf

rm(list = ls(all.names = TRUE))
gc()

# Datos (mismos que los usados para optimización discreta)
library(MASS)
help("birthwt")

str(birthwt)
summary(birthwt)
# Preprocesamiento
bwtMod <- with(birthwt, {
  race <- factor(race, levels=c(1,2,3),labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0, levels=c(TRUE,FALSE),labels=c("TRUE", "FALSE"))
  ftv <- factor(ftv) # por default los niveles son 0,1,2,3,4,5,6
  levels(ftv)[-(1:2)] <- "2+"        #convertir todo los niveles con 2, 3, 4, 5 y 6 en 2+. Forma fácil para recodificar
  data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke > 0),
             ptd, ht = factor(ht > 0), ui = factor(ui > 0), ftv)
})

summary(bwtMod)
str(bwtMod)


library(smurf)

# Smurf requiere que para cada variable se indique el tipo de 
# penalización que se debe usar

# Esto se debe indicar en un objeto tipo fórmula

# Recomendación
# Var. cat. nominales (#cat>2) usar generalized fused lasso "gflasso"
# Var. cat. ordinales (#cat>2) usar fused lasso "flasso", ya debe estar indicado el orden 
# Variables continuas o categóricas de dos niveles con lasso
# las variables categóricas de dos niveles también podrían ser gflasso o flasso

formu <- low ~ p(age, pen = "lasso") + p(lwt, pen = "lasso") +
              p(race, pen = "gflasso") + p(smoke, pen = "lasso") +
              p(ptd, pen = "lasso") + p(ht, pen = "lasso") +
              p(ui, pen = "lasso") + p(ftv, pen = "flasso")

# glmsmurf sí tiene implementada la búsqueda con aic o bic
# también tiene métricas sobre poder predictivo

# family con las mismas opciones que glm

#varios tipos de estandarización, se recomienda "glm.stand"
#lambda, un valor en particular o bien un criterio

# para aumentar el tamaño de la malla o indicarla a mano
# se deben usar argumentos de función glmsmurf.control
# en control

ejemplo.fit <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.bic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
#gráfica mostrando los diferentes BIC y el valor
#de lambda donde se obtiene el menor
plot_lambda(ejemplo.fit)
ejemplo.fit$lambda
log(ejemplo.fit$lambda)


# La siguiente gráfica resume por variable (divididas por líneas verticales)
# los coeficientes que se consideran cero (en cuadrados grises)
# para concatenar niveles de variables categóricas se usan los mismos colores

plot(ejemplo.fit, cex=3)
# el orden de las variables es el que se usa en la fórmula
# el primer espacio corresponde al intercepto

# age coeficiente igual a cero
# lwt coeficiente diferente de cero, aunque pequeño
# race dos variables binarias quitando la referencia, ambos coeficientes igual cero
# smoke al ser definida como lasso (var cat 2 niv), 
#       un coeficiente es cero por default, el otro es cero por la penalización 
# pth   un coeficiente es dif de cero, asociado a la binaria que entra
# ht    un coeficiente es dif de cero, asociado a la binaria que entra
# ui    todo cero
# ftv   cat ordinal, pero todas las iguala a cero


# las estimaciones se presentan en dos columnas
# Estimated corresponde a los betas estimadas que resuelven el problema con penalizaciones
# Re-estimated es la versión relax o las estimaciones que se obtiene al usar
#             máxima verosimilitud considerando sólo las variables asociadas a estimaciones
#             de betas diferentes de cero
summary(ejemplo.fit)

fitBIC.glm <- glm(low ~ I(ptd==TRUE)+lwt+I(ht==FALSE), family = binomial("logit"), data = bwtMod)
summary(fitBIC.glm)
BIC(fitBIC.glm)

# Es más lento que glmnet, también es un poco más complejo definir modelos
# pero permite concatenar categorías

ejemplo.fitAIC <- glmsmurf(formula = formu, family = binomial("logit"), data = bwtMod, 
                        pen.weights = "glm.stand", lambda = "is.aic", control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))
plot(ejemplo.fitAIC, cex=3)
summary(ejemplo.fitAIC)
# Aquí se concatenan los niveles black y other


