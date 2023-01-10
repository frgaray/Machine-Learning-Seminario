# Ejemplo para selección de variables
# como un problema de optimización discreta

# Dos opciones. 1) Por pasos (p pequeña o moderada) 
#             y 2) Mejor subconjunto (sólo para p pequeña)


rm(list = ls(all.names = TRUE))
gc()


# Datos
library(MASS)
help("birthwt")

str(birthwt)
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
srt(bwtMod)

#####################
################# Métodos por pasos


birthwt.glm <- glm(low ~ ., family = binomial("logit"), data = bwtMod)
summary(birthwt.glm)
# Función stepAIC, elimina variable por variable, es decir
# toma como conjunto todas las binarias de una variable categórica
# k=2 para AIC (default) y k = log(n) para BIC
# por default se tiene el método "backward" cuando no se indica nada en el 
# argumento scope (segundo argumento de la función)
birthwt.step <- stepAIC(birthwt.glm, trace = TRUE)  #Poner trace = FALSE en la práctica
summary(birthwt.step)
AIC(birthwt.step)

# también puede buscar entre dos opciones de modelos, incluyendo opciones
# más complejas, como interacciones. Esto se indica en el argumento scope
birthwt.step2 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                                                  + I(lwt^2), lower = ~1), trace = TRUE,direction ="both")
AIC(birthwt.step2)

# comparando con BIC, se penalizan más las interacciones al incluir muchos parámetros
birthwt.step3 <- stepAIC(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                         + I(lwt^2), lower = ~1), trace = TRUE,direction ="both", k=log(dim(bwtMod)[1]))
BIC(birthwt.step3) #Notar que la impresión de stepAIC indica AIC, aunque es el BIC
AIC(birthwt.step3)

# Nota. La función step de stats en R base hace exactamente lo mismo.

birthwt.step4 <- step(birthwt.glm, scope =list(upper = ~ .^2 + I(age^2)
                                                  + I(lwt^2), lower = ~1), trace = TRUE,direction ="both", k=log(dim(bwtMod)[1]))

# La salida es un objeto similar al glm
birthwt.step4$formula

# se pueden verificar los supuestos directamente
library(DHARMa)
set.seed(123)
birthwt.step4res <- simulateResiduals(fittedModel = birthwt.step4)
X11()
plot(birthwt.step4res)
summary(birthwt.step4)


birthwt.step2res <- simulateResiduals(fittedModel = birthwt.step2)
X11()
plot(birthwt.step2res)

summary(birthwt.step2)

# Los modelos están anidados, se podrían comparar
anova(birthwt.step2, birthwt.step4, test="Chisq")
# parece que hay términos significativos que salieron al usar el BIC

# En general, las variables categóricas son penalizadas más fuerte, 
# pues se consideran en bloque todas las binarias que se necesitan
# Una opción en el caso de tener muchas categorías, es concatenarlas (unirlas)
# para reducir parámetros.



#####################
################# Mejor subconjunto

##### Paquete bestglm

# Este paquete sirve para los modelos básicos de glm
# Requiere un formato especial para los datos: Xy
# Es decir, un dataframe donde X significa todas las variables a usar y 
# al final la columna con la variable dependiente y
# En este dataframe al considerar las variables categóricas
# se pueden incluir a mano las variables binarias (k-1, k #categorías)
# o bien se deben declarar como tipo factor

summary(bwtMod)

bwtModXy=bwtMod[,c(2:9,1)]
library(bestglm)

# La búsqueda sólo considera modelos con efectos principales
# desde una variable hasta el modelo con todas las variables
best.logit <- bestglm(bwtModXy,
                      IC = "AIC",                 
                      family=binomial("logit"),
                      method = "exhaustive")
summary(best.logit$BestModel) #objeto similar al obtenido con la función glm

# Para revisar más opciones
best.logit$Subsets

c(AIC(best.logit$BestModel), AIC(birthwt.step2), AIC(birthwt.step))

##### Paquete glmulti

# Este paquete permite usar el formato de fórmula
# para definir las variables a incluir en la búsqueda
# También permite considerar interacciones entre las variables

library(glmulti) #requiere java
# La fórmula sólo sirve para definir variables, realmente no considera las
# interacciones que ahí se puedan definir, eg low ~ .^2 + I(age^2)+ I(lwt^2)
# para eso se usa el argumento level (sólo permite 1 y 2)
# notar que al incluir I(age^2) y las interacciones de segundo orden
# se incluye a age^3 en la búsqueda

# Esto se tarda mucho
#glmulti.logit =  glmulti(low ~ . + I(age^2)+ I(lwt^2) , data = bwtMod,
#          level = 2,               # Orden interacción
#          method = "h",            # Método exhaustivo
#          crit = "bic",            # AIC o BIC
#          confsetsize = 5,         # 5 mejores modelos
#          plotty = F, report = F,  # No mostrar pasos
#          fitfunction = "glm",     # glm
#          family = binomial("logit") ) #argumentos para glm

#  Desventaja del genético es que no es reproducible, la semilla es de java
glmulti.logit =  glmulti(low ~ . + I(age^2)+ I(lwt^2) , data = bwtMod,
          level = 2,               # Orden interacción
          method = "g",            # Método numérico no exhaustivo (con d se analiza # modelos)
          popsize = 200,           # aumentar para mejores resultados, pero se tarda mucho
          crit = "bic",            # AIC o BIC
          confsetsize = 5,         # 5 mejores modelos
          plotty = F, report = F,  # No mostrar pasos
          fitfunction = "glm",     # glm
          family = binomial("logit") ) #argumentos para glm

#Revisión de los 5 modelos
glmulti.logit@formulas

summary(glmulti.logit@objects[[1]])

glmulti.logitME =  glmulti(low ~ . + I(age^2)+ I(lwt^2) , data = bwtMod,
          level = 1,               # Orden interacción
          method = "h",            # Método exhaustivo
          crit = "bic",            # AIC o BIC
          confsetsize = 5,         # 5 mejores modelos
          plotty = F, report = F,  # No mostrar pasos
          fitfunction = "glm",     # glm
          family = binomial("logit") ) #argumentos para glm



#Revisión de los 5 modelos
glmulti.logitME@formulas

summary(glmulti.logitME@objects[[1]])

BIC(glmulti.logitME@objects[[1]])
BIC(birthwt.step3)
BIC(glmulti.logit@objects[[1]])
