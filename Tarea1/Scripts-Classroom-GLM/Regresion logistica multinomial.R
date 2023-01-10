library(xlsx)
library(tidyverse)
library(VGAM)
library(ggplot2)
rm(list = ls(all.names = TRUE))
gc()

########################## Lectura y formato BD ###############################

getwd()
DatosAg <- read.xlsx("Table 8.1 Car preferences.xls",1)

# Acomodamos los datos de forma desagrupada y convertimos
# las variables de tipo factor
Datos <- DatosAg %>% group_by(Sexo,Edad,Respuesta) %>%
  do( data.frame(unos= rep(1, .$Frecuencia)) ) %>% ungroup()
Datos=as.data.frame(Datos)
Datos$Sexo <- factor(Datos$Sexo,levels = c("Mujeres","Hombres"))
Datos$Edad <- factor(Datos$Edad,levels = c("18-23","24-40","> 40"))
Datos$Respuesta <- factor(Datos$Respuesta, levels = c("No/poco","Importante","Muy importante"))
summary(Datos)

######################### Ajuste del modelo nominal ############################

# Ajustamos el modelo con interacciones tomando como referencia la categoría
# "No/poco" que corresponde al primer nivel de la variable del data frame (\pi_1)
fit <- vglm(Respuesta ~ Sexo*Edad,family = multinomial(refLevel = "No/poco"),data = Datos)

# Los coeficientes del modelo son:
coef(fit,matrix=T)

# Recordando que log(\pi_2 / \pi_3) = log((\pi_2 / \pi_1) / (\pi_3 / pi_1)) =
# log(\pi_2 / \pi_1) - log(\pi_3 / \pi_1) que numéricamente es:
coef(fit,matrix=T)[,1]-coef(fit,matrix=T)[,2]
# Ajustando el modelo tomando como referencia a la categoría "Muy importante"
# que corresponde al tercer nivel de la variable del data frame (\pi_3)
fit2 <- vglm(Respuesta ~ Sexo*Edad,family = multinomial(refLevel = "Muy importante"),data = Datos)
coef(fit2,matrix=T)
# En efecto los coeficientes coinciden, de modo que no importa cual categoría se tome como referencia,
# por lo que seguiremos usando el primer modelo

# Realicemos ahora la prueba similar a la asociada a la tabla anova,
# para eso construyamos un modelo sin variables explicativas
fit0 <- vglm(Respuesta ~ 1,family = multinomial(refLevel = "No/poco"),data = Datos)

anova(fit0,fit,type = "I")
lrtest(fit0,fit)
# Recordemos que la prueba es:
# H_0: Modelo reducido    vs     H_a: Modelo completo
# Como p-value < .05, encontramos evidencia estadística contra H_0, por lo tanto
# nos quedamos con el modelo completo.

# Construimos ahora el modelo sin interacciones y hagamos la misma prueba:
fitred <- vglm(Respuesta ~ Sexo+Edad,family = multinomial(refLevel = "No/poco"),data = Datos)

anova(fitred,fit,type = "I")
lrtest(fitred,fit)
# Como p-value > .05, no encontramos evidencia estadística contra H_0, por lo tanto
# asumimos plausible quedarnos con el modelo reducido.

# Hagamos la misma prueba del modelo más sencillo contra el modelo sin interacciones:
anova(fit0,fitred,type = "I")
lrtest(fit0,fitred)
# Como p-value < .05, encontramos evidencia estadística contra H_0, por lo tanto
# nos quedamos con el modelo completo.

summary(fitred)
exp(coef(fitred,matrix=T))
exp(confint(fitred))
summary(fit)

b02 <- coefficients(fitred)[1]
b03 <- coefficients(fitred)[2]
b12 <- coefficients(fitred)[3]
b13 <- coefficients(fitred)[4]
b22 <- coefficients(fitred)[5]
b23 <- coefficients(fitred)[6]
b32 <- coefficients(fitred)[7]
b33 <- coefficients(fitred)[8]

###### Sexo: Mujer -> x1 = 0 ####
# Edad: 18-23 -> x2 = 0  y  x3 = 0 -> 
# log(pi_2/pi_1) = b02
# log(pi_3/pi_1) = b03
tot_m18 <- c(b02,b03)
(pi2_m18 <- exp(tot_m18[1])/(1+sum(exp(tot_m18)))) # Prob mujer de 18-23 años elija "Importante"
(pi3_m18 <- exp(tot_m18[2])/(1+sum(exp(tot_m18)))) # Prob mujer de 18-23 años elija "Muy importante"
(pi1_m18 <- 1-pi2_m18-pi3_m18) # Prob mujer de 18-23 años elija "No/poco importante"

# Edad: 24-40 -> x2 = 1  y  x3 = 0 -> 
# log(pi_2/pi_1) = b02 + b22
# log(pi_3/pi_1) = b03 + b23
tot_m24 <- c(b02+b22,b03+b23)
(pi2_m24 <- exp(tot_m24[1])/(1+sum(exp(tot_m24)))) # Prob mujer de 24-40 años elija "Importante"
(pi3_m24 <- exp(tot_m24[2])/(1+sum(exp(tot_m24)))) # Prob mujer de 24-40 años elija "Muy importante"
(pi1_m24 <- 1-pi2_m24-pi3_m24) # Prob mujer de 24-40 años elija "No/poco importante"

# Edad: > 40 -> x2 = 0  y  x3 = 1 -> 
# log(pi_2/pi_1) = b02 + b32
# log(pi_3/pi_1) = b03 + b33
tot_m40 <- c(b02+b32,b03+b33)
(pi2_m40 <- exp(tot_m40[1])/(1+sum(exp(tot_m40)))) # Prob mujer de > 40 años elija "Importante"
(pi3_m40 <- exp(tot_m40[2])/(1+sum(exp(tot_m40)))) # Prob mujer de > 40  años elija "Muy importante"
(pi1_m40 <- 1-pi2_m40-pi3_m40) # Prob mujer de > 40  años elija "No/poco importante"

###### Sexo: Hombre -> x1 = 1 ####
# Edad: 18-23 -> x2 = 0  y  x3 = 0 -> 
# log(pi_2/pi_1) = b02 + b12
# log(pi_3/pi_1) = b03 + b13
tot_h18 <- c(b02+b12,b03+b13)
(pi2_h18 <- exp(tot_h18[1])/(1+sum(exp(tot_h18)))) # Prob hombre de 18-23 años elija "Importante"
(pi3_h18 <- exp(tot_h18[2])/(1+sum(exp(tot_h18)))) # Prob hombre de 18-23 años elija "Muy importante"
(pi1_h18 <- 1-pi2_h18-pi3_h18) # Prob hombre de 18-23 años elija "No/poco importante"

# Edad: 24-40 -> x2 = 1  y  x3 = 0 -> 
# log(pi_2/pi_1) = b02 + b12 + b22
# log(pi_3/pi_1) = b03 + b13 + b23
tot_h24 <- c(b02+b12+b22,b03+b13+b23)
(pi2_h24 <- exp(tot_h24[1])/(1+sum(exp(tot_h24)))) # Prob hombre de 24-40 años elija "Importante"
(pi3_h24 <- exp(tot_h24[2])/(1+sum(exp(tot_h24)))) # Prob hombre de 24-40 años elija "Muy importante"
(pi1_h24 <- 1-pi2_h24-pi3_h24) # Prob hombre de 24-40 años elija "No/poco importante"

# Edad: > 40 -> x2 = 0  y  x3 = 1 -> 
# log(pi_2/pi_1) = b02 + b12 + b32
# log(pi_3/pi_1) = b03 + b13 + b33
tot_h40 <- c(b02+b12+b32,b03+b13+b33)
(pi2_h40 <- exp(tot_h40[1])/(1+sum(exp(tot_h40)))) # Prob hombre de > 40 años elija "Importante"
(pi3_h40 <- exp(tot_h40[2])/(1+sum(exp(tot_h40)))) # Prob hombre de > 40  años elija "Muy importante"
(pi1_h40 <- 1-pi2_h40-pi3_h40) # Prob hombre de > 40  años elija "No/poco importante"

# Para obtener estas mismas probabilidades aprovechando las funciones de R:
# Primero obtenemos las combinaciones únicas de las dos categorías
combinaciones <- unique(Datos[,1:2]) %>% arrange(Sexo,Edad)

# Ahora con la función predict de tipo "response", podemos obtener las probabilidades
predict(fitred,combinaciones,type = "response")

####### Gráficas ########
# Mujeres
tabla1m <- predict(fitred,combinaciones,type = "response")[1:3,]
row.names(tabla1m) <- c("18-23","24-40","> 40")
tabla2m <- t(tabla1m)

xx <- barplot(tabla1m, beside=TRUE,main = 'Respuesta Mujeres',xlab= "Respuesta",
        ylab='Probabilidades', las=1, col = c("#19889E","#EB5493","#3ECDEB"),ylim = c(0,.7))
legend('top', legend=rownames(tabla1m), bty='n',
       fill=c("#19889E","#EB5493","#3ECDEB"))
text(x=xx,y = tabla1m, pos=3, cex = 1, col = c("#19889E","#EB5493","#3ECDEB"),
     label=round(tabla1m,4))

# Hombres
tabla1h <- predict(fitred,combinaciones,type = "response")[4:6,]
row.names(tabla1h) <- c("18-23","24-40","> 40")
tabla2h <- t(tabla1h)

xx <- barplot(tabla2h, beside=TRUE,main = 'Respuesta Hombres',xlab= "Edades",
        ylab='Probabilidades', las=1, col = c("#517C9E","#EB7673","#8FC2EB"),ylim = c(0,.7))
legend('top', legend=rownames(tabla2h), bty='n',
       fill=c("#517C9E","#EB7673","#8FC2EB"))
text(x=xx,y = tabla2h, pos=3, cex = 1, col = c("#517C9E","#EB7673","#8FC2EB"),
     label=round(tabla2h,4))

# Hagamos la comparación del modelo contra las frecuencias relativas
# Mujeres
DatosM <- Datos %>% filter(Sexo == "Mujeres")
DatosM18 <- DatosM %>% filter(Edad=="18-23")
freq_m18 <- prop.table(table(DatosM18$Respuesta))
DatosM24 <- DatosM %>% filter(Edad=="24-40")
freq_m24 <- prop.table(table(DatosM24$Respuesta))
DatosM40 <- DatosM %>% filter(Edad=="> 40")
freq_m40 <- prop.table(table(DatosM40$Respuesta))
frecuencias_mujeres <- rbind(freq_m18,freq_m24,freq_m40)
row.names(frecuencias_mujeres) <- c("18-23","24-40","> 40")
frecuencias_mujeres2 <- t(frecuencias_mujeres)

par(mfrow=c(2,1),cex =.8)
# Frecuencias relativas
xx <- barplot(frecuencias_mujeres2, beside=TRUE,main = 'Respuesta Mujeres',xlab= "Edades",
        ylab='Frecuencias relativas', las=1, col = c("#19889E","#EB5493","#3ECDEB"),ylim = c(0,.7))
legend('top', legend=rownames(frecuencias_mujeres2), bty='n',
       fill=c("#19889E","#EB5493","#3ECDEB"))
text(x=xx,y = frecuencias_mujeres2, pos=3, cex = 1, col = c("#19889E","#EB5493","#3ECDEB"),
     label=round(frecuencias_mujeres2,4))
# Modelo
xx <- barplot(tabla2m, beside=TRUE,main = 'Respuesta Mujeres',xlab= "Edades",
        ylab='Probabilidades', las=1, col = c("#19889E","#EB5493","#3ECDEB"),ylim = c(0,.7))
legend('top', legend=rownames(tabla2m), bty='n',
       fill=c("#19889E","#EB5493","#3ECDEB"))
text(x=xx,y = tabla2m, pos=3, cex = 1, col = c("#19889E","#EB5493","#3ECDEB"),
     label=round(tabla2m,4))

# Hombres
DatosH <- Datos %>% filter(Sexo == "Hombres")
DatosH18 <- DatosH %>% filter(Edad=="18-23")
freq_H18 <- prop.table(table(DatosH18$Respuesta))
DatosH24 <- DatosH %>% filter(Edad=="24-40")
freq_H24 <- prop.table(table(DatosH24$Respuesta))
DatosH40 <- DatosH %>% filter(Edad=="> 40")
freq_H40 <- prop.table(table(DatosH40$Respuesta))
frecuencias_hombres <- rbind(freq_H18,freq_H24,freq_H40)
row.names(frecuencias_hombres) <- c("18-23","24-40","> 40")
frecuencias_hombres2 <- t(frecuencias_hombres)

# Frecuencias relativas
xx <- barplot(frecuencias_hombres2, beside=TRUE,main = 'Respuesta Hombres',xlab= "Edades",
        ylab='Frecuencias relativas', las=1, col = c("#517C9E","#EB7673","#8FC2EB"),ylim = c(0,.7))
legend('top', legend=rownames(frecuencias_hombres2), bty='n',
       fill=c("#517C9E","#EB7673","#8FC2EB"))
text(x=xx,y = frecuencias_hombres2, pos=3, cex = 1, col = c("#517C9E","#EB7673","#8FC2EB"),
     label=round(frecuencias_hombres2,4))
# Modelo
xx <- barplot(tabla2h, beside=TRUE,main = 'Respuesta Hombres',xlab= "Edades",
        ylab='Probabilidades', las=1, col = c("#517C9E","#EB7673","#8FC2EB"),ylim = c(0,.7))
legend('top', legend=rownames(tabla2h), bty='n',
       fill=c("#517C9E","#EB7673","#8FC2EB"))
text(x=xx,y = tabla2h, pos=3, cex = 1, col = c("#517C9E","#EB7673","#8FC2EB"),
     label=round(tabla2h,4))

# Usando dplyr xD
aux <- Datos %>%
  group_by(Sexo,Edad,Respuesta) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

matrix(aux$freq[10:18],nrow = 3, ncol = 3)

######################### Ajuste del modelo ordinal ############################
# Con el supuesto de probabilidades proporcionales
fitord_p <- vglm(Respuesta ~ Sexo+Edad,family = cumulative(parallel = T),data = Datos)
summary(fitord_p)

# Modelo sin variables explicativas:
fitord0_p <- vglm(Respuesta ~ 1,family = cumulative(parallel = T),data = Datos)

# Prueba para ver si el modelo tiene sentido:
anova(fitord0_p,fitord_p,type = "I")
lrtest(fitord0_p,fitord_p)

# Sin el supuesto de probabilidades proporcionales
fitord_np <- vglm(Respuesta ~ Sexo+Edad,family = cumulative(parallel = F),data = Datos)
summary(fitord_np)

# Prueba para ver si el modelo tiene sentido:
anova(fitord0_p,fitord_np,type = "I")
lrtest(fitord0_p,fitord_np)

# Prueba para ver si quedarnos con el supuesto es suficiente:
anova(fitord_p,fitord_np,type = "I")
lrtest(fitord_p,fitord_np)

# Como p-value > .05, no encontramos evidencia estadística contra H_0, por lo tanto
# asumimos plausible quedarnos con el modelo reducido, i.e. con el
# supuesto de probabilidades proporcionales.

predict(fitord_p,combinaciones,type = "response")

##### Comparativa de modelos con AIC y BIC #####
AIC1 <- AIC(fitred)
AIC2 <- AIC(fitord_p)
AIC3 <- AIC(fitord_np)

BIC1 <- BIC(fitred)
BIC2 <- BIC(fitord_p)
BIC3 <- BIC(fitord_np)

# Por ambos criterios nos quedamos con el modelo ordinal
# con el supuesto de probabilidades proporcionales
