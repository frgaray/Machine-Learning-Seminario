#Herramientas para verificar supuestos
#Ejemplo para modelar las ventas a partir de la publicidad en TV

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2023-1/ApreEstAut")
options(digits=4)  
Datos=read.csv("Advertising.csv", header=TRUE, sep="," )

head(Datos)
summary(Datos)
require(ggplot2)
ggplot(Datos, aes(x=TV,y=sales)) +
  geom_point()

#Modelo uno
glm1=glm(sales~TV, data=Datos, family = Gamma(link="inverse"))
summary(glm1)

#Panel de varias gráficas de utilidad para verificar supuestos

X11()
library(ggplot2)
library(ggResidpanel)
resid_panel(glm1, plots=c("all"))

#En la primera gráfica se observa un problema con
#la forma de definir el componente lineal en este modelo

X11()
library(car)
residualPlots(glm1)

#El test que se presenta sirve para verificar si
#hay un problema en el componente lineal,
#pero es limitado, pues sólo revisa la posible
#necesidad de incluir un término cuadrático de
#la variable en cuestión al componente sistemático

#Se busca no rechazar H0, en caso de rechazar
#se podría incluir el término cuadrático

#Por otro lado.
#Para analizar si es necesaria alguna transformación tipo potencia
#a alguna covariable (digamos x) se sugiere agregar I(x*log(x)) al modelo
#y analizar el p-value, en caso de rechazar H0 
#se estaría observando la necesidad de alguna transformación

glm1lin=glm(sales~TV+I(TV*log(TV)), data=Datos, family = Gamma(link="inverse"))
summary(glm1lin)

#En este caso se rechaza H0 (2.2e-13 ***), lo que sugiere la necesidad de alguna
#transformación a TV

#Una posible regla de dedo para encontrar una potencia para transformar es
(pote=1+coef(glm1lin)[3]/coef(glm1)[2]) #el cociente es entre las estimaciones
                                        #del parámetro del término que se agregó
                                        #y del parámetro asociado a TV en el original
                                        
#En este caso, la potencia -1/2 podría ser una opción a probar

# Otras gráficas y herramientas

# Para analizar el supuesto del componente aleatorio y función liga
# usando residuales quantile

library(statmod)
glm1qr <- qresid( glm1 )
X11()
qqnorm( glm1qr, las=1 ); qqline( glm1qr)
nortest::lillie.test(glm1qr)
shapiro.test(glm1qr)

# Análisis usando los residuales simulados
# La primer gráfica se enfoca en el componente aleatorio y función liga
# La segunda se enfoca más en la parte del componente lineal

library(DHARMa)
set.seed(123)
glm1res <- simulateResiduals(fittedModel = glm1)
X11()
plot(glm1res)

#En este caso se observa en la segunda gráfica un fuerte
#problema en el componente lineal



#Modelo uno con la transformación sugerida
glm1b=glm(sales~I(TV^(-1/2)), data=Datos, family = Gamma(link="inverse"))

summary(glm1b)
AIC(glm1b)

X11()
resid_panel(glm1b, plots=c("all"))

glm1bqr <- qresid( glm1b )
X11()
qqnorm( glm1bqr, las=1 ); qqline( glm1bqr)
nortest::lillie.test(glm1bqr)
shapiro.test(glm1bqr)

glm1bres <- simulateResiduals(fittedModel = glm1b)
X11()
plot(glm1bres)


#Sigue existiendo algo de problemas con el componente lineal


#Analicemos los otros modelos encontrados 

glm2=glm(sales~I(TV^(-1/2)), data=Datos, family = inverse.gaussian(link="inverse"))
glm3=glm(sales~I(log(TV)), data=Datos, family = Gamma(link="log"))

X11()
resid_panel(glm2, plots=c("all"))
glm2qr <- qresid( glm2 )
X11()
qqnorm( glm2qr, las=1 ); qqline( glm2qr)
nortest::lillie.test(glm2qr)
shapiro.test(glm2qr)

glm2res <- simulateResiduals(fittedModel = glm2)
X11()
plot(glm2res)



X11()
resid_panel(glm3, plots=c("all"))
glm3qr <- qresid( glm3 )
X11()
qqnorm( glm3qr, las=1 ); qqline( glm3qr)
nortest::lillie.test(glm3qr)
shapiro.test(glm3qr)

glm3res <- simulateResiduals(fittedModel = glm3)
X11()
plot(glm3res)


# Posibles valores a analizar (valores influyentes)
library(broom)
Datosglm3=augment(glm3)
Datosglm3$index=1:dim(Datos)[1]
Datosglm3=Datosglm3[order(Datosglm3$.cooksd, decreasing = TRUE),]
Datos[Datosglm3$index[1:5],]

glm3b=glm(sales~I(log(TV)), data=Datos[-c(131),], family = Gamma(link="log"))

library(jtools)
export_summs(glm3, glm3b, scale = FALSE)

#Los AIC y BIC no son estrictamente comparables
#Las estimaciones casi no cambian, casi las mismas interpretaciones


X11()
resid_panel(glm3b, plots=c("all"))
glm3bqr <- qresid( glm3b )
X11()
qqnorm( glm3bqr, las=1 ); qqline( glm3bqr)
nortest::lillie.test(glm3bqr)
shapiro.test(glm3bqr)

glm3bres <- simulateResiduals(fittedModel = glm3b)
X11()
plot(glm3bres)


## Parece que el mejor modelo es el glm3, en el sentido de que tiene 
## un AIC competitivo y además no hay evidencia MUY FUERTE contra los supuestos

