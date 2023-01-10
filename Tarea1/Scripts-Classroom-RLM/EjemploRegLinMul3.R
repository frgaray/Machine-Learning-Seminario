# Regresión lineal múltiple
# Ajuste e interpretación
# Variables categóricas

rm(list = ls(all.names = TRUE))
gc()

setwd("~/GitHub/Notas 2022-2/MNPyR")
Datos=read.csv("ejemplo3RLM.csv", header=TRUE )

summary(Datos)
str(Datos)

#Siempre asegurarse que la variable categórica esté definida como factor
Datos$X1c=factor(Datos$X1c)
str(Datos)

par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(Datos)
library(GGally)
ggpairs(Datos)

ggpairs(data=Datos, title="Datos", aes(colour = X1c))

X11()
with(Datos, plot(X2, y, col=c("red", "brown", "blue")[Datos[,c("X1c")]] ))
legend("topleft",levels(Datos[,c("X1c")]), col=c("red", "brown", "blue"), pch = c(0,0,0),  pt.cex=1.5,cex = .9, y.intersp = 1.3 , bty="n")



# R siempre considera como nivel de referencia el primer nivel
# guardado en el metadato de etiquetas
levels(Datos$X1c)
# El modelo que se ajusta por default en R 
# incluyendo sólo las dos variables X1c y X2 es
# E(y;x)=b0+b1A2+b2A3+b3x2

fit=lm(y~X1c+X2, data=Datos)
summary(fit)

#Analicemos la Interpretación del modelo con estos parámetros. 
#Después regresaremos a la prueba asociada a la tabla ANOVA.
#Aquí E(y;x1c, x2)=b0+b1A2+b2A3+b3x2
#con el nivel de referencia igual a A1
#Además notar que en este caso,
#si consideramos que x1c toma el valor del nivel A1,
#entonces el modelo para E(Y;x1c=A1,x2) es
#E(Y;x1c=A1,x2)=b0+b1(0)+b2(0)+b3x2 =b0+b3x2

#para nivel A2: E(Y;x1c=A2,x2)
#E(Y;x1c=A2,x2)=b0+b1(1)+b2(0)+b3x2=(b0+b1)+b3x2

#para nivel A3: E(Y;x1c=A3,x2)
#E(Y;x1c=A3,x2)=b0+b1(0)+b2(1)+b3x2=(b0+b2)+b3x2

#Es decir, con este modelo se considera que la E(Y;x1c,x2)
#es una recta en función del valor de x2 para cada nivel, 
#con diferente intercepto pero misma pendiente.



####### Otras formas de definir el mismo modelo en R
####### cuestiones computacionales
#Indicando las 2 v. dicótomicas (niveles) que deben entrar al modelo
fitAlt=lm(y~I(X1c=="A2")+I(X1c=="A3")+X2, data=Datos)
summary(fitAlt)

#cambiando el nivel de referencia que tiene por default R
#Aquí hay que obtener de nuevo las expresiones de
#E(Y;x1c=A1,x2), E(Y;x1c=A2,x2) y E(Y|;x1c=A3,x2)
fitAlt2=lm(y~relevel(X1c, "A3")+X2, data=Datos)
summary(fitAlt2)

fitAlt3=lm(y~I(X1c=="A1")+I(X1c=="A2")+X2, data=Datos)
summary(fitAlt3)


#el efecto de la función relevel
levels(Datos$X1c)
Datos$X1c=relevel(Datos$X1c, "A3")
levels(Datos$X1c)

#el efecto de la función factor con argumento levels
Datos$X1c=factor(Datos$X1c, levels=c("A1","A2","A3"))
levels(Datos$X1c)


#######
####### Regresemos al modelo

summary(fit)
#Observemos que con la prueba asociada a la tabla ANOVA, 
#se puede concluir que con una significancia de .05,
#se rechaza H0 en el contraste
#H0: b1=0 y b2=0 y b3=0 vs Ha: b1!=0 o b2!=0 o b3!=0

#Además, si analizamos la prueba t, para el coeficiente b3, 
#asociado a X2, podemos observar que se rechaza H0
#en el contraste H0: b3=0 vs Ha: b3 != 0.
#Esto nos indicaría que una vez incluida la variable 
#categórica X1c en el modelo, la variable 
#X2 está agregando información.

#Para analizar si la variable X1c agrega información
#dado que el modelo incluye a X2,
#debemos constrastar 
#H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Opción 1, usando multcomp
library(multcomp)
K=matrix(c(0,1,0,0,
           0,0,1,0), ncol=4, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#Se observa que se rechaza H0, 
#entonces la variable X1c nos proporciona información para
#modelar E(Y;x) aun cuando en el modelo ya está la variable X2.


#Opción 2, usando anova con modelos reducidos
fitred=lm(y~X2, data=Datos)
anova(fitred, fit)

#Opción 3, usar directamente la función drop1
drop1(fit, test = "F")

#Opcón 4, usar directamente la función Anova en librería car
library(car)
Anova(fit, type="II")
#Notar que lo anterior sólo se vale cuando 
#la variable categórica aparece en el modelo sólo a través de sus 
#k-1 v. dicotómicas (no hay interacciones entre variables).


#Por otro lado, si analizamos la prueba t, para el coeficiente b1,
#se rechaza H0. Aquí el contraste es H0: b1=0 vs Ha: b1 != 0.
#Esto nos indica, que condicional en un valor fijo de X2,
#la esperanza de Y es diferente entre 
#el nivel A2 y el A1 (de referencia)



#Ahora, si analizamos la prueba t, para el coeficiente b2,
#se rechaza H0. Aquí el contraste es H0: b2=0 vs Ha: b2 != 0.
#Esto nos indica, que condicional en un valor fijo de X2,
#la esperanza de Y es diferente entre
#el nivel A3 y el A1 (de referencia)

#Con base en lo anterior, 
#parece que no podríamos reducir el modelo, todos 
#los coeficientes parecen significativos.

#Interpretación del modelo

summary(fit)
#R2, el coeficiente de determinación, nos indica que se está
#explicando el 99.9% de la variabilidad observada en Y a través 
#del modelo que incluye ambas variables X1c y X2:
#E(y;X1c, X2)=b0+b1A2+b2A3+b3x2

#Además, b1 se puede interpretar como:
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en 1.31 unidades al comparar el nivel A2 contra el A1.
#(mayor esperanza en nivel A2 contra nivel A1)
#Recordar que nivel A1 es el de referencia en este modelo.
#Esta interpretación se obtiene al comparar
#E(Y;A2, X2)-E(Y;A1, X2)=b0+b1+b3x2-(b0+b3x2)=b1


#b2 se puede interpretar como:
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en .5 unidades al comparar el nivel A3 contra el A1.

#b3 se puede interpretar como
#condicionado en un nivel fijos de la variable X1c, el promedio 
#de la variable Y aumenta en 1.5 unidades 
#al aumentar en una unidad la variable X2.

#¿Cómo se comparan los niveles A2 y A3, dado un valor fijo de X2?
#E(Y;A2, X2)=bo+b1+b3X2
#E(Y;A3, X2)=bo+b2+b3X2

#E(Y;A3, X2)-E(Y;A2, X2)=b2-b1

#Por ejemplo si se tiene interés en contrastar
#H0:E(Y;A2, X2) =E(Y;A3, X2) vs
#Ha:E(Y;A2, X2)!=E(Y;A3, X2)

#Las hipótesis se pueden escribir como
#H0:b2-b1 =0 vs
#Ha:b2-b1!=0

#se puede usar multcomp

library(multcomp)
K=matrix(c(0,-1,1,0), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())
summary(glht(fit, linfct=K, rhs=m))

#Se rechaza la igualdad de medias, es decir, dado un valor de X2, 
#la esperanza de Y es diferente entre los niveles A3 y A2
#De hecho, la esperanza disminuye en .8135 unidades
#al comparar el nivel A3 contra el A2


#Para visualizar resultados del modelo
#E(y;X1c,X2)=b0+b1A2+b2A3+b3x2

#Recordar que en este caso, el modelo
#se relaciona con tres rectas, una por nivel
#Nivel A1
#E(Y;A1,x2)=b0+b3x2

#Nivel A2
#E(Y;A2,x2)=b0+b1+b3x2=(b0+b1)+b3x2

#Nivel A3
#E(Y;A3,x2)=b0+b2+b3x2=(b0+b2)+b3x2

#Es decir como función de X2, 
#son tres rectas que tienen diferente intercepto
#pero misma pendiente b3
fit$coefficients
#coef(fit)
fA1 <- function(X2) {fit$coefficients[1]+ fit$coefficients[4]*X2}
fA2 <- function(X2) {fit$coefficients[1]+fit$coefficients[2]+ fit$coefficients[4]*X2}
fA3 <- function(X2) {fit$coefficients[1]+fit$coefficients[3]+ fit$coefficients[4]*X2}

X11()
with(Datos, plot(X2, y, col=c("red", "green", "blue")[Datos[,2]] ))
legend("topleft",levels(Datos[,2]), col=c("red", "green", "blue"), pch = c(0,0,0), pt.cex=1.5,cex = .9, y.intersp = 1.4 , bty="n" )
curve(fA1, from = min(Datos$X2), to = max(Datos$X2),
      col = "red", add = T)
curve(fA2, from = min(Datos$X2), to = max(Datos$X2),
      col = "green", add = T)
curve(fA3, from = min(Datos$X2), to = max(Datos$X2),
      col = "blue", add = T)

