# Regresi?n lineal m?ltiple
# Ajuste y pruebas de hip?tesis
library(GGally)

rm(list = ls(all.names = TRUE))
gc()

setwd("D:/dione/Documents/Seminario ML")
Datos=read.csv("ejemplo1RLM.csv", header=TRUE )

summary(Datos)

#Dos variables explicativas X1 y X2

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)
#Alternativamente con el paquete GGally:
ggpairs(Datos)
#El scatterplot es m?s d?ficil de usar en la regresi?n lineal m?ltiple
#pues s?lo se muestran relaciones entre pares de variables.
#En la regresi?n lineal m?ltiple nos preguntamos, por ejemplo,
# c?mo es el efecto de X2 en E(Y;X1,X2), as? que debemos
#considerar que en E(Y;X1,X2) aparecen tanto X1 y X2 variables.

#Un posible modelo es:
# E(y;X1, X2)=b0+b1X1+b2X2 

fit=lm(y~X1+X2, data=Datos)

summary(fit)

#Alternativamente:
fit1=lm(y~., data = Datos)

summary(fit1)

#Estimaciones puntuales
#\hat{\beta}
coef(fit)
#\hat{V(\hat{\beta})}
vcov(fit)
#\hat{\sigma}=\sqrt{SCE/(n-p-1)}
sigma(fit)


#Primero revisar prueba asociada a la Tabla ANOVA
#H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Del summary, ?ltimo rengl?n:
#F-statistic: 34.18 on 2 and 97 DF,  p-value: 5.829e-12
#Se rechaza H0. Entonces al menos uno de los coeficientes asociados
#a las variables es diferente de cero y nos ayuda a modelar E(y;x)


#Obtenci?n de la Prueba de la Tabla Anova 
#    usando la Prueba Lineal General.
library(multcomp)
K=matrix(c(0,1,0,
           0,0,1), ncol=3, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())


#Una vez que se realiza la prueba asociada a la tabla Anova
#la pregunta es si ambas variables aportan informaci?n al modelado.
#Para esto podemos hacer pruebas individuales sobre cada coeficiente
#usando las pruebas t o bien la prueba lineal general

#Las pruebas t que se obtienen del summary
#est?n asociadas a cada coeficiente de forma "no simult?nea"
#Para su interpretaci?n se sugiere identificar qu? 
#modelo resulta de considerar el par?metro asociado igual a cero.

#En este caso tenemos b0, b1 y b2. 

#Por ejemplo.

#El modelo que considera que b1=0, ser?a uno en el que s?lo est? X2
#es decir, 
#el modelo reducido E(y;X1, X2)=b0+b2X2 
#vs el completo     E(y;X1, X2)=b0+b1X1+b2X2

#La prueba t del summary para
#H0: b1=0 vs Ha: b1 != 0, nos lleva a concluir que no hay evidencia 
#para rechazar H0 con una significancia de alpha=.05
#Se toma el p-value del summary, rengl?n asociado a X1:
# p-value=0.29 > .05


#En este caso, la prueba est? elaborada para responder
#?La inclusi?n de la variable X1 una vez que se tiene 
#el modelo y=b0+b2X2+e 
#nos est? o no agregando informaci?n adicional?;
#en otras palabras,
#?condicional en la inclusi?n de X2 en el modelo,
#X1 nos agrega informaci?n adicional para modelar E(Y;x)?

#Cuando NO se rechaza Ho, parece que los datos nos sugieren que
#es plausible considerar
# el modelo "reducido"
#y=bo+b2X2+e contra el modelo completo y=bo+b1X1+b2X2+e,
# mientras que si se rechaza H0, entonces X1 s? nos agrega 
#informaci?n al modelo y se prefiere y=bo+b1X1+b2X2+e.


#Un an?lisis similar se puede hacer con b2. 
#Lo importante es notar que estos an?lisis
#corresponden a preguntas independientes sobre cada par?metro en el modelo 
#condicionando en la inclusi?n del resto de variables

#Las pruebas t se enfocan en una s?la combinaci?n de los par?metros.
#Tambi?n se pueden obtener de forma directa con el paquete multcomp
#de dos maneras:

#H0: b1=0 vs Ha: b1 != 0
K=matrix(c(0,1,0), ncol=3, nrow=1, byrow=TRUE)
m=c(0)
#usando prueba F equivalente (no permite pruebas de una cola con alternativa > o <)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#usando prueba t (permite prueba de una cola)
#Notar que s?lo eliminamos el argumento test=
summary(glht(fit, linfct=K, rhs=m))

#Ejemplo de una prueba de una cola
#H0: b1<=0 vs Ha: b1 > 0
summary(glht(fit, linfct=K, rhs=m, alternative = "greater") )


#Nota. Se puede ver que las pruebas t de dos colas, as? como las
#pruebas que se pueden incluir en la prueba lineal general
#son casos particulares en donde se comparan modelos anidados, es decir,
#el reducido vs el completo

#R tiene otras formas de obtener las pruebas
#usando el lenguaje de modelos anidados
#por ejemplo, la funci?n anova nos sirve para comparar modelos dos modelos anidados
#sin necesidad de usar la definici?n matricial (K y m)

#por ejemplo si b1=0, entonces el modelo reducido es
#y=bo+b2X2+e
#Lo ajustamos en R
fitred1=lm(y~X2, data=Datos)
summary(fitred1)

#ahora comparamos el modelo completo y el reducido
#H0: Modelo reducido es plausible
#Ha: Modelo completo
anova(fitred1, fit)
anova(fit,fitred1)

#(SCEred-SCEc)/SCEc  * (n-p-1)/r
SCEred=sigma(fitred1)^2*fitred1$df.residual 
SCEcom=sigma(fit)^2*fit$df.residual 
#n-p-1 son los grados de libertad del modelo completo
#r las combinaciones lineales necesarias para obtener el reducido
( (SCEred-SCEcom)/SCEcom *(fit$df.residual/1) )


#algo similar para obtener la prueba de la tabla anova
# en ese caso el modelo reducido es y=b0+e cuando H0: b1=0 y b2=0 

fitred2=lm(y~1, data=Datos)
summary(fitred2)
anova(fitred2, fit)


## Selecci?n entre los modelos. Aqu? casi los tres modelos
## indican un mismo coeficiente de determinaci?n, se seleccionar?a en 
## general el de menor n?mero de par?metros y 
## con el mayor coeficiente de determinaci?n
## aunque m?s adelante se considerar?n otros criterios BIC y AIC 
## o bien se realiza un an?lisis del poder predictivo del modelo

summary(fit)

fitred1=lm(y~X2, data=Datos)
summary(fitred1)

fitred2=lm(y~X1, data=Datos)
summary(fitred2)




#### ALERTA DE ERROR COM?N
#La lectura de las pruebas t NO debe hacerse de forma simult?nea
#ver por ejemplo,
summary(fit)
#muchas personas cometen el error de leer la salida de forma simult?nea
#concluyendo en este caso que ni X1 ni X2 aportan informaci?n al modelo
#lo que en este caso no es cierto basado en el an?lisis realizado
#de forma correcta con la prueba asociada a la tabla ANOVA.
