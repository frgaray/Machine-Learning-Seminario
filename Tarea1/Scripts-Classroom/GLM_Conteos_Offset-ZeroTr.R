### Ejemplos de modelado para conteos, donde se usa un término offset y
### es necesario usar un modelo especial dado la ausencia de ceros


rm(list = ls(all.names = TRUE))
gc()

# Uso de un término offset (tasas)
library(mdhglm)
data("train")
help("train")
#### Los datos corresponden a accidentes anuales entre trenes
#### y carros en Inglaterra entre 1975 y 2003.

#### Además de los accidentes, la base de datos contiene
#### los millones de kilometros que los trenes
#### viajaron en cada uno de los años

#### La pregunta de investigación era analizar
#### si los accidentes se han incrementado en los últimos años

#### Para lo anterior era necesario estandarizar por el
#### número de millones de kilometros que los trenes viajaron al año
#### es decir, es conveniente hablar de tasas (incidencia) en lugar de conteos

#### En este caso, el interés es ver si
#### log(mu/kil)=b0+b1x
#### es decir los conteos se estandarizan por el número de kilometros
#### y la variable explicativa es el tiempo
#### este último se analiza considerando x=el número de años desde 1975
#### x = 0 para 1975 y x=28 para 2003


head(train)


summary(train)

X11()
plot(train$x, train$y/train$t)

# Notar que para dejar en término de la variable con conteos
# al considerar la liga log se debe incluir log(train$t)
# con otra liga se debe incluir de forma diferente
train$logtkil=log(train$t)

fit1 <- glm(y ~ x+offset(logtkil) , family=poisson(link="log"), data=train)
summary(fit1)

# Regla de dedo para analizar si hay un problema por 
# considerar el parámetro de dispersión igual a 1
deviance(fit1)/df.residual(fit1)

# En este caso no es muy diferente de 1

library(DHARMa)  
set.seed(123)
fit1res <- simulateResiduals(fittedModel = fit1)
X11()
plot(fit1res )

# Tampoco se observan evidencias en contra de los supuestos de este modelo
# Éste se podría usar para analizar las preguntas de investigación

# Antes, veamos si tiene alguna ventaja considerar un modelo
# binomial negativo, de la misma forma considerando la liga log
library(MASS)
fit2 <- glm.nb(y ~ x+offset(logtkil), data=train, link="log")
summary(fit2)

set.seed(123)
fit2res <- simulateResiduals(fittedModel = fit2)
X11()
plot(fit2res)

# También parece un buen modelo 

c(AIC(fit1), AIC(fit2))
# Usando el AIC también se podría optar por el modelo binomial negativo
c(BIC(fit1), BIC(fit2))
# Pero con el BIC se prefiere el Poisson (menos parámetros)

# Realmente la interpretación de ambos modelos llevará a las mismas conclusiones
# pues se parecen mucho.


### Interpretación

summary(fit2)

#log(mu/kil)=b0+b1x
#-4.2-.0337x

#La tasa de accidentes ha decrecido, ya que beta1 es negativo y 

exp(coef(fit2)[2]) #es el factor asociado
#Es decir, por cada año se ha reducido un (1-.966689164)*100%

# Por ejemplo, comparemos las tasas promedio para 1975 y 2003
# Supongamos que además de la estimación puntual también queremos
# intervalos de confianza

# Si se quiere usar predict, es necesario incluir el offset
# pues así se ajustó el modelo, donde este está en el componente lineal

dat7503= data.frame(x=c(0, 28), logtkil=c(train$logtkil[train$x==0], train$logtkil[train$x==28]) )

# Estimamos el componente lineal (guardamos las estimaciones de las raíces de las varianzas)
logmu=predict(fit2, newdata = dat7503, type="link", se.fit = TRUE)
head(logmu)

# Vamos a requerir de la función inversa de la función liga
inverse_log = function(x){
  exp(x)
}

# En términos de conteos, sólo bastaría usar esa función,
# aunque en términos de tasas, hay que pasar el offset del otro lado
# antes de usar la función

# En términos de tasas, debemos ajustar por el término offset
(expected75 = inverse_log(logmu$fit[1]-dat7503$logtkil[1]))

(expected03 = inverse_log(logmu$fit[2]-dat7503$logtkil[2]))

#La tasa de accidentes pasó de .015 en 1975 a .0058 en 2003

#Un intervalo al 95% para la tasa en 1975 es
Z975=qnorm(.975, 0,1, lower.tail=TRUE)
(se_high = inverse_log(logmu$fit[1]-dat7503$logtkil[1]+ (logmu$se.fit[1]*Z975)) )
(se_low = inverse_log(logmu$fit[1]-dat7503$logtkil[1] - (logmu$se.fit[1]*Z975)) )

#Un intervalo al 95% para la tasa en 2003 es
Z975=qnorm(.975, 0,1, lower.tail=TRUE)
(se_high = inverse_log(logmu$fit[2]-dat7503$logtkil[2]+ (logmu$se.fit[2]*Z975)) )
(se_low = inverse_log(logmu$fit[2]-dat7503$logtkil[2] - (logmu$se.fit[2]*Z975)) )

# Recomendación, usar intervalos simultáneos para resumir modelo. 
# Si el interés sólo son las tasas, ya no es necesario tener el
# término offset, pues ese sólo modifica los conteos
# basta con la parte del componente sistemático asociado a los
# parámetros. Multcomp puede ser más útil para tasas

library(multcomp)
K=matrix(c(1,0,
           1,28), ncol=2, nrow=2, byrow=TRUE)

fitE <- glht(fit2, linfct = K)
fitci <- confint(fitE, level = 0.90)

exp(fitci$confint)




### Ejemplo con datos con ceros truncados

library(foreign)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")


# En este conjunto de datos se trabajan los días de estancia en un hospital (stay)
# Por su naturaleza, esta variable empieza en 1

# HMO es una variable dicotómica asociada a si tiene o no seguro
# died es otra variable dicotómica asociada a si el paciente murió durante la estancia o no
# age es la variable edad dividida en 9 grupos
dat <- within(dat, {
  hmo <- factor(hmo)
  died <- factor(died)
  age <- factor(age)
})

summary(dat)

X11()
hist(dat$stay)
tapply(dat$stay, dat$age, summary)
tapply(dat$stay, dat$hmo, summary)
tapply(dat$stay, dat$died, summary)


# Dos opciones.
# Con Poisson
# 1. modelo especial para ceros truncados
library(VGAM)
m1 <- vglm(stay ~ age + hmo + died, family = pospoisson(), data = dat)
summary(m1)

# 2. modelo modificando la variable y con glm
m2 <- glm(I(stay-1) ~ age + hmo + died , family=poisson(link="log"), data=dat)
summary(m2)

c(AIC(m1), AIC(m2))

# Con Binomial negativa
# 1. modelo especial para ceros truncados
# intercepto 2 se refiere al parámetro adicional de la binomial negativa
m1nb <- vglm(stay ~ age + hmo + died, family = posnegbinomial(), data = dat)
summary(m1nb)

# 2. modelo modificando la variable y con glm.nb
m2nb <- glm.nb(I(stay-1) ~ age + hmo + died , link="log", data=dat)
summary(m2nb)

c(AIC(m1nb), AIC(m2nb))

# Notar que sólo los modelos ajustados con glm y glm.bn pueden ser revisados
# en sus supuestos


library(DHARMa)  
set.seed(123)

m2res <- simulateResiduals(fittedModel = m2)
X11()
plot(m2res )


m2nbres <- simulateResiduals(fittedModel = m2nb)
X11()
plot(m2nbres )

# Parece que se comporta mejor una binomial negativa

# Los modelos vglm pueden revisarse de acuerdo con su desempeño
X11()
rootogram4(m1) #parece que se les olvido incluir esta opción
rootogram4(m1nb)
#el ajuste no parece del todo adecuado, falta modelar un poco más


m2nbvglm <- vglm(I(stay-1) ~ age + hmo + died, negbinomial, dat)
rootogram4(m2nbvglm)
summary(m2nbvglm)
summary(m2nb)

# Parece que falta modelar, el problema siguen siendo los ceros (unos originales)
m3nb <- vglm(I(stay-1) ~ age + hmo + died, zinegbinomialff, dat)
rootogram4(m3nb)



c(AIC(m1nb), AIC(m2nb), AIC(m3nb))
