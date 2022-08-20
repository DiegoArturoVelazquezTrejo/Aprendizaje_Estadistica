# Regresión lineal múltiple
# Ajuste y pruebas de hipótesis
library(GGally)

rm(list = ls(all.names = TRUE))
gc()

#setwd("D:/dione/Documents/Seminario ML")
Datos=read.csv("ejemplo1RLM.csv", header=TRUE )

summary(Datos)

#Dos variables explicativas X1 y X2

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)
#Alternativamente con el paquete GGally:
ggpairs(Datos)
#El scatterplot es más díficil de usar en la regresión lineal múltiple
#pues sólo se muestran relaciones entre pares de variables.
#En la regresión lineal múltiple nos preguntamos, por ejemplo,
# cómo es el efecto de X2 en E(Y;X1,X2), así que debemos
#considerar que en E(Y;X1,X2) aparecen tanto X1 y X2 variables.

#Un posible modelo es:
# E(y;X1, X2)=b0+b1X1+b2X2 

fit=lm(y~X1+X2, data=Datos)

summary(fit)

'''
Call:
lm(formula = y ~ X1 + X2, data = Datos)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8730 -0.6607 -0.1245  0.6214  2.0798 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.73700    1.85987   0.934    0.353
X1           0.35404    0.33155   1.068    0.288
X2           0.07937    0.32998   0.241    0.810

Residual standard error: 0.9513 on 97 degrees of freedom
Multiple R-squared:  0.4134,	Adjusted R-squared:  0.4013 
F-statistic: 34.18 on 2 and 97 DF,  p-value: 5.829e-12
'''

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

#Del summary, último renglón:
#F-statistic: 34.18 on 2 and 97 DF,  p-value: 5.829e-12
#Se rechaza H0. Entonces al menos uno de los coeficientes asociados
#a las variables es diferente de cero y nos ayuda a modelar E(y;x)


#Obtención de la Prueba de la Tabla Anova 
#    usando la Prueba Lineal General.
library(multcomp)
K=matrix(c(0,1,0,
           0,0,1), ncol=3, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

'''
	 General Linear Hypotheses

Linear Hypotheses:
       Estimate
1 == 0  0.35404
2 == 0  0.07937

Global Test:
      F DF1 DF2    Pr(>F)
1 34.18   2  97 5.829e-12
'''

#Una vez que se realiza la prueba asociada a la tabla Anova
#la pregunta es si ambas variables aportan información al modelado.
#Para esto podemos hacer pruebas individuales sobre cada coeficiente
#usando las pruebas t o bien la prueba lineal general

#Las pruebas t que se obtienen del summary
#están asociadas a cada coeficiente de forma "no simultánea"
#Para su interpretación se sugiere identificar qué 
#modelo resulta de considerar el parámetro asociado igual a cero.

#En este caso tenemos b0, b1 y b2. 

#Por ejemplo.

#El modelo que considera que b1=0, sería uno en el que sólo está X2
#es decir, 
#el modelo reducido E(y;X1, X2)=b0+b2X2 
#vs el completo     E(y;X1, X2)=b0+b1X1+b2X2

#La prueba t del summary para
#H0: b1=0 vs Ha: b1 != 0, nos lleva a concluir que no hay evidencia 
#para rechazar H0 con una significancia de alpha=.05
#Se toma el p-value del summary, renglón asociado a X1:
# p-value=0.29 > .05


#En este caso, la prueba está elaborada para responder
#¿La inclusión de la variable X1 una vez que se tiene 
#el modelo y=b0+b2X2+e 
#nos está o no agregando información adicional?;
#en otras palabras,
#¿condicional en la inclusión de X2 en el modelo,
#X1 nos agrega información adicional para modelar E(Y;x)?

#Cuando NO se rechaza Ho, parece que los datos nos sugieren que
#es plausible considerar
# el modelo "reducido"
#y=bo+b2X2+e contra el modelo completo y=bo+b1X1+b2X2+e,
# mientras que si se rechaza H0, entonces X1 sí nos agrega 
#información al modelo y se prefiere y=bo+b1X1+b2X2+e.


#Un análisis similar se puede hacer con b2. 
#Lo importante es notar que estos análisis
#corresponden a preguntas independientes sobre cada parámetro en el modelo 
#condicionando en la inclusión del resto de variables

#Las pruebas t se enfocan en una sóla combinación de los parámetros.
#También se pueden obtener de forma directa con el paquete multcomp
#de dos maneras:

#H0: b1=0 vs Ha: b1 != 0
K=matrix(c(0,1,0), ncol=3, nrow=1, byrow=TRUE)
m=c(0)
#usando prueba F equivalente (no permite pruebas de una cola con alternativa > o <)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#usando prueba t (permite prueba de una cola)
#Notar que sólo eliminamos el argumento test=
summary(glht(fit, linfct=K, rhs=m))

#Ejemplo de una prueba de una cola
#H0: b1<=0 vs Ha: b1 > 0
summary(glht(fit, linfct=K, rhs=m, alternative = "greater") )

'''
	 Simultaneous Tests for General Linear Hypotheses

Fit: lm(formula = y ~ X1 + X2, data = Datos)

Linear Hypotheses:
       Estimate Std. Error t value Pr(>t)
1 <= 0   0.3540     0.3315   1.068  0.144
(Adjusted p values reported -- single-step method)
'''

#Nota. Se puede ver que las pruebas t de dos colas, así como las
#pruebas que se pueden incluir en la prueba lineal general
#son casos particulares en donde se comparan modelos anidados, es decir,
#el reducido vs el completo

#R tiene otras formas de obtener las pruebas
#usando el lenguaje de modelos anidados
#por ejemplo, la función anova nos sirve para comparar modelos dos modelos anidados
#sin necesidad de usar la definición matricial (K y m)

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

'''
Analysis of Variance Table

Model 1: y ~ X1 + X2
Model 2: y ~ X2
  Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     97 87.782                           
2     98 88.814 -1   -1.0319 1.1403 0.2882
'''

# Calcular de manera manual los errores cuadráticos 
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
# Contraste de dos modelos H0: quedarse con el modelo reducido, HA: No quedarse con el modelo reducido 
'''
Analysis of Variance Table

Model 1: y ~ 1
Model 2: y ~ X1 + X2
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1     99 149.638                                  
2     97  87.782  2    61.856 34.176 5.829e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Por lo que rechazamos H0 y nos quedamos con el modelo completo
'''



# b0 estimado es igual a la media de la variable que estás describiendo
mean(Datos$y)

'''
Analysis of Variance Table

Model 1: y ~ 1
Model 2: y ~ X1 + X2
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1     99 149.638                                  
2     97  87.782  2    61.856 34.176 5.829e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
'''

## Selección entre los modelos. Aquí casi los tres modelos
## indican un mismo coeficiente de determinación, se seleccionaría en 
## general el de menor número de parámetros y 
## con el mayor coeficiente de determinación
## aunque más adelante se considerarán otros criterios BIC y AIC 
## o bien se realiza un análisis del poder predictivo del modelo

summary(fit)

fitred1=lm(y~X2, data=Datos)
summary(fitred1)

fitred2=lm(y~X1, data=Datos)
summary(fitred2)




#### ALERTA DE ERROR COMÚN
#La lectura de las pruebas t NO debe hacerse de forma simultánea
#ver por ejemplo,
summary(fit)
#muchas personas cometen el error de leer la salida de forma simultánea
#concluyendo en este caso que ni X1 ni X2 aportan información al modelo
#lo que en este caso no es cierto basado en el análisis realizado
#de forma correcta con la prueba asociada a la tabla ANOVA.