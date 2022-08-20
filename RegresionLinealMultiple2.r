
# Regresión lineal múltiple
# Ajuste e interpretación de coeficientes asociados a variables continuas

rm(list = ls(all.names = TRUE))
gc()

setwd("D:/dione/Documents/Seminario ML")
Datos=read.csv("ejemplo2RLM.csv", header=TRUE )
summary(Datos)


par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)

# Consideremos el modelo
# E(y;X1,X2)=b0+b1X1+b2X2

fit=lm(y~X1+X2, data=Datos)

summary(fit)

'''

Call:
lm(formula = y ~ X1 + X2, data = Datos)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.093651 -0.033037 -0.006222  0.031068  0.103991 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.054311   0.041609   25.34   <2e-16 ***
X1          0.496671   0.002622  189.44   <2e-16 ***
x1 sí nos está aportando información 
X2          0.401191   0.004950   81.05   <2e-16 ***
x2 también nos está aportando información 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.04756 on 97 degrees of freedom
Multiple R-squared:  0.9976,	Adjusted R-squared:  0.9976 
por R-sq está ajustando casi a la manera perfecta (casi nunca pasa)
F-statistic: 2.052e+04 on 2 and 97 DF,  p-value: < 2.2e-16

Rechazamos
'''

#Observemos que con la prueba asociada a la tabla ANOVA, se puede concluir
#que con una significancia de .05, se rechaza H0 en el contraste
#H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Además, si analizamos la prueba t para el coeficiente b1,
#se rechaza H0. Aquí el contraste es H0: b1=0 vs Ha: b1 != 0
#Esto nos indica, que aún considerando a la variable X2 en el modelo,
#la variable X1 nos está agregando información para modelar E(Y;X1,X2).

#Por otro lado, si analizamos la prueba t para el coeficiente b2,
#se rechaza H0. Aquí el contraste es H0: b2=0 vs Ha: b2 != 0
#Esto nos indica, que aún considerando a la variable X1 en el modelo,
#la variable X2 nos está agregando información para modelar E(Y;X1,X2).

#Con base en lo anterior, parece que no podríamos reducir el modelo,
#es decir, todos los coeficientes parecen significativos.

#Ahora vamos a interpretar este modelo

#R2, el coeficiente de determinación, nos indica que se está
#explicando el 99.8% de la variabilidad observada en Y a través 
#del modelo y=b0+b1X1+b2X2+e
#Parece un muy buen ajuste

#Además, b1 se puede interpretar como
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y AUMENTA en .5 unidades al aumentar en una unidad la variable X1.

#Por otro lado, b2 se puede interpretar como
#condicionado en un valor fijo de X1, el promedio de la variable 
#Y aumenta en .4 unidades al aumentar en una unidad la variable X2.

#Notemos además que al incluir dos variables en el modelo
#se puede tener un mejor ajuste con base en el coeficiente de
#determinación.  Por ejemplo, los ajustes con una sola variable son:
fitred1=lm(y~X2, data=Datos)
summary(fitred1)
'''
Call:
lm(formula = y ~ X2, data = Datos)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.34965 -0.59583 -0.06255  0.52775  2.21630 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.68207    0.20112  43.169  < 2e-16 ***
X2           0.35474    0.09473   3.745 0.000305 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9115 on 98 degrees of freedom
Multiple R-squared:  0.1252,	Adjusted R-squared:  0.1163 
F-statistic: 14.02 on 1 and 98 DF,  p-value: 0.0003049

con variabilidad explocada de 0.1252
'''


fitred2=lm(y~X1, data=Datos)
summary(fitred2)
'''

Call:
lm(formula = y ~ X1, data = Datos)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.79276 -0.30847 -0.02798  0.24051  1.36769 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.9733     0.3302   5.976  3.7e-08 ***
X1            0.4862     0.0216  22.510  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3923 on 98 degrees of freedom
Multiple R-squared:  0.8379,	Adjusted R-squared:  0.8363 
F-statistic: 506.7 on 1 and 98 DF,  p-value: < 2.2e-16

Estás explicando el 0.8379 de tu variabilidad
'''


#Alternativamente
# el punto significa que estás incluyendo a todas las variables de la base de datos 
# con el menos (-) significa que vas a omitir esa variable de tu base de datos 
# significa que usará todos los datos de la base de datos sin considerar X1
fitred12 = lm(y~.-X1, data = Datos)
summary(fitred12)

fitred22 = lm(y~.-X2, data = Datos)
summary(fitred12)

#Otra alternativa cuando ya tenías un modelo predefinido
fitred13 = update(fit,~.-X1) # ajustas un modelo predefinido 
# si ya tienes al modelo completo, de esta forma trabajas ordenadamente y reduces errores operativos 
# en este caso al poner el x1, es donde se actualizará el modelo 
summary(fitred13)

fitred23 = update(fit,~.-X2)
summary(fitred23)

