#Loading packages
library(datos)
library(ggplot2)
library(readr)
library(scales)
library(stringr)
library(utf8)
library(devtools)
library(ggpubr)
#Loading data base
DBS=read_csv("R basic.csv")
#2. Leer los datos
ej=read.csv("R basic.csv")
attach(ej)
#3. Confirmación de variables
colnames(ej)
#4. Confirmación de las primeras 4 filas
head(ej,4)
#5. Elaboración de histogramas
library(lattice)
histogram(~Test.score,data=ej,breaks = 30,type="count")
#6.Encontrar tendencias centrales
mean(ej$Test.score)
median(Test.score)
sort(table(ej$Gender))
#7. Comparar la dispersión por grupos
sd(Encouragement)
var(ej$Encouragement)
#8. Comparar la dispersión de grupos
tapply(ej$Test.score,ej$Gender,mean)
#9. Dibujar diagramas de caja
boxplot(ej$Test.score,horizontal = TRUE)
boxplot(Test.score~Gender,data=ej,horizontal=TRUE)
#10. Resumen de las estadística
summary(ej$Motivation)
#11. Prueba F de homogeneidad de la varianza
var.test(Scold~Gender, data=ej)
#12.Prueba  T de muestra independientes (homogeinidad de la varianza)
t.test(Scold~Gender, data=ej, var.equal=TRUE)
#13. Prueba T de Welch (No de homogeinidad de la varianza)
t.test(Scold~Gender, data=ej, var.equal=FALSE)
#14.Prueba T pareada
score=c(ej$Encouragement,ej$Motivation)
result=c(rep("Encouragement",500),rep("Motivation",500))
t.test(score~result,paired=TRUE)






