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
#3. Confirmación de variables
colnames(ej)
#4. Confirmación de las primeras 4 filas
head(ej,4)
#5. Elaboración de histogramas
library(lattice)
histogram(~Test.score,data=ej,breaks = 30,type="count")
#6.Encontrar tendencias centrales
mean(ej$Test.score)
median(ej$Test.score)
sort(table(ej$Gender))
#7. Comparar la dispersión por grupos
sd(ej$Encouragement)
var(ej$Encouragement)
#8. Comparar la dispersión de grupos
tapply(ej$Test.score,ej$Gender,mean)
#9. Dibujar diagramas de caja
boxplot(ej$Test.score,horizontal = TRUE)
boxplot(Test.score~Gender,data=ej,horizontal=TRUE)
#10. Resumen de las estadística
summary(ej$Motivation)
#Prueba F de homogeneidad de la varianza
var.test(Scold~Gender, data=ej)
#12.Prueba  T de muestra independientes (homogeinidad de la varianza)
t.test(Scold~Gender, data=ej, var.equal=TRUE)
#13. Prueba T de Welch (No de homogeinidad de la varianza)
t.test(Scold~Gender, data=ej, var.equal=FALSE)
#14.Prueba T pareada
score=c(ej$Encouragement,ej$Motivation)
result=c(rep("Encouragement",500),rep("Motivation",500))
t.test(score~result,paired=TRUE)













#2.Confirmation of the number of rows and columns
dim(jhk)
#3. Confirmation of variables
colnames(jhk)
#4. Confirmation of first 4 rows
head(jhk,4)
#5. Drawing a histogram
library(lattice)
histogram(~Stress,data=jhk,breaks=10,type="count")
#6. Finding central tendencies
mean(jhk$Stress)
median(jhk$Stress)
sort(jhk$Stress)
sort(table(jhk$Stress))
#7. Findings dispersion
sd(jhk$Stress)
var(jhk$Stress)
#8. Drawing histogram by groups
histogram(~Collaboration|Career+Gender,data=jhk,breaks = 10)
#9. Comparison of dispersion by groups
tapply(jhk$Collaboration,jhk$Gender,mean)
tapply(jhk$Collaboration,jhk$Gender,sd)
#10. Drawing boxplot
boxplot(jhk$Stress,horizontal = TRUE)
boxplot(Collaboration~Gender,data=jhk,horizontal=TRUE)
#11. Summary of statistics
summary(jhk$Skill)
#12. F-test for homogeneity of variance
var.test(Collaboration~Gender,data=jhk,horizontal=TRUE)
#13. Independent-samples t-test(homogeneity of variance)
t.test(Collaboration~Gender,data=jhk,var.equal=TRUE)
#14. Welch T-test (NOT homogeneity of variance)
t.test(Collaboration~Gender,data=jhk,var.equal=FALSE)
#15.Paired T-tes
score=c(jhk$Total,jhk$Previous.year)
year=c(rep("Total",800),rep("Previous.year",800))
t.test(score~year,paired=TRUE)

#****************************** Basic analysis of multivariate data **********************
#16. Summarizing multivariate data by column
varname=c("Collaboration","Assertiveness","Skill","Knowledge")
jhk2=jhk[,varname]
apply(jhk2,2,mean)
apply(jhk2,2,sd)
#17. Summarizing multivariate data by row
apply(jhk2,1,sum)
apply(jhk2,1,sd)
#18. Comparing multivariate distribution between groups
by(jhk2, jhk$Gender,apply,2,mean)
by(jhk2,jhk$Gender,apply,2,sd)
#19. Standardizing data (Standardization)
zscore=scale(jhk2)
head(zscore,2)
tscore=zscore*10+50
head(tscore,2)
#20. Drawing Scatter plot matrix
Skill=jhk$Skill
Knowledge=jhk$Knowledge
plot(Skill,Knowledge,xlab="Skill",ylab="Knowledge")
#21. Drawing Scatter plot matrix
cas=c("Collaboration","Assertiveness","Stress")
plot(jhk[,cas])
#22. Drawing Scatter plot by group
xyplot(Knowledge~Skill|Career+Department, data=jhk)
#23. Finding correlation of 2 variables
cor(jhk$Collaboration,jhk$Stress)
#24. Correlation matrix
cor(jhk[,cas])
#25. Covariance matrix
cov(jhk[,cas])
#26. Hypothesis testing on the correlation coefficiente
library(psych)
corresult=corr.test(jhk[,cas])
corresult$t
corresult$p
#27. Multiple regression analysis
jhk3=lm(Total~Collaboration,data=jhk)
summary(jhk3)
jhk4=lm(Total~Collaboration+Assertiveness,data=jhk)
summary(jhk4)
#multiple regression analysis
jhk5=lm(Total~Collaboration+Assertiveness+Skill+Knowledge+Stress,data=jhk)
summary(jhk5)


