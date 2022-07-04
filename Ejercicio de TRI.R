#read data
k=read.csv("data.csv",header=FALSE)
head(k)
#installs "irtoys" and "ltm" packages
library(ltm)
library(irtoys)
#To verify the difficulty level on classical theory of test
colMeans(k)
#To verify the discrimination on classical theory of test
s=rowSums(k)
cor(k,s)
#execute of ITR
#2PL
h=est(resp=k,model="2PL",engine="ltm",run.name="data.2PL")
h
#1PL
i=est(resp=k,model="1PL",engine="ltm",run.name="data.1PL")
i
#Draw the CCI
#2PL
icc=irf(h$est)
plot(icc,label=T,co=NA,main="ICC 2PL")
#1PL
iccl=irf(i$est)
plot(icc1,labale=T,co=NA,main="ICC 1PL")
#Draw the CCI of an specific item
#CCI of item 2
m=2
icc2=irf(h$est[m,])
plot(icc2,main="Item2")

#Draw of Scatter plot of the discrimination capacity x difficulty grade
plot(h$est[,],type="n",xlab="a",ylab="b")
text(h$est[,1],h$est[,2],paste(1:nrow(h$est)))









