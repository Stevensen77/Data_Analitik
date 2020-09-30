library(MASS)
library(ISLR)
Advertising<-read.csv("D:/Semester 6/Data analitik/Praktikum/prak4/Advertising.csv",head=TRUE)[,-1]
wd <- getwd()
file <- paste(wd,"Advertising.csv",sep="/")
Advertising<-read.csv(file, head=TRUE)[,-1]

head(Advertising)

summary(Advertising)

pairs(Advertising, pch=".") 

ad.lm <- lm(Sales~., data=Advertising)
summary(ad.lm)

rse=summary(ad.lm)$sigma

#RSE= 1.686 
mean(Advertising$Sales) 

rse/mean(Advertising$Sales) 

rsq=summary(ad.lm)$r.sq
rsq #0.8972106


yhat=ad.lm$fitted.values #predicted 
y=Advertising$Sales #observed 
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula


#Other way to get R2
var(yhat)/var(y) #other formula

1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula 

cor(yhat,y)^2 #alternate formula
Coef1=summary(ad.lm)$coefficients #Coefficient matrix
Coef1

lolim=Coef1[,1] - 1.96*Coef1[,2]
uplim=Coef1[,1] + 1.96*Coef1[,2]
cbind(lolim,uplim)
confint(ad.lm)

require(car)
vif(ad.lm)
predict(ad.lm, newdata=data.frame(TV=149,Radio=22,Newspaper=25),
        interval="confidence")
predict(ad.lm, newdata=data.frame(TV=149,Radio=22,Newspaper=25),
        interval="prediction")

plot(ad.lm) #diagnostic plot

ad.lm2 <- lm(Sales~.^2, data=Advertising)
summary(ad.lm2)
summary(ad.lm2)$r.sq;summary(ad.lm)$r.sq 
ad.lm3 <- lm(Sales~.+I(TV^2), data=Advertising)
summary(ad.lm3)
anova(ad.lm,ad.lm3)

par(mfrow=c(2,2))
plot(ad.lm3)

ad.lm4 <- lm(Sales~.+poly(TV,3), data=Advertising)
summary(ad.lm4)
anova(ad.lm,ad.lm4)
anova(ad.lm3,ad.lm4)
par(mfrow=c(2,2))
plot(ad.lm4)

ad.lm5 <- lm(Sales~.+poly(TV,3)+poly(Radio,3), data=Advertising)
plot(ad.lm5)
anova(ad.lm4,ad.lm5)
require(FNN)
head(Advertising)

trainx=Advertising[,-4] #X-matrix
ad.knn <- knn.reg(trainx, test = NULL, Advertising$Sales, k = 3)
plot(Advertising$Sales,ad.knn$pred, xlab="y", ylab=expression(hat(y)))

var(ad.knn$pred)/var(Advertising$Sales) 

y=Advertising$Sales
yhat=ad.knn$pred
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2);rsq

cor(yhat,y)^2 #approximate rsq very well. 

yhat2=predict(ad.lm,Advertising)
plot(Advertising$Sales,yhat2,xlab="y",ylab=expression(hat(y)))

rsq2=1-sum((y-yhat2)^2)/sum((y-mean(y))^2);rsq2

cor(yhat2,y)^2 #approximate rsq very w

dim(Advertising)

train <- sample(1:dim(Advertising)[1],.7*dim(Advertising)[1])
test=-train

train.Ad <- Advertising[train,]
test.Ad <- Advertising[test,]
lm.tr <- lm(Sales ~., data=train.Ad)
summary(lm.tr)