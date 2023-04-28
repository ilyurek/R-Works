#Simple Linear Regression
#Correlation does not imply causation
#Linear Regression 

library(MASS)
head(survey)

?survey
str(survey)
summary(survey)

plot(survey$Height~survey$Wr.Hnd,
     xlab="Writing handspan (cm)",
     ylab="Height (cm)",
     col="purple",
     cex=1)

cor(survey$Height,survey$Wr.Hnd,use="complete.obs") #only use the complete observantions (passes NA values)

incomplete.obs<-which(is.na(survey$Height)|is.na(survey$Wr.Hnd))
length(incomplete.obs)

survfit<-lm(Height~Wr.Hnd,data=survey)
survfit

plot(survey$Height~survey$Wr.Hnd,
     xlab="Writing handspan (cm)",
     ylab="Height (cm)",
     col="purple",
     cex=1)
abline(survfit)

obsA<-c(survey$Wr.Hnd[197],
        survey$Height[197])
obsB<-c(survey$Wr.Hnd[154],
        survey$Height[154])

obsB
names(survfit)

mycoeff<-coef(survfit)
mycoeff

survfit$coefficients
beta0.hat<-mycoeff[1]
beta1.hat<-mycoeff[2]

plot(survey$Height~survey$Wr.Hnd,
     xlab ="Writing handspan (cm)", ylab="Height (cm)")

abline(survfit,lwd = 2, col = "blue")

segments(x0=c(obsA[1],obsB[1]),y0=beta0.hat+beta1.hat*c(obsA[1],obsB[1]),
         x1=c(obsA[1],obsB[1]),y1=c(obsA[2],obsB[2]),lty=2, lwd = 2, col = "red")

summary(survfit)

confint(survfit,level=0.95)

rho.xy<-cor(survey$Height,survey$Wr.Hnd,
            use="complete.obs")

rho.xy^2

as.numeric(beta0.hat+beta1.hat*14.5)
as.numeric(beta0.hat+beta1.hat*24)


xvasl<-data.frame(Wr.Hnd=c(14.5,24))
xvasl

mypred.ci<-predict(survfit,newdata = xvasl,
                   interval = "confidence",
                   level=0.95)
mypred.ci

mypred.pi<-predict(survfit,newdata=xvasl,interval="prediction",level=0.95)
mypred.pi
################################






