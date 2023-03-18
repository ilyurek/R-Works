install.packages("olsrr")
install.packages("bestNormalize")
install.packages("ISLR")

library(olsrr)
library(bestNormalize)
library(ISLR)

data(Hitters)
Hitters <- na.omit(Hitters)
head(Hitters)

summary(Hitters)

# 1-Conduct Linear Regression by taking salary as response

model1 <- lm(Salary~.,data=Hitters)
summary(model1)

# 2-Apply variable selection

ols_step_forward_p(model1,details = F)
ols_step_backward_p(model1,detail=F)

# Conduct model with selected features

model2 <- lm(Salary ~ AtBat  + Hits  + Walks  + CAtBat  +  CRuns  + CRBI + CWalks + League + Division  + PutOuts +  Assists , data=Hitters)
summary(model2)

# 3-Lets check the multicollinearity

car::vif(model2)

# VIF values of AtBat, Hits, CatBat, Cruns, CRIBI, and CWalks > 10 so there
# is serious multicollinearity in the model.

model3 <- lm(Salary~  Hits   +   Walks  +    CRBI  +  CWalks  +  League + Division  + PutOuts +  Assists, 
             data=Hitters)
summary(model3)
car::vif(model3)

# For the model lets use the significan variables based on the p-values
model4 <- lm(Salary~  Hits   +   Walks  +    CRBI   + Division  + PutOuts, data=Hitters)
summary(model4)
car::vif(model4)

# 4-Lets check assumptions for linear regression for final model:

# Homogeneity of the variance and linearity(homoscedasticity)

plot(model4,1)

# Normality

plot(model4,2)

# 5-If assumptions of the linear regression is not satisfied, apply appropriate
# transformation

# If normality of response is not satisfied use power transformation of response
# If constant variance assumption is not satisfied use appropriate transformation on the response
# If linearity assumption is not satisfied transformation of X can solve the problem

# Lets check the normality assumption with the shapiro.test
shapiro.test(Hitters$Salary)

set.seed(12345)
bestNormalize(Hitters$Salary)

#based on the bestNormalize orderNorm Transformation is the best
orderNorm(Hitters$Salary)

new_response <- orderNorm(Hitters$Salary)$x.t

shapiro.test(new_response)

model5 <- lm(new_response ~  Hits   +   Walks  +    CRBI   + Division  + PutOuts, data=Hitters)
summary(model5)

plot(model5,1)
plot(model5,2)

# 6-Model Diagnostics

# Outliers can be detected using standardized residuals,
# studentized residuals, press residuals, if absoulute values
# of these type of residuals bigger than 2 or 3, that points
# are outlier

# Leverage Points can be detected using Hat matrix,
# if hii > 2p/n then these points are considered outlying 
# wrt. to X (high leverage point)

# Influential Point is an observation whose removal from the
# data set would cause a large change in the estimate reg.mod
# DFFIT, DFBETAS, Cook's Distance and COVRATIO can be used to
# detect an influencial point

ols_plot_resid_stud(model5)
ols_plot_resid_lev(model5)
ols_plot_dffits(model5)
ols_plot_cooksd_chart(model5)

which(abs(rstandard(model5))>3) #outliers

#leverage points

n<-nrow(Hitters) #sample size
p<-length(coef(model5))#number of beta coefs

which(abs(hatvalues(model5)) > 2*p/n )

#influential points

which(abs(dffits(model5)) > 2*sqrt(p/n))


