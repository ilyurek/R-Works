# Interpretation olacak
# RNB file i ind    Rmarkdownu html'e dönüştüreceğiz[] #logistic regressiona kadar you will upload the file 

# Logistic Regression 

# (GLM) binary categorical variable using numerical and categorical predictors
# whether a virus is COVID-19 or not
# whether an e_mail is spam or not 
# We assume a binomical dist. prduced theh outcome variable and we therefore want to model
# interval of 0-1 will be prob
# log(p(x)/1-p(x)) = v0 + b1x -> whenn we want add comments we will talk about the odds (accuracy)

#statquest
library(tidyverse)
library(dplyr)
#Inverse Logistic Regression

# Mutliple Logistic Regression

df<-read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(df)
head(df)
summary(df)

df$rank<-as.factor(df$rank)
summary(df)

logit<- glm(admit~gre+gpa+rank,data=df,family="binomial") #we can use "." for all the functions 
logit

model<-summary(logit)  # Explanation of this part will be on the exam ! 

# One units increase in (p values are statistical significant)
# 


#Start by calculating the predictted porb of admision at eaach value of rank,holidn gre and gpa at their means.First Create and irw dataframe

newdata1<- with(df,data.frame(gre=mean(gre), #####!!!!!!!!!!!!!########
                              gpa=mean(gpa),
                              rank = factor(1:4)))



newdata1$rankp<- predict(logit,newdata=newdata1,type="response")

#Bernoulli Distribution








