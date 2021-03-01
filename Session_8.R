###########################
#### EXERCISE SESSION 8 ###
###########################

# 1.  Import the dataset called BLOOD.DAT into RStudio
# ----------------------------------------------------
blood.df <-read.table(file=file.choose(), header=TRUE, sep=",")

summary(blood.df)
hist(blood.df$testost)
blood.df <- subset(blood.df, blood.df$testost!='999',)
summary(blood.df$testost)
hist(blood.df$testost)

# 2.  Use logistic regression to assess the association
# between testosterone and breast cancer risk. 
blood.log1 <- glm(case ~ testost, family=binomial(link=logit), data=blood.df)
summary(blood.log1)


# a) Calculate and interpret the Odds Ratio. (see page 10 chapter 10)
# 0.013891 is the coefficient beta 1 of testosterone
exp(0.013891) 

# for each increase of 1 unit of testosterone, the odds on breast cancer increases with 1.3988%
# or for each increase of 10 units of testosterone, the odds on breast cancer increases with 13.988% ~ 14%

# b) Look at the predicted values and visualize them (page 17)
combine <- cbind(blood.df$testost, blood.df$case, fitted(blood.log1))
colnames(combine) <- c("testost", "case", "p")
head(combine,5)

# visualize this:
par(mfrow=c(1,1))
plot(blood.df$testost, blood.df$case, type="p", xlab="testost", ylab="Success",col="green")
points(blood.df$testost, fitted(blood.log1), type="p", col="red")

# c) Create the classification table with different cut-off values. (page 18)
# Can classify values with predicted probability above 0.5 as positive cancer case, else negative
# Comparison of different threshold on page 20
table(blood.df$case, fitted(blood.log1)> 0.5)

# very high specificity: 
337/338*100
# but very low sensitivity;
1/160*100

# classification table with cut-off value 0.35
table(blood.df$case, fitted(blood.log1)> 0.35)

# high specificity: 
278/(278+60)*100
# but relatively low sensitivity;
31/(129+31)*100

# d) Calculate the "deviance" of the model and interpret. See page 16
#Null deviance is when you would only use intercept
#So you have deviance explained by intercept - deviance of your model is low
#Then your model didn't predit much new stuff
#So would need more variables
#DONT MESS UP WITH RESIDUAL DEVIANCE WHICH SAYS A LOW NUMBER INDICATES THAT MODEL IS TRAINED WELL
blood.log1$deviance
blood.log1$null.deviance
prop.dev1 <- (blood.log1$null.deviance - blood.log1$deviance)/blood.log1$null.deviance
prop.dev1

# Can be interpreted as that proportion of deviance explained by this model.
# this is very low which points to a "bad" model

# e) Create the ROC curve, the AUC and interpret. See page 21-22
# install.packages("ROCR")
library(ROCR)
predict <- fitted(blood.log1)
pred <- prediction(predict, blood.df$case)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate",
     colorize=TRUE)
perf_auc <- performance(pred, measure = "auc")
perf_auc@y.values
# not a good model as can be seen by the low AUC



# 3.  Repeat exercise 2 but now with age at blood sampling as predictor.
# ---------------------------------------------------------------------
blood.log1b <- glm(case ~ age, family=binomial(link=logit), data=blood.df)
summary(blood.log1b)

# try yourself..


# 4.  Repeat exercise 2 but now with testosterone and age at blood sampling as predictors. 
# Is this multiple logistic regression model better than the simple one?
# ----------------------------------------------------------------------------------------

blood.log2 <- glm(case ~ testost + age, family=binomial(link=logit), data=blood.df)
summary(blood.log2)


# look at predicted values
combine <- cbind(blood.df$testost, blood.df$age, blood.df$case, fitted(blood.log2))
colnames(combine) <- c("testost", "age", "case", "p")
head(combine,5)

# visualize this:

library(scatterplot3d)
s3d <- scatterplot3d(blood.df$testost, blood.df$age, fitted(blood.log2), col.grid=3, zlim=c(0,1))
# Now adding some points to the "scatterplot3d"
s3d$points3d(blood.df$testost, blood.df$age, blood.df$case,
             col="blue", type="p", pch=16)

# classification table again with cut-off value 0.5
table(blood.df$case, fitted(blood.log2)> 0.5)
# very high specificity: 
337/338*100
# but very low sensitivity;
1/160*100

# classification table again with cut-off value 0.35
table(blood.df$case, fitted(blood.log2)> 0.35)
# high specificity: 
278/(278+60)*100
# but relatively low sensitivity;
32/(128+32)*100

# d) Calculate the "deviance" of the model and interpret.
blood.log2$deviance
blood.log2$null.deviance
prop.dev2 <- (blood.log2$null.deviance - blood.log2$deviance)/blood.log2$null.deviance
prop.dev2

# ROC curve
#install.packages("ROCR")
library(ROCR)
predict <- fitted(blood.log2)
pred <- prediction(predict, blood.df$case)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="sensitivity vs false positive rate", colorize=TRUE)
# "false positive rate" is the same as "1-specificity"

# compute area under the curve
perf_auc <- performance(pred, measure = "auc")
perf_auc@y.values

#compare deviances of both models see page 28:
# If pvalue rejects H0 then model 2 so multiple logistic regression model is better
diff.dev <- blood.log1$deviance - blood.log2$deviance
diff.dev
1-pchisq(diff.dev,1)
# This p-value is big (> 0.05) hence model 2 is NOT a better model.

