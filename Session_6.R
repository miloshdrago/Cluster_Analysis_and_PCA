###########################
#### EXERCISE SESSION 6 ###
###########################

senic.df <- read.table(file=file.choose(), header=TRUE, sep="\t")
senic.df$reg <- as.factor(senic.df$reg)

## a) Construct 3 dummy variables X1, X2, and X3, defined as follows:
X1 <- as.vector(rep(0,nrow(senic.df)))
X2 <- X1
X3 <- X1

X1[which(senic.df$reg == 1)] <- 1
X2[which(senic.df$reg == 2)] <- 1
X3[which(senic.df$reg == 3)] <- 1

senic.df <- data.frame(senic.df, X1, X2, X3)

senic.df[,13:15]

## b) Fit a linear model of infection risk against these three dummy variables, 
# but do not include any other variables in the model.
  senic.lm <- lm(risk~X1+X2+X3, data = senic.df)
  summary(senic.lm)

## c)Use the mean values obtained in b) to calculate a 95% confidence 
#interval for these means, using the tsum.test function 
#(Session 2 - package BSDA) and the values in the table below:
library(BSDA)
#This below step just takes coefficients of dummies from the fit step from above 
senic.lm$coefficients
s.mean <- c(0,0,0,0)
# So coefficient intercept + coefficient Xi
s.mean[1] <- senic.lm$coefficients[1]+senic.lm$coefficients[2]
s.mean[2] <- senic.lm$coefficients[1]+senic.lm$coefficients[3]
s.mean[3] <- senic.lm$coefficients[1]+senic.lm$coefficients[4]
# Simply the intercept
s.mean[4] <- senic.lm$coefficients[1]

#Standard deviation
s.sd <- as.matrix(by(senic.df$risk, senic.df$reg, sd))

# n size
s.n <- c(28, 32, 37, 16)

# Here build confidence interval
senic.conf <- matrix(data = 0, nrow = 4, ncol = 4)
senic.conf[,1] <- c(1, 2, 3, 4)
senic.conf[,2] <- s.mean
for(j in 1:4){
  y <- tsum.test(s.mean[j], s.x = s.sd[j], n.x = s.n[j], conf.level=0.95, var.equal=TRUE)
  senic.conf[j,3] <- y[[4]][1]
  senic.conf[j,4] <- y[[4]][2]
  if(j == 4){print(senic.conf)}
}
#EXTRA: draw line plots of the confidence intervals to investigate differences between the means;
# See page 18 chapter 9
dotchart(senic.conf[,2], labels = senic.conf[,1], col = "red", xlim = c(min(senic.conf[,3:4]), max(senic.conf[,3:4])))
for(i in 1:4){
  lines(x = c(senic.conf[i,3], senic.conf[i,4]), y = c(senic.conf[i,1], senic.conf[i,1]))
}

## d) Test, by using the aov function and with a significance of 0.05% 
#if the mean infection risk ("risk") is the same in the four regions ("reg").
senic.aov <- aov(risk~reg, data = senic.df)
summary(senic.aov)
#p value lower tahn 0.05 so reject H0 and one mean is different from others

## e) Calculate, using the TukeyHSD function, 
# the family-wise confidence intervals of the means. 
#Compare the results with the values in c). 
#Look at the differences plot, what can you conclude from this and compare with b).
senic.tuk <- TukeyHSD(senic.aov, conf.level = 0.95)
plot(senic.tuk)
# We compare two regions. If 0 not in CI then throw away H0.

## f) Check the model conditions and assumptions for the model constructed in b):
#install.packages("car")
#Are the groups independent?
library(car)
# Check within group homogeniety of variances
leveneTest(risk~reg, data = senic.df)

# Normally distributed residuals?
shapiro.test(senic.lm$residuals)
hist(senic.lm$residuals)
#Test all variables one by one
by(senic.df$risk, INDICES = senic.df$reg, FUN = shapiro.test)
# Normality seems tenable

# Influential observations
#?plot(cooks.distance())
plot(senic.lm, which = 5)

