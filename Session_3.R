###########################
#### EXERCISE SESSION 3 ###
###########################

# EXERCISE 1
#The annual rainfall (in centimeters) was measured in Iran for the last 10 years and for Belgium in the last 18 years:
# Iran: 128,125,133,104,146,132,125,118,129 and 124
# Belgium: 160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162 and 172

Iran <- c(128,125,133,104,146,132,125,118,129,124)
Belgium <- c(160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162,172)

# a)  Which assumptions have to be checked before testing? (hint: use the functions shapiro.test,var.test)
# check normality:
shapiro.test(Iran)
shapiro.test(Belgium)
# check variances equal or not:
var.test(Iran, Belgium)

# c) Give a 90% confidence interval for the difference in the means.(CI given in output of code below)
# d)Test this hypothesis with 90% confidence and compare the result with c)
t.test(Iran, Belgium, var.equal=FALSE, conf.level=0.90)

# e) Give the hypothesis and test if there is significant less rainfall in Iran than in Belgium.
# (hint: use the function t.test)
t.test(Iran, Belgium, var.equal=FALSE, alternative = c("less"), conf.level=0.90)

# EXERCISE 2
# Import the BLOOD.DAT dataset.
blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# a) Make two subsets: the persons younger than 50 and older than 68 years.
#(hint: use the function subset)
blood.Y <- subset(blood.df, age < 50)
blood.O <- subset(blood.df, age > 68)

# b) Give the hypotheses to test if both age groups have a significant different testosterone level.

# c) With which test would you test these hypotheses?
# (hint: use the functions shapiro.test, var.test)
nrow(blood.Y)
nrow(blood.O)
# Small subsets, so check normality:
shapiro.test(blood.Y$testost)
shapiro.test(blood.O$testost)
# If at least one is non-normal, use non-parametric test for 2 samples: "Wilcoxon rank sum test"

#d) Test this hypotheses with 95% confidence. (hint: use the function t.test)
t.test(blood.Y$testost, blood.O$testost)
wilcox.test(blood.Y$testost, blood.O$testost, exact = FALSE)
#	exact is a logical indicating whether an exact p-value should be computed.
# Put exact = FALSE since there are ties in the data


# EXERCISE 3 https://en.wikipedia.org/wiki/Correlation_does_not_imply_causation
# Install the "faraway" package and load the "tvdoctor" dataset. 
#It gives the life expectancy, # of doctors and # of televisions in 38 countries collected in 1993.

install.packages("faraway")
library(faraway)
tvdoc.df <- tvdoctor

# a & b & c
#a) Give the correlation coefficient between the life expectancy and the no. of televisions.
#b) Give the correlation coefficient between the life expectancy and the no. of doctors.
#c) Give the correlation coefficient between the no. of doctors and the no. of televisions.
#First test normality of each variable
apply(tvdoc.df, 2, FUN = shapiro.test)
# If all normal, use pearson correlation coefficient
# cor(tvdoc.df, method = "pearson")
# Since at least one non normal then need to use spearman correlation coefficient
cor(tvdoc.df, method = "spearman")

# d What can you conclude from this? (hint: use the function pairs)
pairs(tvdoc.df)
#There seems to be some non-linear correlatiosn between variables

# exercise 4
senic.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

# a What can you tell about the following variables by means of descriptive statistics ?
# Do you expect any outlying values?  #Any expected correlations by looking at the scatterplots? 
# (hint: use the functions cor, boxplot, summary).

#Create subset of 4 variables of interest
senic.sub <- data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)
#Calculate mean, median and quartiles
summary(senic.sub)
#Caluclate variance and standard deviation
senic.var <- diag(var(senic.sub))
senic.sd <- diag(var(senic.sub)^0.5)
senic.var
senic.sd

# Boxpplots of variables of interest
par(mfrow=c(2,2))
boxplot(senic.df$length, main = "length")
boxplot(senic.df$risk, main = "risk")
boxplot(senic.df$fac, main = "facilities")
boxplot(senic.df$xray, main = "X-ray")

#Correlations?
pairs(senic.sub)

# some interesting additional plots
# density plots
par(mfrow=c(2,2))
plot(density(senic.df$length), main = "length")
plot(density(senic.df$risk), main = "risk")
plot(density(senic.df$fac), main = "facilities")
plot(density(senic.df$xray), main = "X-ray")

# # index plot of sorted values
par(mfrow=c(2,2))
plot(sort(senic.df$length), main = "length", cex = 1.2)
plot(sort(senic.df$risk), main = "risk", cex = 1.2)
plot(sort(senic.df$fac), main = "facilities", cex = 1.2)
plot(sort(senic.df$xray), main = "X-ray", cex = 1.2)

# b) Calculate the good correlation coefficient between the infection risk and other three variables. What do you expect regarding linear regression? (hint: use the function cor)
# Test for normality
apply(senic.sub, 2, FUN = shapiro.test)
# Normality is rejected for length of stay (its 3.26e-09) and comes close to being rejected for infection risk
# Since non normal, use Spearman correlation
senic.cor <- cor(senic.sub, method = "spearman")
senic.cor[c(1,3,4),2]

# c Regress the infection risk on the average length of stay 
# (hint: use the functions lm, summary, plot and abline)
senic.lmL <- lm(risk~length, data = senic.df)
summary(senic.lmL)

par(mfrow=c(1,1))
plot(risk~length, data = senic.df, main = "risk~length", xlab = "Length", ylab = "Risk")
abline(senic.lmL)
# Two outliers in length of stay in the far right

# (Optional!)
# Same as above, but without outliers, observations 47 and 112
senic.wo <- senic.df[c(1:46,48:111,113),]

# c.2 
#. Interpret the estimated value for beta 1
# For every one unit increase in length, the risk variable will increase by the beta 1 coefficient value
#. Interpret the R2 value
# R2 value is the proportion of the total variability of risk that length explains 
# (Multiple R-square value in output)
senic.lmL2 <- lm(risk~length, data = senic.df) 
summary(senic.lmL2)

#Plot the data points together with the regression line. 
# Do you think that linear regression was appropriate?
par(mfrow=c(1,1))
plot(risk~length, data = senic.df, xlim = c(6.5,20), main = "risk~length", xlab = "Length", ylab = "Risk")
abline(senic.lmL2, col = "RED")

# d) Regress the infection risk on available facilities and do the same exercises as in c)
senic.lmF <- lm(risk~fac, data = senic.df)
summary(senic.lmF)

#par(mfrow=c(1,1))
plot(risk~fac, data = senic.df, main = "risk~facilities", xlab = "Facilities", ylab = "Risk")
abline(senic.lmF)

# e) Regress the infection risk on routine X-rays and do the same exercises as in c)
senic.lmX <- lm(risk~xray, data = senic.df)
summary(senic.lmX)

#par(mfrow=c(1,1))
plot(risk~xray, data = senic.df, main = "risk~X-ray", xlab = "X-ray", ylab = "Risk")
abline(senic.lmX)

# (Optional!)
# Multiple linear regression 
#senic.df$reg <- as.factor(senic.df$reg)
#lm.f <- lm(risk~.-ID, data = senic.df)
#summary(lm.f)
#par(mfrow = c(2, 2))
#plot(lm.f)

