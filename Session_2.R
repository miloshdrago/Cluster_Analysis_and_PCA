###########################
#### EXERCISE SESSION 2 ###
###########################

# EXERCISE 1
#Observe the throwing of a fair dice

# a define the random variable: X = "result of a dice throw", give possible outcomes: 1 ... 6
# c) Draw the probability density function of this random variable. 
# (hint: create a vector of probabilities and use the function barplot)
dice <- seq(1:6)
View(dice)
dice.p <- as.vector(rep(1/6,6))
dice.c <- as.vector(rep(1/6,6))
for (i in 2:length(dice.p)){
  dice.c[i] <- dice.c[i-1] + dice.p[i]
  }

barplot(dice.p, space = 0.2, main="Probability Density Function",
        ylim = c(0,0.2), names.arg=c("1","2","3","4","5","6"), 
        axes = FALSE, col = "red")
axis(2, at = c(0,1/6), labels = c("0", "1/6"), lty = 1)

# d) Draw the cumulative distribution function of this random variable. 
# (hint: create a vector of probabilities and use the function plot)
plot(dice, dice.c, type="n", 
     main="Cumulative Distribution Function", xlab = "", ylab = "Fx", axes = FALSE,)
lines(dice, dice.c, type="s")
# label y axis
axis(2, at = c(0,dice.c), 
     labels = c("0", "1/6", "1/3", "1/2", "2/3", "5/6", "1"), lty = 1)
# label x axis
axis(1, at = dice, lty = 1)

# EXERCISE 2
#Observe 10 tosses of a fair coin.


# d) What is the probability of tossing 6 or fewer heads in 10 tosses of a fair coin?
#(hint: use the function pbinom)
round(pbinom(6,10,0.5), 2)

# e) Calculate the probability of 7 or more heads in 10 tosses of a fair coin?
#(hint: use the function pbinom)
1-pbinom(6,10,0.5) # or equivalent:
pbinom(6,10,0.5, lower.tail=FALSE)

# f) What is the median number of heads in 10 tosses of a fair coin?
#(hint: use the function qbinom)
qbinom(0.5,10,0.5)

# g) What is the 3th quartile?(hint: use the function qbinom)
qbinom(0.75,10,0.5)

# EXERCISE 3
#A junior software developer writes on average 2 bugs every ten minutes while programming

# c)What is the probability that the developer writes no bugs in ten minutes?
# (hint: use the function ppois)
ppois(0,2)

# d) What is the probability that the developer at least one bug in ten minutes?
#(hint: use the function ppois)
1-ppois(0,2) 
# or equivalent
ppois(0,2,lower.tail=FALSE)

# e) What is the probability that the developer writes no bugs after 50 minutes of coding?
#(hint: use the function ppois)
(ppois(0,2))^5 
# or equivalent
ppois(0,10)

# f) Plot the density distribution of the number of bugs per ten minutes coding 
# with a mean number of 4 bugs per ten minutes of coding.
#(hint: use the functions density and barplot)
# range here is just how many columns we have in plot so number from 0 to 16 augmenting by 1
range <- seq(0,16,1)
#range from 0 to 16 and lambda is 4.
density <- dpois(range,4)
barplot(density, ylim=c(0,0.2))

# EXERCISE 4
# Take as random variable the bodyweight of adult men with mean 85 kg and variance 500 kg 
# Standard deviation is sqrt of variance
s.d <- sqrt(500)

# c) What is the probability that the bodyweight is exactly 100 kg?
# (hint: use the function dnorm)
dnorm(100,85, s.d)

# d) What is the probability that the bodyweight is less than 100 kg?
#(hint: use the function pnorm)
pnorm(100,85, s.d) 
# or equivalent by standardizing
z <- (100-85)/s.d
pnorm(z, 0, 1)

# e) What is the probability that the bodyweight is less than 80 kg?
# (hint: use the function pnorm)
pnorm(80, 85, s.d) 
# or equivalent by standardizing
z <- (80-85)/s.d
pnorm(z, 0, 1)

# f) What is the probability that the bodyweight exceeds 60 kg?
# (hint: use the function pnorm)
1-pnorm(60, 85, s.d) 
# or equivalent
pnorm(60, 85, s.d, lower.tail=FALSE) 
# or equivalent by standardizing
z <- (60-85)/s.d
pnorm(z, 0, 1, lower.tail=FALSE)

# g) Draw the probability from (f) (hint: use the functions dnorm and plot)
x.60 <- 60
lim <- qnorm(0.975, 85, s.d)
lim.u <- 170
lim.l <- 0
x <- seq(lim.l,lim.u,length=1000)
i.60 <- x >= x.60 & x <= lim.u
hx <- dnorm(x, mean = 85, sd = s.d)

plot(x, hx, type="n", xlab="", ylab="",
     main=expression(mu ~ "= 85 kg," ~ sigma ~ "= 22.36 kg"),
     axes=FALSE)
lines(x, hx)
polygon(c(x.60,x[i.60],lim.u), c(0,hx[i.60],0), col="green")
result <- "P(X > 60)"
mtext(result,3)
axis(1, at=c(lim.l, x.60, 85, lim.u), pos=0)

# h) What s the 97.5% quantile?(hint: use the function quantile)
qnorm(0.975, 85, s.d)

# EXERCISE 5
#Given a t-distribution T with 15 degrees of freedom


# a) What is the probability of T bigger than 1?
1-pt(1, 15) 
# or equivalent
pt(1, 15, lower.tail=FALSE)

# b) Determine t when P(T > t) = 0.05. (hint: use the function qt
qt(0.95, 15) 
# or equivalent
qt(0.05, 15, lower.tail = FALSE)

# c) Draw the probability from (b). (hint: use the functions dt, plot and polygon)
x.95 <- round(qt(0.95, 15), 2)
lim.u <- 4 #round(qt(0.999, 15), 2)
lim.l <- -lim.u
x <- seq(lim.l,lim.u,length=1000)
i.95 <- x >= x.95 & x <= lim.u
hx <- dt(x, 15)

plot(x, hx, type="n", xlab="", ylab="",
     main="Student t with 15 DF",
     axes=FALSE)
lines(x, hx)
polygon(c(x.95,x[i.95],lim.u), c(0,hx[i.95],0), col="green")
result <- "P(T > t) = 0.05"
mtext(result,3)
axis(1, at=c(lim.l, 0, x.95, lim.u), pos=0)

# EXERCISE 6
#Given a Chi-square distribution with 10 degrees of freedom

# a) Determine c when P(X > c) = 0.05.(hint: use the function qchisq)
qchisq(0.95, 10) 
# or equivalent
qchisq(0.05, 10, lower.tail=FALSE)

# EXERCISE 7
# Given an F distribution with 4 numerator and 9 denominator degrees of freedom

# a) Determine P(5 < F < 10). (hint: use the function pf)
P10 <- pf(10, 4, 9)
P5 <- pf(5, 4, 9)
P10 - P5

# b) Determine f0 if P(F ??? f0) = (1 ??? ??) and ?? = 0.05. (hint: use the function qf)
qf(0.95, 4, 9) # or equivalent
qf(0.05, 4, 9, lower.tail=FALSE)

# INSTALLING "BSDA" and "PropCIs" PACKAGES
install.packages("BSDA")
install.packages("PropCIs")
library(BSDA)
library(PropCIs)

# EXERCISE 8
# The standard deviation of the thermal conductivity at 38°C and 550W is assumed to be 0.3. 
# Ten measurements were taken with a mean of 41.924. 
# Let us assume that thermal conductivity is normally distributed

# a) Construct a 95% confidence interval around the mean conductivity.
#(hint: apply the formula for the confidence interval of a normal distribution,
# using the function qnorm)
sigma <- 0.3
n <- 10
xmean <- 41.924
conf <- 0.95
alpha <- 1-conf

lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
CI <- list(lcl=lcl, ucl=ucl)
CI

# b) Construct, using the zsum.test function from the BSDA package, a 95% confidence interval around the mean conductivity. (hint: type ?zsum.test in the R console for help)
zsum.test(41.924, 0.3, 10, conf.level=0.95)

# EXERSISE 9
# 20 measurements of the testosterone level of healthy men resulted in a mean value of 750 ng/dl. Assume that testosterone level follows a normal distribution with a sample standard deviation of 30 ng/dl.
# a) Find the 95% confidence interval on the mean testosterone level.(hint: use the function tsum.test)
zsum.test(750, 30, 20, conf.level=0.95)

# EXERCISE 10
#Import the dataset BLOOD as blood.df

blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# a) Construct, using the z.test function from the BSDA package, a 90% confidence interval 
# around the mean of the variable "age" assuming that the true standard deviation is 5 years.
z.test(blood.df$age, sigma.x=5, conf.level=0.90)

# b) Construct a 95% confidence interval around the mean of the variable "prolactn".
# (hint: use the function t.test)
t.test(blood.df$prolactn, conf.level=0.95)

# c)What is the proportion of persons with an age between 50 and 60?
#(hint: use the function subset to subset the observations with age between 50 and 60,
# then divide by the total number of observations).

#nrow returns number of columns or rows
tot.obs <- nrow(blood.df)
subset.df <- subset(blood.df, age > 50 & age < 60)
sub.obs <- nrow(subset.df)
prop <- sub.obs/tot.obs
prop

# d)Construct a 95% confidence interval for this proportion.(hint: use the function prop.test)
scoreci(sub.obs, tot.obs, conf.level=0.95) 
# or equivalent from "stats" package
prop.test(sub.obs, tot.obs, conf.level=0.95) 

# e) Make a subset "subset.df" from blood.df of those with an age lower than 50.
# (hint: use the function subset)
subset.df <- subset(blood.df, age < 50)
nrow(subset.df)

# f) Construct in this subset a 99% confidence interval around the mean of the variable "testost".
#(hint: use the functions shapiro.test and t.test)
# first check normality because sample is small!!!!!!!!!!!!!!!
shapiro.test(subset.df$testost)
# Since H0 not rejected can assume normality so can then use t.test
# now calculate confidence limits
t.test(subset.df$testost, conf.level=0.99)
