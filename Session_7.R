###########################
#### EXERCISE SESSION 7 ###
###########################

lab.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

summary(lab.df)

x <- c(1,3,4)
for (i in 1:length(x)){
  lab.df[,x[i]] <- as.factor(lab.df[,x[i]])
}

summary(lab.df)

## b)Due to randomization issues we cannot use all four measured concentrations 
#per replication as such, with the techniques seen in this course. 
# The only thing we can use is the mean measured value over these four observations.
# Make a new column containing the mean observation per solution and replication
Mean <- rowMeans(lab.df[,6:9])
lab.df <- data.frame(lab.df, Mean)

## c) As response variable we will use the difference between 
# the target Ca concentration and the mean observed value.
# Make a new column containing the difference between the target 
# and the mean observation per solution and replication
Diff <- lab.df[,5] - lab.df[,10]
lab.df <- data.frame(lab.df, Diff)

## d) Now it is time to start comparing the different labs. 
# The client wants to know which lab is giving the best overall performance
install.packages("psych")
library(psych)
# d.1) Are we dealing with balanced or unbalanced data? see page 34
# Balanced as all variables have same number of n.
describe <- describeBy(lab.df$Diff, list(lab.df$Lab, lab.df$Rep), mat=TRUE)
describe.st <- subset(describe, select=c("group1", "group2", "mean", "sd", "n"))
describe.st

# d.2) Draw and interpret the interaction plot by using following function in R:
# See page 26-27 There is interaction between lab and rep
interaction.plot(lab.df$Lab, lab.df$Rep, lab.df$Diff, type="b", pch=c(18,24), col=c(1,2))

# d.3) Run a two-way ANOVA with the difference between target 
# and measured as response variable and the laboratory 
# and replication as predictor variables. 
#Include the interaction in the model. Interpret your results.
# Full model
lab.aov <- aov(Diff~Lab*Rep, 
               contrasts=list(Lab="contr.sum", Rep="contr.sum"),
               data = lab.df)
summary(lab.aov)
# Interaction term is not significant so can drop it and redo ANOVA

# Reduced model without interaction
lab.aov2 <- aov(Diff~Lab+Rep, 
                contrasts=list(Lab="contr.sum", Rep="contr.sum"),
                data = lab.df)
summary(lab.aov2)

# d.4)Run some diagnostics on the proposed model. Adjust the model if needed.
# Checking homogeneity of variance for each factor separately
#install.packages("car")
library(car)
# Here variance not homogeneous
leveneTest(Diff~Lab, data = lab.df)
#Here variance homogeneous
leveneTest(Diff~Rep, data = lab.df)

# Using more robust ANOVA as homogeneity was rejected above
Anova(lab.aov2, type='III',white.adjust='hc3')

# Checking for outlying values
number <- 1:nrow(lab.df)
cook <- cooks.distance(lab.aov2)
par(mfrow=c(1,1))
plot(cook~ number)
#identify(cook, labels=number)

# Checking for normality of residuals
#Normality rejected here as p value significant
shapiro.test(lab.aov2$residuals)
hist(lab.aov2$residuals)

# As non-normal we use a non-parametric test for differences between labs (One-Way ANOVA)
# Use the Kruskal Wallis Test from page 21
# Since H0 rejected conclude that not all location parameters are in the same regions.
# In at least one region there is a shift in location parameter
kruskal.test(Diff~Lab, data = lab.df)

# d.5)Do a multiple comparison on the means of the laboratory using the appropriate technique. 
# Given the results from the diagnostics, can we use the Tukey method?
#Tuskey Test assumption is equal variance, independent observations and 
#that normality of residuals holds which is clearly not the case here.
#Can be seen as we used Krustal Wallis test before which is non-parametric.
# Using non-parametric method for comparing means see p.22
# install.packages("pgirmess")
library(pgirmess)
kruskalmc(Diff~Lab, data = lab.df)

# Plotting significantly different means
# in easy to interpret dotchart
# Everything to the right of the threshold has significantly different mean
# so everything in graph here as threshold is line on the left.
# We see that H occurs the most so that is the lab we choose
lab.kru <- data.frame(kruskalmc(Diff~Lab, data = lab.df))
lab.kru <- lab.kru[,3:5]
kru.sig <- as.data.frame(matrix(data = NA, nrow = nrow(lab.kru), ncol = ncol(lab.kru), dimnames = dimnames(lab.kru)))
for(i in 1:nrow(lab.kru)){
  if(lab.kru[i,3] == "TRUE"){
    kru.sig[i,] <- lab.kru[i,]
  }
}

kru.sig <- na.omit(kru.sig)

dotchart(kru.sig[,1], labels = rownames(kru.sig), col = "red", xlim = c(52, 90))
abline(v = 52.39141, lty = 2, col = "grey")

###########################
####       EXTRA        ###
###########################

# we can introduce the "Solution" factor in the model, but this is nested in the lab factor

lab.aov3 <- aov(Diff~Lab+Rep+Sol%in%Lab+Lab:Rep, 
               contrasts=list(Lab="contr.sum", Rep="contr.sum", Sol="contr.sum"),
               data = lab.df)
summary(lab.aov3)
interaction.plot(lab.df$Lab, lab.df$Sol, lab.df$Diff, type="b", pch=c(18,24), col=c(1,2))

anova(lab.aov, lab.aov3)
