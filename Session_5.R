  ###########################
#### EXERCISE SESSION 5 ###
###########################

fev.df <- read.table(file=file.choose(), header=TRUE, sep=" ")

## a) Look at this dataset with descriptive statistics.

summary(fev.df)
# Dont include Sex and Smoke as they are categorical columns so no values.
#Look at correlation matrix between variables
fev.cor <- cor(fev.df[,1:3])
fev.cor
#Correlation plots
pairs(fev.df[,1:3])

# boxplots
par(mfrow=c(1,3))
boxplot(fev.df[,1], main = "Age")
boxplot(fev.df[,2], main = "FEV")
boxplot(fev.df[,3], main = "Height")


# density plots
par(mfrow=c(1,3))
plot(density(fev.df[,1]), main = "Age")
plot(density(fev.df[,2]), main = "FEV")
plot(density(fev.df[,3]), main = "Height")

# index plots
par(mfrow=c(1,3))
plot(sort(fev.df[,1]), ylab = "", main = "Age", pch = 16)
plot(sort(fev.df[,2]), ylab = "", main = "FEV", pch = 16)
plot(sort(fev.df[,3]), ylab = "", main = "Height", pch = 16)

## b)Add a new dummy variable X1 to the dataset such that X1=0 if Sex ="Male" and X1=1 if Sex="Female". 
#Add a dummy variable X2 such that X2=0 if smoke="Yes" and X2=1 if smoke="NO" 
# (hint: use the function ifelse).

X1 <- as.vector(rep(0,nrow(fev.df)))
X2 <- X1
X1 <- ifelse(fev.df$Sex == "Male", 0, 1) # Females are represented by an 1!
X2 <- ifelse(fev.df$Smoke == "No", 0, 1) # Smokers are represented by an 1!
fev.df <- data.frame(fev.df[,1:4], X1, fev.df[,5], X2)
names(fev.df) <- c("Age", "Fev", "Height", "Sex", "X1", "Smoke", "X2")

## c)During the discussion with the client, he mentioned  that he ran 
# some statistical methods on the data himself, yielding unexpected results.

# c.1)Compare the boxplots of FEV in function of the smoking status and explain; 
#(hint: use the function boxplot)
par(mfrow = c(1,1))
boxplot(fev.df$Fev~fev.df$Smoke)
#### CONFOUNDING ####

# c.2) Run a simple linear regression with the dummy variable for smoking status (X2)
# as predictor variable and explain the outcome;(hint: use the function lm)
fev.s <- lm(Fev~X2, data = fev.df)

summary(fev.s)

plot(fev.df[,2]~fev.df[,7])
abline(fev.s, col = "red")

# c.3) Explain, using regression diagnostics, why we should not use this model 
#(hint: look at the residuals plots );
# So check linearity
fev.fit <- fitted(fev.s) # gives the fitted values used in calculating the residuals
fev.rs <- rstandard(fev.s) # gives standardized residuals

par(mfrow = c(2, 2))
plot(fev.s)

plot(fev.rs~fev.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

#Clearly  not random pattern
# We need to control for the other variables too!

## d ##Now you need to work on the report for the client. 
#Run a multiple linear regression on the data with FEV as predicted variable. 
#Use the global structure for regression analysis (Chapter 8 p.7) as guidance.
#When you start model building, do not forget to include all interaction
#terms with the dummy variables. 
#The original variables ("Sex" and "Smoke") should not be used in
#the model, only their dummies. Try to answer all the questions below:

#d.1)
#Should we use all the data or is it better to use only a subset 
#(It is not needed to start subsetting straight away, still a good extra exercise)?

# d.2) Fit the full model on the complete dataset and look at the residual plots. 
#(hint: use the function lm) 
#Do you think you have to include polynomial terms? 
#(hint: look at the residuals plots);
#Redraw the scatterplots of FEV in function of height and age 
#using the scatter.smooth(x, y) function. What do you think now? 
#Include all possible useful polynomial terms and re-examine the model.
# Full model
fev.F <- lm(Fev~Height+Age+X1+X2+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.F)
# Results are now in line with intuition

#Check for non linearity
fevF.fit <- fitted(fev.F) # gives the fitted values used in calculating the residuals
fevF.rs <- rstandard(fev.F) # gives standardized residuals
par(mfrow = c(1, 1))
plot(fevF.rs)
#Seems to follow pattern so not good need to check more

# Looking for polynomial terms
par(mfrow = c(1, 1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# Age doesnt seem to be polynomial as it is somehow random (see page 20 chapter7)
scatter.smooth(fev.df$Age, fev.df$Fev, lpars = list(col="Red"))
# Height is not random and therefore polynomial. Add quadratic term
scatter.smooth(fev.df$Height, fev.df$Fev, lpars = list(col="Red"))

# Include polynomial term in full model (added quadratic term for height)
fev.P <- lm(Fev~Height+Age+X1+X2+
              I(Height^2)+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.P)
par(mfrow = c(2, 2))
plot(fev.P)

# Recheck non-linearity. So checking polynomial fit
fevP.fit <- fitted(fev.P) # gives the fitted values used in calculating the residuals
fevP.rs <- rstandard(fev.P) # gives standardized residuals

# Here graph from before before adding polynomial
par(mfrow=c(1,1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5), main = "First-order")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# Significant improvement after adding polynomial
plot(fevP.rs~fevP.fit, ylim = c(-4.5, 4.5), main = "Polynomial")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)
# Mild heteroscedasticity is present but otherwise OK! 

# Reduce the model via AIC (backward selection)
fev.R <- step(fev.P, direction = "backward")
summary(fev.R)
# What is the effect of smoking on FEV, according to this model?
#Smoking has a negative impact on FEV, all other variables constant

#Remember the primary question of interest. Is there a difference in FEV 
#between smokers and non-smokers?(hint:look at the regression coefficients and p-values)
#Height has a positive impact and a higher impact on smoker than non smoker
#assuming all other variables are the 

#e) About a week after you handed the results of your analysis to the client,
#you get a very upset e-mail. The e-mail tells you that the client 
# did a similar survey and used your model to predict the FEV of the subjects. 
#He was surprised that the outcome did not corresponded at all with the true
#measured FEV and now he is questioning your statistical capabilities.
# What is wrong with this reasoning? Why should the proposed model 
#not be used to make predictions in this case?
#(hint: Think about the primary question of interest during when the model was built).
I'd say that the issue is that you never checked if your model generalises. 
#You never used a validation set. -> your model overfit on your dataset
#If you only make a model for your data and not check for unseen data, 
#this could happen that your model is not performing wel