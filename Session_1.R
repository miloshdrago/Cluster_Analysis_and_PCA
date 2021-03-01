###########################
#### EXERCISE SESSION 1 ###
###########################

# EXERCISE 0

# importing BLOOD.DAT dataset
blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# EXERCISE 1

# a) Import the BETACAR2 dataset as beta.df (hint: use the function read.table).
beta.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

# b) See/Call the column names of beta.df (hint: use the function names).
names(beta.df)

# EXERCISE 2

# a) Import the ChickWeight dataset as chick.df (hint: use the function read.table).
chick.df <- read.table(file=file.choose(), sep="\t")

# b) See/Call the column names of chick.df (hint: use the function names).
names(chick.df)

# c) Give the mean, median, quartiles of the "weight" variable
summary(chick.df$weight)
# Give the variance of the "weight" variable
var(chick.df$weight)
# Give the standard deviation of the "weight" variable
sd(chick.df$weight)

# d) Do the same for the "chicken" variable. 
# Does this make sense? Any solutions? (hint : use the functions summary, var and sd).
View(chick.df)
summary(chick.df$chicken)
# This is nonsense, as these numbers are simply labels which just identify the chicken number

# e) Change the name of the "chicken" variable to "No." (hint: convert the "chicken" variable to categorical by using the function as.factor then use the function names)
names(chick.df)[1] <- "No."
names(chick.df)

# f) Give the mean, median, quartiles of the "weight" variable by feed (hint: use the function by).
by(chick.df$weight, chick.df$feed, summary)
# Give the variance of the "weight" variable by feed (hint: use the function by).
by(chick.df$weight, chick.df$feed, var)
# Give the standard deviation of the "weight" variable by feed (hint: use the function by).
by(chick.df$weight, chick.df$feed, sd)

# g) Give the frequency table of the feed used in the experiment (hint: use the function table).
table(chick.df$feed)



# EXERCISE 3

# a)Import the monica dataset as monica.df (hint: use the function read.table).
monica.df <- read.table(file=file.choose(), header=TRUE, sep=";")

# b) Give the mean, median, quartiles of the "age" variable by sex
by(monica.df$age, monica.df$sex, summary)
# b) Give the variance of the "age" variable by sex 
by(monica.df$age, monica.df$sex, var)
# Give standard deviation of the "age" variable by sex 
by(monica.df$age, monica.df$sex, sd)

# c) Draw a boxplot of the "age" variable (hint: use the function boxplot).
par(mfrow = c(1,1 ))
boxplot(monica.df$age)

# d) Draw a separate boxplot of the "age" variable for each sex 
#(hint: use the functions by and boxplot).
boxplot(age~sex, data = monica.df)

# f) Draw (in R) an estimation of the density function of the age of the population for each sex 
#(hint: For each sex, use the functions hist and lines)
monica.den <- by(monica.df$age, monica.df$sex, density) # calculate densities
par(mfrow=c(1,2)) # put two graphs next to each other in one row, two columns
plot(monica.den[[1]], main="Females")
plot(monica.den[[2]], main="Males")
# NOTE: Females is the first density:

# alternative with histogram and lines
par(mfrow=c(1,2))
hist(monica.df$age[which(monica.df$sex == 'm')], probability = TRUE, main = "Males", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'm')]))
hist(monica.df$age[which(monica.df$sex == 'f')], probability = TRUE, main = "Females", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'f')]))
