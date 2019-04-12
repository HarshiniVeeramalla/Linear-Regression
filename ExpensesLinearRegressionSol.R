
setwd("D:\\jigsaw\\Regression\\8.3")

getwd()

data <- read.csv("expenses.csv", header = T, stringsAsFactors = T, na.strings = c("","NA","Missing Values"))

# Data Exploration

dim(data)
str(data)
View(data)
summary(data)

# Data Tabulation

table(data$sex)   # gives count of male and female
table(data$smoker)

hist(data$charges)    # charges are right skewed

                  # checking for outliers

?boxplot
x <- boxplot(data$charges)
x
out <- x$out
out           # as there are many outliers in the dependent/ target variable we should never modify them because our target will be changed

summary(data$charges)

# check outliers for age

y <- boxplot(data$age)
y
outA <- y$out   # puts outlier values in outA
outA

# REPLACE outliers in age

index <- which(data$age %in% y$out)     # here we get the row number
index
summary(data$age)
data$age[index] <- 39

# check outliers in sex

s <- boxplot(data$sex)    #as it is factor variable no need to check

# check outliers in bmi

b <- boxplot(data$bmi)
b
outB <- b$out
outB

# replace outliers in bmi

indexB <- which(data$bmi %in% b$out)
indexB
summary(data$bmi)
data$bmi[indexB] <- 30

# check outliers in children

c <- boxplot(data$children) # no outliers


# check outliers in smoker

s <- boxplot(data$smoker)   # factor variable

# check outliers for region

r <- boxplot(data$region)  # factor variable

# check outliers for charges 

# not required bcoz we however dont change any values in the target variable

            
                       # MISSING VALUES

summary(data)   # shows which variable column has missing values

# so impute mean value for the missing values in bmi

data$bmi[is.na(data$bmi)] <- 30.67

summary(data)


# Convert categoric to numeric variables or dummy variables

data$smoker <- ifelse(data$smoker == "yes",1,0)
data$sex <- ifelse(data$sex == "female",1,0)

# Creating bmi Buckets  # just to improve the R2 value

summary(data$age)
data$bmi_Bkt[data$bmi <= 30] <- '1'
data$bmi_Bkt[data$bmi > 30 & data$bmi <= 60] <- '2'


summary(data$age)
data$age_Bkt[data$age <= 30] <- 'young'
data$age_Bkt[data$age > 30 & data$age <= 60] <- 'Middle'
data$age_Bkt[data$age > 60 & data$age <= 100] <- 'Old'

# also checked for age, but that didnt improve R2 value


# ==== Data Exploration & Preparation ends=====#

# ----------Building the model -----------------#

?lm

# lm function is used to fit linear models. 
# It can be used to carry out regression.


Reg <- lm(charges ~ age + bmi + sex + children + smoker + region + bmi_Bkt + age_Bkt, data)

# Reg <- lm(charges ~., data)   # charges ~. this includes all the columns

step(Reg,direction = "backward")



# Multivariate Linear Regression Iteration # 1

MulReg <- lm(charges ~ age + bmi + children + smoker + bmi_Bkt + 
                  age_Bkt, data = data)
step(MulReg,direction = "backward")

summary(MulReg)


# Multivariate Linear Regression Iteration # 2

MulReg <- lm(charges ~ age + children + smoker + bmi_Bkt, data)
MulReg
summary(MulReg)


# Multivariate Linear Regression Iteration # 3

MulReg <- lm(charges ~ age + bmi_Bkt + children + smoker, data)
class(MulReg)
MulReg
summary(MulReg)
formula(MulReg)


                    # FINDING PREDICTED VARIABLES

# predict(name of model, name of dataset)

predcharges <- predict(MulReg, data = data)
predcharges
class(predcharges)
length(predcharges)
head(predcharges)
data$pred <- predcharges # creating new col within the dataset

# OR (can also predict values using)

MulReg$fitted.values


                        # FINDING RESIDUALS

# difference between predicted and original

 ?resid
resi <- resid(MulReg)
resi

# OR                 # Predicted values and residuals can also be extracted from MulReg object

MulReg$residuals

data$residuals <- MulReg$residuals
View(data)

summary(MulReg)

                           # HOMOSCEDASTIC

# residuals should be homoscedastic

# homoscedastic : the variance should be constant through out # there should be no pattern in the plot

library(ggplot2)

plot(data$resi)


# OR

#  residuals should be normally distributed to check normality use histogram or qqplot

?hist()

hist(data$resi)

qplot(data$resi)


install.packages("car")

library(car)    # library for qqplot

# qqplot shows if residuals are normally distributed or not

qplot(data$resi)   # residuals are dots in the plot

summary(data$resi)

                                      

                                # MULTI-COLLINEARITY (CO-RELATION)

# there should be no multi collinearity

library(corrgram)

# checking through excel sheet

cordata <- corrgram(data) # shows heat maps

cordata

write.csv(cordata, "cordata.csv")  # putting object in csv file and open it # shows in excel


#  OR ANOTHER METHOD (corelation)

library(car)

vif(MulReg)   # VIF : Variance Inflation Factor

#  as all the values are less than 4 there is no multicolinearity


                                  # FIT CHART

# model is good or not

# plotting actual and predicted together

# there should not be too much difference

# Creating Fit Chart 

library(ggplot2)


p <- ggplot(data, aes(x = as.numeric(rownames(data))))

p + geom_line(aes(y = data$charges, color = "green")) + geom_line(aes(y = data$pred, color = "blue"))

# to what extent actual and predicted are meeting

cor(data$charges, data$pred)

                                
                                   # MAPE  (MEAN ABSOLUTE PERCENTAGE ERROR)  


data$PE <- (abs(data$resi)/data$charges) * 100  # adding column pe to the data

MAPE <- mean(data$PE)
  
MAPE
