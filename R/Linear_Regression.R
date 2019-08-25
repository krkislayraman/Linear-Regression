# Package loading
library(car)
library(forecast)

getwd()

# Change the working directory
setwd("C:/Users/RAMAN/Documents/R_Theory_Rahul")

getwd() # Check whether the directory has changed

# Read the data file
insurance <- read.csv("Insurance_Data.csv")
# insurance <- read.csv(file.choose())y

# Have a look at the data by head-(first 6 words of the data) or tail -(last 6 rows the data)
head(insurance)
tail(insurance)

# Check for NAs - 3 different methods
names(insurance) # name of columns
colSums(is.na(insurance))
is.na(insurance)
any(is.na(insurance))

# Check the summary of the file
summary(insurance)
# describe(insurance) # library(psych)
# Losses.in.Thousands has some outliers (can be seen from the summary)

#########################
# Univariate Analysis
# analyse single variable once at a time
#########################

# For Losses.in.Thousands column , check the quantile to find out the outlier limit/range
quantile(insurance$Losses.in.Thousands, probs = c(0.90, 0.95, 0.99, 0.995, 1))
boxplot(insurance$Losses.in.Thousands)
# We can deduce from the result that there are outlier in the losses. columns at 99th percentile
# Let's cap the dependent variable

insurance$Capped.losses <- ifelse(insurance$Losses.in.Thousands > quantile(insurance$Losses.in.Thousands, 0.99), quantile(insurance$Losses.in.Thousands, 0.99), insurance$Losses.in.Thousands)

# Verify you result by head() or summary()
head(insurance)
summary(insurance)

### Convert factor/ character variables to dummy variables 
str(insurance)

insurance$Gender.Male <- ifelse(insurance$Gender == "M", 1, 0)
head(insurance) # Check whether if the above line works

insurance$Single <- ifelse(insurance$Married == "Single", 1, 0)
head(insurance) # Check whether the above line works

# Lets now drop the redundant variables from the dataset
# Drop Ac_No, Gender, Married, Losses.in.Thousands
insurance.data2 <- within(insurance, rm("Ac_No", "Gender", "Married", "Losses.in.Thousands"))
# insurance_data2 = subset(insurance, select = -c(Ac_No, Gender, Married, Losses.in.Thousands))
# View(insurance_data2)
head(insurance.data2)

############################
# Bivariate Analysis
############################

# Generate plots to see the relation between the independent variables and the dependent variable

# Single plot
plot(x = insurance.data2$Age, y = insurance.data2$Capped.losses)

for (i in colnames(insurance.data2)){
  plot(insurance.data2[ ,i], insurance.data2$Capped.losses, xlab = i, ylab = "Capped Losses")
}

# Lets generate a correlation plot to understand "Independent" variable to variable relationships
windows() # open a different window for multi-purposes
pairs(insurance.data2)

####################################
# Create some interaction variables
####################################

# Interaction Variable between Gender == M and Single variable age <= 25
insurance.data2$GenderM.Age.Below25 <- ifelse(insurance.data2$Gender.Male == 1 & insurance.data2$Age <= 25, 1, 0)

# Interaction Variable between Gender == M and Single variable age >= 60
insurance.data2$GenderM.Age.Above60 <- ifelse(insurance.data2$Gender.Male == 1 & insurance.data2$Age >= 60, 1, 0)

# Interaction Variable between Gender == M and Single 
insurance.data2$Gender.Male.Single <- ifelse(insurance.data2$Gender.Male == 1 & insurance.data2$Single == 1, 1, 0)

# verify the result, View(), head(), dim(df), summary() - whatever pleases you
summary(insurance.data2)
head(insurance.data2)

#####################################################
# Sampling : Divide the data in to TrainSet and TestSet
#####################################################

set.seed(628) # This is used to reproduce the SAME composition of the sample EVERYTIME, the number can be anything(any number)
RowNumbers <- sample(x = 1:nrow(insurance.data2), size = 0.70*nrow(insurance.data2))

# head(RowNumbers)

TrainData = insurance.data2[RowNumbers, ] # TrainsSet
TestData = insurance.data2[-RowNumbers, ] # TestSet

# Check the number of rows in each dataset
nrow(TestData)
nrow(TrainData)

colnames(TrainData)
# VIF Check
M0 = lm(Capped.losses ~ ., data=TrainData)
# M0 = lm(CappedLosses ~ Age + Years.of.Experience + ..., data=TrainData)
vif(M0)

# As we can see in the vif Age and Years.of.Experience has high multi-collinearity
# VIF > 10 suggests presence of multicollinearity.
# Variance inflation factor (VIF) quantifies 
# the severity/magnitude of multicollinearity amongst the independent variables in regression analysis.
# Remove the variable with highest VIF value and iterate again

TrainData.2 <- within(TrainData, rm("Age"))
M1 <- lm(Capped.losses ~ ., data = TrainData.2)
vif(M1) # No multicollinearity

############ OR #############
# Remove variable "Years.of.Experience" from the model
# M1 = lm(CappedLosses ~ . - Years.of.Experience, data=TrainData)
# vif(M1) # No multicollinearity

# Please note that "Age" and "Years.of.Driving.Experience" variables are quite highly correlated, hence
# if business suggests that Age is more important than Years of Driving Exp, then
# Age can be included in the model provided you remove Years of Driving Exp

M11 <- lm(Capped.losses ~ . - Years.of.Experience, data = TrainData)
vif(M11)

summary(M11)

# Remove insignificant variable "Number.of.Vehicles" from the model
M2 = lm(Capped.losses ~ . - Years.of.Experience - Number.of.Vehicles, data=TrainData)
summary(M2)

# Homoskedasticity check
plot(M2$fitted.values, M2$residuals) # Should not show prominent non-constant variance (heteroskadastic) of errors against fitted values
# Normality of errors check
hist(M2$residuals) # Should not be completely non-normal distribution

# After doing all of this, the model has to be checked against the test data as well
# Lets predict on testset and then calculate a metric called MAPE to estimate the errors on testing data
M2_Pred = predict(M2, TestData)
head(M2_Pred)
head(TestData$Capped.losses)

############################
Actual = TestData$Capped.losses
Prediction = M2_Pred

# RMSE - Root Mean Square Error
sqrt(mean((Actual - Prediction)^2)) # 182.2344

# MAPE - Mean Absolute Percentage error
mean(abs((Actual - Prediction)/Actual))*100 # 64.98909

# Using library(forecast)
accuracy(M2_Pred, TestData$Capped.losses)

# The output of accuracy()
# ME     RMSE      MAE       MPE     MAPE
# Test set 2.808493 178.9948 137.1624 -42.54651 66.50524
