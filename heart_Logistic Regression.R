###############################################################################################
## STAT 581 Project: Heart data logistic Regression
## Author: Tina Dhekial-Phukan
###############################################################################################
rm(list = ls(all.names = TRUE))
gc()
options(digits=20)

## Install packages
install.packages(c('tibble', 'dplyr', 'readr'))
install.packages("ggplot2")
install.packages("lattice")
install.packages("car")
install.packages("glmtoolbox")
install.packages("misty")
install.packages("corrplot")
install.packages("fastDummies")

## Install libraries
library(tibble)
library(dplyr)
library(readr)
library(caret)
library(MASS)
library(dplyr)
library(car)
library(glmtoolbox)
library(stringr)
library(tidyverse)
library(misty)
library(corrplot)
library(boot)
library(fastDummies)

################################################################################
## Data clean-up & Feature Engineering
################################################################################
heart <- read.csv(file="heart.csv")
View(heart)

## Defining variable types
# Age -> numeric
heart$Age <- as.numeric(heart$Age)
# Sex -> factor
heart$Sex <- as.factor(heart$Sex)
# ChestPainType -> factor
heart$ChestPainType <- as.factor(heart$ChestPainType)
# RestingECG -> factor
heart$RestingECG <- as.factor(heart$RestingECG)
# MaxHR -> numeric
heart$MaxHR <- as.numeric(heart$MaxHR)
# ExerciseAngina -> factor
heart$ExerciseAngina <- as.factor(heart$ExerciseAngina)
# Oldpeak -> already numeric
# ST_Slope -> factor
heart$ST_Slope <- as.factor(heart$ST_Slope)
# RestingBP -> numeric
heart$RestingBP <- as.numeric(heart$RestingBP)
which(heart$RestingBP==0)
heart <- heart[-450,]
# HeartDisease -> factor
heart$DiseasedHeart <- as.factor(ifelse(heart$HeartDisease == 0, "NO", "YES"))
# Cholesterol -> numeric                                                                      
heart$Cholesterol <- as.numeric(heart$Cholesterol)
# FastingBloodSugar -> as factor 
heart$FastingBloodSugar <- as.factor(ifelse(heart$FastingBS == 0, "<= 120mg/dl", "> 120mg/dl"))
# Remove redundant columns HeartDisease and FastingBS
heart <- subset(heart, select = -c(HeartDisease, FastingBS))

# Impute 0 values of Cholesterol
# Grouped by DiseasedHeart, FastingBloodSugar, Sex and Resting ECG
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"])

heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "M" & heart$RestingECG != "Normal"])

heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "NO" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"])

heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "<= 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG == "Normal"])
heart$Cholesterol[heart$Cholesterol == 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"] <- median(heart$Cholesterol[heart$Cholesterol != 0 & heart$DiseasedHeart == "YES" & heart$FastingBloodSugar == "> 120mg/dl" & heart$Sex == "F" & heart$RestingECG != "Normal"])

###################################################################################
## Logistic Regression Model Fitting 
###################################################################################

str(heart)

## Convert cholesterol to log(cholesterol)
heart$log_Cholesterol <- log(heart$Cholesterol)

## Convert cholesterol to log(cholesterol)
heart$log_RestingBP <- log(heart$RestingBP)

heart.df1 <- subset(heart, select = -c(Cholesterol, RestingBP))

## Step 1: Create an initial main-effects model  
logist.model1 <- glm(DiseasedHeart~., data = heart.df1, family=binomial("logit"))
summary(logist.model1)

# p-value for test that all beta's = 0
1-pchisq(1260.95-604.24, 916-901) 

# Likelihood Ratio Tests for individual explanatory variables
library(car)
Anova(logist.model1)

# Select variables with p-values < 0.2
# Age, Sex, ChestPainType, MaxHR, ExerciseAngina, Oldpeak, ST_Slope, FastingBloodSugar, log_Cholesterol
logist.model2 <- glm(DiseasedHeart~., data = subset(heart.df1, select = -c(RestingECG, log_RestingBP)), 
                     family=binomial("logit"))
summary(logist.model2)
Anova(logist.model2)

## Step 2: Conduct Stepwise (backward, both) elimination
library(MASS)
stepAIC(logist.model1,direction = "backward")
stepAIC(logist.model1,direction = "both")

## Step 4: Check for plausible interactions between age and sex
logist.model3 <- glm(DiseasedHeart ~ Age + Sex + ChestPainType + MaxHR + 
                       ExerciseAngina + Oldpeak + ST_Slope + FastingBloodSugar + 
                       log_Cholesterol+ Age*Sex, data=heart.df1, family=binomial("logit"))
summary(logist.model3)
anova(logist.model2, logist.model3, test="LRT")

 
logist.model4 <- glm(DiseasedHeart~., data = subset(heart.df1, select = -c(Age, RestingECG, log_RestingBP)), 
                     family=binomial("logit"))
summary(logist.model4)
Anova(logist.model4)
anova(logist.model2, logist.model4, test="LRT")

## Check for significance of gender
logist.model5 <- glm(DiseasedHeart~., data = subset(heart.df1, select = -c(Age, Sex, RestingECG, log_RestingBP)), 
                     family=binomial("logit"))
summary(logist.model5)
Anova(logist.model5)
anova(logist.model4, logist.model5, test="LRT")

## Final logistic model
logist.model4 <- glm(DiseasedHeart~., data = subset(heart.df1, select = -c(Age, RestingECG, log_RestingBP)), 
                     family=binomial("logit"))
summary(logist.model4)
Anova(logist.model4)

## AIC values
AIC(logist.model1)
AIC(logist.model2)
AIC(logist.model3)
AIC(logist.model4)
AIC(logist.model5)

## BIC values
BIC(logist.model1)
BIC(logist.model2)
BIC(logist.model3)
BIC(logist.model4)
BIC(logist.model5)


###################################################################################
## Diagnostics & Goodness of Fit 
###################################################################################
residualPlots(logist.model4)

## Pearson's residuals
residualPlots(logist.model4, type="pearson")
residualPlots(logist.model4, terms=~MaxHR, type="pearson")
residualPlots(logist.model4, terms=~Oldpeak, type="pearson")
residualPlots(logist.model4, terms=~log_Cholesterol, type="pearson")

## Deviance residuals
residualPlots(logist.model4, type="deviance")
residualPlots(logist.model4, terms=~MaxHR, type="deviance")
residualPlots(logist.model4, terms=~Oldpeak, type="deviance")
residualPlots(logist.model4, terms=~log_Cholesterol, type="deviance")

logist.model4a <- glm(DiseasedHeart~Sex+ChestPainType+MaxHR+ExerciseAngina+Oldpeak+I(Oldpeak^2)
                      +ST_Slope+FastingBloodSugar+log_Cholesterol, family=binomial("logit"), data=heart.df1)
summary(logist.model4a)
Anova(logist.model4a)

residualPlots(logist.model4a, type="pearson")
residualPlots(logist.model4a, terms=~MaxHR, type="pearson")
residualPlots(logist.model4a, terms=~Oldpeak, type="pearson")
residualPlots(logist.model4a, terms=~log_Cholesterol, type="pearson")

## Deviance residuals
residualPlots(logist.model4a, type="deviance")
residualPlots(logist.model4a, terms=~MaxHR, type="deviance")
residualPlots(logist.model4a, terms=~Oldpeak, type="deviance")
residualPlots(logist.model4a, terms=~log_Cholesterol, type="deviance")

par(mfrow=c(2,3))
residualPlots(logist.model4a, terms=~Sex, type="deviance")
residualPlots(logist.model4a, terms=~ChestPainType, type="deviance")
residualPlots(logist.model4a, terms=~ExerciseAngina, type="deviance")
residualPlots(logist.model4a, terms=~ST_Slope, type="deviance")
residualPlots(logist.model4a, terms=~FastingBloodSugar, type="deviance")

## Marginal plot
marginalModelPlots(logist.model4a, terms=~MaxHR)
marginalModelPlots(logist.model4a, terms=~Oldpeak)
marginalModelPlots(logist.model4a, terms=~log_Cholesterol)

## Outliers test
outlierTest(logist.model4a)

## Leverage
library(car)
par(mfrow=c(1,1))
influenceIndexPlot(logist.model4a)

## Influence
library(car)
par(mfrow=c(1,1))
influencePlot(logist.model4a, col="red")

## Remove the two most influential observations (w/ largest Cook's distance)
## and compare impact on the coefficient estimates.
logist.model4b <- update(logist.model4a, subset=c(-703, -497))
compareCoefs(logist.model4a, logist.model4b)

contrasts(logist.model4a$DiseasedHeart)
## Check dependence of gender by removing variable Sex
logist.model4c <- glm(DiseasedHeart~ChestPainType+MaxHR+ExerciseAngina+Oldpeak+I(Oldpeak^2)
                      +ST_Slope+FastingBloodSugar+log_Cholesterol, family=binomial("logit"), data=heart.df1)
residualPlots(logist.model4c)
summary(logist.model4c)
Anova(logist.model4c)
anova(logist.model4c, logist.model4a, test="LRT")

###################################################################################
## Build Predictive Model 
###################################################################################

table(heart.df1$Sex)
table(heart.df1$ChestPainType)
table(heart.df1$ExerciseAngina)
table(heart.df1$ST_Slope)
table(heart.df1$FastingBloodSugar)
table(heart.df1$DiseasedHeart)

## Create stratified training and test data sets
heart.df2 <- subset(heart.df1, select = -c(Age, RestingECG, log_RestingBP))
set.seed(2)
train <- createDataPartition(paste(heart.df2$Sex, heart.df2$ChestPainType,
                                   heart.df2$ExerciseAngina, heart.df2$ST_Slope,
                                   heart.df2$FastingBloodSugar, heart.df2$DiseasedHeart,
                                   sep = ""), p = 0.7, list = FALSE)

train.data <- heart.df2[train, ]
dim(train.data)
test.data <- heart.df2[-train, ]
dim(test.data)
DiseasedHeart.test <- heart.df2$DiseasedHeart[-train]
length(DiseasedHeart.test)

## Logistic model regression with training data
logistic.fit <- glm(DiseasedHeart~Sex+ChestPainType+MaxHR+ExerciseAngina+Oldpeak+I(Oldpeak^2)
                    +ST_Slope+FastingBloodSugar+log_Cholesterol, family=binomial("logit"), data=train.data)
summary(logistic.fit)
Anova(logistic.fit)

## Predict probability for "response" with test data
logistic.fit.probs <- predict(logistic.fit, test.data, type = "response")

## To predict "YES" for DiseasedHeart by applying a 50% threshold to posterior probabilities
dim(test.data)
logistic.fit.pred <- rep("NO", 227)
logistic.fit.pred[logistic.fit.probs > 0.5] <- "YES"

## Produce the confusion matrix
table(logistic.fit.pred, DiseasedHeart.test)

## Test error calculation
mean(logistic.fit.pred == DiseasedHeart.test)
mean(logistic.fit.pred != DiseasedHeart.test)

## Create the ROC curve
library(pROC)
ROC <- roc(DiseasedHeart.test,logistic.fit.probs)
par(mfrow=c(1,1))
plot(ROC, col = "blue", main = "ROC - Logistic Regression", cex.main =0.9,
     ylim = c(0, 1.02))

# Calculate the area under the curve (AUC)
auc(ROC)


###################################################################################
## Back-up codes
###################################################################################

### Pearson Residuals v.s. observation 
par(mfrow=c(1,1))
plot(residuals(logist.model4,type="pearson"),main="pearson residual plot (model 4)")

### Deviance Residuals v.s. observation
plot(residuals(logist.model4,type="deviance"),main="deviance residual plot (model 4)")

### Hat Diagonal Plot
plot(hatvalues(logist.model4),ylab="H",main= "Hat Matrix Values (model 4)", xlab="Case Number Index")

### Intercept DfBeta Plot
plot(dfbetas(logist.model4)[,2],ylab="DFBETA1",main= "DFBETA Values (model 4)", xlab="Case Number Index")

## Additional diagnostics needed for evaluating the overall fit of the model when 
## having a continuous predictor(s) 
lm.influence(logist.model2)

## odds-ratio estimate and its 95% Wald Confidence Interval
exp(coefficients(logist.model2)[2])

## Exponentiate to get on the odds-scale
exp(confint(logist.model2))
