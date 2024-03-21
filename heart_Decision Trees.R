###############################################################################################
## STAT 581 Project: Heart data decision Trees
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

################################################################################
## Create training and test data sets using stratified sampling
################################################################################

## Convert cholesterol to log(cholesterol)
heart$log_Cholesterol <- log(heart$Cholesterol)

## Convert cholesterol to log(cholesterol)
heart$log_RestingBP <- log(heart$RestingBP)

heart.df3 <- subset(heart, select = -c(Cholesterol, RestingBP))

## Create stratified training and test data sets
set.seed(2)
train <- createDataPartition(paste(heart.df3$Sex, heart.df3$ChestPainType,
                                   heart.df3$ExerciseAngina, heart.df3$ST_Slope,
                                   heart.df3$FastingBloodSugar, heart.df3$DiseasedHeart,
                                   sep = ""), p = 0.7, list = FALSE)

train.data <- heart.df3[train, ]
dim(train.data)
test.data <- heart.df3[-train, ]
dim(test.data)
DiseasedHeart.train <- heart.df3$DiseasedHeart[train]
length(DiseasedHeart.train)
DiseasedHeart.test <- heart.df3$DiseasedHeart[-train]
length(DiseasedHeart.test)

################################################################################
## Fit a classification tree to the training data, with Disease as the response  
## and the other variables as predictors.
################################################################################

library(tree)
## Fitting classification tree 
tree.heart <- tree(DiseasedHeart ~ ., data = train.data, method = "recursive.partition", 
                      split = "deviance")
summary(tree.heart)

## Get detailed text output of the tree object
tree.heart

## Create a plot of the tree
par(mfrow = c(1,1))
plot(tree.heart)
text(tree.heart, pretty = 0, cex= 0.5)

## Compute training error rates for the unpruned tree
tree.train.pred <- predict(tree.heart, train.data, type = "class")
table(tree.train.pred, DiseasedHeart.train)
mean(tree.train.pred == DiseasedHeart.train)
mean(tree.train.pred != DiseasedHeart.train)

## Compute test error rates for the unpruned tree 
tree.test.pred <- predict(tree.heart, test.data, type = "class")
table(tree.test.pred, DiseasedHeart.test)
mean(tree.test.pred == DiseasedHeart.test)
mean(tree.test.pred != DiseasedHeart.test)

## Check whether pruning the tree might lead to improved results and determine 
## the optimal tree size
set.seed(2)
cv.Disease <- cv.tree(tree.heart, FUN = prune.misclass, K = 10)
names(cv.Disease)

## Get detailed text output of the cv object
cv.Disease

## Plot the error rate as a function of size
par(mfrow=c(1,1))
plot(cv.Disease$size, cv.Disease$dev, type = "b") # tree size versus classification error

## Apply the prune.missclass() to prune the tree to obtain the 6-node tree
prune.Disease <- prune.misclass(tree.heart, best = 6)
plot(prune.Disease)
text(prune.Disease, pretty = 0, cex= 0.5)

## Get detailed text output of the tree object
prune.Disease

## Compute training error rates for the pruned tree
tree.pruned.pred.train <- predict(prune.Disease, train.data, type = "class")
table(tree.pruned.pred.train, DiseasedHeart.train)
mean(tree.pruned.pred.train == DiseasedHeart.train)
mean(tree.pruned.pred.train != DiseasedHeart.train)

## Predict the response on the test data, and compute test error rates for the 
## pruned tree 
tree.pruned.pred.test <- predict(prune.Disease, test.data, type = "class")
table(tree.pruned.pred.test, DiseasedHeart.test)
mean(tree.pruned.pred.test == DiseasedHeart.test)
mean(tree.pruned.pred.test != DiseasedHeart.test)

################################################################################
## Bagging
################################################################################

library(randomForest)
## Fit the model with the training data set
set.seed(2)
bag.heart <- randomForest(DiseasedHeart ~., data = train.data, mtry = 4, 
                             importance = TRUE)
bag.heart
importance(bag.heart)
par(mfrow = c(1,1))
varImpPlot(bag.heart)

## Assess performance of the bagged model on the test data set
yhat.bag <- predict(bag.heart, newdata = test.data)
plot(x = yhat.bag, y = DiseasedHeart.test)

## Compute training error rates for the bagged tree 
bag.test.train <- predict(bag.heart, train.data, type = "class")
table(bag.test.train, DiseasedHeart.train)
mean(bag.test.train == DiseasedHeart.train)
mean(bag.test.train != DiseasedHeart.train)

## Compute test error rates for the bagged tree 
bag.test.pred <- predict(bag.heart, test.data, type = "class")
table(bag.test.pred, DiseasedHeart.test)
mean(bag.test.pred == DiseasedHeart.test)
mean(bag.test.pred != DiseasedHeart.test)

## Change the number of trees grown to 25
bag.heart1 <- randomForest(DiseasedHeart ~., data = train.data, mtry = 4, ntree = 25)
yhat.bag1 <- predict(bag.heart1, newdata = test.data)
plot(x = yhat.bag1, y = DiseasedHeart.test)

## Compute training error rates for the bagged tree with ntree = 25
bag.train.pred1 <- predict(bag.heart1, train.data, type = "class")
table(bag.train.pred1, DiseasedHeart.train)
mean(bag.train.pred1 == DiseasedHeart.train)
mean(bag.train.pred1 != DiseasedHeart.train)

## Compute test error rates for the bagged tree with ntree = 25
bag.test.pred1 <- predict(bag.heart1, test.data, type = "class")
table(bag.test.pred1, DiseasedHeart.test)
mean(bag.test.pred1 == DiseasedHeart.test)
mean(bag.test.pred1 != DiseasedHeart.test)

importance(bag.heart1)
varImpPlot(bag.heart1)

################################################################################
## Boosting
################################################################################

install.packages("gbm")
library(gbm)

heart.df4 <- subset(heart, select = -c(Cholesterol, RestingBP))

## Convert the response to be in {0, 1} and create a new outcome variable as factor type
heart.df4$Outcome <- ifelse(heart.df4$DiseasedHeart == "NO", 0, 1)
str(heart.df4)

## Create stratified training and test data sets
set.seed(2)
train <- createDataPartition(paste(heart.df4$Sex, heart.df4$ChestPainType,
                                   heart.df4$ExerciseAngina, heart.df4$ST_Slope,
                                   heart.df4$FastingBloodSugar, heart.df4$DiseasedHeart,
                                   sep = ""), p = 0.7, list = FALSE)

train.data <- heart.df4[train, ]
dim(train.data)
test.data <- heart.df4[-train, ]
dim(test.data)
DiseasedHeart.train <- heart.df4$DiseasedHeart[train]
length(DiseasedHeart.train)
DiseasedHeart.test <- heart.df4$DiseasedHeart[-train]
length(DiseasedHeart.test)


## Fit the model with the training data set
set.seed(2)
boost.heart <- gbm(Outcome ~., data = train.data[-9], distribution = "bernoulli",
                      n.trees = 5000, interaction.depth = 4)
summary(boost.heart)

## Use the boosted model to predict probability of "Disease" on the test set. 
boost.test.probs <- predict(boost.heart, test.data[-9], type = "response")
boost.test.pred <- rep("NO", 227)
boost.test.pred[boost.test.probs > 0.5] <- "YES"

## Produce a confusion matrix for test data
table(boost.test.pred, DiseasedHeart.test)
mean(boost.test.pred == DiseasedHeart.test)
mean(boost.test.pred != DiseasedHeart.test)

## Create the ROC curve
library(pROC)
ROC.boost.init <- roc(DiseasedHeart.test, boost.test.probs)
plot(ROC.boost.init, col = "blue", main = " ROC - initial boosted model", cex.main = 0.9,
     ylim = c(0, 1.02))
auc(ROC.boost.init)

## Tuning the shrinkage parameter
hyper_grid <- expand.grid(
  shrinkage = c(0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05, 0.001),
  n.minobsinnode = c(5, 10, 15),
  min_train.error = 0  # a place to hold the error results
)

# total number of combinations
nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = Outcome ~ .,
    distribution = "bernoulli",
    data = train.data[-9],
    n.trees = 5000,
    interaction.depth = 4,
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = 0.5,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  # add min training error to grid
  hyper_grid$min_train.error[i] <- which.min(gbm.tune$train.error)
}

hyper_grid %>% 
  dplyr::arrange(min_train.error) %>%
  head(10)

# for reproducibility
set.seed(123)
# train final GBM model with the tuned parameters
gbm.fit.final <- gbm(
  formula = Outcome ~ .,
  distribution = "bernoulli",
  data = train.data[-9],
  n.trees = 5000,
  interaction.depth = 4,
  shrinkage = 0.4,
  n.minobsinnode = 5,
  bag.fraction = 0.5, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  

par(mar = c(5, 8, 1, 1))
summary(gbm.fit.final, cBars = 10, method = relative.influence, las = 2)

install.packages("vip")
library(vip)
vip::vip(gbm.fit.final)

## Partial dependence plots for "Cholesterol", "MaxHR", "Age", "Oldpeak" 
plot(gbm.fit.final, i = "log_Cholesterol")
plot(gbm.fit.final, i = "MaxHR")
plot(gbm.fit.final, i = "Age")
plot(gbm.fit.final, i = "Oldpeak")

## Use the tuned model to predict probability of "Disease" on the test set. 
tuned.test.probs <- predict(gbm.fit.final, test.data[-9], type = "response")
tuned.test.pred <- rep("NO", 227)
tuned.test.pred[tuned.test.probs > 0.5] <- "YES"

## Produce a confusion matrix for the test data
table(tuned.test.pred, DiseasedHeart.test)
mean(tuned.test.pred == DiseasedHeart.test)
mean(tuned.test.pred != DiseasedHeart.test)
contrasts(DiseasedHeart.test)

## Create the ROC curve
library(pROC)
ROC.boost <- roc(DiseasedHeart.test, tuned.test.probs)
plot(ROC.boost, col = "blue", main = " ROC - tuned boosted model", cex.main = 0.9,
     ylim = c(0, 1.02))
auc(ROC.boost)




























































































































































