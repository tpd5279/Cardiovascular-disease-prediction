###############################################################################################
## STAT 581 Project: Heart data exploratory data analysis
## Author: Tina Dhekial-Phukan
###############################################################################################
rm(list = ls(all.names = TRUE))
gc()
options(digits=12)

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
## EDA on raw data set
################################################################################
heart <- read.csv(file="heart.csv")
View(heart)

## Checking for missing data in each column
NA.sum <- colSums(is.na(heart))
NA.sum

## Identify duplicate rows
library(misty)
which(duplicated(heart))

## Checking summary statistics of the data set
summary(heart)

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
length(which(heart$Cholesterol==0))
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

## Checking summary statistics of the data set
summary(heart)

View(heart)
summary(heart)
## Checking for missing data in each column
NA.sum <- colSums(is.na(heart))
NA.sum
## Identify duplicate rows
library(misty)
which(duplicated(heart))

table(heart$Sex)
table(heart$ChestPainType)
table(heart$RestingECG)
table(heart$ExerciseAngina)
table(heart$ST_Slope)
table(heart$DiseasedHeart)
table(heart$FastingBloodSugar)

## Create subset of the numeric variables names
num_var <- names(subset(heart, select = c(Age, RestingBP, Cholesterol, MaxHR, Oldpeak)))
## Convert cholesterol to log(cholesterol)
heart$log_Cholesterol <- log(heart$Cholesterol)
## Convert cholesterol to log(cholesterol)
heart$log_RestingBP <- log(heart$RestingBP)
## Plot univariate histograms for the numeric variables
par(mfrow=c(2,4))
for (v in num_var) 
{
  hist(heart[, v], main="", ylab="Freq", xlab = paste(v, sep=""), font.lab = 2, font.axis = 2, cex.lab = 1.5, breaks = 25)
}
hist(heart$log_RestingBP, main="", ylab="Freq", xlab = paste("log_RestingBP"), font.lab = 2, font.axis = 2, cex.lab = 1.5, breaks = 25)
hist(heart$log_Cholesterol, main="", ylab="Freq", xlab = paste("log_cholesterol"), font.lab = 2, font.axis = 2, cex.lab = 1.5, breaks = 25)

## Box plots of the numeric variables grouped by diagnosis
library(ggplot2)
library(patchwork)
bxplt1 <- ggplot(heart, aes(x=DiseasedHeart, y=Age, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt2 <- ggplot(heart, aes(x=DiseasedHeart, y=RestingBP, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt3 <- ggplot(heart, aes(x=DiseasedHeart, y=Cholesterol, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt4 <- ggplot(heart, aes(x=DiseasedHeart, y=MaxHR, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt5 <- ggplot(heart, aes(x=DiseasedHeart, y=Oldpeak, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt6 <- ggplot(heart, aes(x=DiseasedHeart, y=log_RestingBP, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt7 <- ggplot(heart, aes(x=DiseasedHeart, y=log_Cholesterol, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=0.4))
bxplt1/bxplt2/bxplt3|bxplt4/bxplt5/bxplt6/bxplt7

bxplt1 <- ggplot(heart, aes(x=DiseasedHeart, y=Age, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt2 <- ggplot(heart, aes(x=DiseasedHeart, y=log_RestingBP, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt3 <- ggplot(heart, aes(x=DiseasedHeart, y=log_Cholesterol, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt4 <- ggplot(heart, aes(x=DiseasedHeart, y=MaxHR, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))
bxplt5 <- ggplot(heart, aes(x=DiseasedHeart, y=Oldpeak, color=DiseasedHeart))+ geom_boxplot()+theme(axis.text.x=element_text(size=1))

par(mfrow=c(2,2))
hist(heart$Age, main="", ylab="Freq", xlab = "Age", breaks = 25)
hist(log(heart$RestingBP), main="", ylab="Freq", xlab = "log(RestingBP)", breaks = 25)
hist(log(heart$Cholesterol), main="", ylab="Freq", xlab = "log(Cholesterol)", breaks = 25)
hist(heart$MaxHR, main="", ylab="Freq", xlab = "MaxHR", breaks = 25)

heart.df1 <- subset(heart, select = c(Age, log_RestingBP, log_Cholesterol, MaxHR, Oldpeak, DiseasedHeart))
x.disease <- subset(heart.df1, DiseasedHeart %in% "YES")
x.disease <- subset(x.disease, select = -c(DiseasedHeart))
x.normal <- subset(heart.df1, DiseasedHeart %in% "NO")
x.normal <- subset(x.normal, select = -c(DiseasedHeart))
par(mfrow=c(1,1))
mqqnorm(x.disease, main = "Multivariate normal Q-Q Plot for Diseased Heart")
par(mfrow=c(1,1))
mqqnorm(x.normal, main = "Multivariate normal Q-Q Plot for Normal Heart")

## For plotting correlation matrix of the variables
install.packages("corrplot")
library(corrplot)
corrmat <- cor(rbind(x.disease, x.normal), method = "pearson")
par(mfrow=c(1,1))
corrplot(corrmat, method = 'number', tl.cex=0.9, number.cex = 0.9, type = "lower",
         diag = FALSE,col = colorRampPalette(c("white", "orange", "red"))(100))

# Export to CSV
heart.df2 <- subset(heart.df1, select = -Cholesterol)
write.csv(heart.df2,file='C:/Users/tinad/OneDrive - The Pennsylvania State University/Penn_State_Online_MAS/STAT_581/Datasets/Kaggle/heart_df2.csv', row.names = FALSE)












































