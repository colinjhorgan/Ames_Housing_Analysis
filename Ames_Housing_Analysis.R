getwd()

library(tidyverse)
library(ICSNP)
library(psych)
library(qwraps2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)

#set path to import data
path <- "~/Classes/Fall 2020/STAT613/Exam 1/P6/ames_housing_regression.csv"

#read data into a tibble for easy manipulation
data <- read_csv(path)

########################### PART A #############################################

#store dependent variables in depvars
depvars <- cbind(data$SalePrice, data$Age)

#store independent variables in indvars
indvars <- cbind(data$TotalBasementSF, 
                 data$TotalLivingAreaSF,
                 data$FullBath,
                 data$HalfBath,
                 data$BedroomAbvGround,
                 data$TotalRoomsAbvGround,
                 data$GarageCars,
                 data$GarageSF,
                 data$WoodDeckSF,
                 data$TotalPorchSF)

#create a multivariate linear model with specified depvars and indvars
lin_mod <- lm(depvars ~ indvars)
summary(lin_mod)

######################### PART B ###############################################

#Our goal is to check our independent and dependent variables for normality. 

#graph dependent variables
hist(data$SalePrice)
hist(data$Age)

#describe dependent variables
psych::describe(depvars)

#transform dependent variables
depvars_tr <- mutate(select(data, SalePrice, Age),
                     SalePrice = log(SalePrice + 1),
                     Age = log(Age + 1))

#describe transformed independent variables
psych::describe(depvars_tr)


#independent variables
hist(data$TotalBasementSF)
hist(data$TotalLivingAreaSF)
hist(data$FullBath)
hist(data$HalfBath)
hist(data$BedroomAbvGround)
hist(data$TotalRoomsAbvGround)
hist(data$GarageCars)
hist(data$GarageSF)
hist(data$WoodDeckSF)
hist(data$TotalPorchSF)

#transform independent variables
indvars_raw <- select(data,
                    TotalBasementSF, 
                    TotalLivingAreaSF,
                    FullBath,
                    HalfBath,
                    BedroomAbvGround,
                    TotalRoomsAbvGround,
                    GarageCars,
                    GarageSF,
                    WoodDeckSF,
                    TotalPorchSF)

#Summarize independent variables
psych::describe(indvars_raw)

#Transform independent variables
indvars_tr <- mutate(indvars_raw,
                     TotalBasementSF = log(TotalBasementSF + 1), 
                     TotalLivingAreaSF = log(TotalLivingAreaSF + 1),
                     FullBath = log(FullBath + 1),
                     HalfBath = log(HalfBath + 1),
                     BedroomAbvGround = log(BedroomAbvGround + 1),
                     TotalRoomsAbvGround = log(TotalRoomsAbvGround + 1),
                     GarageCars = log(GarageCars + 1),
                     GarageSF = log(GarageSF + 1),
                     WoodDeckSF = log(WoodDeckSF + 1),
                     TotalPorchSF = log(TotalPorchSF + 1))

#Summarize transformed independent variables
psych::describe(indvars_tr)

#Cast depvars_tr and indvars_tr as matrices for dependent and independent variables

depvars_tr <- cbind(depvars_tr$SalePrice,
                    depvars_tr$Age)

indvars_tr <- cbind(indvars_tr$TotalBasementSF,
                    indvars_tr$TotalLivingAreaSF,
                    indvars_tr$FullBath,
                    indvars_tr$HalfBath,
                    indvars_tr$BedroomAbvGround,
                    indvars_tr$TotalRoomsAbvGround,
                    indvars_tr$GarageSF,
                    indvars_tr$GarageCars,
                    indvars_tr$WoodDeckSF,
                    indvars_tr$TotalPorchSF)

lin_mod <- lm(depvars_tr ~ indvars_tr)
summary(lin_mod)

######################### PART C ################################################

#Our goal is to plot the residuals vs. the predicted values, and then plot the
#residuals vs. each independent variable.

#store the predicted values of our model
p <- predict(lin_mod)

#store the residuals of our model
r <- residuals(lin_mod)

#move data on independent variables from a matrix to a tibble
indvars_tr <- as_tibble(data.frame(indvars_tr))

indvars_tr <- indvars_tr %>%
                rename(
                  TotalBasementSF = X1, 
                  TotalLivingAreaSF = X2,
                  FullBath = X3,
                  HalfBath = X4,
                  BedroomAbvGround = X5,
                  TotalRoomsAbvGround = X6,
                  GarageCars = X7,
                  GarageSF = X8,
                  WoodDeckSF = X9,
                  TotalPorchSF = X10
                )

#combine predicted values and residuals in order to plot in ggplot2
plot_data <- as_tibble(data.frame(p, r))

#rename columns for easier reading
plot_data <- plot_data %>%
  rename(
    PredictedPrice = X1,
    PredictedAge = X2,
    ResidualPrice = X1.1,
    ResidualAge = X2.1
  )

ggplot(plot_data, aes(y = ResidualPrice, x=PredictedPrice)) + geom_point()

ggplot(plot_data, aes(y = ResidualAge, x=PredictedAge)) + geom_point()

#Create graphs of Price Residuals vs. independent variables
for (col in 1:ncol(indvars_tr)){
  scatter.hist(y = plot_data$ResidualPrice, x = indvars_tr[[col]],
               ylab = "ResidualPrice", xlab = colnames(indvars_tr)[col],
               correl = FALSE, ellipse = FALSE, density = FALSE)
}

#Create graphs of Age Residuals vs. independent variables
for (col in 1:ncol(indvars_tr)){
  scatter.hist(y = plot_data$ResidualAge, x = indvars_tr[[col]],
               ylab = "ResidualAge", xlab = colnames(indvars_tr)[col],
               correl = FALSE, ellipse = FALSE, density = FALSE)
}

###################### PART D ################################################

#We want to perform a stepwise multivariate regression on our model.
#Backwards stepwise was easiest to learn, so we'll do that one.

#In order to perform the regression we need to convert tibbles back to matrices
depvars_tr <- cbind(depvars_tr$SalePrice,
                    depvars_tr$Age)

indvars_tr <- cbind(indvars_tr$TotalBasementSF,
                    indvars_tr$TotalLivingAreaSF,
                    indvars_tr$FullBath,
                    indvars_tr$HalfBath,
                    indvars_tr$BedroomAbvGround,
                    indvars_tr$TotalRoomsAbvGround,
                    indvars_tr$GarageSF,
                    indvars_tr$GarageCars,
                    indvars_tr$WoodDeckSF,
                    indvars_tr$TotalPorchSF)

#Perform stepwise linear regression

lin_mod <- lm(depvars_tr[,1] ~ indvars_tr[,1] + indvars_tr[,2]
              + indvars_tr[,3] + indvars_tr[,4] + indvars_tr[,5]
              + indvars_tr[,6] + indvars_tr[,7] + indvars_tr[,8]
              + indvars_tr[,9] + indvars_tr[,10])
summary(lin_mod)

#remove variables of lin_mod that are p>=1.5 in lin_mod2
lin_mod2 <- lm(depvars_tr[,1] ~ indvars_tr[,1] + indvars_tr[,2]
              + indvars_tr[,3] + indvars_tr[,5]
              + indvars_tr[,6] + indvars_tr[,7] + indvars_tr[,8]
              + indvars_tr[,9] + indvars_tr[,10])
summary(lin_mod2)

stats::anova(lin_mod, lin_mod2)








