getwd()

library(tidyverse)
library(ICSNP)
library(ICS)
library(bestNormalize)
library(psych)

#Get path to data
path <- '../Downloads/ames_housing_regression.csv'

#import data as tidyverse tibble
data <- read_csv(path)
spec(data)

#store dependent variables in depvars
depvars <- tibble(Y1 = yeojohnson(data$SalePrice)$x.t, 
                  Y2 = yeojohnson(data$Age)$x.t)

#store independent variables in indvars while Normalizing and Standardizing
indvars <- tibble(X1 = bestNormalize(data$TotalBasementSF)$x.t, 
                  X2 = yeojohnson(data$TotalLivingAreaSF)$x.t,
                  X3 = yeojohnson(data$FullBath)$x.t,
                  X4 = yeojohnson(data$HalfBath)$x.t,
                  X5 = yeojohnson(data$BedroomAbvGround)$x.t,
                  X6 = yeojohnson(data$TotalRoomsAbvGround)$x.t,
                  X7 = yeojohnson(data$GarageCars)$x.t,
                  X8 = bestNormalize(data$GarageSF)$x.t,
                  X9 = yeojohnson(data$WoodDeckSF)$x.t,
                  X10 = bestNormalize(data$TotalPorchSF)$x.t)
                   

psych::describe(indvars, skew=FALSE)

#visualize the data to check for normality
for (col in 1:ncol(indvars)){
  qqnorm(indvars[[col]], main=paste('X', toString(col), sep=''))
}

#Perform Principal Component Analysis
princomps <- prcomp(indvars)
summary(princomps)

#Plot our PC's using screeplot
screeplot(princomps, col='red', type='lines')

#From the output of PCA, we want to retain 6 components. We want to check
#this does not produce a statistically significant model when compared to our 
#intial 10 components

pca_data = cbind(depvars, princomps$x)

pca_struct = tibble(pca_data)

pca_reg_full <- lm(cbind(Y1, Y2)~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=pca_struct)
summary(pca_reg_full)

pca_reg_reduced <- update(pca_reg_full, .~. -PC7 -PC8 -PC9 -PC10)
summary(pca_reg_reduced)

anova(pca_reg_full, pca_reg_reduced)
#Despite results, we will proceed with the reduced model.

#Get factor loadings to interpret our 6 Principle Components
factor_loads <- princomps$rotation
factor_loads



