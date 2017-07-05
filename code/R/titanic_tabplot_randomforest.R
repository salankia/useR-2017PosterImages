library(titanic)
library(dplyr)
library(randomForest)
library(tabplot)

source("R/titanic_transformation.R")

tt <- titanic_train

############ Transformation #################
tt <- titanic_transformation(tt)
summary(tt)
set.seed(23)

############ Model Building #################
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      CabinNumber + CabinSign + Embarked,
                    data=tt, 
                    importance=TRUE, 
                    ntree=1000,
                    maxnodes = 5,
                    proximity = TRUE)
tt$Predicted <- fit$predicted
important_vars <- names(sort(fit$importance[, 4], decreasing = T))


############ Tabplot drawing #################
tabplot::tableplot(tt, select_strin = c("Survived", "Predicted", important_vars),
                   numPals = "PRGn",
                   pals = list("Set1", "Set5", "Set6",  "HCL1", "HCL3"),
                   fontsize = 18)
