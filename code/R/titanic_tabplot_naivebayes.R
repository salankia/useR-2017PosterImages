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
fit <- naiveBayes(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                           CabinNumber + CabinSign + Embarked,
                         data=tt)

tt$Predicted <- predict(fit, tt)
important_vars <- c("Sex", "Pclass", "CabinSign", "Embarked",
                    "Fare", "CabinNumber", "Age", "Parch", "SibSp")


############ Tabplot drawing #################
tabplot::tableplot(tt, select_strin = c("Survived", "Predicted", important_vars),
                   numPals = "PRGn",
                   pals = list("Set1", "Set5", "Set6",  "HCL1", "HCL3"),
                   fontsize = 18)

