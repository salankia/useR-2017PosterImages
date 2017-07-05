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
important_vars <- colnames(tt)[2:9]


############ Tabplot drawing #################
tabplot::tableplot(tt, select_string = c("Survived", "Predicted", important_vars),
                   fontsize = 18)
