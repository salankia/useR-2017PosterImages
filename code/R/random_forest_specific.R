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


votes <- fit$votes
p <- predict(fit, tt, norm.votes = F, nodes = T, predict.all = T)
m <- as.data.frame(as.matrix(p[[2]]))
m$rownumber <- row.names(tt)
melted <- gather(m, key = trees, value = prediction, 1:1000)

variables <- data.frame(trees = numeric(), variables = numeric())
for(i in seq(1, 1000)) {
  tree <- getTree(fit, k = i)
  variables <- rbind(variables, data.frame(trees = i, variables = tree[, 3]))
}

variables <- variables[variables$variables != 0, ]
variables$trees <- paste0("V", variables$trees)
joined <- left_join(melted, variables)
joined$rownumber <- as.numeric(as.character(joined$rownumber))

summarised <- joined %>%
  group_by(rownumber, variables) %>%
  summarise(ntrees_with_var = length(prediction),
             pred0 = sum(prediction == "no"),
             pred1 = sum(prediction == "yes")) %>%
  ungroup(rownumber, variables)
summarised$pmax <-  pmax(summarised$pred0, summarised$pred1)
summarised$value <-  summarised$pmax / summarised$ntrees_with_var
summarised$value_to_plot <- apply(summarised, MARGIN = 1, FUN = function(x){
  if (x[6] == x[4]) {return(x[7])}
  else return(as.numeric(x[7]) * (-1))
})
summarised$category <- apply(summarised, MARGIN = 1, FUN = function(x){
  if (x[6] == x[4]) {return("no")}
  else return("yes")
})
summarised$value_to_plot <- as.numeric(summarised$value_to_plot)

predicted_values <- data.frame(rownumber = sort(unique(summarised$rownumber)), 
                               Survived = tt$Survived,
                               Predicted = fit$predicted)
summarised <- left_join(summarised, predicted_values)
summarised <- summarised %>%
  arrange(value_to_plot) %>%
  mutate(rownumber = factor(rownumber, levels = unique(rownumber))) %>%
  mutate(variables_text = c("Pclass", "Sex" , "Age" , "SibSp" , "Parch" , "Fare" ,
                              "CabinNumber",  "CabinSign", "Embarked")[variables]) %>%
  mutate(variables_text = factor(variables_text, levels = important_vars))


ggplot(summarised) +
  stat_bin2d(aes(x = variables_text,
                 y = as.factor(rownumber),
                 fill = category,
                 alpha = value),
             geom = "tile") +
  scale_fill_manual("Predicted category", values= c("#a6611a", "#2c7fb8", "#4dac26")) +
  scale_alpha_continuous("Confidence") +
  facet_grid(paste0("Actual: ", Survived) + paste0("Predicted: ", Predicted) ~ ., scales = "free", drop = T,
              space = "free") +
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank()) +
  xlab("Variables") + ylab("Records") +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(face = "bold"),
        text = element_text(face = "bold", size = 18))
