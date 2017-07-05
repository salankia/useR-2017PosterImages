library(titanic)
library(dplyr)
library(randomForest)
library(tabplot)
library(RColorBrewer)

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

fit$tables
variables <- c("Pclass", "Sex" , "Age" , "SibSp" , "Parch" , "Fare" ,
               "CabinNumber",  "CabinSign", "Embarked")

density_df <- data.frame(variable = character(0),
                         class = character(0),
                         generated_values = numeric(0),
                         real_values = numeric(0))
for(i in seq_along(variables)){
  print(i)
  if(is.numeric(tt[, variables[i]])) {
    table_tmp <- as.data.frame(fit$tables[[variables[i]]] )
    lapply(seq_along(table_tmp$V1), function(j){
      density_df <<- rbind(density_df,
                           data.frame(variable = variables[i],
                                      class = row.names(table_tmp)[j],
                                      generated_values = rnorm(n = c(sum(tt$Survived == "no"),
                                                                     sum(tt$Survived == "yes"))[j], 
                                                     mean = table_tmp$V1[j],
                                                    sd = table_tmp$V2[j]),
                                      real_values = tt[tt$Survived == row.names(table_tmp)[j],
                                                       variables[i]]
                                      ))
      T
    })
  }
}

ggplot(density_df) +
  geom_density(aes(x = generated_values, fill = as.factor(class)), alpha = 0.5) +
  geom_rug(aes(x = real_values, color = as.factor(class))) +
  facet_wrap( ~variable, scales = "free", nrow = 1) +
  scale_fill_manual("Original category", values= c("#a6611a", "#2c7fb8", "#4dac26")) +
  scale_color_manual("Original category", values= c("#a6611a", "#2c7fb8", "#4dac26")) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18, face = "bold"))

## a kategoriukusokra meg rajzolunk mozaikplotot
for(i in seq_along(variables)){
  if(is.factor(tt[, variables[i]])) {
    table_tmp <- as.data.frame(fit$tables[[variables[i]]] )
    print(ggplot(table_tmp) +
      geom_col(aes_string(x = "Y", y = "Freq", fill = colnames(table_tmp)[2])) +
      scale_fill_manual(variables[i], values = rep(brewer.pal(8, "Set2"), times = 20)) +
      xlab("Survived") +
      scale_x_discrete(labels = c("no", "yes")) +
      ylab("") +
      theme_bw() +
      theme(legend.position = "bottom",
      text = element_text(size = 18, face = "bold")))
  }
}

