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
important_vars <- c("Sex", "Pclass", "CabinSign", "Embarked",
                    "Fare", "CabinNumber", "Age", "Parch", "SibSp")

## parallel coordinates for the details
## for each of the misclassified rows which are misclassified the
## same way, we create a parallel coordinates plot:
tt$correctness = (tt$Survived == tt$Predicted)

part1 <- tt[tt$Survived == "yes" & tt$Predicted == "no", ]
part1.1 <- tt[tt$Survived == "yes" & tt$Predicted == "yes", ]
part1.2 <- tt[tt$Survived == "no" & tt$Predicted == "no", ]

## parallel coordinates drawing
joint_data_set <- part1 %>%
  mutate(category = "misclassified_it_should_be_yes_we_said_no") %>%
  bind_rows(part1.1 %>%
              mutate(category = "categorized_to_yes_correctly")) %>%
  bind_rows(part1.2 %>%
              mutate(category = "categorized_to_no_correctly"))
joint_data_set <- joint_data_set %>%
  mutate(seq = seq_along(Survived))

## we should create plust variables to number values
joint_data_set_new <- joint_data_set

joint_data_set_old <- joint_data_set %>%
  #mutate_at(important_vars, funs(scale(as.numeric(.), center = T)))
  mutate_at(important_vars, funs((as.numeric(.)-min(as.numeric(.)))/(max(as.numeric(.))
                                                                     -min(as.numeric(.))))
  ) 
joint_data_set_new <- joint_data_set_new[, important_vars]
colnames(joint_data_set_new) <- paste0(important_vars, "_new")
joint_data_set <- bind_cols(joint_data_set_new, joint_data_set_old)

## we should count which takes the pairs next to each other and counts
## the frequency
## lehet, hogy darabokban kellene szetszedni?

freqs_all <- lapply(seq(1, to = length(important_vars) - 1), function(i) {
  column_candidates <- c(important_vars[i], important_vars[i + 1])
  pc_candidate <- joint_data_set[, c("Survived", "Predicted",
                                     "correctness", "category",column_candidates)]
  colnames(pc_candidate)[5:6] <- c("First_var", "Second_var")
  pc_candidate$First_var_name <- column_candidates[1]
  pc_candidate$Second_var_name <- column_candidates[2]
  freqs <- as.data.frame(table(pc_candidate))
  freqs <- freqs[freqs$Freq > 0, ]
})

freqs_all <- bind_rows(freqs_all)
head(freqs_all)
freqs_all$First_var_name <- factor(freqs_all$First_var_name, levels = important_vars)
freqs_all$Second_var_name <- factor(freqs_all$Second_var_name, levels = important_vars)
freqs_all <- freqs_all %>%
  arrange(category)


## csinaljunk most ide egy olyat, hogy pontonkent megmondjuk, 1) hany pont van ott
## es 2) mekkora a konfidenciank

points <- freqs_all[freqs_all$correctness == T, ] %>%
  group_by(First_var, First_var_name, category) %>%
  summarise(freq  = sum(Freq)) %>%
  ungroup(First_var, First_var_name, category)

points_spread <- spread(points, category, freq)
points_spread[is.na(points_spread)] <- 0
points_spread$sum <- points_spread$categorized_to_no_correctly + points_spread$categorized_to_yes_correctly
points_spread$pmax <- pmax(points_spread$categorized_to_no_correctly,
                           points_spread$categorized_to_yes_correctly)
points_spread$alpha <- points_spread$pmax / points_spread$sum
points_spread$cat <- unlist(lapply(seq_along(points_spread$First_var), function(i){
  c("categorized_to_no_correctly", "categorized_to_yes_correctly")[
    points_spread[i,c("categorized_to_no_correctly", "categorized_to_yes_correctly")] == points_spread$pmax[i]][1]
}))

## lets label the categorical variables and
## put the numerical minimums and maximums

uniques <- lapply(important_vars, function(var){
  if(is.factor(joint_data_set[, paste0(var, "_new")])) {
    values <- unique(joint_data_set[, c(var, paste0(var, "_new"))])
    return(data.frame(First_var_name = var,
                      First_var = values[, 1],
                      First_var_text = values[, 2]))
  }
  if(is.numeric(joint_data_set[, paste0(var, "_new")])) {
    values_min <- joint_data_set[joint_data_set[, var] == min(joint_data_set[, var]),
                                 c(var, paste0(var, "_new"))][1, ]
    values_max <- joint_data_set[joint_data_set[, var] == max(joint_data_set[, var]),
                                 c(var, paste0(var, "_new"))][1, ]
    return(data.frame(First_var_name = var,
                      First_var = c(values_min[, 1], values_max[, 1]),
                      First_var_text = as.character(c(values_min[, 2], values_max[, 2]))))
  }
})

texts <- bind_rows(uniques)

library(ggplot2)
ggplot(freqs_all) +
  #  geom_point(aes(x = First_var_name, y = as.numeric(First_var))) +
  geom_segment(aes(x = as.factor(First_var_name), xend = as.factor(Second_var_name),
                   y = as.numeric(First_var), yend = as.numeric(Second_var),
                   color = category, size = Freq)) +
  scale_x_discrete(breaks = as.character(important_vars), drop= F) +
  geom_point(data = points_spread, aes(x = as.factor(First_var_name), y = as.numeric(First_var),
                                       color = cat, size = sum, alpha = alpha)) +
  scale_color_manual("Prediction results", values = c("#a6611a", "#2c7fb8", "#fdae6b"),
                     labels = c("Categorized correctly to: No",
                                "Categorized correctly to: Yes",
                                "Miscategorized: should be Yes and predicted No")) +
  scale_alpha(guide=FALSE) +
  scale_size(guide=FALSE) +
  theme_bw()  +
  theme(text = element_text(face = "bold", size = 18),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key.size = unit(2, "cm")) +
  xlab("") + ylab("") +
  geom_label_repel(data = texts, aes(x = as.factor(First_var_name),
                                     y = as.numeric(First_var),
                                     label = First_var_text), stat = "identity",
                   nudge_y = ifelse(texts$First_var == 1, 1.3, 0),
                   min.segment.length = unit(0.25, "lines"))
