titanic_transformation <- function(tt) {
  ## creating factors from characters
  tt$CabinNumber <- as.numeric(as.character(gsub(pattern = "[ABCDEFGT]", replacement = "", tt$Cabin)))
  tt$CabinNumber[is.na(tt$CabinNumber)] <- 0
  tt$CabinSign <- gsub(pattern = "[0123456789]", replacement = "", tt$Cabin)
  tt$CabinSign[tt$CabinSign == ""] <- "Unknown"
  tt$CabinSign <- factor(tt$CabinSign)
  
  tt <- tt %>%
    select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, CabinNumber, CabinSign,
           Embarked) %>%
    filter(Embarked != "")
  
  tt$Sex <- as.factor(tt$Sex)
  tt$Embarked <- as.factor(tt$Embarked)
  tt$Survived[tt$Survived == 0] <- "no"
  tt$Survived[tt$Survived == 1] <- "yes"
  tt$Survived <- factor(tt$Survived)
  tt$Pclass <- factor(tt$Pclass)
  
  ## TBD: Missing values
  tt <- na.omit(tt)
  
  return(tt)
}