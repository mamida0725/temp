url <- "https://storage.googleapis.com/py_ds_basic/kaggle_titanic_test.csv"
to_predict <- read.csv(url)
summary(to_predict)
dim(to_predict)

#Fill in Embarked missing level
titanic$Embarked <- as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked == ""] <- "S"
titanic$Embarked <- factor(titanic$Embarked)
table(titanic$Embarked)

train$Embarked <- as.character(train$Embarked)
train$Embarked[train$Embarked == ""] <- "S"
train$Embarked <- factor(train$Embarked)
table(train$Embarked)

to_predict$Embarked <- as.character(to_predict$Embarked)
to_predict$Embarked[to_predict$Embarked == ""] <- "S"
to_predict$Embarked <- factor(to_predict$Embarked)
table(to_predict$Embarked)

# Fare contains 1 NA; fill in with fare_mean
fare_mean <- mean(to_predict$Fare, na.rm = TRUE)
to_predict$Fare[is.na(to_predict$Fare)] <- fare_mean

# Age contains 86 NA; group first group by class; fill in with grouped age mean
library(dplyr)
library(magrittr)

#to_predict %>%
#  group_by(Pclass) %>%
  

mean_age_by_Pclass <- to_predict %>%
  group_by(Pclass) %>%
  summarise(mean_age = round(mean(Age, na.rm = TRUE)))
filter_1 <- is.na(to_predict$Age) & to_predict$Pclass == 1
filter_2 <- is.na(to_predict$Age) & to_predict$Pclass == 2
filter_3 <- is.na(to_predict$Age) & to_predict$Pclass == 3
mean_age_by_Pclass

to_predict[filter_1, ]$Age <- 41
to_predict[filter_2, ]$Age <- 29
to_predict[filter_3, ]$Age <- 24

# Summary after imputation
summary(to_predict)

# ready for prediction
predicted <- predict(rf_clf, newdata = to_predict[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")])
to_submit <- data.frame(to_predict[, "PassengerId"], predicted)
names(to_submit) <- c("PassengerId", "Survived")
head(to_submit, n = 10)

# change format to csv
write.csv(to_submit, file = "to_submit.csv", row.names = FALSE)
getwd()
