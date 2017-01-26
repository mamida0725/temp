url = "https://storage.googleapis.com/2017_ithome_ironman/data/kaggle_titanic_train.csv"
titanic <- read.csv(url)

#remove data without age info 891-177=714
titanic <- titanic[complete.cases(titanic), ]

titanic$Survived <- factor(titanic$Survived)
# Split
n <- nrow(titanic)
set.seed(87)
shuffled_titanic <- titanic[sample(n), ]

train_indices <- 1:round(0.7 * n)
train <- shuffled_titanic[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled_titanic[test_indices, ]

#install.packages("rpart")
#library(rpart)
#create model
tree_fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Predict: type=pro
prediction <- predict(tree_fit, test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], type = "class")

# calculate accuracy
confusion_matrix <- table(test$Survived, prediction)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy