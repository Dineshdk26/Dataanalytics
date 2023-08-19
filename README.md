# Titanic-Survival_DS

I am a beginner in the field of data analytics and am willing to work on it hence I'm using this repository as a medium to share my work and learn new things from the errors in my work.

This is my code for Titanic Data set I have learnt it from https://www.youtube.com/watch?v=Zx2TguRHrJE here also have added few more changes of myown which ensures this work is inspired from the above walkthrough but not copied.

titanic.train <- read.csv( file = "train.csv",stringsAsFactors = FALSE, header = TRUE)

titanic.test <- read.csv( file = "test.csv",stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainset <- TRUE
titanic.test$IsTrainset <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train,titanic.test)

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#clean missing values of fare
age.mean <- mean(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.mean

#clean missing values of fare
fare.mean <- mean(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.mean

#categorical casting for everycolumn except survived
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$SibSp <- as.factor(titanic.full$SibSp)
titanic.full$Parch <- as.factor(titanic.full$Parch)

#split dataset back out in to train and test
titanic.train <- titanic.full[titanic.full$IsTrainset==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainset==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

Survived.equation <- "Survived ~ Pclass + Sex + Age + Ticket + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
install.packages("randomForest")
library(randomForest)

 titanic.model <- randomForest(formula = Survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))
 
 features.equation <- " Pclass + Sex + Age + Ticket + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_Titanicsurv.csv" , row.names = FALSE)
