library(readr)
training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/training_set_clean.csv")
validation_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/validation_set_clean.csv")
final_training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/final_training_set_clean.csv")
test_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/test_clean.csv")
  
head(training_set_clean,1)

str(training_set_clean,give.attr = FALSE)
str(validation_set_clean,give.attr = FALSE)
str(final_training_set_clean,give.attr = FALSE)
str(test_clean,give.attr = FALSE)

training_set_clean$Survived <- as.factor(training_set_clean$Survived)
training_set_clean$Pclass <- as.factor(training_set_clean$Pclass)
training_set_clean$Sex <- as.factor(training_set_clean$Sex)
training_set_clean$Embarked <- as.factor(training_set_clean$Embarked)

validation_set_clean$Survived <- as.factor(validation_set_clean$Survived)
validation_set_clean$Pclass <- as.factor(validation_set_clean$Pclass)
validation_set_clean$Sex <- as.factor(validation_set_clean$Sex)
validation_set_clean$Embarked <- as.factor(validation_set_clean$Embarked)

final_training_set_clean$Survived <- as.factor(final_training_set_clean$Survived)
final_training_set_clean$Pclass <- as.factor(final_training_set_clean$Pclass)
final_training_set_clean$Sex <- as.factor(final_training_set_clean$Sex)
final_training_set_clean$Embarked <- as.factor(final_training_set_clean$Embarked)

test_clean$Pclass <- as.factor(test_clean$Pclass)
test_clean$Sex <- as.factor(test_clean$Sex)
test_clean$Embarked <- as.factor(test_clean$Embarked)


# KNN Method


training_set_cleanKNN <- as.data.frame(training_set_clean)
validation_set_cleanKNN <- as.data.frame(validation_set_clean)

training_set_cleanKNN$Sex <- sapply(as.character(training_set_cleanKNN$Sex), switch, 'male' = 0, 'female' = 1)
validation_set_cleanKNN$Sex <- sapply(as.character(validation_set_cleanKNN$Sex), switch, 'male' = 0, 'female' = 1)
training_set_cleanKNN$Embarked <- sapply(as.character(training_set_cleanKNN$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
validation_set_cleanKNN$Embarked <- sapply(as.character(validation_set_cleanKNN$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)

training_set_cleanKNN$Sex <- as.factor(training_set_cleanKNN$Sex)
validation_set_cleanKNN$Sex <- as.factor(validation_set_cleanKNN$Sex)
training_set_cleanKNN$Embarked <- as.factor(training_set_cleanKNN$Embarked)
validation_set_cleanKNN$Embarked <- as.factor(validation_set_cleanKNN$Embarked)

library(class)
start.time <- Sys.time()
KNN_pred = knn(train = training_set_cleanKNN[, -1],
               test = validation_set_cleanKNN[, -1],
               cl = training_set_cleanKNN[, 1],
               k = 5,
               prob = TRUE)
end.time <- Sys.time()
time.takenKNN <- end.time - start.time

View(training_set_cleanKNN[, -1])
View(validation_set_cleanKNN[, -1])

# create confusion matrix
KNN_cm = table(t(validation_set_clean[, 1]), KNN_pred)
# compute the overall success rate
KNN_success = sum(KNN_cm[1,1]+KNN_cm[2,2])/sum(KNN_cm)*100
# compute the success rate to predict survival
KNN_success_survived = KNN_cm[2,2]/(KNN_cm[2,1]+KNN_cm[2,2])*100
# compute the success rate to predict casualty
KNN_success_notsurvived = KNN_cm[1,1]/(KNN_cm[1,2]+KNN_cm[1,1])*100
# predicted in vertical and actual horizontal ( 68 survival in validation set)
KNN_cm

CrossTable(x = t(validation_set_clean[, 1]), y = KNN_pred, prop.chisq=FALSE)

