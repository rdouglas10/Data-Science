library(readr) 
train <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/train.csv")
test <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/test.csv")


library(caTools)
set.seed(123) 
split = sample.split(train$Survived, SplitRatio = 0.8)
training_set = subset(train, split == TRUE)
validation_set = subset(train, split == FALSE)
final_training_set = train

colSums(is.na(training_set)) 
colSums(is.na(validation_set))
colSums(is.na(test))

training_set_clean <- subset(training_set, select = -c(PassengerId,Ticket,Cabin,Name))
validation_set_clean <- subset(validation_set, select = -c(PassengerId,Ticket,Cabin,Name))
test_clean <- subset(test, select = -c(Ticket,Cabin,Name))
final_training_set_clean <- subset(final_training_set, select = -c(PassengerId,Ticket,Cabin,Name))

training_set_clean$Age[is.na(training_set_clean$Age)]<-median(na.omit(training_set_clean$Age)) # replace Missing Age with median
validation_set_clean$Age[is.na(validation_set_clean$Age)]<-median(na.omit(validation_set_clean$Age)) # replace Missing Age with median
test_clean$Age[is.na(test_clean$Age)]<-median(na.omit(test_clean$Age)) # replace Missing Age with median
final_training_set_clean$Age[is.na(final_training_set_clean$Age)]<-median(na.omit(final_training_set_clean$Age))

plot(test_clean$Age)

test_clean$Fare[is.na(test_clean$Fare)]<-median(na.omit(test_clean$Fare))
#test_clean$Fare[is.na(test_clean$Fare)]<-median(na.omit(test_clean$Fare))

table(training_set_clean$Embarked) 
table(validation_set_clean$Embarked)
table(test_clean$Embarked)

training_set_clean$Embarked[is.na(training_set_clean$Embarked)]<-"S"
final_training_set_clean$Embarked[is.na(final_training_set_clean$Embarked)]<-"S"

colSums(is.na(training_set_clean)) 
colSums(is.na(validation_set_clean))
colSums(is.na(test_clean))
colSums(is.na(final_training_set_clean))

training_set_clean[4] = scale(training_set_clean[4])
training_set_clean[7] = scale(training_set_clean[7])
validation_set_clean[4] = scale(validation_set_clean[4])
validation_set_clean[7] = scale(validation_set_clean[7])
test_clean[4] = scale(test_clean[4])
test_clean[7] = scale(test_clean[7])
final_training_set_clean[4] = scale(final_training_set_clean[4])
final_training_set_clean[7] = scale(final_training_set_clean[7])


write.csv(training_set_clean, file = "training_set_clean.csv", row.names = FALSE)
write.csv(validation_set_clean, file = "validation_set_clean.csv", row.names = FALSE)
write.csv(test_clean, file = "test_clean.csv", row.names = FALSE)
write.csv(final_training_set_clean, file = "final_training_set_clean.csv", row.names = FALSE)
