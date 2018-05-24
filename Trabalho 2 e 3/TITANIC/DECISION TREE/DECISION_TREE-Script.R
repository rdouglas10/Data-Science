training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/training_set_clean.csv")
validation_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/validation_set_clean.csv")
final_training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/final_training_set_clean.csv")
test_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/test_clean.csv")


library(rpart)
start.time <- Sys.time()
Dtree <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=training_set_clean,
)
Dtree_pred = predict(Dtree, newdata = validation_set_clean[-1], type = 'class')
end.time <- Sys.time()
time.takenDT <- end.time - start.time
par(mfrow=c(1,2)) 
plot(Dtree)
text(Dtree)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(Dtree)
plot(Dtree)

Dtree_cm = table(t(validation_set_clean[, 1]), Dtree_pred)
Dtree_success = sum(diag(Dtree_cm))/sum(Dtree_cm)*100 
Dtree_success_survived = Dtree_cm[2,2]/(Dtree_cm[2,1]+Dtree_cm[2,2])*100
Dtree_success_notsurvived = Dtree_cm[1,1]/(Dtree_cm[1,2]+Dtree_cm[1,1])*100
Dtree_cm

CrossTable(x = t(validation_set_clean[, 1]), y = Dtree_pred, prop.chisq=FALSE)

