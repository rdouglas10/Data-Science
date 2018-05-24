library(readr) 
library(readr)
training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/training_set_clean.csv")
validation_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/validation_set_clean.csv")
final_training_set_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/final_training_set_clean.csv")
test_clean <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/TITANIC/test_clean.csv")


library(e1071)
start.time <- Sys.time()
NBayes = naiveBayes(x = training_set_clean[-1],
                    y = training_set_clean$Survived)

NBayes_pred = predict(NBayes, newdata = validation_set_clean[-1], type = 'class')
end.time <- Sys.time()
time.takenNB <- end.time - start.time

NBayes_cm = table(t(validation_set_clean[, 1]), NBayes_pred)

NBayes_success = sum(diag(NBayes_cm))/sum(NBayes_cm)*100 
NBayes_success_survived = NBayes_cm[2,2]/(NBayes_cm[2,1]+NBayes_cm[2,2])*100 
NBayes_success_notsurvived = NBayes_cm[1,1]/(NBayes_cm[1,2]+NBayes_cm[1,1])*100
NBayes_cm

CrossTable(x = t(validation_set_clean[, 1]), y = NBayes_pred, prop.chisq=FALSE)
