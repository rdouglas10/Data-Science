library(readr)

iris <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/Iris.csv")

iris <- iris[-1]
colnames(iris)[1:5]=c("sepal_length","sepal_width","petal_length","petal_width","class")
summary(iris)

iris$class=factor(iris$class)

str(iris)

table(iris$class)

sample_iris=sample(150,110,replace = FALSE)

iris_training=iris[sample_iris,]
iris_test=iris[-sample_iris,]

iris_training_labels=iris[sample_iris,]$class
iris_test_labels=iris[-sample_iris,]$class

table(iris_training$class)
table(iris_test$class)

library(e1071)
iris_classifier=naiveBayes(iris_training,iris_training_labels)
iris_test_pred=predict(iris_classifier,iris_test)
iris_test_pred

library(gmodels)
CrossTable(iris_test_pred,iris_test_labels,prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('predicted', 'actual'))
