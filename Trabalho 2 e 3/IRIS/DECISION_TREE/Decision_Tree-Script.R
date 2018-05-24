  set.seed(1234)
  data(iris)
  ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
  trainData <- iris[ind == 1, ]
  testData <- iris[ind == 2, ]
  
  library(party)
  myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
  iris_ctree <- ctree(myFormula, data = trainData)
  
  table(predict(iris_ctree), trainData$Species)
  
  library(gmodels)
  CrossTable(predict(iris_ctree),trainData$Species,prop.chisq = FALSE, prop.t = FALSE, 
             prop.r = FALSE, dnn = c('predicted', 'actual'))
  
  
  plot(iris_ctree, type = "simple")
  
