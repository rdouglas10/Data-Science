library(readr)
iris <- read_csv("Desktop/Mestrado PPGMCC 2017:2/3 Exploração e mineração de dados/Trabalhos/2 - Trabalho/Iris.csv")
iris <- iris[-1]

View(iris)

data("iris")

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Visualizando os dados:

library(tidyverse)

iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))+
  geom_point()

iris %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width, colour = Species))+
  geom_point()

# Construindo função normalizar:
normalizar <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# aplicando a função normalizar para as variáveis numéricas
iris_norm <- as.data.frame(lapply(iris[1:4], normalizar))

# Obtendo um resumo do novo dataset
summary(iris_norm)

# Construindo um indicador para amostra aleatória:
set.seed(1234)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Construindo o conjunto de treinamento:
iris.treinamento <- iris[ind==1, 1:4]

# Construindo o conjunto de treinamento:
iris.test <- iris[ind==2, 1:4]

# Inspecionando os conjuntos:
head(iris.treinamento)
head(iris.test)

# Criando o vetor com os nomes(labels) para o conjunto treinamento
iris.trainLabels <- iris[ind==1, 5]

# Criando o vetor com os nomes(labels) para o conjunto teste
iris.testLabels <- iris[ind==2, 5]

# Inspecionando o resultado
print(iris.trainLabels)
print(iris.testLabels)

# Instale e carregue o pacote class:
# install.packages("class")
library(class)

# Veja mais detalhes sobre a função knn:
?knn

# Construindo o modelo
iris_pred <- knn(train = iris.treinamento, test = iris.test, cl = iris.trainLabels, k=3)

# Inspecionando `iris_pred`
iris_pred

# Instale e carregue o pacote gmodels:
# install.packages("gmodels")
library(gmodels)

# Construindo a tabela:
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)

View(iris.testLabels)
View(iris_pred)

