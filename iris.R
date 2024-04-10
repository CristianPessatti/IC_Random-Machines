library(randomMachines)
library(caret)
require(tidyverse)

iris_b <- iris %>% 
  filter(Species!='virginica') %>% 
  mutate(Species = as.integer(Species)-1)

as.integer(iris_b$Species)

indices <- unlist(createDataPartition(iris_b$Sepal.Length, p=0.8))

treino <- iris_b[indices,]
teste <- iris_b[-indices,]

# Regressão Linear Simples

modelo_linear <- lm(Sepal.Length~., data = treino)
sep_len_hat <- predict(modelo_linear, teste)

sum((teste$Sepal.Length - sep_len_hat)^2)

# Random Machines

modelo_rm <- randomMachines(Sepal.Length~., train = treino)
sep_len_pred <- predict(modelo_rm, teste)

sum((teste$Sepal.Length - sep_len_pred)^2)

# CLASSIFICAÇÃO DE ESPÉCIE

modeloc_rm <- randomMachines(Species~., train=treino, prob_model = FALSE)
species_pred <- predict(modeloc_rm, teste)
