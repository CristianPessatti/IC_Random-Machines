library(randomMachines)
library(MASS)
library(caret)
require(tidyverse)

data(cats)

# Separando dados de teste e treino
indices <- unlist(createDataPartition(cats$Bwt, p=0.8))

treino <- cats[indices,]
teste <- cats[-indices,]

# Fit com modelo linear simples
fit <- lm(Hwt~Bwt, treino)
coef <- fit$coefficients

predict(fit, teste)

# EQM com modelo linear simples
sum((teste$Hwt - predict(fit, teste))^2)

plot(teste$Bwt, teste$Hwt)
abline(-0.3042894, 4.0189977)

# Fit com máquinas aleatórias
rm_fit <- randomMachines(Hwt~Bwt, train=treino)

predict(rm_fit, teste)
# EQM random machines
sum((teste$Hwt - predict(rm_fit, teste))^2)

# CLASSIFICAÇÃO ENTRE MACHO E FÊMEA

# Regressão Logística

datac <- cats %>% 
  mutate(Sex=as.integer(cats$Sex)-1)

treinoc <- datac[indices,]
testec <- datac[-indices,]

lr_fit <- glm(Sex~., family = 'binomial', data = treinoc)
lr_pred_prob <- predict(lr_fit, testec, type = 'response')

lr_pred_c <- as.integer(lr_pred_prob > 0.5)

confusionMatrix(as.factor(testec$Sex), as.factor(lr_pred_c))

# Random Machines

rm_fit_c <- randomMachines(Sex~., train=treino, prob_model = F)
rm_pred_c <- predict(rm_fit_c, teste)

confusionMatrix(rm_pred_c, teste$Sex)
