setwd('C:\\Users\\criss\\OneDrive\\Documentos\\Projetos\\PIBIC\\IC_Random-Machines')
library(tidyverse)

source('rm_new.R')

kernels = list(vanilladot(), rbfdot(1), laplacedot())
n <- 100

iris %>% 
  ggplot(aes(x=Sepal.Width, y=Sepal.Length, colour=Species)) +
    geom_point()

accs <- data.frame(random_machines = numeric(n),
             svm_vanilla = numeric(n),
             svm_rbf = numeric(n),
             svm_laplace = numeric(n))

for(i in 1:n) {
  idx <- sample(1:nrow(iris), 120)
  
  x_treino <- iris[idx,]
  x_teste <- iris[-idx,]
  
  ajuste_rm <- rm_multiclass(Species~., x_train=x_treino, B=25, C=1, beta=2)
  accs[i,1] <- predict(ajuste_rm, newdata=x_teste) %>% 
    Metrics::accuracy(actual = x_teste$Species)

  ajuste_vanilla <- kernlab::ksvm(Species~., data=x_treino, C=1, kernel=kernels[[1]])
  accs[i,2] <- predict(ajuste_vanilla, newdata=x_teste) %>% 
    Metrics::accuracy(actual = x_teste$Species)
  
  ajuste_rbf <- kernlab::ksvm(Species~., data=x_treino, C=1, kernel=kernels[[2]])
  accs[i,3] <- predict(ajuste_rbf, newdata=x_teste) %>% 
    Metrics::accuracy(actual = x_teste$Species)
  
  ajuste_laplace <- kernlab::ksvm(Species~., data=x_treino, C=1, kernel=kernels[[3]])
  accs[i,4] <- predict(ajuste_laplace, newdata=x_teste) %>% 
    Metrics::accuracy(actual = x_teste$Species)

  print(i)
}

accs
boxplot(accs)


# RANDOM-MACHINES VS. RM_NEW

t1 <- Sys.time()
ajuste <- rm_multiclass(Species~., x_train=iris, B=25, C=1, beta=2)
t2 <- Sys.time()
t2-t1

library(randomMachines)
t3 <- Sys.time()
ajuste <- randomMachines(Species~., train=iris, B=25, cost=1, beta=2)
t4 <- Sys.time()

t4-t3
