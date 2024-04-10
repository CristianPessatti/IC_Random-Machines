setwd('C:\\Users\\criss\\OneDrive\\Documentos\\Projetos\\PIBIC\\IC_Random-Machines')

source('rm_new.R')
beans <- openxlsx::read.xlsx("beandata.xlsx") %>%
  mutate(Class = factor(Class, ordered = FALSE))
#ajuste_rm <- rm_multiclass(Class ~ Petal.Width+Petal.Length, x_train = beans[,c(3:5)])

# tgrid <- expand.grid(seq(0,7,length.out=100),seq(0,3,length.out=100)) %>% as.data.frame()
# colnames(tgrid) <- c('Petal.Length', 'Petal.Width')
# vgrid <- predict(ajuste_rm, tgrid)
# 
# beans %>% 
#   ggplot(aes(x = Petal.Length, y = Petal.Width, colour = Class)) +
#     xlab('Petal Length')+
#     ylab('Petal Width')+
#     geom_point(data=tgrid,aes(x=Petal.Length,y=Petal.Width,colour=vgrid),alpha=0.1)+
#     geom_point()
# 

n = 10
accs_rm <- accs_laplace <- accs_rbf <- accs_vanilla <- numeric(n)
for(i in 1:n) {
  ntrain <- round(0.7 * nrow(beans), 0)
  indices <- sample(1:nrow(beans), ntrain) 
  
  x_treino <- beans[indices,]
  x_teste <- beans[-indices,]
  
  rm_fit <- rm_multiclass(Class~., x_train = x_treino, beta = 10)
  Class_hat <- predict(rm_fit, newdata=x_teste)
  vanilla_hat <- ksvm(Class~., data = x_treino, kernel = vanilladot()) %>% 
    predict(x_teste)
  rbf_hat <- ksvm(Class~., data = x_treino, kernel = rbfdot(1)) %>% 
    predict(x_teste)
  laplace_hat <- ksvm(Class~., data = x_treino, kernel = laplacedot(1)) %>% 
    predict(x_teste)
  accs_rm[i] <- Metrics::accuracy(x_teste$Class, Class_hat)
  accs_vanilla[i] <- Metrics::accuracy(x_teste$Class, vanilla_hat)
  accs_rbf[i] <- Metrics::accuracy(x_teste$Class, rbf_hat)
  accs_laplace[i] <- Metrics::accuracy(x_teste$Class, laplace_hat)
  print(i)
}

ntrain <- round(0.7 * nrow(beans), 0)
indices <- sample(1:nrow(beans), ntrain) 

x_treino <- beans[indices,]
x_teste <- beans[-indices,]

rm_fit <- rm_multiclass(Class~., x_train = x_treino, beta = 2, B=25,
                        kernels = list(rbfdot(1), laplacedot()))
Class_hat <- predict(rm_fit, newdata=x_teste)
Metrics::accuracy(x_teste$Class, Class_hat)

accs <- data.frame(modelo = c(rep('rm',n),rep('vanilla',n),rep('rbf',n),rep('lpc',n)),
                   acc = c(accs_rm, accs_vanilla, accs_rbf, accs_laplace))

accs %>% 
  ggplot(aes(x=modelo, y=acc)) +
    geom_boxplot()
