x <- sort(runif(100,0,10))
y <- 3 + (1)*((x)^2)+rnorm(100, sd=10)

sim_data <- data.frame(X=x,Y=y)

ajuste_lm <- lm(Y~.,data=sim_data)

ajuste_rm <- randomMachines(Y~., train=sim_data)
yh <- predict(ajuste_rm,sim_data)

sim_data %>% 
  ggplot(aes(x=X, y=Y)) +
  geom_point() +
  #geom_abline(intercept=ajuste_lm$coefficients[1], slope=ajuste_lm$coefficients[2], col='red',size=1) +
  geom_line(aes(y=yh), col='blue', size=1) +
  geom_line(aes(y=3+(x^2)), col='green', size=1.2)
