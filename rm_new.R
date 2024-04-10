bootstrap <- function(df) {
  idx <- sample(1:nrow(df), size = nrow(df), replace = TRUE)
  
  bs_sample <- df[idx,]
  oob_sample <- df[-idx,]
  
  return(list(bs_sample = bs_sample, oob_sample = oob_sample))
}

bootstrap_rep <- function(df, B) {
  s <- lapply(1:B, FUN=function(x){bootstrap(df)})
  return(s)
}

lambda_class <- function(loss_vector) {
  log((loss_vector/(1-loss_vector))) / sum(log((loss_vector/(1-loss_vector))))
}

omega_class <- function(loss_vector) {
  1 / ((1 - loss_vector)^2)
}

omega_exp <- function(loss_vector, beta){
  exp(loss_vector*beta)/sum(exp(loss_vector*beta))
}

#encoding 2xn data.frame  with first column as original factor levels and second
#with equivalent numeric value
class_encode <- function(labels, encoding){
  encoded_classes <- sapply(1:length(labels), function(x){
    encoding[which(encoding[,1] == labels[x]),2]
  })
  return(list(encoding = encoding, 
              encoded_classes = as.numeric(encoded_classes)))
}

class_decode <- function(encoding, encoded_classes){
  decoded_classes <- sapply(1:length(encoded_classes), function(x){
    encoding[which(encoding[,2] == encoded_classes[x]),1]
  })
  return(factor(decoded_classes, levels = encoding[,1], ordered = FALSE)) 
}

# IMPORTANDO KERNLAB MANUALMENTE:
require(kernlab)
rm_multiclass <- function(formula,
                          x_train,
                          x_valid = NULL,
                          kernels = list(vanilladot(), polydot(2), rbfdot(1), laplacedot(1)),
                          B = 25,
                          C = 1,
                          epsilon = 0.1,
                          beta = 2,
                          loss_function = Metrics::accuracy)
{

  if(is.null(x_valid)){ validation_set <- x_train } else { validation_set <- x_valid }

  if(class(formula) == "character"){
    form <- as.formula(formula) 
  }else if(class(formula) == "formula"){
    form <- formula
  }else{
    stop("Specified formula is not of a valid class.")
  }
  target <- as.character(form)[2]

  models <- lapply(kernels, function(k) {
    kernlab::ksvm(form, kernel=k, data=x_train, C=C, epsilon = epsilon)
  })

  loss_step1 <- sapply(1:length(models), FUN=function(x){ 
    target_hat <- kernlab::predict(models[[x]], validation_set)
    do.call(loss_function, list(actual = validation_set[, target], 
                                predicted = target_hat))
  })
  lambdas <- omega_exp(loss_step1, beta)
  
  bs_kernels <- sample(1:length(kernels), B, prob = lambdas, replace = TRUE)
  bs_samples <- bootstrap_rep(x_train, B)

  bs_models <- lapply(1:B, function(b){
    kernlab::ksvm(form, data = bs_samples[[b]]$bs_sample, 
                  kernel = kernels[[bs_kernels[b]]], C=C, epsilon=epsilon)
  })

  loss_bs <- sapply(1:B, FUN=function(b){
    bs_fit <- kernlab::predict(bs_models[[b]], bs_samples[[b]]$oob_sample)
    return(do.call(Metrics::accuracy, 
                   list(actual = bs_samples[[b]]$oob_sample[, target],
                        predicted = bs_fit)))
  })
  
  ensemble_model <- list(train = x_train,
                         target = target,
                         kernel_lambdas = lambdas,
                         bs_samples = bs_samples,
                         bs_models = bs_models,
                         weights = omega_exp(loss_bs, beta))
  attr(ensemble_model, "class") <- "rm_class"
  return(ensemble_model)
}

rm_class <- setClass("rm_class",
  slots = list(train = "data.frame",
               target = "character",
              kernel_lambdas = "numeric",
              weights = "numeric",
              bs_samples = "list",
              bs_models = "list")
)
predict.rm_class <- function(rm_model, newdata){
  labels <- rm_model$train[, rm_model$target]
  encoding_df <- data.frame(x = levels(labels), y = 1:length(levels(labels)))

  rm_models <- rm_model$bs_models
  encoded_predictions <- sapply(X = 1:length(rm_models), function(x){
    prd <- kernlab::predict(rm_models[[x]], newdata)
    prd_encode <- class_encode(prd, encoding = encoding_df)[[2]]
    return(prd_encode)
  })
  final <- sapply(1:nrow(encoded_predictions), function(x){
    round(sum(encoded_predictions[x, ]*rm_model$weights), 0)
  })
  final <- class_decode(encoding_df, final)
  return(final)
}
