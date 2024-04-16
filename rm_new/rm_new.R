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

remove_element <- function(lst, idx) {
  lst[idx] <- NULL
  return(lst)
}

factor_to_dataframe <- function(f) {
  col_names <- levels(f)
  r <- matrix(F, ncol = length(col_names), nrow = length(f), 
              dimnames = list(NULL, col_names)) %>% as.data.frame()
  sapply(1:length(f), FUN = function(x){
    for(i in 1:length(col_names)) {
      if(f[x] == col_names[i]) {
        r[x,i] <<- TRUE; break
      }
    }
  })
  return(r)
}

mcc_custom <- function(actual, predicted) {
  df_actual <- factor_to_dataframe(actual)
  df_predicted <- factor_to_dataframe(predicted)
  
  MCC <- mltools::mcc(preds = df_predicted, actuals = df_actual)
  return(MCC)
}

lambda_class <- function(loss_vector) {
  lambda <- (log(loss_vector) / (1 - loss_vector))/ 
               (sum(log(loss_vector) / (1 - loss_vector)))
}

#USAR UMCC
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

gen_cv <- function(data, Kfolds, ...){
  size_Kfolds <- nrow(data)/Kfolds
  idx <- 1
  result <- lapply(1:Kfolds, FUN=function(k) {
    idx_end <- k + size_Kfolds - 1
    full_out <- data[idx:idx_end,]
  })
  return(result)
}

gen_cv_data <- function(data, folds, label_variable){
  #separating label indexes to guarantee balanced folds
  fold_n <- ceiling(nrow(data)/folds)
  size_folds <- numeric(folds)
  fold_indexes <- vector(mode = "list", length = folds)
  classes <- levels(data[, label_variable])
  classes_indexes <- vector(mode = "list", 
                            length = length(classes))
  for(i in 1:length(classes)){
    class_indexes <- which(data[, label_variable] == classes[i])
    classes_indexes[[i]] <- split(class_indexes, cut(seq_along(class_indexes),
                                                     folds, labels = FALSE))
    sapply(X = 1:folds, function(x){
      fold_indexes[[x]] <<- rbind(fold_indexes[[x]], 
                                  data[classes_indexes[[i]][[x]],])
    })
  }
  return(fold_indexes)
}

rm_multiclass <- function(formula,
                          x_train,
                          x_valid = NULL,
                          kernels = list(vanilladot(), polydot(2), rbfdot(1), laplacedot(1)),
                          B = 25,
                          C = 1,
                          epsilon = 0.1,
                          beta = 2,
                          K = 5,
                          loss_function = mcc_custom)
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
  
  folds <- gen_cv_data(validation_set, K, 'Species')
  
  result <- sapply(1:length(models), FUN = function(i) {
    sapply(1:K, FUN = function(k){
      f_test <- folds[[k]]
      f_train <- do.call(rbind, remove_element(folds, k))
      f_fit <- kernlab::ksvm(form, data = f_train, kernel = kernels[[i]])
      f_hat <- kernlab::predict(f_fit, f_test)
      return(do.call(loss_function, list(actual = f_test[, target], 
                                             predicted = f_hat)))
    })
  })
  
  lambdas <- omega_exp(colMeans(result), beta)
  print(lambdas)
  
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
  attr(ensemble_model, "class") <- "rm_class2"
  return(ensemble_model)
}

rm_class2 <- setClass("rm_class2",
  slots = list(train = "data.frame",
               target = "character",
              kernel_lambdas = "numeric",
              weights = "numeric",
              bs_samples = "list",
              bs_models = "list")
)
predict.rm_class2 <- function(rm_model, newdata){
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
