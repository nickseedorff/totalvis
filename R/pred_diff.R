#' Evaluate difference in predictions for imputing an entire PC vs
#' taking a step in the PC direction for a single feature
#' @param x Object of type "diagvis"


pred_diff <- function(x) UseMethod("pred_diff")

pred_diff.reg <-
function(object) {
  
  ## Unlist object
  features <- object$features
  unique_val <- object$unique_val
  location <- object$location
  model <- object$model
  overall_pred <- object$overall_pred
  pca_object <- object$pca_object
  
  ## Length of output vectorsF
  len_out <- length(unique_val)
  
  ## Vector to store prediction
  pred_load_diff <- function(feature) {
    pred_vec <- rep(0, length(unique_val))
    for (i in 2:(length(unique_val))) {
      
      ## Define matrices
      mat_tmp_old <- pca_object$x
      mat_tmp_new <- pca_object$x
      
      ## Old and new PC values
      mat_tmp_old[, location] <- unique_val[i - 1]
      mat_tmp_new[, location] <- unique_val[i]
      
      ## New and old original matrices
      dat_temp_old <- rev_pca(data = mat_tmp_old, pca_object = pca_object)
      dat_temp_new <- rev_pca(data = mat_tmp_new, pca_object = pca_object)
      
      ## Use the old matrix except the updated value for the feature of interest
      loc_load <- which(colnames(dat_temp_new) == feature)
      dat_temp_old[, loc_load] <- dat_temp_new[, loc_load]
      
      ## Default predict with df, specific predict types for gbm and xgboost
      if(length(intersect(class(model), c("gbm", "xgb.Booster"))) == 0) {
        res <- try(mean(predict(model, as.data.frame(dat_temp_old))), 
                   silent = TRUE)
      } else if ("gbm" %in% class(model)) {
        res <- mean(predict(model, as.data.frame(dat_temp_old), 
                            n.trees = model$n.trees))
      } else if ("xgb.Booster" %in% class(model)) {
        res <- mean(predict(model, dat_temp_old))
      }
      
      ## Stop if incorrect object type
      if (!is.numeric(res)) stop("Unrecognized object (model) type")
      
      ## Predict numeric outcomes, most object types allow a DF
      pred_vec[i] <- res - overall_pred[i]
    }
    pred_vec
  }
  vapply(features, pred_load_diff, FUN.VALUE = numeric(len_out))
}


pred_diff.cla <-
  function(object) {
    
    ## Unlist object
    features <- object$features
    unique_val <- object$unique_val
    location <- object$location
    model <- object$model
    overall_pred <- object$overall_pred
    pca_object <- object$pca_object
    
    ## Length of output vectors
    len_out <- length(unique_val)
    
    ## Vector to store prediction
    pred_load_diff <- function(feature) {
      pred_vec <- rep(0, length(unique_val))
      for (i in 2:(length(unique_val))) {
        
        ## Define matrices
        mat_tmp_old <- pca_object$x
        mat_tmp_new <- pca_object$x
        
        ## Old and new PC values
        mat_tmp_old[, location] <- unique_val[i - 1]
        mat_tmp_new[, location] <- unique_val[i]
        
        ## New and old original matrices
        dat_temp_old <- rev_pca(data = mat_tmp_old, pca_object = pca_object)
        dat_temp_new <- rev_pca(data = mat_tmp_new, pca_object = pca_object)
        
        ## Use the old matrix except the updated value for the feature of interest
        loc_load <- which(colnames(dat_temp_new) == feature)
        dat_temp_old[, loc_load] <- dat_temp_new[, loc_load]
        
        
        ## Default predict with df, specific predict types for gbm and xgboost
        if(length(intersect(class(model), c("gbm", "xgb.Booster", "lm"))) == 0){
          res <- try(mean(predict(model, as.data.frame(dat_temp_old), 
                                  type = "prob")[, 2]), silent = TRUE)
        } else if ("lm" %in% class(model)) {
          res <- mean(predict(model, as.data.frame(dat_temp_old), 
                              type = "response"))
        } else if ("gbm" %in% class(model)) {
          res <- mean(predict(model, as.data.frame(dat_temp_old), 
                              n.trees = model$n.trees)[, 2])
        } else if ("xgb.Booster" %in% class(model)) {
          res <- mean(predict(model, dat_temp_old, type = "prob")[, 2])
        }
        
        ## Stop if incorrect object type
        if (!is.numeric(res)) stop("Unrecognized object (model) type")
        
        ## Predict numeric outcomes, most object types allow a DF
        pred_vec[i] <- res - overall_pred[i]
      }
      pred_vec
    }
    vapply(features, pred_load_diff, FUN.VALUE = numeric(len_out))
  }
