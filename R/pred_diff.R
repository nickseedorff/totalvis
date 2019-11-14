#' Evaluate difference in predictions for imputing an entire PC vs
#' taking a step in the PC direction for a single feature
#' @param object a preddiff object passed through the partial_effects function 

pred_diff <-
  function(object) {
    
    ## Unlist object
    features <- object$features
    unique_val <- object$unique_val
    pc_num <- object$pc_num
    model <- object$model
    overall_pred <- object$overall_pred
    pca_object <- object$pca_object
    type <- object$type
    
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
        mat_tmp_old[, pc_num] <- unique_val[i - 1]
        mat_tmp_new[, pc_num] <- unique_val[i]
        
        ## New and old original matrices
        dat_temp_old <- rev_pca(data = mat_tmp_old, pca_object = pca_object)
        dat_temp_new <- rev_pca(data = mat_tmp_new, pca_object = pca_object)
        
        ## Use the old matrix except the updated value for the feature of interest
        loc_load <- which(colnames(dat_temp_new) == feature)
        dat_temp_old[, loc_load] <- dat_temp_new[, loc_load]
        
        ## Get predictions
        if(type == "regression") {
          res <- regression_preds(model, dat_temp_old)
        } else {
          res <- classification_preds(model, dat_temp_old)
        }
        
        ## Predict numeric outcomes, most object types allow a DF
        pred_vec[i] <- res - overall_pred[i]
      }
      pred_vec
    }
    vapply(features, pred_load_diff, FUN.VALUE = numeric(len_out))
  }
