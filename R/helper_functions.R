#' Inverse of the PCA transformation, including centering and scaling
#' @param data Dataframe or matrix of same dim as original data
#' @param pca_object A prcomp object

rev_pca <-
	function(data, pca_object) {
		t(tcrossprod(pca_object$rotation, data) * pca_object$scale 
			+ pca_object$center)
	}

#' Calculate the average prediction after imputing the value for a column 
#' @param x A prediction object

pred_val <- function(x) UseMethod("pred_val")

pred_val.reg <- 
function(object) {
	
  ## Unlist object
  unique_val <- object$unique_val
  location <- object$location
  model <- object$model
  pca_object <- object$pca_object
  feature <- object$feature
  data <- object$data
  
  pred_func <- function(value) {
 
		## Location calculates the average over a PC, feature does it over a feature
		if (is.null(feature)) {
			mat_tmp <- pca_object$x
			mat_tmp[, location] <- value
			mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)
		} else {
			mat_new <- data
			mat_new[, feature] = value
		}
		
    ## Default predict with df, specific predict types for gbm and xgboost
    if(length(intersect(class(model), c("gbm", "xgb.Booster"))) == 0) {
      res <- try(mean(predict(model, as.data.frame(mat_new))), 
                 silent = TRUE)
    } else if ("gbm" %in% class(model)) {
      res <- mean(predict(model, as.data.frame(mat_new), 
                          n.trees = model$n.trees))
    } else if ("xgb.Booster" %in% class(model)) {
      res <- mean(predict(model, mat_new))
    }
		
		## Stop if incorrect object type
		if (!is.numeric(res)) stop("Unrecognized object (model) type")
		res
  }
	vapply(unique_val, pred_func, FUN.VALUE = numeric(1))
}


pred_val.cla <- 
function(object) {
	
  ## Unlist object
  unique_val <- object$unique_val
  location <- object$location
  model <- object$model
  pca_object <- object$pca_object
  feature <- object$feature
  data <- object$data
  
	pred_func <- function(value) {
		
		## Location calculates the average over a PC, feature does it over a feature
		if (is.null(feature)) {
			mat_tmp <- pca_object$x
			mat_tmp[, location] <- value
			mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)
		} else {
			mat_new <- data
			mat_new[, feature] = value
		}
		
		## Default predict
		res <- try(mean(predict(model, as.data.frame(mat_new), type = "prob")[, 2]),
		           silent = TRUE)
		
		## Default predict with df, specific predict types for gbm and xgboost
		if(length(intersect(class(model), c("gbm", "xgb.Booster", "lm"))) == 0) {
		  res <- try(mean(predict(model, as.data.frame(mat_new), 
		                          type = "prob")[, 2]), silent = TRUE)
		} else if ("lm" %in% class(model)) {
		  res <- mean(predict(model, as.data.frame(mat_new), type = "response"))
		} else if ("gbm" %in% class(model)) {
		  res <- mean(predict(model, as.data.frame(mat_new), 
		                      n.trees = model$n.trees)[, 2])
		} else if ("xgb.Booster" %in% class(model)) {
		  res <- mean(predict(model, mat_new, type = "prob")[, 2])
		}
		
		## Stop if incorrect object type
		if (!is.numeric(res)) stop("Unrecognized object (model) type")
		res
	}
	vapply(unique_val, pred_func, FUN.VALUE = numeric(1))
}

pred_val.pinreg <- 
  function(object) {
  
  ## Unlist object
  unique_val <- object$unique_val
  location <- object$location
  model <- object$model
  pca_object <- object$pca_object
  pin <- object$pin

  pred_func <- function(value) {
    
    ## Location calculates the average over a PC, feature does it over a feature
    mat_tmp <- pca_object$x
    mat_tmp[, location] <- value
    mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)

    ## Default predict with df, specific predict types for gbm and xgboost
    if(length(intersect(class(model), c("gbm", "xgb.Booster"))) == 0) {
      res <- try(mean(predict(model, as.data.frame(mat_new))), 
                 silent = TRUE)
    } else if ("gbm" %in% class(model)) {
      res <- mean(predict(model, as.data.frame(mat_new), 
                          n.trees = model$n.trees))
    } else if ("xgb.Booster" %in% class(model)) {
      res <- mean(predict(model, mat_new))
    }
    
    ## Stop if incorrect object type
    if (!is.numeric(res)) stop("Unrecognized object (model) type")
    c(res, mean(mat_new[, pin]), median(mat_new[, pin]))
  }
  vapply(unique_val, pred_func, FUN.VALUE = numeric(3))
}

pred_val.pincla <- 
function(object) {
  
  ## Unlist object
  unique_val <- object$unique_val
  location <- object$location
  model <- object$model
  pca_object <- object$pca_object
  pin <- object$pin
  
  pred_func <- function(value) {
    
    ## Location calculates the average over a PC, feature does it over a feature
    mat_tmp <- pca_object$x
    mat_tmp[, location] <- value
    mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)
    
    ## Default predict
    res <- try(mean(predict(model, as.data.frame(mat_new), type = "prob")[, 2]),
               silent = TRUE)
    
    ## Default predict with df, specific predict types for gbm and xgboost
    if(length(intersect(class(model), c("gbm", "xgb.Booster", "lm"))) == 0) {
      res <- try(mean(predict(model, as.data.frame(mat_new), 
                              type = "prob")[, 2]), silent = TRUE)
    } else if ("lm" %in% class(model)) {
      res <- mean(predict(model, as.data.frame(mat_new), type = "response"))
    } else if ("gbm" %in% class(model)) {
      res <- mean(predict(model, as.data.frame(mat_new), 
                          n.trees = model$n.trees)[, 2])
    } else if ("xgb.Booster" %in% class(model)) {
      res <- mean(predict(model, mat_new, type = "prob")[, 2])
    }
    
    ## Stop if incorrect object type
    if (!is.numeric(res)) stop("Unrecognized object (model) type")
    c(res, mean(mat_new[, pin]), median(mat_new[, pin]))
  }
  vapply(unique_val, pred_func, FUN.VALUE = numeric(3))
}