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

pred_val.pc <- 
function(object) {
	
  ## Unlist object
  unique_val <- object$unique_val
  pc_num <- object$pc_num
  model <- object$model
  pca_object <- object$pca_object
  feature <- object$feature
  data <- object$data
  type <- object$type
  
  pred_func <- function(value) {
 
		## pc_num calculates the average over a PC, feature does it over a feature
		if (is.null(feature)) {
			mat_tmp <- pca_object$x
			mat_tmp[, pc_num] <- value
			mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)
		} else {
			mat_new <- data
			mat_new[, feature] = value
		}
    
    ## Get predictions
    if(type == "regression") {
      regression_preds(model, mat_new)
    } else {
      classification_preds(model, mat_new)
    }
  }
	vapply(unique_val, pred_func, FUN.VALUE = numeric(1))
}


###############################################################################
# Pin features
###############################################################################

pred_val.pin <- 
  function(object) {
  
  ## Unlist object
  unique_val <- object$unique_val
  pc_num <- object$pc_num
  model <- object$model
  pca_object <- object$pca_object
  pin <- object$pin
  type <- object$type

  pred_func <- function(value) {
    
    ## pc_num calculates the average over a PC, feature does it over a feature
    mat_tmp <- pca_object$x
    mat_tmp[, pc_num] <- value
    mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)

    ## Get predictions, as well as mean and median of feature
    if(type == "regression") {
      res <- regression_preds(model, mat_new)
    } else {
      res <- classification_preds(model, mat_new)
    }
    c(res, mean(mat_new[, pin]), median(mat_new[, pin]))
    
  }
  vapply(unique_val, pred_func, FUN.VALUE = numeric(3))
}

###############################################################################
# Ice plots
###############################################################################

pred_val.ice <- 
function(object) {
  
  ## Unlist object
  unique_val <- object$unique_val
  pc_num <- object$pc_num
  model <- object$model
  pca_object <- object$pca_object
  feature <- object$feature
  data <- object$data
  type <- object$type
  
  pred_func <- function(value) {
    
    ## pc_num calculates the average over a PC, feature does it over a feature
    if (is.null(feature)) {
      mat_tmp <- pca_object$x
      mat_tmp[, pc_num] <- value
      mat_new <- rev_pca(data = mat_tmp, pca_object = pca_object)
    } else {
      mat_new <- data
      mat_new[, feature] = value
    }

    ## Get predictions
    if(type == "regression") {
      regression_preds(model, mat_new)
    } else {
      classification_preds(model, mat_new)
    }
  }
  vapply(unique_val, pred_func, FUN.VALUE = numeric(nrow(data)))
}

#' Control flow for regression predictions
#' @param model model object to be used for prediction
#' @param X matrix to be used in predictions

regression_preds <- function(model, X) {
  if(length(intersect(class(model), c("gbm", "xgb.Booster"))) == 0) {
    res <- try(mean(predict(model, as.data.frame(X))), 
               silent = TRUE)
  } else if ("gbm" %in% class(model)) {
    res <- mean(predict(model, as.data.frame(X), 
                        n.trees = model$n.trees))
  } else if ("xgb.Booster" %in% class(model)) {
    res <- mean(predict(model, X))
  }
  
  ## Stop if incorrect object type
  if (!is.numeric(res)) stop("Unrecognized object (model) type")
  res
}

#' Control flow for classification predictions
#' @param model model object to be used for prediction
#' @param X matrix to be used in predictions

classification_preds <- function(model, X) {
  diff_mods <- c("gbm", "xgb.Booster", "lm", "MLModelFit", "svm")
  if(length(intersect(class(model), diff_mods)) == 0) {
    res <- try(mean(as.numeric(predict(model, as.data.frame(X), 
                                       type = "prob")[, 2])), silent = TRUE)
  } else if ("MLModelFit" %in% class(model)) {
    res <- mean(predict(model, X, type = "prob"))
  } else if ("svm" %in% class(model)) {
    res <- mean(attr(predict(model, X, probability = TRUE), 
                     "probabilities")[, 2])
  } else if ("lm" %in% class(model)) {
    res <- mean(predict(model, as.data.frame(X), type = "response"))
  } else if ("gbm" %in% class(model)) {
    res <- mean(predict(model, as.data.frame(X), 
                        n.trees = model$n.trees, type = "response"))
  } else if ("xgb.Booster" %in% class(model)) {
    res <- mean(predict(model, X))
  }
  
  ## Stop if incorrect object type
  if (!is.numeric(res)) stop("Unrecognized object (model) type")
  res
}


#' Test number of level used for predictions, warn if regression was chosen for 
#' a classification
#' @param model model object to be used for prediction
#' @param X matrix to be used in predictions

check_regression_preds <- function(model, X) {
  if(length(intersect(class(model), c("gbm", "xgb.Booster"))) == 0) {
    res <- try(predict(model, as.data.frame(X)), 
               silent = TRUE)
  } else if ("gbm" %in% class(model)) {
    res <- predict(model, as.data.frame(X), n.trees = model$n.trees)
  } else if ("xgb.Booster" %in% class(model)) {
    res <- predict(model, X)
  }
  
  ## Stop if incorrect object type
  if (length(unique(res)) <= 2) {
    warning("2 or fewer unique predicted values, 
             should type = 'classifcation'?", call. = FALSE, immediate. = TRUE)
  }
}
