#' Diagnostic plot for assessing effects of individual features on a principal 
#' component
#' @param model A fitted model object to visualize
#' @param X Design matrix that the object was trained on
#' @param type Outcome type, either 'classification' (binary) or 'regression'
#' @param pc_num Principal component to impute
#' @param samp_size Number of steps to take in PC direction
#' @param num_load Number of top loadings to plot
#' @export

partial_effects <-
function(model, X, pc_num = 1, samp_size = 20, num_load = 5, 
         type = "regression") {
  
  if (!is.numeric(pc_num) | length(pc_num) != 1) {
    stop("Invalid pc_num or input type")
  }
  
  ## convert to a matrix if input is a dataframe
  if (!class(X) %in% c("matrix", "data.frame")) {
    stop("Input data must be a numeric matrix of data.frame of numeric columns")
  } else if (class(X) == "data.frame") {
    if(!all(apply(X, 2, is.numeric))) {
      stop("Could not convert input to a numeric matrix.")
    }
    mat <- as.matrix(X)
  } else {
    mat <- X
  }
  
  ## Check unique predictions values, warn if type may need to be changed
  if (type == "regression") check_regression_preds(model, X)
  
  ## Select minimum of number of feates and num_load
  num_load <- min(num_load, ncol(mat))
  
  ## Decompose the matrix
  pca_dat <- prcomp(mat, center = TRUE, scale = TRUE)  
  mat_pca <- pca_dat$x
  
  ## Get unique values for chosen feature, get quantiles
  unique_val <- unique(mat_pca[, pc_num])
  
  ## Ensure samp_size is not larger than the number of unique values
  if (length(unique_val) < samp_size) {
    samp_size = unique_val
  }
  
  ## Select a sample of the unique values to plot
  unique_val <- seq(min(unique_val), max(unique_val), length.out = samp_size)
  
  ## Top loadings to plot against
  loading_ord <- order(abs(pca_dat$rotation[, pc_num]), decreasing = T)
  top_loads <- pca_dat$rotation[loading_ord[1:num_load], pc_num, drop = FALSE]
  
  ## Apply pdp function for regression or classification
  pred_obj <- structure(list(unique_val = unique_val, pc_num = pc_num, 
  													 model = model, pca_object = pca_dat, 
  													 feature = NULL, type = type), 
  											class = "pc")
  pred_vec <- pred_val(pred_obj)
  
  
  ## Apply pdp function over individual features
  pred_obj <- structure(list(features = rownames(top_loads), 
                             unique_val = unique_val, pc_num = pc_num, 
                             model = model, pca_object = pca_dat, 
                             overall_pred = pred_vec, type = type), 
                        class = "preddiff")
  pred_diff_mat <- pred_diff(pred_obj)
  
  ## Return partialvis object
  structure(list(pred_mat = pred_diff_mat, x_vec = unique_val, 
                 pc_num = pc_num, pca_object = pca_dat,
                 load_names = rownames(top_loads)), 
            class = "partialvis")
}
