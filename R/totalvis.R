#' Creat an object for visiualing pdp plots for a principal component
#' @param model A fitted model object of an appropriate class
#' @param X Design matrix that the object was trained on
#' @param type Outcome type, either 'classification' (binary) or 'regression'
#' @param pc_num Integer of principal component to visualize
#' @param samp_size Number of unique PC values marginalize and plot
#' @param feature Feature to plot a partial dependence plot for
#' @param pin Feature to pin pdp plot to over a range of a principal component
#' @param ice Logical, options to include individual expectation curves
#' @return A pdp style plot of the specified principal component
#' @import graphics
#' @import stats 
#' @export


totalvis <-
function(model, X, type = "regression", pc_num = 1, samp_size = 50,
         feature = NULL, pin = NULL, ice = FALSE) {
  
  ## Ensure pc_num is an integer
  if ((!is.numeric(pc_num) | length(pc_num) != 1) & !is.null(pc_num)){
    stop("Invalid pc_num or input type")
  }
  
  ## Warning of pc_num and feature are supplied
  if (!is.null(pc_num) & !is.null(feature) & !ice) {
    message("Feature input overrides pc_num. Plotting this object will return
            a standard partial depedence plot.")
  }
  
  ## Can only create ice curves if feature is includes
  if (is.null(feature) & ice) {
    stop("A feature must be given when ice = TRUE")
  }
  
  ## Can only create ice curves if pin is null
  if ((!is.null(pin) | !is.null(pc_num)) & ice) {
    pin <- NULL
    message("ice = TRUE sets pc_num and pin to NULL")
  }
  
  ## convert to a matrix if input is a dataframe
  if (!class(X) %in% c("matrix", "data.frame")) {
    stop("Input data must be a numeric matrix or data.frame of numeric columns")
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
  
  ## Decompose the matrix, keep unique values for the feature
  if (is.null(feature)) {
    pca_dat <- prcomp(mat, center = TRUE, scale = TRUE)  
    mat_pca <- pca_dat$x
    unique_val <- unique(mat_pca[, pc_num])
  } else {
    pca_dat <- NULL
    unique_val <- unique(mat[, feature])
  }
  
  ## Ensure samp_size is not larger than the number of unique values
  if (length(unique_val) < samp_size) {
    samp_size = unique_val
  }
  
  ## Select a sample of the unique values to plot
  step_size <- 1 / max((samp_size - 1), 1)
  unique_val <- quantile(unique_val, seq(0, 1, step_size))
  
  ## Define class
  if (!is.null(pin)) {
    class_use <- "pin"
  } else {
    class_use <- "pc"
  }
  
  ## Apply pdp function for regression or classification
  pred_obj <- structure(list(unique_val = unique_val, pc_num = pc_num, 
  													 model = model, pca_object = pca_dat, 
  													 feature = feature, data = mat, pin = pin,
  													 type = type), 
  											class = class_use)
  pred_vec <- pred_val(pred_obj)
  
  ## calculate curves for ice plots
  if (ice) {
    class_use <- "ice"
    pred_ice <- structure(list(unique_val = sort(unique_val), 
                               model = model, pca_object = pca_dat,
                               feature = feature, data = mat,
                               type = type), 
                          class = class_use)
    ice_mat <- pred_val(pred_ice)
  }
  
  
  ## Store prediction results in a dataframe and sort by unique_val
  if (is.null(pin)) {
    pred_df <- data.frame(avg_pred = pred_vec, x_vals = unique_val)
    pred_df <- pred_df[order(pred_df$x_vals), ]
  } else {
  	pred_df <- as.data.frame(t(pred_vec))
  	colnames(pred_df) <- c("avg_pred", "pin_mean", "pin_median")
  	pred_df <- pred_df[order(pred_df$pin_mean), ] 
  }

  ## Return various types of objects to plot
  if (is.null(feature) & is.null(pin) & !ice) {
    structure(list(pred_df = pred_df, pc_num = pc_num, 
    							 pca_object = pca_dat), class = "totalvis")
  } else if (ice & (!is.null(feature) | !is.null(pc_num))){
    structure(list(avg_pred = pred_df$avg_pred, xvals = pred_df$x_vals,
                   ice_mat = ice_mat, data = mat, feature = feature), 
              class = "featice")  
  } else if (is.null(pin)){
    structure(list(pred_df = pred_df, feature = feature,
                   data = mat), class = "featvis")
  } else {
    structure(list(pred_df = pred_df, pin = pin, pc_num = pc_num), 
              class = "pinvis")
  } 
}
