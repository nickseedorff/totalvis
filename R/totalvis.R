#' Creat an object for visiualing pdp plots for a principal component
#' @param model A fitted model object of an appropriate class
#' @param data Dataframe of matrix of numeric type (Note: Excludes the outcome 
#' or the outcome is specified with'outcome')
#' @param type Outcome type, either 'classification' (binary) or 'regression'
#' @param location Integer of principal component to visualize
#' @param samp_size Number of unique PC values marginalize and plot
#' @param feature Feature to plot a partial dependence plot for
#' @param pin Feature to pin pdp plot to over a range of a principal component
#' @return A pdp style plot of the specified principal component
#' @examples
#' @import graphics
#' @import stats 
#' @export


totalvis <-
function(model, data, type = "regression", location = 1, samp_size = 100,
         feature = NULL, pin = NULL) {
  
  ## Ensure location is an integer
  if ((!is.numeric(location) | length(location) != 1) & !is.null(location)){
    stop("Invalid location or input type")
  }
  
  ## Warning of location and feature are supplied
  if (!is.null(location) & !is.null(feature)) {
    warning("Feature input overrides location. Plotting this object will return
             a standard partial depedence plot.")
  }
  
  ## convert to a matrix if input is a dataframe
  if (class(data) != "matrix") {
    mat <- as.matrix(data)
  } else {
    mat <- data
  }
  
  ## Decompose the matrix, keep unique values for the feature
  if (is.null(feature)) {
    pca_dat <- prcomp(mat, center = TRUE, scale = TRUE)  
    mat_pca <- pca_dat$x
    unique_val <- unique(mat_pca[, location])
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
    class_use <- paste0("pin", c(substring(type, 1, 3)))
  } else {
    class_use <- c(substring(type, 1, 3))
  }
  
  ## Apply pdp function for regression or classification
  pred_obj <- structure(list(unique_val = unique_val, location = location, 
  													 model = model, pca_object = pca_dat, 
  													 feature = feature, data = mat, pin = pin), 
  											class = class_use)
  pred_vec <- pred_val(pred_obj)
  
  ## Store prediction results in a dataframe and sort by unique_val
  if (is.null(pin)) {
    pred_df <- data.frame(avg_pred = pred_vec,x_vals = unique_val)
    pred_df <- pred_df[order(pred_df$x_vals), ]
  } else {
  	pred_df <- t(pred_vec)
  	colnames(pred_df) <- c("avg_pred", "pin_mean", "pin_median")
  	pred_df <- pred_df[order(pred_df$pin_mean), ] 
  }

  ## Return a totalvis or featvis object
  if (is.null(feature) & is.null(pin)) {
    structure(list(pred_df = pred_df, location = location, 
    							 pca_object = pca_dat), class = "totalvis")
  } else if (is.null(pin)){
    structure(list(pred_df = pred_df, feature = feature,
                   data = mat), class = "featvis")
  } else {
    structure(list(pred_df = pred_df, pin = pin, location = location), 
              class = "pinvis")
  }
}
