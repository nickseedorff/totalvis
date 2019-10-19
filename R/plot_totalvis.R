#' Generate a partial depedence plot for a specified principal component 
#' @param x A totalvis.object returned from a call to totalvis
#' @param num_load Number of loading to include in the legend
#' @param return_res Logical, return a dataframe with info about the top loadings
#' @param legend_loc Location of the legend, guesses either topleft or topright
#' @param rug Adds a rug representation of the principal component
#' @param ... Additional optional arguments to be passed to plot, accepts xlab or main as arguments
#' @export

plot.totalvis <-
function(x, num_load = 5, return_res = TRUE, rug = TRUE, 
         legend_loc = NULL, ...) {

  ## Unlist object
  pred_df <- x$pred_df
  pc_num <- x$pc_num
  pca_object <- x$pca_object
  
  ## Default plotting values
  optionals <- list(...)
  defaults <- list(xlab = ifelse("xlab" %in% names(optionals), 
                                 optionals[["xlab"]], paste0("PC", pc_num)),
                   main = ifelse("main" %in% names(optionals), 
                                 optionals[["main"]], 
                                 paste0("PDP Plot of PC", pc_num)))
  
	## Ensure num_loads is less than the number of covariates
	num_load = min(num_load, length(pca_object$sdev))
  
  ## Plot PC value by average prediction
  plot(pred_df$x_vals, pred_df$avg_pred, type = "l", 
       xlab = defaults[["xlab"]], 
  		 main = defaults[["main"]],
       ylab = "yhat", col = "dodgerblue", lwd = 4)
  
  if (rug) {
    rug(pca_object$x[, pc_num])
    lines(pred_df$x_vals, pred_df$avg_pred, col = "dodgerblue",
          lwd = 4)
  }
  
  ## Placement for legend in plot
  if (is.null(legend_loc)) {
    legend_loc <- ifelse(pred_df$avg_pred[1] <= 
                             pred_df$avg_pred[length(pred_df$avg_pred)], 
                           "topleft", "topright")
  }
  
  ## Names and color for legend in plot
  loading_ord <- order(abs(pca_object$rotation[, pc_num]), decreasing = T)
  top_loads <- pca_object$rotation[loading_ord[1:num_load], pc_num, 
                                   drop = FALSE]
  load_df <- data.frame(name = rownames(top_loads), 
                        symbol = ifelse(top_loads[, 1] > 0, 2, 6))
  load_df$col <- ifelse(top_loads[, 1] > 0, "green", "red")
  
  ## Add legend to plot
  legend(legend_loc, legend = load_df$name, pch = load_df$symbol, 
         col = load_df$col, cex = 0.8, title = "Primary Features")
  
  if (return_res) {
    data.frame(feature_name = as.character(load_df$name), 
               loading_value = as.numeric(top_loads), stringsAsFactors = FALSE)
  }
}
