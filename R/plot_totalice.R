#' Generate an ICE plots over for a principal component
#' @param x A totalvis.object returned from a call to totalvis
#' @param num_load Number of loading to include in the legend
#' @param legend_loc Location of legend, default of topleft
#' @param center Logical, produced centered ice plots
#' @param rug Adds a rug representation of the principal component
#' @param plot_frac Fraction of individual conditional expectation curves to plot
#' @param ... Additional optional arguments to be passed to plot
#' @export

plot.totalice <-
function(x, num_load = 5, legend_loc = "topleft", center = TRUE, rug = TRUE, 
         plot_frac = 0.1, ...) {

  ## Unlist object
  avg_pred <- x$avg_pred
  xvals <- x$xvals
  location <- x$location
  pca_object <- x$pca_object
  ice_mat <- x$ice_mat
  
	## Ensure num_loads is less than the number of covariates
	num_load = min(num_load, length(pca_object$sdev))
  lines_to_samp <- sample(1:nrow(ice_mat), 
                          size = round(nrow(ice_mat) * plot_frac))
	
  ## unique_value uses location, pdp_mat uses order of locations
	if(center) {
	  ice_mat_plot <- ice_mat[lines_to_samp, ] - ice_mat[lines_to_samp, 1]
	  avg_pred_plot <- avg_pred - avg_pred[1]
	} else {
	  ice_mat_plot <- ice_mat[lines_to_samp, ]
	  avg_pred_plot <- avg_pred
	}
	
  plot(xvals, ice_mat_plot[1, ], type = "l", 
       xlab = paste0("PC", location), 
  		 main = paste0("ICE Plot of PC", location),
       ylab = "yhat", col = "black", lwd = 2,
  		 ylim = c(min(ice_mat_plot), max(ice_mat_plot)))
  
  if (rug) {
    rug(pca_object$x[, location])
  }
  
  ## Add lines for individual curves
  for(i in 2:nrow(ice_mat_plot)) {
    points(xvals, ice_mat_plot[i, ], type = "l", lwd = 2)
  }
  lines(xvals, avg_pred_plot, col = "red", lwd = 5)
  
  ## Names and color for legend in plot
  loading_ord <- order(abs(pca_object$rotation[, location]), decreasing = T)
  top_loads <- pca_object$rotation[loading_ord[1:num_load], location, 
                                   drop = FALSE]
  load_df <- data.frame(name = rownames(top_loads), 
                        symbol = ifelse(top_loads[, 1] > 0, 2, 6))
  load_df$col <- ifelse(top_loads[, 1] > 0, "green", "red")
  
  ## Add legend to plot
  legend(legend_loc, legend = load_df$name, pch = load_df$symbol, 
         col = load_df$col, cex = 0.8, title = "Primary Features:")
  
  data.frame(feature_name = as.character(load_df$name), 
             loading_value = top_loads, stringsAsFactors = FALSE)
}
