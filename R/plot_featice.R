#' Generate an ICE plots over for a specified feature
#' @param x A totalvis.object returned from a call to totalvis
#' @param num_load Number of loading to include in the legend
#' @param legend_loc Location of legend, default of topleft
#' @param center Logical, produced centered ice plots
#' @param rug Adds a rug representation of the principal component
#' @param plot_frac Fraction of individual conditional expectation curves to plot
#' @param ... Additional optional arguments to be passed to plot
#' @export

plot.featice <-
function(x, num_load = 5, legend_loc = "topleft", center = TRUE, rug = TRUE, 
         plot_frac = 0.1, ...) {

  ## Unlist object
  avg_pred <- x$avg_pred
  xvals <- x$xvals
  feature <- x$feature
  data <- x$data
  ice_mat <- x$ice_mat
  
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
	
  ## Initalize plot
  plot(xvals, ice_mat_plot[1, ], type = "l", 
       xlab = feature, 
  		 main = paste0("ICE Plot of ", feature),
       ylab = "yhat", col = "black", lwd = 2,
  		 ylim = c(min(ice_mat_plot), max(ice_mat_plot)))
  
  if (rug) {
    rug(data[, feature])
  }
  
  ## Add lines for individual curves
  for(i in 2:nrow(ice_mat_plot)) {
    points(xvals, ice_mat_plot[i, ], type = "l", lwd = 2)
  }
  lines(xvals, avg_pred_plot, col = "red", lwd = 5)
}
