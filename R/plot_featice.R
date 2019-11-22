#' Generate an ICE plots over for a specified feature
#' @param x A featice.object returned from a call to totalvis
#' @param center Logical, produces centered ice plots
#' @param rug Adds a rug of the feature of intervest
#' @param plot_frac Fraction of individual conditional expectation curves to plot
#' @param legend_cex Character expansion factor for the legend
#' @param ... Additional optional arguments to be passed to plot, accepts xlab or main as arguments
#' @export

plot.featice <-
function(x, center = TRUE, rug = TRUE, plot_frac = 0.1, legend_cex = 1, ...) {

  
  ## Unlist object
  avg_pred <- x$avg_pred
  xvals <- x$xvals
  feature <- x$feature
  data <- x$data
  ice_mat <- x$ice_mat
  
  lines_to_samp <- sample(1:nrow(ice_mat), 
                          size = round(nrow(ice_mat) * plot_frac))
	
  ## Default plotting values
  optionals <- list(...)
  defaults <- list(xlab = ifelse("xlab" %in% names(optionals), 
                                 optionals[["xlab"]], feature),
                   main = ifelse("main" %in% names(optionals), 
                                 optionals[["main"]], 
                                 paste0("ICE plot of ", feature)))
  
  ## unique_value uses pc_num, pdp_mat uses order of pc_nums
	if(center) {
	  ice_mat_plot <- ice_mat[lines_to_samp, ] - ice_mat[lines_to_samp, 1]
	  avg_pred_plot <- avg_pred - avg_pred[1]
	} else {
	  ice_mat_plot <- ice_mat[lines_to_samp, ]
	  avg_pred_plot <- avg_pred
	}
	
  ## Initalize plot
  plot(xvals, ice_mat_plot[1, ], type = "l", 
       xlab = defaults[["xlab"]], 
       main = defaults[["main"]],
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
