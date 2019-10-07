#' Diagnostic plot for totalvis function
#' @param x A diagvis.object returned from a call to totalvis_diag 
#' @param legend_loc Legend placement; 'topleft', 'topright', 'bottom', ...
#' @param rug Adds a rug representation of the principal component
#' @param ... Additional optional arguments to be passed to plot
#' @export


plot.diagvis <- function(x, legend_loc = "topleft", rug = TRUE, ...){
  
  ## Unlist object
  pred_mat <- x$pred_mat
  x_vec <- x$x_vec
  location <- x$location
  pca_object <- x$pca_object
  load_names <- x$load_names
  
  n_col <- ncol(pred_mat)
  
  ## Define color palette
  cbPalette <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                     "#0072B2", "#D55E00", "#CC79A7"), n_col)
  
  ## Plot lines against 
  plot(x_vec, rep(0, length(x_vec)), 
       ylim = c(min(pred_mat), max(pred_mat)), 
       type = "l", xlab = paste0("PC", location), 
       ylab = expression(hat(y)[t] - hat(y)[t - 1]), 
       main = paste0("PC", location), lwd = 3)
  
  ## Add rug
  if (rug) {
    rug(pca_object$x[, location])
  }
  
  ## Legend individual curves curves
  legend(legend_loc, legend = load_names, col = cbPalette[c(1:n_col)], 
         lty = 1, lwd = 3, cex = 1)
  
  ## Add lines for individual curves
  for(i in 1:n_col) {
    points(x_vec, pred_mat[, i], col = cbPalette[i], 
           type = "b", lwd = 3)
  }
}

