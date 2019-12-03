#' Diagnostic plot for totalvis function
#' @param x A diagvis.object returned from a call to totalvis_diag 
#' @param differenced logical, plot differences instead of true predictions
#' @param lag logical, use the previous principal component value for reference predictions
#' @param legend_loc Legend placement; 'topleft', 'topright', 'bottom', ...
#' @param rug Adds a rug representation of the principal component
#' @param legend_cex Character expansion factor for the legend
#' @param ... Additional optional arguments to be passed to plot, accepts xlab or main as arguments
#' @export

plot.partialvis <- function(x, differenced = TRUE, lag = FALSE,
                            legend_loc = "topleft", rug = TRUE, 
                            legend_cex = 1, ...){
  
  ## Unlist object
  pred_mat <- x$pred_mat
  overall_pred <- x$overall_pred
  x_vec <- x$x_vec
  pc_num <- x$pc_num
  pca_object <- x$pca_object
  load_names <- x$load_names
  
  ## Default plotting values
  optionals <- list(...)
  defaults <- list(xlab = ifelse("xlab" %in% names(optionals), 
                                 optionals[["xlab"]], paste0("PC", pc_num)),
                   main = ifelse("main" %in% names(optionals), 
                                 optionals[["main"]], 
                                 paste0("Partial Effects of PC", pc_num)))
  
  ## Define color palette
  n_col <- ncol(pred_mat)
  cbPalette <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                     "#0072B2", "#D55E00", "#CC79A7"), n_col)
  
  ## Plot lines against
  if (differenced & !lag) {
    pred_mat <- sweep(pred_mat, 1, c(0, overall_pred[-1]))
    plot(x_vec, rep(0, length(x_vec)), 
         ylim = c(min(pred_mat), max(pred_mat)), 
         type = "l", xlab = defaults[["xlab"]], 
         main = defaults[["main"]],
         ylab = expression(y[partial] - y[hat]), 
         lwd = 3)
  } else if (differenced) {
    pred_mat <- sweep(pred_mat, 1, 
                      c(0, (overall_pred[1:length(overall_pred) - 1])))
    plot(x_vec, rep(0, length(x_vec)), 
         ylim = c(min(pred_mat), max(pred_mat)), 
         type = "l", xlab = defaults[["xlab"]], 
         main = defaults[["main"]],
         ylab = expression(y[partial] - y[hat]), 
         lwd = 3)
  } else {
    pred_mat[1, ] <- overall_pred[1]
    plot(x_vec, overall_pred, 
         ylim = c(min(pred_mat, overall_pred), c(max(pred_mat, overall_pred))), 
         type = "l", xlab = defaults[["xlab"]], 
         main = defaults[["main"]],
         ylab = "yhat", 
         lwd = 4)
  }
  
  ## Add rug
  if (rug) {
    rug(pca_object$x[, pc_num])
  }
  
  ## Legend individual curves curves
  if (differenced) {
    legend(legend_loc, legend = load_names, col = cbPalette[c(1:n_col)], 
           lty = 1, lwd = 3, cex = legend_cex)
  } else {
    legend(legend_loc, legend = c("Total Effect", load_names), 
           col = c(1, cbPalette[c(1:n_col)]), 
           lty = 1, lwd = c(4, rep(3, n_col)), cex = legend_cex)
  }
    
  ## Add lines for individual curves
  for(i in 1:n_col) {
    points(x_vec, pred_mat[, i], col = cbPalette[i], 
           type = "b", lwd = 3)
  }
}

