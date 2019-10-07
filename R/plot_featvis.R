#' Generate a partial depedence plot for a specified feature
#' @param x A featvis object returned from a call to totalvis 
#' @param rug Adds a rug representation of the feature
#' @param ... Additional optional arguments to be passed to plot
#' @export

plot.featvis <-
function(x, rug = TRUE, ...) {
  
  ## Unlist object
  pred_df <- x$pred_df
  feature <- x$feature
  data <- x$data
  
  ## unique_value uses location, pdp_mat uses order of locations
  plot(pred_df$x_vals, pred_df$avg_pred, type = "l", 
       xlab = feature, 
       main = paste0("PDP Plot of ", feature),
       ylab = expression(hat(y)), col = "dodgerblue", lwd = 4)
  
  if (rug) {
    rug(data[, feature])
    lines(pred_df$x_vals, pred_df$avg_pred, col = "dodgerblue",
          lwd = 4)
  }
}

