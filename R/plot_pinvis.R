#' Generate a total effect plot for a specific principal component and feature
#' @param x A total_vis object returned from a call to total_vis
#' @param rug Adds a rug representation of the principal component
#' @param ... Additional optional arguments to be passed to plot
#' @export

plot.pinvis <-
function(x, rug = TRUE, ...) {
  
  ## Unlist object
  pred_df <- x$pred_df
  pin <- x$pin
  location <- x$location
  
  ## unique_value uses location, pdp_mat uses order of locations
  plot(pred_df$pin_mean, pred_df$avg_pred, type = "l", 
       xlab = paste0("Pinned Feature: ", pin), 
       main = paste0("Total Effect: PC = ", location, ", Pin = ", pin),
       ylab = "yhat", col = "dodgerblue", lwd = 4)
  
  if (rug) {
    rug(pred_df$pin_mean)
    lines(pred_df$pin_mean, pred_df$avg_pred, col = "dodgerblue",
          lwd = 4)
  }
}
