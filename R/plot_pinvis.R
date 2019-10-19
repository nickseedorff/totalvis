#' Generate a total effect plot for a specific principal component and feature
#' @param x A total_vis object returned from a call to total_vis
#' @param rug Adds a rug representation of the principal component
#' @param ... Additional optional arguments to be passed to plot, accepts xlab or main as arguments
#' @export

plot.pinvis <-
function(x, rug = TRUE, ...) {
  
  ## Unlist object
  pred_df <- x$pred_df
  pin <- x$pin
  pc_num <- x$pc_num
  
  ## Default plotting values
  optionals <- list(...)
  defaults <- list(xlab = ifelse("xlab" %in% names(optionals), 
                                 optionals[["xlab"]], 
                                 paste0("Pinned Feature: ", pin)),
                   main = ifelse("main" %in% names(optionals), 
                                 optionals[["main"]], 
                                 paste0("Pin Plot with PC = ", pc_num)))
                   
  ## Plot average covariate value by average pred
  plot(pred_df$pin_mean, pred_df$avg_pred, type = "l", 
       xlab = defaults[["xlab"]], 
       main = defaults[["main"]],
       ylab = "yhat", col = "dodgerblue", lwd = 4)
  
  if (rug) {
    rug(pred_df$pin_mean)
    lines(pred_df$pin_mean, pred_df$avg_pred, col = "dodgerblue",
          lwd = 4)
  }
}
