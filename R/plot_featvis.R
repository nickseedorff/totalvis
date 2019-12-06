#' Generate a partial depedence plot for a specified feature
#' @param x A featvis object returned from a call to totalvis 
#' @param rug Adds a rug representation of the feature
#' @param ... Additional optional arguments to be passed to plot, accepts xlab or main as arguments
#' @export

plot.featvis <-
function(x, rug = TRUE, ...) {
  
  ## Unlist object
  pred_df <- x$pred_df
  feature <- x$feature
  data <- x$data
  
  ## Default plotting values
  optionals <- list(...)
  defaults <- list(xlab = ifelse("xlab" %in% names(optionals), 
                                 optionals[["xlab"]], feature),
                   main = ifelse("main" %in% names(optionals), 
                                 optionals[["main"]], 
                                 paste0("PDP of ", feature)))
  
  ## Plot pdp
  plot(pred_df$x_vals, pred_df$avg_pred, type = "l", 
       xlab = defaults[["xlab"]], 
       main = defaults[["main"]],
       ylab = "yhat", col = "dodgerblue", lwd = 4)
  
  if (rug) {
    rug(data[, feature])
    lines(pred_df$x_vals, pred_df$avg_pred, col = "dodgerblue",
          lwd = 4)
  }
}

