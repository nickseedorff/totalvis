#' Screeplot wrapper function for totalvis objects
#' @param x An object produced from the totalvis or partial_effects functions 
#' @param main title of the scree plot
#' @param ... Additional arguments to be passed to screeplot
#' @import stats
#' @export

scree_plot <- function(x, main = "Scree Plot", ...){
  screeplot(x$pca_object, main = main, ...)
}
