% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_partial_effects.R
\name{plot.partialvis}
\alias{plot.partialvis}
\title{Diagnostic plot for totalvis function}
\usage{
\method{plot}{partialvis}(
  x,
  differenced = TRUE,
  legend_loc = "topleft",
  rug = TRUE,
  legend_cex = 1,
  ...
)
}
\arguments{
\item{x}{A diagvis.object returned from a call to totalvis_diag}

\item{differenced}{logical, plot differences instead of true predictions}

\item{legend_loc}{Legend placement; 'topleft', 'topright', 'bottom', ...}

\item{rug}{Adds a rug representation of the principal component}

\item{legend_cex}{Character expansion factor for the legend}

\item{...}{Additional optional arguments to be passed to plot, accepts xlab or main as arguments}
}
\description{
Diagnostic plot for totalvis function
}
