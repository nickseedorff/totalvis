# totalvis

<!-- badges: start -->
<!-- badges: end -->

`totalvis` is a model agnostic vizualization tool used to summarize complex relationships in black-box models. The package relies on a PCA based transformation of the training data to group correlated features and display their total effect on the response. The 'Introduction' vignette demonstrates base usage of 'totalvis' by reproducing all figures used in the associated paper, as well as introducing a few additional components.


## Prerequisites
```{r eval = FALSE}
## Prerequisites
install.packages("devtools")
library("devtools")
```

## Installation

```{r eval = FALSE}
## Download the package without the vignette
install_github("nickseedorff/totalvis")
library(totalvis)

## Download the package with the vignette
install_github("nickseedorff/totalvis", build_vignettes = T)
library(totalvis)

## View the vignette
vignette("Introduction", package = "totalvis")
```

