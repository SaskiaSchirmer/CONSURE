# CONSURE
This package estimates survival, migratory connectivity and recovery probability in continuous space from dead recovery data. The data can be simulated by the package or committed as a dataset. The package implements the continuous and combined approach described in detail in the dissertation of Saskia Schirmer.

### Getting started
For getting started with **CONSURE** read the [vignettes](https://saskiaschirmer.github.io/CONSURE/articles/index.html).

### Installation

* Install latest development version from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("SaskiaSchirmer/CONSURE", dependencies = TRUE, build_vignettes = FALSE)
```

The vignettes take a while to be build. Therefore, we recommend not to include them in the installation, but use the online version [here](https://saskiaschirmer.github.io/CONSURE/articles/index.html).

![](https://img.shields.io/github/license/SaskiaSchirmer/CONSURE)
![](https://img.shields.io/github/workflow/status/SaskiaSchirmer/CONSURE/R-CMD-check)
