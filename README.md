# CONSURE
This package estimates survival, migratory connectivity and recovery probability in continuous space from dead recovery data. The data can be simulated by the package or committed as a dataset. The package implements the continuous and combined approach described in detail in the dissertation of Saskia Schirmer.

### Getting started
For getting started with **CONSURE** read the [vignettes](https://github.com/SaskiaSchirmer/CONSURE).

### Installation

# * Install from CRAN:
# 
# ```r
# install.packages("CONSURE")
# ```

* Install latest development version from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("CONSURE", dependencies = TRUE, build_vignettes = FALSE)
```

This installation won't include the vignettes (they take some time to build), but all of the vignettes are 
available online [here](https://github.com/SaskiaSchirmer/CONSURE).

