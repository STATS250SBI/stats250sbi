# stats250sbi

An R package designed for the simulation-based inference cluster of STATS 250 at the University of Michigan.

## Installation

This package is not available on CRAN, and must be installed via GitHub using the `remotes` package. To install `stats250sbi`, run the following code in your R console:

```{r}
install.packages("remotes")
remotes::install_github("STATS250SBI/stats250sbi")
```

To use the package, make sure the following line of code is run every time you start R, or by making sure it is in the setup chunk of your R Markdown document:

```{r}
library(stats250sbi)
```
