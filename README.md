
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clusterapply

<!-- badges: start -->

<!-- badges: end -->

Clusterapply is meant to break datasets into smaller frames on which
functions (regressions, for example) can be applied. Much of this can
already be accomplished with the family of \*apply functions, but here
we are building towards parallelization with an eye on memory use. The
package does not yet contain all intended features and should be
considered provisional.

This package is used primarily in the EPIDEMIA forecasting system’s
epidemiar package:
<https://github.com/EcoGRAPH/epidemiar/releases/latest> with an
associated demonstration project available at:
<https://github.com/EcoGRAPH/epidemiar-demo/releases/latest>

## Installation

The development version, whenever public, is available from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EcoGRAPH/clusterapply")
```

## Example

Please see the associated ca-vignette.Rmd file for an example of
clusterapply’s use.
