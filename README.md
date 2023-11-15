
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factor256

<!-- badges: start -->

<!-- badges: end -->

The goal of factor256 is to minimize the memory footprint of data
analysis that uses categorical variables with fewer than 256 unique
values.

## Installation

You can install the development version of factor256 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HughParsonage/factor256")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(factor256)
x <- factor256(LETTERS)
typeof(x)
#> [1] "raw"
identical(recompose256(x), LETTERS)
#> [1] TRUE
```

``` r
library(data.table)
DT <-
  CJ(Year = 2000:2020,
     State = rep_len(c("WA", "SA", "NSW", "NT", "TAS", "VIC", "QLD"), 1000),
     Age = rep_len(0:100, 10000))
# pryr::object_size(DT)
# 3.36GB
for (j in seq_along(DT)) {
  set(DT, j = j, value = factor256(.subset2(DT, j)))
}
# pryr::object_size(DT)
# 630 MB

DT[, val := sample(.N)]
```
