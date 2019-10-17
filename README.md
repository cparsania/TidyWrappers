
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TidyWrappers

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/cparsania/tidywrappers)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

Aim of TidyWrappers is to provide handy wrapper functions to deal with
`tbl` object.

# Examples

``` r

## count variables having all values are 0 in a tbl. 
count_zero_vars()

## remove variables having all values are 0 in a tbl.
remove_zero_vars()

## get variables (names) having all values are 0 in a tbl.
get_zero_vars()

## get records (rows) having all values are 0 in a tbl.
get_zero_records()

## remove records (rows) having all values are 0 in a tbl.
remove_zero_records()
```
