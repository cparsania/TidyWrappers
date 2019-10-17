
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
tbl_count_zero_vars()

## remove variables having all values are 0 in a tbl.
tbl_remove_zero_vars()

## get variables (names) having all values are 0 in a tbl.
tbl_get_zero_vars()

## get records (rows) having all values are 0 in a tbl.
tbl_get_zero_records()

## remove records (rows) having all values are 0 in a tbl.
tbl_remove_zero_records()

# apply log2 on numeric variables
tbl_convert_log2()

# replace numeric values greater than given cutoff
tbl_replace_greater_than()

#replace numeric values greater than or equal to given cutoff
tbl_replace_greater_than_or_equal()

#replace numeric values less than given cutoff
tbl_replace_less_than()

#replace numeric values less than or equal given cutoff
replace_less_than_or_equal()
```
