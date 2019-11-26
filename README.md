
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TidyWrappers

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-blue.svg)](https://github.com/cparsania/tidywrappers)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

TidyWrappers is an R package to deal with an object `tbl`. Utilities
given here are appropriate to use when a `tbl` has rows are features /
observations and columns are variables. It provides handy wrapper
functions to get, count, convert, keep, remove and replace data from
`tbl` object.

## Install

Install current version of TidyWrappers on your system.

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("cparsania/TidyWrappers", dependencies = TRUE)
```

## Functions

### Convert tbl operations - `tbl_convert_*`

`tbl_convert_*` funtions return a `tbl`. Functions apply to only numeric
columns of a input `tbl`.

``` r
tbl_convert_column_zscore() ## Scale numeric columns. 
tbl_convert_log()   ## Apply log on numeric columns.
tbl_convert_log10() ## Apply log10 on numeric columns.
tbl_convert_log2()  ## Apply log2 on numeric columns.
tbl_convert_row_zscore()    ## Scale rows considering numeric columns.
```

### Count columns operations - `tbl_count_vars*`

`tbl_count_*` functions return a numeric value. Functions apply to both
numeric and non numeric columns of a `tbl`.

``` r
tbl_count_vars_NA_all() ## Count columns having all values are NA.
tbl_count_vars_NA_any() ## Count columns having atleast one NA.
tbl_count_vars_zero_all()   ## Count columns having all values are 0.
tbl_count_vars_zero_any()   ## Count columns having atleast one 0.
```

### Get vars operations - `tbl_get_vars*`

`tbl_get_vars*` functions returns a character vector of column names.
Functions apply to both numeric and non numeric columns of a `tbl`.

``` r
tbl_get_vars_NA_all()   ## Get columns names having all values are NA.
tbl_get_vars_NA_any()   ## Get columns names having atleast one NA.
tbl_get_vars_zero_all() ## Get columns names having all values are 0.
tbl_get_vars_zero_any() ## Get columns names having atleast one 0.
```

### Keep rows operations - `tbl_keep_rows*`

`tbl_keep_rows*` functions returns a `tbl`. Functions apply to numeric
columns of a
`tbl`.

``` r
tbl_keep_rows_less_than_or_equal_all()  ## Keep rows with less than or equal 'cutoff' in all numeric columns.
tbl_keep_rows_less_than_or_equal_any()  ## Keep rows with less than or equal 'cutoff' in any numeric column.
tbl_keep_rows_greater_than_or_equal_all()   ## Keep rows with greater than or equal 'cutoff' in all numeric columns.
tbl_keep_rows_greater_than_or_equal_any()   ## Keep rows with greater than or equal 'cutoff' in any numeric column.
tbl_keep_rows_zero_all()    ## Keep rows having all values are 0.
tbl_keep_rows_zero_any()    ## Keep rows having atleast one 0.
```

### Remove rows operations - `tbl_remove_rows*`

`tbl_remove_rows*` functions returns a `tbl`. Functions apply to numeric
columns of a
`tbl`.

``` r
tbl_remove_rows_greater_than_or_equal_any() ## Remove rows with greater than or equal 'cutoff' in any numeric column.
tbl_remove_rows_less_than_or_equal_any()    ## Remove rows with less than or equal 'cutoff' in any numeric column.
tbl_remove_rows_greater_than_or_equal_all() ## Remove rows with greater than or equal 'cutoff' in all numeric column.
tbl_remove_rows_less_than_or_equal_all()    ## Remove rows with less than or equal 'cutoff' in all numeric column.
tbl_remove_rows_zero_all()  ## Remove rows having all values are 0.
tbl_remove_rows_zero_any()  ## Remove rows having atleast one 0
```

### Remove columns operations - `tbl_remove_vars*`

`tbl_remove_vars*` functions returns a `tbl`. Functions apply to numeric
and non columns of a `tbl`.

``` r
tbl_remove_vars_NA_all()    ## Remove columns having all values are NA.
tbl_remove_vars_NA_any()    ## Remove columns having atlease one NA
tbl_remove_vars_zero_all()  ## Remove columns having all values are 0.
tbl_remove_vars_zero_any()  ## Remove columns having atleast one 0
```

### Replace value operations - `tbl_replace*`

`tbl_replace*` functions returns a `tbl`. Functions apply to both
numeric and non numeric columns of a
`tbl`.

``` r
tbl_replace_greater_than()  ## Replace numeric values greater than given 'cutoff'.
tbl_replace_greater_than_or_equal() ## Replace numeric values greater than or equal to given 'cutoff'.
tbl_replace_less_than() ## Replace numeric values less than given 'cutoff'.
tbl_replace_less_than_or_equal()    ## Replace numeric values less than or equal given 'cutoff'.
tbl_replace_string()    ## Replace all occurance of 'pattern' in all non numeric columns. Wrapper around 'stringr::str_replace_all()'.
```
