
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- load libraries -->

# TidyWrappers

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-blue.svg)](https://github.com/cparsania/tidywrappers)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

TidyWrappers is an R package to deal with an object `tbl`. Utilities
given here are appropriate to use when a `tbl` has rows are features /
observations and columns are variables. It provides handy wrapper
functions on top of `dplyr` verbs to get, count, convert, keep, remove
and replace data from a `tbl` object.

## Motivation

The way R is being used, has changed drastically in last few years due
to the availability of
[`tidyverse`](https://www.tidyverse.org/packages/) and more specifically
[`dplyr`](https://dplyr.tidyverse.org) (all credits to [Hadley
Wickham](http://hadley.nz) and [RStudio](https://rstudio.com) team).
Rich documentation, stable release and excellent functionalities of
these packages have provoked significant number of R users to switch
from traditional `dataframes` to intelligible data structure, `tibble`
(aka `tbl`). `dplyr` provides users with rich utilities like `select`,
`mutate`, `filter`, `group_by` , `summarise` and `arrange` for easy
manipulation of a `tibble`. However, for newbie it takes a while to
become familiar with `tidy data` and `dplyr` core and helper functions.

`TidyWrappers` contains set of functions, which save you little from
writing and implementing core `dplyr` verbs while manipulating data from
`tbl`. Additionaly, redundant lines of code can be avoided using
functions given in `TidyWrappers`. There are more than 30 functions
implemented in `TidyWrappers`, which wrap various `dplyr` functions
internally. See examples
below.

``` r
  tbl <- tibble::tibble(a = letters[1:6],x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
  tbl
#> # A tibble: 6 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 a         0     0     0
#> 2 b         1     1     1
#> 3 c         2     2     2
#> 4 d         0     0     0
#> 5 e         3     3     3
#> 6 f         5     5     5
  
  ## keep rows having  0 across all numeric columns. 
  
  # using dplyr
  
  tbl %>% dplyr::filter_if(is.numeric , dplyr::any_vars( . == 0) )
#> # A tibble: 2 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 a         0     0     0
#> 2 d         0     0     0
  
  # using TidyWrappers
  
  tbl %>% tbl_keep_rows_zero_any()
#> # A tibble: 2 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 a         0     0     0
#> 2 d         0     0     0

  ## remove rows having  0 across all numeric columns. 
  
  # using dplyr 
  
  tbl %>% dplyr::filter_if(is.numeric , dplyr::any_vars( . > 0) )
#> # A tibble: 4 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 b         1     1     1
#> 2 c         2     2     2
#> 3 e         3     3     3
#> 4 f         5     5     5
  
  # using TidyWrappers 
  
  tbl %>% TidyWrappers::tbl_remove_rows_zero_any()
#> # A tibble: 4 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 b         1     1     1
#> 2 c         2     2     2
#> 3 e         3     3     3
#> 4 f         5     5     5
```

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
