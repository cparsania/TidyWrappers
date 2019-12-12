
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- load libraries -->

# TidyWrappers

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-blue.svg)](https://github.com/cparsania/tidywrappers)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

TidyWrappers is an R package to deal with an object `tbl`. It provides
handy wrapper functions on top of `dplyr` verbs to get, count, convert,
keep, remove and replace data from a `tbl` object.

## Motivation

The way R is being used, has changed drastically in last few years due
to the availability of
[`tidyverse`](https://www.tidyverse.org/packages/) and more specifically
[`dplyr`](https://dplyr.tidyverse.org) (all credits to [Hadley
Wickham](http://hadley.nz) and [RStudio](https://rstudio.com) team).
Rich documentation, stable release and excellent functionalities of
these packages have provoked significant number of R users to switch
from traditional `dataframes` to intelligible data structure, `tibble`
(aka `tbl`). [`dplyr`](https://dplyr.tidyverse.org) provides users with
rich utilities like `select`, `mutate`, `filter`, `group_by` ,
`summarise` and `arrange` for easy manipulation of a `tbl`. However, for
newbies it takes a while to become familiar with [`tidy
data`](https://vita.had.co.nz/papers/tidy-data.pdf) and
[`dplyr`](https://dplyr.tidyverse.org) core and helper functions.

`TidyWrappers` contains set of functions, which save you little from
writing and implementing core [`dplyr`](https://dplyr.tidyverse.org)
verbs while manipulating data from `tbl`. Additionaly, redundant lines
of code can be avoided using functions given in `TidyWrappers`. Current
version of `TidyWrappers` containes more than 30 functions, which are
wrapped on top of various [`dplyr`](https://dplyr.tidyverse.org)
functions. See below in examples.

## Install

Install current version of TidyWrappers on your system.

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("cparsania/TidyWrappers", dependencies = TRUE)
```

## Examples

``` r
library(dplyr)
library(TidyWrappers)
library(magrittr)

  tbl <- tibble::tibble(a = letters[1:6], b = NA_character_, x = c(0,1,2,0,0,NA) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
  tbl
#> # A tibble: 6 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     <NA>      0     0     0
#> 2 b     <NA>      1     1     1
#> 3 c     <NA>      2     2     2
#> 4 d     <NA>      0     0     0
#> 5 e     <NA>      0     3     3
#> 6 f     <NA>     NA     5     5
  
  ## keep rows having  0 across all numeric columns. 
  
  # using dplyr
  tbl %>% dplyr::filter_if(is.numeric , dplyr::all_vars( . == 0) )
#> # A tibble: 2 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     <NA>      0     0     0
#> 2 d     <NA>      0     0     0
  
  # using TidyWrappers
  tbl %>% TidyWrappers::tbl_keep_rows_zero_all()
#> # A tibble: 2 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     <NA>      0     0     0
#> 2 d     <NA>      0     0     0

  ## remove rows having  0 across all numeric columns. 
  
  # using dplyr 
  tbl %>% dplyr::filter_if(is.numeric , dplyr::any_vars( . > 0) )
#> # A tibble: 4 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 b     <NA>      1     1     1
#> 2 c     <NA>      2     2     2
#> 3 e     <NA>      0     3     3
#> 4 f     <NA>     NA     5     5
  
  # using TidyWrappers 
  tbl %>% TidyWrappers::tbl_remove_rows_zero_all()
#> # A tibble: 4 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 b     <NA>      1     1     1
#> 2 c     <NA>      2     2     2
#> 3 e     <NA>      0     3     3
#> 4 f     <NA>     NA     5     5
  
  ## Convert log2 to all numeric columns optionally adding numeric fraction to original values. 
  
  # using dplyr 
  tbl %>% dplyr::mutate_if(is.numeric , ~ log2 (. +  1))
#> # A tibble: 6 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     <NA>   0     0     0   
#> 2 b     <NA>   1     1     1   
#> 3 c     <NA>   1.58  1.58  1.58
#> 4 d     <NA>   0     0     0   
#> 5 e     <NA>   0     2     2   
#> 6 f     <NA>  NA     2.58  2.58
 
  # using TidyWrappers
  tbl %>% TidyWrappers::tbl_convert_log2(frac = 1)
#> # A tibble: 6 x 5
#>   a     b         x     y     z
#>   <chr> <chr> <dbl> <dbl> <dbl>
#> 1 a     <NA>   0     0     0   
#> 2 b     <NA>   1     1     1   
#> 3 c     <NA>   1.58  1.58  1.58
#> 4 d     <NA>   0     0     0   
#> 5 e     <NA>   0     2     2   
#> 6 f     <NA>  NA     2.58  2.58

  
  ## Remove columns / variables  having atleaset one NA
  
  # using dplyr
  tbl %>% dplyr::select_if( ~ ( (is.na(.)) %>% any(., na.rm = T)  %>% `!`) ) 
#> # A tibble: 6 x 3
#>   a         y     z
#>   <chr> <dbl> <dbl>
#> 1 a         0     0
#> 2 b         1     1
#> 3 c         2     2
#> 4 d         0     0
#> 5 e         3     3
#> 6 f         5     5
  
  # using TidyWrappers
  tbl %>% TidyWrappers::tbl_remove_vars_NA_any()
#> # A tibble: 6 x 3
#>   a         y     z
#>   <chr> <dbl> <dbl>
#> 1 a         0     0
#> 2 b         1     1
#> 3 c         2     2
#> 4 d         0     0
#> 5 e         3     3
#> 6 f         5     5
  
  ## Remove columns / variables  having all values are NA
  
  # using dplyr
  tbl %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(., na.rm = T)  %>% `!`) ) 
#> # A tibble: 6 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 a         0     0     0
#> 2 b         1     1     1
#> 3 c         2     2     2
#> 4 d         0     0     0
#> 5 e         0     3     3
#> 6 f        NA     5     5
  
  # using TidyWrappers
  tbl %>% TidyWrappers::tbl_remove_vars_NA_all()
#> # A tibble: 6 x 4
#>   a         x     y     z
#>   <chr> <dbl> <dbl> <dbl>
#> 1 a         0     0     0
#> 2 b         1     1     1
#> 3 c         2     2     2
#> 4 d         0     0     0
#> 5 e         0     3     3
#> 6 f        NA     5     5
```

## Reference

Follow page
[Reference](https://cparsania.github.io/TidyWrappers/reference/) to see
full list of functions.
