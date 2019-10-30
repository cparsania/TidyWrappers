
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TidyWrappers

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/cparsania/tidywrappers)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

TidyWrappers is an R package to deal with an object `tbl`. It provide
handy wrapper functions to get, count, convert, keep, remove and replace
data from `tbl` object.

# Examples

``` r



tbl_convert_column_zscore() ## Scale numeric columns. 
tbl_convert_log()   ## Apply log on numeric columns.
tbl_convert_log10() ## Apply log10 on numeric columns.
tbl_convert_log2()  ## Apply log2 on numeric columns.
tbl_convert_row_zscore()    ## Scale rows considering numeric columns.

tbl_count_vars_NA_all() ## Count columns having all values are NA.
tbl_count_vars_NA_any() ## Count columns having atleast one NA.
tbl_count_vars_zero_all()   ## Count columns having all values are 0.
tbl_count_vars_zero_any()   ## Count columns having atleast one 0.

tbl_get_vars_NA_all()   ## Get columns names having all values are NA.
tbl_get_vars_NA_any()   ## Get columns names having atleast one NA.
tbl_get_vars_zero_all() ## Get columns names having all values are 0.
tbl_get_vars_zero_any() ## Get columns names having atleast one 0.

tbl_keep_rows_less_than_or_equal_all()  ## Keep rows with less than or equal 'cutoff' in all numeric columns.
tbl_keep_rows_less_than_or_equal_any()  ## Keep rows with less than or equal 'cutoff' in any numeric column.
tbl_keep_rows_greater_than_or_equal_all()   ## Keep rows with greater than or equal 'cutoff' in all numeric columns.
tbl_keep_rows_greater_than_or_equal_any()   ## Keep rows with greater than or equal 'cutoff' in any numeric column.
tbl_keep_rows_zero_all()    ## Keep rows having all values are 0.
tbl_keep_rows_zero_any()    ## Keep rows having atleast one 0.

tbl_remove_rows_greater_than_or_equal_any() ## Remove rows with greater than or equal 'cutoff' in any numeric column.
tbl_remove_rows_less_than_or_equal_any()    ## Remove rows with less than or equal 'cutoff' in any numeric column.
tbl_remove_rows_greater_than_or_equal_all() ## Remove rows with greater than or equal 'cutoff' in all numeric column.
tbl_remove_rows_less_than_or_equal_all()    ## Remove rows with less than or equal 'cutoff' in all numeric column.
tbl_remove_rows_zero_all()  ## Remove rows having all values are 0.
tbl_remove_rows_zero_any()  ## Remove rows having atleast one 0
tbl_remove_vars_NA_all()    ## Remove columns having all values are NA.
tbl_remove_vars_NA_any()    ## Remove columns having atlease one NA
tbl_remove_vars_zero_all()  ## Remove columns having all values are 0.
tbl_remove_vars_zero_any()  ## Remove columns having atleast one 0

tbl_replace_greater_than()  ## Replace numeric values greater than given 'cutoff'.
tbl_replace_greater_than_or_equal() ## Replace numeric values greater than or equal to given 'cutoff'.
tbl_replace_less_than() ## Replace numeric values less than given 'cutoff'.
tbl_replace_less_than_or_equal()    ## Replace numeric values less than or equal given 'cutoff'.
tbl_replace_string()    ## Replace all occurance of 'pattern' in all non numeric columns. Wrapper around 'stringr::str_replace_all()'.
```
