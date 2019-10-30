
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


tbl_convert_column_zscore()     ## Scale numeric vars (columns).
tbl_convert_log()   #Apply log on numeric variables
tbl_convert_log10() #Apply log10 on numeric variables
tbl_convert_log2()  #Apply log2 on numeric variables
tbl_convert_row_zscore()    #Scale numeric records (rows).
tbl_count_vars_NA_all() #Count variables having all values are NA in a tbl.
tbl_count_vars_NA_any() #Count variables having atleast one NA
tbl_count_vars_zero_all()   #Count variables having all values are 0 in a tbl.
tbl_count_vars_zero_any()   #Count variables having atleast one 0
tbl_get_vars_NA_all()   #Get variables names having all values are NA in a tbl.
tbl_get_vars_NA_any()   #Get variables names having atleast one NA
tbl_get_vars_zero_all() #Get variable (names) having all values are 0 in a tbl.
tbl_get_vars_zero_any() #Get variable (names) having atleast one 0
tbl_keep_greater_than_or_equal_all()    #keep records with greater than or equal 'cutoff' in all numeric var
tbl_keep_greater_than_or_equal_any()    #keep records with greater than or equal 'cutoff' in any numeric var
tbl_keep_less_than_or_equal_all()   #keep records with less than or equal 'cutoff' in all numeric var
tbl_keep_less_than_or_equal_any()   #keep records with less than or equal 'cutoff' in any numeric var
tbl_keep_rows_zero_all()    #Subset records (rows) having all values are 0 in a tbl.
tbl_keep_rows_zero_any()    #Subset records (rows) having atleast one 0
tbl_remove_greater_than_or_equal_all()  #remove records with greater than or equal 'cutoff' in all numeric var
tbl_remove_greater_than_or_equal_any()  #remove records with greater than or equal 'cutoff' in any numeric var
tbl_remove_less_than_or_equal_all() #remove records with less than or equal 'cutoff' in all numeric var
tbl_remove_less_than_or_equal_any() #remove records with less than or equal 'cutoff' in any one of the numeric var
tbl_remove_rows_zero_all()  #Remove records having all values are 0 in a tbl.
tbl_remove_rows_zero_any()  #Remove records having atleast one 0
tbl_remove_vars_NA_all()    #Remove variables having all values are NA in a tbl.
tbl_remove_vars_NA_any()    #Remove variables having atlease one NA
tbl_remove_vars_zero_all()  #Remove variables having all values are 0 in a tbl.
tbl_remove_vars_zero_any()  #Remove variables having atleast one 0
tbl_replace_greater_than()  #replace numeric values greater than given 'cutoff'
tbl_replace_greater_than_or_equal() #replace numeric values greater than or equal to given 'cutoff'
tbl_replace_less_than() #replace numeric values less than given 'cutoff'
tbl_replace_less_than_or_equal()    #replace numeric values less than or equal given 'cutoff'
tbl_replace_string()    #Replace all occurance of 'pattern' in all non numeric variables. Wrapper around 'stringr::str_replace_all()'.
```
