

#' Count variables having all values are 0 in a tbl.
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
#' @importFrom purrr as_mapper
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  ggplot2::mpg %>% count_zero_vars()
#'
#'  ###########################################
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl %>% count_zero_vars()
#' }
count_zero_vars <- function( tbl) {
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.)) ) %>% ncol() )
        tbl %>% mm()
}


#' Remove variables having all values are 0 in a tbl.
#'
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#' @importFrom purrr as_mapper
#' @importFrom dplyr select_if
#' @examples
#' \donotrun{
#'
#'  ggplot2::mpg %>% remove_zero_vars()
#'
#'  ###########################################
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% remove_zero_vars()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4)
#'  tbl2 %>% remove_zero_vars()
#' }
remove_zero_vars <- function(tbl){
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.)  %>% `!`) ))
        tbl %>% mm()
}

#' Get variable (names) having all values are 0 in a tbl.
#'
#'
#' @param tbl a tbl.
#'
#' @return a cheracter vector contaning variable names
#' @importFrom purrr as_mapper
#' @importFrom dplyr select_if
#' @export
#'
#' @examples
#' \donotrun{
#'  ggplot2::mpg %>% get_zero_vars()
#'
#'  ###########################################
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% get_zero_vars()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% get_zero_vars()
#' }
get_zero_vars <- function(tbl){
        mm <-   purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.)) ) %>% colnames() )
        tbl %>% mm()
}


#' Subset records (rows) having all values are 0 in a tbl.
#'
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \donotrun{
#'  ggplot2::mpg %>% get_zero_records()
#'
#'  ###########################################
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% get_zero_vars()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% get_zero_vars()
#' }
get_zero_records <- function(tbl){
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_if(is.numeric , dplyr::all_vars( . == 0) ))
        tbl %>% mm()
}



#' remove_all_zero_rows
#'
#'
#'
#' From the given tibble function removes all the rows in which all the numeric values are 0. Tibble of remaining rows will be returned.
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
remove_zero_records <- function(tbl){
        mm <- as_mapper(~ .x %>% filter_if(is.numeric , any_vars(. > 0 )))
        tbl %>% mm()
}



#' add_frac_to_numeric_vals
#'
#'
#' The given numeric value will be added into all the numeric columns of the given tibble and resulted values will be log2
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
add_log2_frac_to_numeric_vals <- function(tbl , frac){

        mm <- as_mapper(~ (.x %>% mutate_if(is.numeric , ~ log2 (. +  !!.y) )))

        tbl %>% mm(frac)
}


# Pooja edit
# replace_less_than <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. < !!..2 , !!..3 , as.double(.) ) ))
#
# mpg %>% replace_less_than(2, 200)
#
# replace_less_than_or_equal <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. <= !!..2 , !!..3 , as.double(.) ) ))
#
# mpg %>% replace_less_than_or_equal(2, 200)
#
# replace_greater_than <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. > !!..2 , !!..3 , as.double(.) ) ))
#
# mpg %>% replace_greater_than(2, 200)
#
# replace_greater_than_or_equal <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. >= !!..2 , !!..3 , as.double(.) ) ))
#
# mpg %>% replace_greater_than_or_equal(2, 200)
#





