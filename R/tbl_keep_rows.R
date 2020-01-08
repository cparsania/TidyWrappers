
#' Subset rows having atleast one NA
#'
#' Subset rows having atleast one NA across all columns / variables from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,NA) , y = c(0,1,NA,0,3,NA) , z = c(0,1,2,0,3,NA) )
#'  tbl
#'  tbl %>% tbl_keep_rows_NA_any()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_keep_rows_NA_any()
#' }
tbl_keep_rows_NA_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_all(dplyr::any_vars(is.na(.)) ))
        tbl %>% mm()
}



#' Subset rows having all NA
#'
#' Subset rows having all NA across all columns / variables from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,NA) , y = c(0,1,NA,0,3,NA) , z = c(0,1,2,0,3,NA) )
#'  tbl
#'  tbl %>% tbl_keep_rows_NA_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_keep_rows_NA_all()
#' }
tbl_keep_rows_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_all(dplyr::all_vars(is.na(.)) ))
        tbl %>% mm()
}


#' Subset rows having all values are 0
#'
#' Subset rows having all values are 0 across all numeric columns / variables from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% tbl_keep_rows_zero_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_keep_rows_zero_all()
#' }
tbl_keep_rows_zero_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_if(is.numeric , dplyr::all_vars( . == 0) ))
        tbl %>% mm()
}


#' Subset rows having atleast one 0
#'
#' Subset rows having atleast one 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% tbl_keep_rows_zero_any()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = NA , z = c(0,2,3,NA,5) , xx= 0:4)
#'  tbl2 %>% tbl_keep_rows_zero_any()
#' }
tbl_keep_rows_zero_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_if(is.numeric , dplyr::any_vars( . == 0) ))
        tbl %>% mm()
}




#' Keep rows with less than or equal \code{'cutoff'} in any numeric column
#'
#' Keep rows with less than or equal \code{'cutoff'} in any numeric column from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as \code{'cutoff'}.
#'
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = seq(1,10,by=2))
#' tbl %>% tbl_keep_rows_less_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_rows_less_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. <= !!..2)) )
        tbl %>% mm(cutoff)
}



#' Keep rows with less than or equal \code{'cutoff'} in all numeric columns
#'
#' Keep rows with less than or equal \code{'cutoff'} in all numeric columns from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as cutoff.
#'
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#'
#' @return a tbl
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = seq(1,10,by=2))
#' tbl %>% tbl_keep_rows_less_than_or_equal_all(cutoff =4)
#' }
#'
tbl_keep_rows_less_than_or_equal_all <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. <= !!..2)) )
        tbl %>% mm(cutoff)
}



#' Keep rows with greater than or equal \code{'cutoff'} in any numeric column
#'
#' Keep rows with greater than or equal \code{'cutoff'} in any numeric column from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as cutoff.
#'
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = seq(1,10,by=2))
#' tbl %>% tbl_keep_rows_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_rows_greater_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}


#' Keep rows with greater than or equal  \code{'cutoff'} in all numeric columns
#'
#' Keep rows with greater than or equal  \code{'cutoff'} in all numeric columns from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as  \code{'cutoff'}.
#'
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = seq(1,10,by=2))
#' tbl %>% tbl_keep_rows_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_keep_rows_greater_than_or_equal_all <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}
