
#' Remove rows having all values are 0.
#'
#' Remove rows having all values are 0 across all numeric columns / variables from a tbl.
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% tbl_remove_rows_zero_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_remove_rows_zero_all()
#' }
tbl_remove_rows_zero_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- as_mapper(~ .x %>% filter_if(is.numeric , any_vars(. > 0 )))
        tbl %>% mm()
}





#' Remove rows having atleast one 0
#'
#' Remove rows having atleast one 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% tbl_remove_rows_zero_any()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 4:0  , xx = 0:4)
#'  tbl2 %>% tbl_remove_rows_zero_any()
#' }
tbl_remove_rows_zero_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- as_mapper(~ .x %>% filter_if(is.numeric , all_vars(. > 0 )))
        tbl %>% mm()
}





#' Remove rows with less than or equal \code{'cutoff'} in any numeric column
#'
#' Remove rows with less than or equal \code{'cutoff'} in any numeric column from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as \code{'cutoff'}.
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = seq(1,10,by=2))
#' tbl %>% tbl_remove_rows_less_than_or_equal_any(cutoff =4)
#' }
#'
tbl_remove_rows_less_than_or_equal_any <- function(tbl, cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>% dplyr::filter_if(is.numeric, all_vars(. > !!..2)  ))
        tbl %>% mm(cutoff)
}


#' Remove rows with less than or equal \code{'cutoff'} in all numeric columns
#'
#' Remove rows with less than or equal \code{'cutoff'} in all numeric columns from a tbl.
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
#' tbl %>% tbl_remove_rows_less_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_rows_less_than_or_equal_all <- function(tbl, cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. > !!..2)) )
        tbl %>% mm(cutoff)
}





#' Remove rows with greater than or equal \code{'cutoff'} in any numeric column
#'
#' Remove rows with greater than or equal \code{'cutoff'} in any numeric column from a tbl.
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
#' tbl %>% tbl_remove_rows_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_remove_rows_greater_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}



#' Remove rows with greater than or equal \code{'cutoff'} in all numeric columns
#'
#' Remove rows with greater than or equal \code{'cutoff'} in all numeric columns from a tbl.
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
#' tbl %>% tbl_remove_rows_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_rows_greater_than_or_equal_all <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}
