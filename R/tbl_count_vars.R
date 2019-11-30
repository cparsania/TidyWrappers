#' Count columns / variables having all values are NA
#'
#' Count columns / variables having all values are NA from a tbl. \code{tbl_count_vars_NA_all()} takes care of both numeric and non-numeric values.
#'
#' @param tbl a tbl.
#'
#' @return an integer.
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_count_vars_NA_all()
#' }
tbl_count_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(. ,na.rm = T)) ) %>% ncol() )
        tbl %>% mm()
}


#' Count columns / variables having all values are 0
#'
#' Count columns / variables having all values are 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return an integer.
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl %>% tbl_count_vars_zero_all()
#' }
tbl_count_vars_zero_all <- function( tbl) {
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.,na.rm = T)) ) %>% ncol() )
        tbl %>% mm()
}

#' Count columns / variables having atleast one NA
#'
#' Count columns / variables having atleast one NA from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return an integer.
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) , xx= 1:5)
#'  tbl %>% tbl_count_vars_NA_any()
#' }
tbl_count_vars_NA_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% any(. , na.rm = T)) ) %>% ncol() )
        tbl %>% mm()
}




#' Count columns / variables having atleast one 0
#'
#' Count columns / variables having atleast one 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return an integer.
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(0,2,3,NA,5) , xx= 0:4)
#'  tbl %>% tbl_count_vars_zero_any()
#' }
tbl_count_vars_zero_any <- function( tbl) {
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% any(. ,na.rm = T )) ) %>% ncol() )
        tbl %>% mm()
}
