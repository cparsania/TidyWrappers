
#' Keep columns / variables having all values are NA
#'
#' Keep columns / variables having all values are NA from a tbl. \code{tbl_keep_vars_NA_all()} takes care of both  numeric and non numeric columns.
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'  tbl <- tibble::tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_keep_vars_NA_all()
#' }
#'
#' @seealso \code{\link[TidyWrappers]{tbl_remove_vars_NA_all}}
tbl_keep_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(.,na.rm = T)) ))
        tbl %>% mm()
}


#' Keep columns / variables having all values are 0
#'
#' Keep columns / variables having all values are 0 from a tbl.
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% tbl_keep_vars_zero_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4)
#'  tbl2 %>% tbl_keep_vars_zero_all()
#' }
#'
#' @seealso \code{\link[TidyWrappers]{tbl_remove_vars_zero_all}}
tbl_keep_vars_zero_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.,na.rm = T)) ))
        tbl %>% mm()
}


#' Keep columns / variables  having atlease one NA
#'
#' Keep columns / variables  having atlease one NA from a tbl. \code{tbl_keep_vars_NA_any()} takes care of both  numeric and non numeric columns.
#' @param tbl a tbl.
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'  tbl <- tibble::tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_keep_vars_NA_any()
#' }
#' @seealso \code{\link[TidyWrappers]{tbl_remove_vars_NA_any}}
tbl_keep_vars_NA_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% any(.,na.rm = T)) ))
        tbl %>% mm()
}

#' Keep columns / variables having atleast one 0
#'
#' Keep columns / variables having atleast one 0 from a tbl.
#'dev
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 ,a = 0:4 , b = 1:5)
#'  tbl
#'  tbl %>% tbl_keep_vars_zero_any()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4)
#'  tbl2 %>% tbl_keep_vars_zero_any()
#' }
#'
#' @seealso \code{\link[TidyWrappers]{tbl_remove_vars_zero_any}}
tbl_keep_vars_zero_any <- function(tbl){

        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% any(.,na.rm = T)) ))
        tbl %>% mm()
}
