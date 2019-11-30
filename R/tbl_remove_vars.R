

#' Remove columns / variables having all values are NA
#'
#' Remove columns / variables having all values are NA from a tbl. \code{tbl_remove_vars_NA_all()} takes care of both  numeric and non numeric columns.
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
#'  tbl %>% tbl_remove_vars_NA_all()
#' }
tbl_remove_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(.,na.rm = T)  %>% `!`) ))
        tbl %>% mm()
}

#' Remove columns / variables having all values are 0
#'
#' Remove columns / variables having all values are 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#' @importFrom purrr as_mapper
#' @importFrom dplyr select_if
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% tbl_remove_vars_zero_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4)
#'  tbl2 %>% tbl_remove_vars_zero_all()
#' }
tbl_remove_vars_zero_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.,na.rm = T)  %>% `!`) ))
        tbl %>% mm()
}



#' Remove columns / variables  having atlease one NA
#'
#' Remove columns / variables  having atlease one NA from a tbl.
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
#'  tbl %>% tbl_remove_vars_NA_any()
#' }
tbl_remove_vars_NA_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% any(., na.rm = T)  %>% `!`) ))
        tbl %>% mm()
}





#' Remove columns / variables having atleast one 0
#'
#' Remove columns / variables having atleast one 0 from a tbl.
#'
#' @param tbl a tbl.
#'
#' @return a tbl.
#' @export
#' @importFrom purrr as_mapper
#' @importFrom dplyr select_if
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% tbl_remove_vars_zero_any()
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(0,2,3,NA,5) , xx= 0:4)
#'  tbl %>% tbl_remove_vars_zero_any()
#' }
tbl_remove_vars_zero_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% any(. , na.rm = T)  %>% `!`) ))
        tbl %>% mm()
}
