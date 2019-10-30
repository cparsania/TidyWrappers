#' Count columns having all values are NA.
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_count_vars_NA_all()
#' }
tbl_count_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(. ,na.rm = T)) ) %>% ncol() )
        tbl %>% mm()
}

#' Remove columns having all values are NA.
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_remove_vars_NA_all()
#' }
tbl_remove_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (is.na(.)) %>% all(.,na.rm = T)  %>% `!`) ))
        tbl %>% mm()
}

#' Get columns names having all values are NA.
#'
#'
#' @param tbl a tbl.
#'
#' @return a cheracter vector contaning variable names
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) )
#'  tbl %>% tbl_get_vars_NA_all()
#' }
tbl_get_vars_NA_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-   purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( is.na(.) %>% all(.,na.rm = T)) ) %>% colnames() )
        tbl %>% mm()
}


#' Count columns having all values are 0.
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl %>% tbl_count_vars_zero_all()
#' }
tbl_count_vars_zero_all <- function( tbl) {
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.,na.rm = T)) ) %>% ncol() )
        tbl %>% mm()
}


#' Remove columns having all values are 0.
#'
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

#' Get columns names having all values are 0.
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
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% tbl_get_vars_zero_all()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_get_vars_zero_all()
#' }
tbl_get_vars_zero_all <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-   purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% all(.,na.rm = T)) ) %>% colnames() )
        tbl %>% mm()
}


#' Subset rows having all values are 0.
#'
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



#' Remove rows having all values are 0.
#'
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




#' Count columns having atleast one NA.
#'
#' @param tbl a tbl.
#'
#' @return an integer
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

#' Remove columns having atlease one NA
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
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

#' Get columns names having atleast one NA.
#'
#'
#' @param tbl a tbl.
#'
#' @return a cheracter vector contaning variable names
#' @importFrom purrr as_mapper
#' @importFrom tibble is_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = NA , z = c(1,2,3,NA,5) , xx= 1:5)
#'  tbl %>% tbl_get_vars_NA_any()
#' }
tbl_get_vars_NA_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-   purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( is.na(.) %>% any(. , na.rm = T)) ) %>% colnames() )
        tbl %>% mm()
}


#' Count columns having atleast one 0.
#'
#'
#' @param tbl a tbl.
#'
#' @return an integer
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


#' Remove columns having atleast one 0
#'
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

#' Get columns names having atleast one 0.
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
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% tbl_get_vars_zero_any()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = NA , z = c(0,2,3,NA,5) , xx= 0:4)
#'  tbl2 %>% tbl_get_vars_zero_any()
#' }
tbl_get_vars_zero_any <- function(tbl){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-   purrr::as_mapper(~ .x %>% dplyr::select_if( ~ ( (. == 0) %>% any(. , na.rm = T)) ) %>% colnames() )
        tbl %>% mm()
}


#' Subset rows having atleast one 0.
#'
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



#' Remove rows having atleast one 0.
#'
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



#' Apply log2 on numeric columns.
#'
#' @param tbl a tbl
#' @param frac a numeric value to be added before applying log2
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 )
#'  tbl
#'  tbl %>% tbl_convert_log2()
#'  tbl %>% tbl_convert_log2(frac = 0.01)
#'
#' }
tbl_convert_log2 <- function(tbl , frac = 0){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ (.x %>% dplyr::mutate_if(is.numeric , ~ log2 (. +  !!.y) )))

        tbl %>% mm(frac)
}

#' Apply log10 on numeric columns.
#'
#' @param tbl a tbl
#' @param frac a numeric value to be added before applying log10
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 )
#'  tbl
#'  tbl %>% tbl_convert_log10()
#'  tbl %>% tbl_convert_log10(frac = 0.01)
#'
#' }
tbl_convert_log10 <- function(tbl , frac = 0){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ (.x %>% dplyr::mutate_if(is.numeric , ~ log10(. +  !!.y) )))

        tbl %>% mm(frac)
}


#' Apply log on numeric columns.
#'
#' @param tbl a tbl
#' @param base a positive number with respect to which log are computed
#' @param frac a numeric value to be added before applying log
#'
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 )
#'  tbl
#'  tbl %>% tbl_convert_log(base = 3)
#'  tbl %>% tbl_convert_log(base = 3 , frac = 0.01)
#'
#' }
tbl_convert_log <- function(tbl , frac = 0, base = 2){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ (..1 %>% dplyr::mutate_if(is.numeric , ~ log(. +  !!..2 ,base = !!..3) )))

        tbl %>% mm(frac , base = base)
}


#' Replace numeric values less than given 'cutoff'.
#'
#' @param tbl  a tbl.
#' @param cutoff a numeric value to be used as a cutoff.
#' @param replace_by a numeric value to be used to replace less than \code{cutoff}
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = 5:1)
#' tbl %>% tbl_replace_less_than(4, 0 )
#' }
#'
tbl_replace_less_than <- function(tbl, cutoff, replace_by ){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. < !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff, replace_by)
}


#' Replace numeric values less than or equal given 'cutoff'.
#'
#' @param tbl  a tbl.
#' @param cutoff a numeric value to be used as a cutoff.
#' @param replace_by a numeric value to be used to replace less than or equal \code{cutoff}
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = 5:1)
#' tbl %>% tbl_replace_less_than_or_equal(4, 0 )
#' }
#'
tbl_replace_less_than_or_equal <- function(tbl, cutoff, replace_by ){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <-  purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. <= !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff, replace_by)
}


#' Replace numeric values greater than given 'cutoff'.
#'
#' @param tbl a tbl.
#' @param cutoff a numeric value to be used as a cutoff.
#' @param replace_by a numeric value to be used to replace greater than \code{cutoff}
#'
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = 5:1)
#' tbl %>% tbl_replace_greater_than(2, 1000 )
#' }
#'
tbl_replace_greater_than <- function(tbl, cutoff, replace_by){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. > !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff , replace_by)
}


#' Replace numeric values greater than or equal to given 'cutoff'.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as a cutoff.
#' @param replace_by a numeric value to be used to replace greater than or equal \code{cutoff}
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 1:5 , w = 5:1)
#' tbl %>% tbl_replace_greater_than_or_equal(2, 1000 )
#' }#'
#'
#'
tbl_replace_greater_than_or_equal <- function(tbl, cutoff, replace_by){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1 %>% dplyr::mutate_if(is.numeric , ~ if_else(. >= !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff , replace_by)
}




#' Remove rows with less than or equal 'cutoff' in any numeric column.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
#' @importFrom dplyr filter_if
#' @importFrom dplyr any_vars
#' @importFrom purrr as_mapper
#' @return a tbl
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


#' Remove rows with less than or equal 'cutoff' in all numeric columns.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_remove_rows_less_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_rows_less_than_or_equal_all <- function(tbl, cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. > !!..2)) )
        tbl %>% mm(cutoff)
}



#'  Remove rows with greater than or equal 'cutoff' in any numeric column.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_remove_rows_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_remove_rows_greater_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}


#' Remove rows with greater than or equal 'cutoff' in all numeric columns.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_remove_rows_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_rows_greater_than_or_equal_all <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}



#' Keep rows with less than or equal 'cutoff' in any numeric column.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_keep_rows_less_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_rows_less_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. <= !!..2)) )
        tbl %>% mm(cutoff)
}


#' Keep rows with less than or equal 'cutoff' in all numeric columns.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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


#' Keep rows with greater than or equal 'cutoff' in any numeric column.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_keep_rows_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_rows_greater_than_or_equal_any <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}

#' Keep rows with greater than or equal 'cutoff' in all numeric columns.
#'
#' @param tbl a tbl
#' @param cutoff numeric value to be used as cutoff
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
#' tbl %>% tbl_keep_rows_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_keep_rows_greater_than_or_equal_all <- function(tbl , cutoff){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}


#' Replace all occurance of 'pattern' in all non numeric columns. Wrapper around 'stringr::str_replace_all()'.
#'
#' @param tbl a tbl.
#' @param pattern pattern to look for. Will be passed to argument \code{pattern} to function \code{stringr::str_replace_all()}
#' @param replacement a character vector of replacements. Will be passed to argument \code{replacement} to function \code{stringr::str_replace_all()}
#'
#' @return a tbl
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#'
#' \dontrun{
#'  tbl <- tibble::tibble(x = c(letters[1:5] ,letters[1:5] ) ,
#'          y = c(LETTERS[1:5],LETTERS[1:5]) , z = 1:10 )
#'  tbl %>% tbl_replace_string(pattern = "a" , replacement = "a_changed")
#'  tbl %>% tbl_replace_string("[aeiou]", toupper)
#'  tbl %>% tbl_replace_string("([aeiou])", "")
#'  tbl %>% tbl_replace_string(c("a", "e", "i"), "-")
#'  tbl %>% tbl_replace_string("b", NA_character_)
#'
#' }
#' @seealso stringr::str_replace_all()
#'
tbl_replace_string <- function(tbl, pattern, replacement){

        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        mm <- purrr::as_mapper(~ ..1 %>% dplyr::mutate_if(is.character, ~ stringr::str_replace_all( . ,pattern = !!..2 ,
                                                                                                replacement = !!..3)))
        tbl %>% mm(pattern , replacement)
}


#' Scale numeric columns.
#'
#' @param tbl a tbl.
#' @param scale TRUE  look at \code{scale} argument of function base::scale()
#' @param center TRUE look at \code{center} argument of function base::scale()
#'
#' @return a tbl
#' @export
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @examples
#' \dontrun{
#' tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) , zz = letters[1:6])
#' tbl %>% tbl_convert_column_zscore()
#' }
#'
#'
tbl_convert_column_zscore <- function(tbl, scale = TRUE, center = TRUE){

        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")


        mm <- purrr::as_mapper(~ ..1  %>% dplyr::mutate_if(is.numeric , ~ scale(. , scale = !!..2, center = !!..3) %>% .[,1]))
        tbl  %>% mm(scale, center)
}


#' Scale rows considering numeric columns.
#'
#' @param tbl a tbl.
#' @param scale TRUE  for further details refer  \code{scale} argument of function \code{\link[base]{scale}}.
#'   base::scale()
#' @param center TRUE for further details refer \code{center} argument of function \code{\link[base]{scale}}.
#'
#' @return a tbl
#' @export
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select_if
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select_if
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom rlang syms
#' @seealso  \code{\link[base]{scale}}
#' @examples
#' \dontrun{
#' set.seed(12345)
#' tbl <- tibble(x = sample(c(1:10) , 5) , y = sample(c(1:10) , 5) , z = sample(c(1:10) , 5) , zz = letters[1:5])
#' tbl %>% tbl_convert_row_zscore()
#' }
#'
#'
tbl_convert_row_zscore <- function(tbl, scale = TRUE, center = TRUE){
        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")

              .f =   function(...){

                       ## create necessary vars
                        row_names_var <- paste0("row_names_",stringi::stri_rand_strings(n = 1, length = 10 ,pattern = '[a-zA-Z]'))
                        key_var <- paste0("key_",stringi::stri_rand_strings(n = 1, length = 10 ,pattern = '[a-zA-Z]'))
                        value_var <- paste0("value_",stringi::stri_rand_strings(n = 1, length = 10 ,pattern = '[a-zA-Z]'))
                        zscore_var <- paste0("zscore_",stringi::stri_rand_strings(n = 1, length = 10 ,pattern = '[a-zA-Z]'))

                        ## get input tbl vars
                        all_vars <- ..1 %>% colnames()
                        numeric_vars <- ..1 %>% dplyr::select_if(is.numeric ) %>% colnames()

                        ..1 %>%
                                tibble::rownames_to_column(var = row_names_var) %>%
                                tidyr::pivot_longer(cols = c(numeric_vars) , names_to = key_var ,values_to = value_var) %>%
                                dplyr::group_by(!!rlang::sym(row_names_var)) %>%
                                dplyr::mutate(!!rlang::sym(zscore_var) := scale(!!rlang::sym(value_var) , scale = ..2 , center = ..3)) %>%
                                ungroup() %>% dplyr::select(-!!rlang::sym(value_var)) %>%
                                tidyr::pivot_wider(names_from = !!rlang::sym(key_var) ,
                                                   values_from = c(!!rlang::sym(zscore_var)))%>%
                                dplyr::select(!!! rlang::syms(all_vars))
                }


        .f(..1 = tbl , ..2 = scale, ..3 = center)


}








