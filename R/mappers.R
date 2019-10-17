

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
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl %>% tbl_count_zero_vars()
#' }
tbl_count_zero_vars <- function( tbl) {
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
#' \dontrun{
#'
#'
#'  tbl <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% remove_zero_vars()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4)
#'  tbl2 %>% tbl_remove_zero_vars()
#' }
tbl_remove_zero_vars <- function(tbl){
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
#' \dontrun{
#'
#'  tbl <- tibble::tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0 )
#'  tbl
#'  tbl %>% get_zero_vars()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_get_zero_vars()
#' }
tbl_get_zero_vars <- function(tbl){
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
#' \dontrun{
#'
#'  tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
#'  tbl
#'  tbl %>% tbl_get_zero_records()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_get_zero_records()
#' }
tbl_get_zero_records <- function(tbl){
        mm <- purrr::as_mapper(~ .x %>% dplyr::filter_if(is.numeric , dplyr::all_vars( . == 0) ))
        tbl %>% mm()
}



#' Remove records having all values are 0 in a tbl.
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
#'  tbl %>% tbl_remove_zero_records()
#'
#'  tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
#'  tbl2 %>% tbl_remove_zero_records()
#' }
tbl_remove_zero_records <- function(tbl){
        mm <- as_mapper(~ .x %>% filter_if(is.numeric , any_vars(. > 0 )))
        tbl %>% mm()
}



#' Apply log2 on numeric variables
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

        mm <- purrr::as_mapper(~ (.x %>% dplyr::mutate_if(is.numeric , ~ log2 (. +  !!.y) )))

        tbl %>% mm(frac)
}


#' replace numeric values less than given \code{cutoff}
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
        mm <-  purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. < !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff, replace_by)
}


#' replace numeric values less than or equal given \code{cutoff}
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
        mm <-  purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. <= !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff, replace_by)
}


#' replace numeric values greater than given \code{cutoff}
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

        mm <- purrr::as_mapper(~ ..1 %>% mutate_if(is.numeric , ~ if_else(. > !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff , replace_by)
}


#' replace numeric values greater than or equal to given \code{cutoff}
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
        mm <- purrr::as_mapper(~ ..1 %>% dplyr::mutate_if(is.numeric , ~ if_else(. >= !!..2 , !!..3 , as.double(.) ) ))
        tbl %>% mm(cutoff , replace_by)
}



#' Replace first occurance of \code{pattern} in all variables. Wrapper around \code{stringr::str_replace_all()}.
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

        mm <- purrr::as_mapper(~ ..1 %>% dplyr::mutate_if(is.character, ~ stringr::str_replace_all( . ,pattern = !!..2 ,
                                                                                                replacement = !!..3)))
        tbl %>% mm(pattern , replacement)
}







