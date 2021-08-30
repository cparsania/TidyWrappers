

#' Replace numeric values less than given \code{'cutoff'}
#'
#' Replace numeric values less than given \code{'cutoff'} from a tbl.
#'
#' @param tbl  a tbl.
#' @param cutoff a numeric value to be used as a cutoff.
#' @param replace_by a numeric value to be used to replace less than \code{cutoff}.
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



#' Replace numeric values less than or equal given '\code{'cutoff'}
#'
#' Replace numeric values less than or equal given '\code{'cutoff'} from a tbl.
#'
#' @param tbl  a tbl.
#' @param cutoff a numeric value to be used as a \code{'cutoff'}.
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


#' Replace numeric values greater than given \code{'cutoff'}
#'
#' Replace numeric values less than or equal given \code{'cutoff'} from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff a numeric value to be used as a \code{'cutoff'}.
#' @param replace_by a numeric value to be used to replace greater than \code{'cutoff'}.
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


#' Replace numeric values greater than or equal to given \code{'cutoff'}
#'
#' Replace numeric values greater than or equal to given \code{'cutoff'} from a tbl.
#'
#' @param tbl a tbl.
#' @param cutoff numeric value to be used as a \code{'cutoff'}.
#' @param replace_by a numeric value to be used to replace greater than or equal \code{'cutoff'}.
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



#' Wrapper around \code{stringr::str_replace_all()}
#'
#' Replace all occurance of \code{'pattern'} in all non numeric columns from a tbl.
#'
#' @param tbl a tbl.
#' @param pattern pattern to look for. Will be passed to argument \code{pattern} to function \code{stringr::str_replace_all()}.
#' @param replacement a character vector of replacements. Will be passed to argument \code{replacement} to function \code{stringr::str_replace_all()}.
#'
#' @return a tbl.
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

#' @title  Replace numeric value in a tibble.
#' @description In a given tibble a numeric value can be replaced by another numeric value.
#' @param tbl a tbl.
#' @param value a numeric which is to be replaced from \code{tbl}.
#' @param replace_by a numeric replacement for \code{value}.
#' @return a tbl
#' @export
#' @examples
#' \dontrun{
#' set.seed(1234)
#' tbl <- tibble::tibble(x = c(letters[1:5] ,letters[1:5]),
#'                        y = sample(1:5, 10 ,replace = T),
#'                        z = sample(1:5, 10,replace = T))
#'  tbl %>% tbl_replace_numeric(2, 1000)
#'  tbl %>% tbl_replace_numeric(4, 1000)
#' }
#'
tbl_replace_numeric <- function(tbl, value , replace_by){

        if ( !is_tibble(tbl)  ) stop("tbl is not tbl")
        stopifnot(is.numeric(value))
        stopifnot(is.numeric(replace_by))

        mm <- purrr::as_mapper(~ ..1 %>% dplyr::mutate_if(is.numeric , ~ dplyr::if_else(. == !!..2 , !!..3 , as.double(.) )))
        tbl %>% mm(value , replace_by)
}
