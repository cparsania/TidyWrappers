

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
#'  tbl %>% tbl_remove_zero_vars()
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

#' Apply log10 on numeric variables
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

        mm <- purrr::as_mapper(~ (.x %>% dplyr::mutate_if(is.numeric , ~ log10(. +  !!.y) )))

        tbl %>% mm(frac)
}


#' Apply log on numeric variables
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

        mm <- purrr::as_mapper(~ (..1 %>% dplyr::mutate_if(is.numeric , ~ log(. +  !!..2 ,base = !!..3) )))

        tbl %>% mm(frac , base = base)
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




#' remove records with less than or equal \code{cutoff} in any one of the numeric var
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
#' tbl %>% tbl_remove_less_than_or_equal_any(cutoff =4)
#' }
#'
tbl_remove_less_than_or_equal_any <- function(tbl, cutoff){
        mm <- purrr::as_mapper(~ ..1  %>% dplyr::filter_if(is.numeric, all_vars(. > !!..2)  ))
       tbl %>% mm(cutoff)
}


#' remove records with less than or equal \code{cutoff} in all numeric var
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
#' tbl %>% tbl_remove_less_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_less_than_or_equal_all <- function(tbl, cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. > !!..2)) )
        tbl %>% mm(cutoff)
}



#' remove records with greater than or equal \code{cutoff} in any numeric var
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
#' tbl %>% tbl_remove_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_remove_greater_than_or_equal_any <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}


#' remove records with greater than or equal \code{cutoff} in all numeric var
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
#' tbl %>% tbl_remove_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_remove_greater_than_or_equal_all <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. < !!..2)) )
        tbl %>% mm(cutoff)
}



#' keep records with less than or equal \code{cutoff} in any numeric var
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
#' tbl %>% tbl_keep_less_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_less_than_or_equal_any <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. <= !!..2)) )
        tbl %>% mm(cutoff)
}


#' keep records with less than or equal \code{cutoff} in all numeric var
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
#' tbl %>% tbl_keep_less_than_or_equal_all(cutoff =4)
#' }
#'
tbl_keep_less_than_or_equal_all <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. <= !!..2)) )
        tbl %>% mm(cutoff)
}


#' keep records with greater than or equal \code{cutoff} in any numeric var
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
#' tbl %>% tbl_keep_greater_than_or_equal_any(cutoff =4)
#' }
#'
tbl_keep_greater_than_or_equal_any <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, any_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}

#' keep records with greater than or equal \code{cutoff} in all numeric var
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
#' tbl %>% tbl_keep_greater_than_or_equal_all(cutoff =4)
#' }
#'
tbl_keep_greater_than_or_equal_all <- function(tbl , cutoff){
        mm <- purrr::as_mapper(~ ..1  %>%  dplyr::filter_if(is.numeric, all_vars(. >= !!..2)) )
        tbl %>% mm(cutoff)
}


#' Replace all occurance of \code{pattern} in all non numeric variables. Wrapper around \code{stringr::str_replace_all()}.
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


#' Scale numeric vars (columns).
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

        mm <- purrr::as_mapper(~ ..1  %>% dplyr::mutate_if(is.numeric , ~ scale(. , scale = !!..2, center = !!..3) %>% .[,1]))
        tbl %>% mm(scale, center)
}


#' Scale numeric records (rows).
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








