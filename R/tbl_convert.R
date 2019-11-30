


#' Apply \code{log2()} on numeric columns / variables
#'
#' Apply \code{log2()} on numeric columns / variables from a tbl.
#'
#' @param tbl a tbl.
#' @param frac a numeric value to be added before applying log2.
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl.
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




#' Apply \code{log10()} on numeric columns / variables
#'
#' Apply \code{log10()} on numeric columns / variables from a tbl.
#'
#' @param tbl a tbl.
#' @param frac a numeric value to be added before applying log10.
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl.
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



#' Apply \code{log()} on numeric columns / variables
#'
#' Apply \code{log()} on numeric columns / variables from a tbl.
#'
#'
#' @param tbl a tbl.
#' @param base a positive number with respect to which log are computed.
#' @param frac a numeric value to be added before applying log.
#'
#' @importFrom purrr as_mapper
#' @importFrom dplyr mutate_if
#' @return a tbl.
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




#' Scale numeric columns
#'
#' Scale numeric columns from a tbl. It uses \code{base::scale()} to scale the numeric vector.
#'
#' @param tbl a tbl.
#' @param scale TRUE  look at \code{scale} argument of function base::scale().
#' @param center TRUE look at \code{center} argument of function base::scale().
#'
#' @return a tbl.
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



#' Scale rows
#'
#' Scale rows considering numeric columns from a tbl.
#'
#' @param tbl a tbl.
#' @param scale TRUE  for further details refer  \code{scale} argument of function \code{\link[base]{scale}}.
#'   base::scale().
#' @param center TRUE for further details refer \code{center} argument of function \code{\link[base]{scale}}.
#'
#' @return a tbl.
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
