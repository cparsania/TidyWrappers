% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_remove_rows.R
\name{tbl_remove_rows_NA_any}
\alias{tbl_remove_rows_NA_any}
\title{Remove rows having atleast one NA}
\usage{
tbl_remove_rows_NA_any(tbl)
}
\arguments{
\item{tbl}{a tbl.}
}
\value{
a tbl.
}
\description{
Remove rows having atleast one NA across all columns / variables from a tbl.
}
\examples{
\dontrun{

 tbl <- tibble(x = c(0,1,2,0,3,NA) , y = c(0,1,NA,0,3,NA) , z = c(0,1,2,0,3,NA) )
 tbl
 tbl \%>\% tbl_remove_rows_NA_any()

 tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
 tbl2 \%>\% tbl_remove_rows_NA_any()
}
}
