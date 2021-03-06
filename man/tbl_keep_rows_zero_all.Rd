% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_keep_rows.R
\name{tbl_keep_rows_zero_all}
\alias{tbl_keep_rows_zero_all}
\title{Subset rows having all values are 0}
\usage{
tbl_keep_rows_zero_all(tbl)
}
\arguments{
\item{tbl}{a tbl.}
}
\value{
a tbl.
}
\description{
Subset rows having all values are 0 across all numeric columns / variables from a tbl.
}
\examples{
\dontrun{

 tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
 tbl
 tbl \%>\% tbl_keep_rows_zero_all()

 tbl2 <- tibble(x = letters[1:5] , y = LETTERS[1:5] , z = 0  , xx = 0:4 , yy = 0)
 tbl2 \%>\% tbl_keep_rows_zero_all()
}
}
