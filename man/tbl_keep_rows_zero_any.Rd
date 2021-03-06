% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_keep_rows.R
\name{tbl_keep_rows_zero_any}
\alias{tbl_keep_rows_zero_any}
\title{Subset rows having atleast one 0}
\usage{
tbl_keep_rows_zero_any(tbl)
}
\arguments{
\item{tbl}{a tbl.}
}
\value{
a tbl.
}
\description{
Subset rows having atleast one 0 from a tbl.
}
\examples{
\dontrun{

 tbl <- tibble(x = c(0,1,2,0,3,5) , y = c(0,1,2,0,3,5) , z = c(0,1,2,0,3,5) )
 tbl
 tbl \%>\% tbl_keep_rows_zero_any()

 tbl2 <- tibble(x = letters[1:5] , y = NA , z = c(0,2,3,NA,5) , xx= 0:4)
 tbl2 \%>\% tbl_keep_rows_zero_any()
}
}
