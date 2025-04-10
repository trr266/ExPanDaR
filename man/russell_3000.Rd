% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{russell_3000}
\alias{russell_3000}
\title{Annual Financial Accounting and Stock Return Data for a Sample of Russell 3000 Firms (2013-2016)}
\format{
An object of class \code{"data.frame"}.
}
\source{
Has been collected using the \code{tidyquant::tq_get} function family in Summer 2017.
 The code to generate this data is available in the
 \href{https://github.com/trr266/ExPanDaR}{github repository} of this package.
 As the Google Finance API providing financial statement data is currently unavailable,
 the data cannot be replicated by running the code.
 Use in scientific studies is not advised without prior cleaning/checking.
}
\usage{
data(russell_3000)
}
\description{
Data collected from Google Finance and Yahoo finance using the package \code{tidyquant}.
}
\examples{
data(russell_3000)
prepare_missing_values_graph(russell_3000, ts_id = "period")

}
\keyword{acccounting}
\keyword{data}
\keyword{datasets}
