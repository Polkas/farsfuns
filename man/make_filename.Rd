% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{\code{make_filename} create and print a name of a file with accident data for a certain year.}
\source{
Coursera Mastering Software Development in R - Johns Hopkins University
}
\usage{
make_filename(year)
}
\arguments{
\item{year}{integer or character - a year of the certain file}
}
\value{
a character vector of file name for a certain year
}
\description{
\code{make_filename} create and print a name of a file with accident data for a certain year.
}
\examples{
\dontrun{
   make_filename(year)
}

}
\seealso{
\code{\link{fars_read}} \code{\link{fars_read_years}} \code{\link{fars_summarize_years}} \code{\link{fars_map_state}}
}
