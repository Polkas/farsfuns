% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{\code{fars_map_state} filters the data by a certain US state and year and plots a map of accidents in that state.
the files need to be in the working directory.}
\source{
Coursera Mastering Software Development in R - Johns Hopkins University
}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{a number of a state in US as integer}

\item{year}{a year as integer/character}
}
\value{
a map with locations of accidents for the provided state number and year
}
\description{
\code{fars_map_state} filters the data by a certain US state and year and plots a map of accidents in that state.
the files need to be in the working directory.
}
\examples{
\dontrun{
   fars_map_state(45, 2015)
   fars_map_state(2016) #example of error
   fars_map_state(60, 2016) #example of error
}


}
\seealso{
\code{\link{make_filename}} \code{\link{fars_read_years}} \code{\link{fars_summarize_years}} \code{\link{fars_read}}
}
