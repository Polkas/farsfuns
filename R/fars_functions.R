#' \code{fars_read} checks if the file exists and then load it as data.frame.
#' If the file does not exist function will generate an error.
#'
#' @param filename character - a path to the file or the file name
#'
#' @return load data in a data.frame format
#'
#' @source Coursera Mastering Software Development in R - Johns Hopkins University
#'
#' @seealso \code{\link{make_filename}} \code{\link{fars_read_years}} \code{\link{fars_summarize_years}} \code{\link{fars_map_state}}
#'
#' @examples
#' \dontrun{
#'    fars_read(filename)
#' }
#'
#'
#' @export


fars_read <- function(filename) {
  if(!file.exists(system.file("exdata",filename,package = "farsfuns")))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(system.file("exdata",filename,package = "farsfuns"))
  })
  dplyr::tbl_df(data)
}


#' \code{make_filename} create and print a name of a file with accident data for a certain year.
#'
#' @param year integer or character - a year of the certain file
#'
#' @return a character vector of file name for a certain year
#'
#' @source Coursera Mastering Software Development in R - Johns Hopkins University
#'
#' @seealso \code{\link{fars_read}} \code{\link{fars_read_years}} \code{\link{fars_summarize_years}} \code{\link{fars_map_state}}
#'
#' @examples
#' \dontrun{
#'    make_filename(year)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' \code{fars_read_years} takes a list of years and returns a list of data.frames with month and year columns based on provided files.
#' the files need to be in the working directory.
#'
#' @param years a list or vector of integers/characters - years of origin
#'
#' @return a list of data.frames with month and year columns
#'
#' @source Coursera Mastering Software Development in R - Johns Hopkins University
#'
#' @seealso \code{\link{make_filename}} \code{\link{fars_read}} \code{\link{fars_summarize_years}} \code{\link{fars_map_state}}
#'
#' @examples
#' \dontrun{
#'    fars_read_years(year:(year+n))
#'    fars_read_years(list(year,(year+n)))
#'    fars_read_years(year)
#'    fars_read_years(list(year:(year+n))) #example of error (return NULL)
#' }
#'
#'@import magrittr
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, "year" = year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' \code{fars_summarize_years} return the data.frame with total number of accidents for each month.
#' the files need to be in the working directory.
#'
#' @return a data.frame in a wide format with total number of accidents for each month
#'
#' @source Coursera Mastering Software Development in R - Johns Hopkins University
#'
#' @seealso \code{\link{make_filename}} \code{\link{fars_read_years}} \code{\link{fars_read}} \code{\link{fars_map_state}}
#'
#' @examples
#' \dontrun{
#'    fars_summarize_years(2013:2015)
#'    fars_summarize_years(list(2014:2015)) #example of error
#' }
#'
#' @inheritParams fars_read_years
#'
#'@import magrittr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH")%>%
    dplyr::summarize_("n" = "n()") %>%
    tidyr::spread_("year", "n")
}

#' \code{fars_map_state} filters the data by a certain US state and year and plots a map of accidents in that state.
#' the files need to be in the working directory.
#'
#' @param state.num a number of a state in US as integer
#' @param year a year as integer/character
#'
#' @return a map with locations of accidents for the provided state number and year
#'
#' @source Coursera Mastering Software Development in R - Johns Hopkins University
#'
#' @seealso \code{\link{make_filename}} \code{\link{fars_read_years}} \code{\link{fars_summarize_years}} \code{\link{fars_read}}
#'
#' @examples
#' \dontrun{
#'    fars_map_state(45, 2015)
#'    fars_map_state(2016) #example of error
#'    fars_map_state(60, 2016) #example of error
#' }
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, "STATE == state.num")
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
