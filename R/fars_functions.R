#' Read FARS Data
#'
#' \code{fars_read} reads FARS into the enviroment
#'
#' This Function reads data from teh US National Highway Trafffice Safety
#' Administrations' Fatality Analysis reporting System (FARS), give a
#' filename for the data. It returns a tibble of the data. For this function
#' to work properly, a filename pointing to an existing file must be give.
#'
#' @param filename A character string giving the file of the FARS data.
#' @return This funciton returns a tibble containting the FARS data. If an
#'   incorrect filename is entered the function will stop.
#' @examples
#' full_filename <- system.file('extdata', 'accident_2013.csv.bz2', package = farsdata')
#' fars_read(filename = full_filename)
#'
#' \dontrun{fars_read(filename = 'filedoesnotexist)
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS files for one or more years.
#'
#' \code{fars_read_years} produces a list of tibbles of FARS data, given an
#'   input vector of years.
#'
#' This function takes a vector of years and produces a list of tibbles,
#'   where each tibble is that year's FARS file year and MONTH observations.
#'   This is a simple function that strips all useful data out of the FARS
#'   tables and produces a completely useless tibble, but is meant for
#'   practice in the Coursera course.  For this function to work, valid
#'   years should be entered. Invalid years will have a NULL entry in the
#'   returned list.
#'
#' @param years Vector of years' FARS files to open.  Vector members must be
#'    an integer, or a string or numeric that can be coerced to a string,
#'    of the year of interest.
#' @importFrom magrittr "%>%"
#' @return This function returns a list of tibbles, where each tibble contains
#'   containing the year and month from the observations in the corresponding
#'   year's FARS data.  If an invalid year is given, the corresponding
#'   list will be NULL.
#' @examples
#' fars_read_years(years = c(2013, 2014, 2015))
#' fars_read_years(years = 2013)
#'
#' \dontrun{
#' fars_read_years(years = 2000) # error
#' }
#'
#' @export

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Produce a Summary of FARS Files.
#'
#' \code{fars_summarize_years} produces a summary tibble of FARS years and
#'   months given a vector of years.
#'
#' This function takes a vector of years, pulls the FARS data for
#'   those years, and then produces a summary tibble. The summary tibble shows
#'   the number of observations for each month/year combination for the
#'   extracted FARS data. For this function to work properly, the years must
#'   be years with valid data.
#'
#' @param years Vector of years' FARS files to open.  Vector members must be
#'    an integer, or a string or numeric that can be coerced to a string.
#' @return This function returns tibble where the first column is the month,
#'   the second and following columns are the requested years, and the
#'   rows for the year columns are the number of FARS observations for
#'   that month/year combination.  The returned columns are only for years
#'   with valid FARS data. If no valid years are found, the function
#'   well error out.
#' @examples
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2013)
#'
#' \dontrun{
#' fars_summarize_years(years = 2000)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map State Motor Vehicle Fatalities.
#'
#' \code{fars_map_state} maps state motor vehicle fatalities given a year and
#'   state id number.
#'
#' This function takes a state number and a year, and draws
#'   a state outline with dots to represent the location of motor vehicle
#'   fatalities for that year.  This function will throw an error if an invalid
#'   state number is chosen or the chosen year's data does not exist.
#'
#'   You must have library(mapdata) loaded in your namespace for this to work.
#'
#' @param state.num Numerical code for US state.
#' @param year  An integer, or a string or numeric that can be coerced to a string,
#'   of the year of interest.
#' @return NULL
#' @examples
#' library(mapdata)
#' fars_map_state(12, 2014)
#' fars_map_state(36, 2014)
#'
#' \dontrun{
#' fars_map_state(3, 2014)   # error
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
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
