#' Sum shoreline dates
#'
#' Function for finding the summed probability distribution of multiple
#'  shoreline dates.
#'
#' @param shoreline_dates Object of class `shoreline_date`.
#' @param cut_off Calender year specifying where dates should be cut off.
#'  Defaults to 2500 BCE.
#' @param cut_off_level Numerical value between 0 and 1 indicating the
#'  probability mass that has to faller after the cut-off for a date to be
#'  excluded. Defaults to 1, retaining all dates.
#' @param normalise Logical value indicating whether the probability sum of the
#'  dates should be normalised to sum to unity. Defaults to TRUE.
#'
#' @return List of class `shoredate_sum` holding the elements:
#'  * `sum` data frame with the columns `bce` where negative values
#'  indicate years BCE and positive CE, as well as `probability`, which gives
#'  the probability mass for each year.
#'  * `dates_n` number of dates that make up the sum after applying any
#'  specified cut-off. One date per site per isobase direction.
#'
#' @export
#'
#' @examples
#' target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
#'                             sf::st_point(c(538300, 6544250)))
#' target_points <- sf::st_as_sf(target_points, crs = 32632)
#'
#' target_dates <- shoreline_date(target_points,
#'                                elevation = c(65, 70),
#'                                cal_reso = 100)
#'
#' sum_shoredates(target_dates)
sum_shoredates <- function(shoreline_dates, cut_off = -2500,
                           cut_off_level = 1, normalise = TRUE){

  if(cut_off_level < 0 | cut_off_level > 1){
    stop("Probability level for cut-off should be a value between 0 and 1.")
  }

  # Define function to check if date falls before cut-off
  within_date_range <- function(x){
    x$cumulative_prob <-  cumsum(x[,"probability"])

    # Check if year at probability cut-off lies above the threshold
    if (x$bce[min(which(x$cumulative_prob >= cut_off_level))] > cut_off) {
      FALSE
    } else {
      TRUE
    }
  }

  # Recursive function for unnesting. Taken from answer by @ekoam here:
  # https://stackoverflow.com/questions/70512869/extract-data-frames-from-nested-list
  unnest_date <- function(x) {
    if (is.data.frame(x)) {
      return(list(x))
    }
    if (!is.list(x)) {
      return(NULL)
    }
    unlist(lapply(x, unnest_date), FALSE)
  }

  # Check for multiple isobase directions
  if (length(shoreline_dates[[1]]) > 1) {
    multiple_directions <- TRUE
  } else {
    multiple_directions <- FALSE
  }

  # Check if the dates were returned as sparse.
  if (length(shoreline_dates[[1]][[1]]) == 2) {

    # In case of multiple isobase directions, unnest these to a list
    # of data frames
    if (multiple_directions) {
      shoreline_dates <- lapply(shoreline_dates,
                                function(x){lapply(x, as.data.frame)})
      dates_dfs <- unnest_date(shoreline_dates)

    } else {
      # Dates as list of dates data frames
      dates_dfs <- lapply(shoreline_dates, as.data.frame)
    }

    # Exclude dates that fall after the cut-off
    dates_dfs <- dates_dfs[which(sapply(dates_dfs, within_date_range))]

    ndates <- length(dates_dfs)

    # Combine dates into a single data frame
    sdates <- do.call(rbind, dates_dfs)

    # Sum probability by year
    sdates <-  aggregate(sdates$probability,
                         by = list(bce = sdates$bce), FUN = sum)

  # If the dates are not sparse
  } else {

    # Check for multiple isobase directions
    if (multiple_directions) {
      shoreline_dates <- unnest_date(shoreline_dates)
      dates_dfs <- shoreline_dates[names(shoreline_dates) %in% "date"]

    } else {

      dates_list <- lapply(shoreline_dates, unnest_date)

      # Retrieve date data frame from each list
      dates_dfs <- sapply(dates_list, function(x) x["date"])
    }

    # Select dates that fall before cut-off
    dates_dfs <- dates_dfs[which(sapply(dates_dfs, within_date_range))]

    ndates <- length(dates_dfs)

    # Collapse the retrieved data frames
    sdates <- do.call(rbind, dates_dfs)
    sdates <-  aggregate(sdates$probability,
                         by = list(bce = sdates$bce), FUN = sum)
  }

  # Normalise sum of dates to sum to unity
  if (normalise) {
    sdates$probability <- sdates$x/sum(sdates$x)
    sdates <- sdates[, c("bce", "probability")]

  # If not, only rename columns
  } else {
    names(sdates) <- c("bce", "probability")
  }

  # Return sum and number of dates within threshold
  result <- list(
    sum = sdates,
    dates_n = ndates
  )

  class(result) <- c("shoredates_sum", class(result))

  result
}

