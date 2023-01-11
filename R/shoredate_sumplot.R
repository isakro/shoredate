#' Plot the probability sum of multiple shoreline dates
#'
#' Function to plot the sum of the probabilities of multiple shoreline dates as
#' resulting from running `sum_shoredates()`.
#'
#' @param shoredates_sum Object of class `shoredates_sum`.
#'
#' @return A line plot showing the provided probability sum and number of dates.
#' @export
#'
#' @import ggplot2
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
#' target_sum <- sum_shoredates(target_dates)
#'
#' shoredate_sumplot(target_sum)
shoredate_sumplot <- function(shoredates_sum){

  if (!inherits(shoredates_sum, "shoredates_sum")) {
    stop("Sum to be plotted must be of class shoredates_sum, as returned from sum_shoredates()")
  }

  ggplot2::ggplot(shoredates_sum$sum) +
    ggplot2::geom_line(ggplot2::aes(x = .data$bce, y = .data$probability)) +
    ggplot2::labs(y = "Meters above present sea-level",
                  x = "Shoreline date (BCE/CE)") +
    ggplot2::labs(x = "BCE/CE", y = "Summed probability",
                  subtitle = paste("Number of sites:",
                                      shoredates_sum$dates_n)) +
    ggplot2::theme_bw()
}
