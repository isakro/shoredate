#' Plot the summed probability distribution of multiple shoreline dates
#'
#' Function to plot the sum of the probabilities of multiple shoreline dates as
#' resulting from running `sum_shoredates()`.
#'
#' @param shoredates_sum Object of class `shoredates_sum`.
#' @param sample_size Logical indicating whether or not to display the number of
#'  summed dates on the plot. Defaults to TRUE.
#'
#' @return A line plot showing the provided summed probability distribution.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
#'                             sf::st_point(c(538300, 6544250)))
#' target_points <- sf::st_as_sf(target_points, crs = 32632)
#'
#' # Shoreline date, reducing resolution on elevation and calendar scales for
#' # speed.
#' target_dates <- shoreline_date(target_points,
#'                                elevation = c(65, 70),
#'                                elev_reso = 10,
#'                                cal_reso = 750)
#'
#' target_sum <- sum_shoredates(target_dates)
#'
#' shoredate_sumplot(target_sum)
shoredate_sumplot <- function(shoredates_sum, sample_size = TRUE){

  if (!inherits(shoredates_sum, "shoredates_sum")) {
    stop("Sum to be plotted must be of class shoredates_sum, as returned from sum_shoredates()")
  }

  plt <- ggplot2::ggplot(shoredates_sum$sum) +
    ggplot2::geom_line(ggplot2::aes(x = .data$bce, y = .data$probability)) +
    ggplot2::labs(x = "BCE/CE", y = "Summed probability") +
    ggplot2::theme_bw()

  if (sample_size) {
    plt <- plt + ggplot2::labs(subtitle = paste("Summed dates = ",
                                         shoredates_sum$dates_n))
  }

  plt
}
