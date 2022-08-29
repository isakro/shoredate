#' Plot shoreline displacement curves
#'
#' Function for plotting shoreline displacement curves. Calling to plot without providing interpolated curves will display the four underlying curves.
#'
#' @param interpolated_curve List holding one or more interpolated shoreline displacement curves.
#'
#' @return A plot displaying the underlying shoreline displacement curves and, if provided, interpolated curves.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' # Create example point using the required coordinate system WGS84 UTM32N (EPSG: 32632).
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#'
#' # Interpolate shoreline displacement curve to the target point location.
#' target_curve <- interpolate_curve(target_pt)
#'
#' # Call to plot
#' displacement_plot(target_curve)
displacement_plot <- function(interpolated_curve = NA){

  # Load pre-compiled displacement curves
  dispdat =
    get(load(
      system.file("extdata/displacement_curves.rda",
                  package = "shoredate",
                  mustWork = TRUE)))

  plt <- ggplot2::ggplot() +
    ggplot2::scale_colour_manual(values = c("Arendal" = "black",
                                   "Larvik" = "darkgreen",
                                   "Tvedestrand" = "blue",
                                   "Horten" = "darkorange")) +
    ggplot2::ylab("Meters above present sea-level") +
    ggplot2::xlab("BCE/CE") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.direction = "horizontal")

    if(any(is.na(interpolated_curve))){
      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name)) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name))
    } else{
      intcurves <- as.data.frame(do.call(rbind, interpolated_curve))
      intcurves$name <- "Interpolated curve"

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name)) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name)) +
        ggplot2::scale_colour_manual(values = c("Arendal" = "black",
                                                "Larvik" = "darkgreen",
                                                "Tvedestrand" = "blue",
                                                "Horten" = "darkorange",
                                                "Interpolated curve" = "red"))
    }
  plt
}
