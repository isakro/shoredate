#' @title Plot shoreline displacement curves
#'
#' @description Function for plotting shoreline displacement curves. Calling to plot without providing interpolated curves will display the
#'
#' @param interpolated_curve List holding one or more interpolated shoreline displacement curves.
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#' target_curve <- interpolate_curve(target_pt)
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
                           ggplot2::aes(x = bce, y = upperelev, col = name)) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = bce, y = lowerelev, col = name))
    } else{
      intcurves <- as.data.frame(do.call(rbind, interpolated_curve))
      intcurves$name <- "Interpolated curve"

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = bce, y = upperelev, col = name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = bce, y = lowerelev, col = name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = bce, y = upperelev, col = name)) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = bce, y = lowerelev, col = name)) +
        ggplot2::scale_colour_manual(values = c("Arendal" = "black",
                                                "Larvik" = "darkgreen",
                                                "Tvedestrand" = "blue",
                                                "Horten" = "darkorange",
                                                "Interpolated curve" = "red"))
    }
}
