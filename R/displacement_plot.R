#' Plot shoreline displacement curves
#'
#' Function for plotting shoreline displacement curves. Calling to plot without providing interpolated curves will display the four underlying curves.
#'
#' @param interpolated_curve List holding one or more interpolated shoreline displacement curves.
#' @param greyscale Logical value indicating whether the plot should include colours or not. Defaults to FALSE.
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
displacement_plot <- function(interpolated_curve = NA, greyscale = FALSE){

  # Load pre-compiled displacement curves
  dispdat <-
    get(load(
      system.file("extdata/displacement_curves.rda",
                  package = "shoredate",
                  mustWork = TRUE)))

  if(greyscale){
    if(is.na(interpolated_curve)){
      colour_scheme <- c("Arendal" = "black",
                         "Larvik" = "black",
                         "Tvedestrand" = "black",
                         "Horten" = "black")

      line_scheme <- c("Arendal" = "solid",
                       "Larvik" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Horten" = "twodash")
    } else{
      colour_scheme <- c("Arendal" = "black",
                         "Larvik" = "black",
                         "Tvedestrand" = "black",
                         "Horten" = "black",
                         "Interpolated curve" = "black")

      line_scheme <- c("Arendal" = "1342",
                       "Larvik" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Horten" = "twodash",
                       "Interpolated curve" = "solid")
    }
  } else{
      if(is.na(interpolated_curve)){
      colour_scheme <- c("Arendal" = "black",
                "Larvik" = "darkgreen",
                "Tvedestrand" = "blue",
                "Horten" = "darkorange")

      line_scheme <- c("Arendal" = "solid",
                       "Larvik" = "solid",
                       "Tvedestrand" = "solid",
                       "Horten" = "solid")

      } else{
        colour_scheme <- c("Arendal" = "black",
                  "Larvik" = "darkgreen",
                  "Tvedestrand" = "blue",
                  "Horten" = "darkorange",
                  "Interpolated curve" = "red")

        line_scheme <- c("Arendal" = "solid",
                         "Larvik" = "solid",
                         "Tvedestrand" = "solid",
                         "Horten" = "solid",
                         "Interpolated curve" = "solid")
    }
  }


  plt <- ggplot2::ggplot() +
    ggplot2::scale_colour_manual(values = colour_scheme) +
    ggplot2::scale_linetype_manual(values = line_scheme) +
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
                                        col = .data$name,
                                        linetype = .data$name)) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name))
    } else{
      intcurves <- as.data.frame(do.call(rbind, interpolated_curve))
      intcurves$name <- "Interpolated curve"

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name),
                           alpha = 0.4) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name)) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name)) +
        ggplot2::scale_colour_manual(values = colour_scheme) +
        ggplot2::scale_linetype_manual(values = line_scheme)
    }
  plt
}
