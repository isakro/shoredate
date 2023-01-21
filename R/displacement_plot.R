#' Plot shoreline displacement curves
#'
#' Function for plotting shoreline displacement curves. Calling to plot without
#'  providing interpolated curves will display the four underlying curves.
#'
#' @param interpolated_curve List holding one or more interpolated shoreline
#'  displacement curves.
#'@param displacement_curves Character vector specifying which geologically
#'  informed displacement curves that should be plotted. Accepted values are
#'  `c("Horten", "Porsgrunn", "Tvedestrand", "Arendal")`. All are included by
#'  default.
#' @param greyscale Logical value indicating whether the plot should include
#'  colours or not. Defaults to FALSE.
#'
#' @return A plot displaying the underlying shoreline displacement curves and,
#'  if provided, an interpolated curve.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' # Empty plot for speed of the example
#' displacement_plot(displacement_curves = "")
#'
displacement_plot <- function(interpolated_curve = NA,
                              displacement_curves = c("Horten", "Porsgrunn",
                                                     "Tvedestrand", "Arendal"),
                              greyscale = FALSE){

  # Load pre-compiled geological displacement curves
  dispdat <-
    get(load(
      system.file("extdata/displacement_curves.rda",
                  package = "shoredate",
                  mustWork = TRUE)))

  dispdat <- dispdat[dispdat$name %in% displacement_curves,]

  if (greyscale) {
    if (is.na(interpolated_curve)) {

      colour_scheme <- c("Horten" = "black",
                         "Porsgrunn" = "black",
                         "Tvedestrand" = "black",
                         "Arendal" = "black")

      line_scheme <- c("Horten" = "twodash",
                       "Porsgrunn" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Arendal" = "longdash")
    } else {

      colour_scheme <- c("Horten" = "black",
                         "Porsgrunn" = "black",
                         "Tvedestrand" = "black",
                         "Arendal" = "black",
                         "Interpolated curve" = "black")

      line_scheme <- c("Horten" = "twodash",
                       "Porsgrunn" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Arendal" = "longdash",
                       "Interpolated curve" = "solid")
    }
  } else {

      if (is.na(interpolated_curve)) {

      colour_scheme <- c("Horten" = "darkorange",
                         "Porsgrunn" = "darkgreen",
                         "Tvedestrand" = "blue",
                         "Arendal" = "black")

      line_scheme <- c("Horten" = "solid",
                       "Porsgrunn" = "solid",
                       "Tvedestrand" = "solid",
                       "Arendal" = "solid")

      } else {

        colour_scheme <- c("Horten" = "darkorange",
                           "Porsgrunn" = "darkgreen",
                  "Tvedestrand" = "blue",
                  "Arendal" = "black",
                  "Interpolated curve" = "red")

        line_scheme <- c("Horten" = "solid",
                        "Porsgrunn" = "solid",
                         "Tvedestrand" = "solid",
                         "Arendal" = "solid",
                         "Interpolated curve" = "solid")
    }
  }


  plt <- ggplot2::ggplot() +
    ggplot2::ylab("Meters above present sea-level") +
    ggplot2::xlab("BCE/CE") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.direction = "horizontal")

    if (any(is.na(interpolated_curve))) {

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name),
                                        na.rm = TRUE) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name),
                                        na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = colour_scheme) +
        ggplot2::scale_linetype_manual(values = line_scheme)

    } else {
      intcurves <- as.data.frame(do.call(rbind, interpolated_curve))
      intcurves$name <- "Interpolated curve"

      limit_scheme <- c(unique(dispdat$name), "Interpolated curve")

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name,),
                           alpha = 0.4, na.rm = TRUE) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name),
                           alpha = 0.4, na.rm = TRUE) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = as.factor(.data$name),
                                        linetype = as.factor(.data$name)),
                           na.rm = TRUE) +
        ggplot2::geom_line(data = intcurves,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = as.factor(.data$name),
                                        linetype = as.factor(.data$name)),
                           na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = colour_scheme,
                                     limits = limit_scheme) +
        ggplot2::scale_linetype_manual(values = line_scheme,
                                       limits = limit_scheme)
    }
  plt
}
