#' Plot shoreline displacement curves
#'
#' Function for plotting shoreline displacement curves. Calling to plot without
#'  providing a target curve will display the four underlying geologically
#'  derived displacement curves.
#'
#' @param target_curve List holding one or more shoreline displacement curves.
#' @param displacement_curves Character vector specifying which geologically
#'  informed displacement curves that should be plotted. Accepted values are
#'  `c("Horten", "Porsgrunn", "Tvedestrand", "Arendal")`. All are included by
#'  default.
#' @param greyscale Logical value indicating whether the plot should be in
#'  greyscale or not. Defaults to FALSE.
#'
#' @return A plot displaying the underlying shoreline displacement curves and,
#'  if provided, a target curve.
#' @export
#'
#' @examples
#' # Empty plot for speed
#' displacement_plot(displacement_curves = "")
#'
displacement_plot <- function(target_curve = NA,
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
    if (is.na(target_curve)) {

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
                         "Target curve" = "black")

      line_scheme <- c("Horten" = "twodash",
                       "Porsgrunn" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Arendal" = "longdash",
                       "Target curve" = "solid")
    }
  } else {

      if (is.na(target_curve)) {

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
                  "Target curve" = "red")

        line_scheme <- c("Horten" = "solid",
                        "Porsgrunn" = "solid",
                         "Tvedestrand" = "solid",
                         "Arendal" = "solid",
                         "Target curve" = "solid")
    }
  }


  plt <- ggplot2::ggplot() +
    ggplot2::ylab("Meters above present sea-level") +
    ggplot2::xlab("BCE/CE") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.direction = "horizontal")

    if (any(is.na(target_curve))) {

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
      intcurves <- as.data.frame(do.call(rbind, target_curve))
      intcurves$name <- "Target curve"

      limit_scheme <- c(unique(dispdat$name), "Target curve")

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
