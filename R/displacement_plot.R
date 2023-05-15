#' Plot shoreline displacement curves
#'
#' Function for plotting shoreline displacement curves. Calling to plot without
#'  providing a target curve will display the four underlying geologically
#'  derived displacement curves.
#'
#' @param target_curve Data frame holding a shoreline displacement curve.
#' @param displacement_curves Character vector specifying which geologically
#'  informed displacement curves should be plotted. Accepted values are
#'  `c("Horten", "Porsgrunn", "Tvedestrand", "Arendal")`. All are included by
#'  default.
#' @param target_name Character value specifying the name that is given to the
#'  target curve, if provided. Defaults to `"Target curve"`.
#' @param target_line Character value specifying the linetype that is used for
#' the target curve, if this is provided. Defaults to `"solid"`.
#' @param target_col Character value specifying the colour that is used for the
#'  target curve, if this is provided. Defaults to `"red"`.
#' @param target_alpha Numerical value specifying the alpha value that is used
#' for the target curve, if this is provided. Defaults to `1`.
#' @param displacement_line Character vector specifying the linetypes that are
#'  used for the geological displacement curves to be plotted. Defaults to
#'  `c("Horten" = "solid", "Porsgrunn" = "solid", "Tvedestrand" = "solid",
#'  "Arendal" = "solid")`.
#' @param displacement_col Character vector specifying the colours that are
#'  used for the geological displacement curves to be plotted. Defaults to
#'  `c("Horten" = "darkorange", "Porsgrunn" = "darkgreen", "Tvedestrand" =
#'  "blue", "Arendal" = "black")`.
#' @param displacement_alpha Numerical value specifying the alpha value that are
#'  used for all of the geological displacement curves to be plotted. Defaults
#'  to 1.
#' @param greyscale Logical value indicating whether the plot should be in
#'  greyscale or not. Defaults to `FALSE`.
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
                              target_name = "Target curve",
                              target_line = "solid",
                              target_col = "red",
                              target_alpha = 1,
                              displacement_line = c("Horten" = "solid",
                                                    "Porsgrunn" = "solid",
                                                    "Tvedestrand" = "solid",
                                                    "Arendal" = "solid"),
                              displacement_col = c("Horten" = "darkorange",
                                                    "Porsgrunn" = "darkgreen",
                                                    "Tvedestrand" = "blue",
                                                    "Arendal" = "black"),
                              displacement_alpha = 1,
                              greyscale = FALSE){

  # Load pre-compiled geological displacement curves
  dispdat <-
    get(load(
      system.file("extdata/displacement_curves.rda",
                  package = "shoredate",
                  mustWork = TRUE)))

  geocurves <- c("Horten", "Porsgrunn", "Tvedestrand", "Arendal")

  # To be used for making sure that the curves are always given in the same order
  if(all(is.na(target_curve))){
    plotorder <- c("Horten", "Porsgrunn", "Tvedestrand", "Arendal")
  } else{
    plotorder <- c("Horten", "Porsgrunn", "Tvedestrand", "Arendal", target_name)
  }

  dispdat <- dispdat[dispdat$name %in% displacement_curves,]

  if (greyscale) {
    if (all(is.na(target_curve))) {

      colour_scheme <- c("Horten" = "black",
                         "Porsgrunn" = "black",
                         "Tvedestrand" = "black",
                         "Arendal" = "black")

      line_scheme <- c("Horten" = "twodash",
                       "Porsgrunn" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Arendal" = "longdash")

      alpha_scheme <- stats::setNames(rep(displacement_alpha, 4),
                                      geocurves)

    } else {

      colour_scheme <- c("Horten" = "black",
                         "Porsgrunn" = "black",
                         "Tvedestrand" = "black",
                         "Arendal" = "black",
                         stats::setNames("black", target_name))

      line_scheme <- c("Horten" = "twodash",
                       "Porsgrunn" = "dashed",
                       "Tvedestrand" = "dotted",
                       "Arendal" = "longdash",
                       stats::setNames("solid", target_name))

      alpha_scheme <- c(stats::setNames(rep(displacement_alpha, 4),
                                        geocurves),
                        stats::setNames(target_alpha, target_name))
    }
  } else {

      if (all(is.na(target_curve))) {

        colour_scheme <- displacement_col

        line_scheme <- displacement_line

        alpha_scheme <- stats::setNames(rep(displacement_alpha, 4),
                                        geocurves)

      } else {

        colour_scheme <- c(displacement_col,
                           stats::setNames(target_col, target_name))

        line_scheme <- c(displacement_line,
                         stats::setNames(target_line, target_name))

        alpha_scheme <- c(stats::setNames(rep(displacement_alpha, 4),
                                          geocurves),
                          stats::setNames(target_alpha, target_name))
    }
  }


  plt <- ggplot2::ggplot() +
    ggplot2::ylab("Meters above present sea-level") +
    ggplot2::xlab("BCE/CE") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.direction = "horizontal")

    if (all(is.na(target_curve))) {

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name,
                                        alpha = .data$name),
                                        na.rm = TRUE) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name,
                                        alpha = .data$name),
                                        na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = colour_scheme,
                                     breaks = plotorder) +
        ggplot2::scale_alpha_manual(values = alpha_scheme,
                                    breaks = plotorder) +
        ggplot2::scale_linetype_manual(values = line_scheme,
                                       breaks = plotorder)

    } else {
      # Unpack if displacement curve is a list of data frames
      if (inherits(target_curve, "list")) {
        target_curve <- do.call(rbind.data.frame, target_curve)
      }

      target_curve$name <- target_name

      limit_scheme <- c(unique(dispdat$name), target_name)

      plt <- plt +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = .data$name,
                                        linetype = .data$name,
                                        alpha = .data$name),
                           alpha = displacement_alpha, na.rm = TRUE) +
        ggplot2::geom_line(data = dispdat,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = .data$name,
                                        linetype = .data$name,
                                        alpha = .data$name),
                                        na.rm = TRUE) +
        ggplot2::geom_line(data = target_curve,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev,
                                        col = as.factor(.data$name),
                                        linetype = as.factor(.data$name),
                                        alpha = .data$name),
                                        na.rm = TRUE) +
        ggplot2::geom_line(data = target_curve,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev,
                                        col = as.factor(.data$name),
                                        linetype = as.factor(.data$name),
                                        alpha = .data$name),
                                        na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = colour_scheme,
                                     limits = limit_scheme,
                                     breaks = plotorder) +
        ggplot2::scale_linetype_manual(values = line_scheme,
                                       limits = limit_scheme,
                                       breaks = plotorder) +
        ggplot2::scale_alpha_manual(values = alpha_scheme,
                                    limits = limit_scheme,
                                    breaks = plotorder)
    }
  plt
}
