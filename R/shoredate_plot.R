#' Plot shoreline date
#'
#' Function for plotting shoreline date along with associated metadata.
#'
#' @param shorelinedate A list of objects returned from shoreline_date().
#' @param site_elevation Logical value indicating whether the site elevation and exponential decay function should be displayed. Default is TRUE.
#' @param displacement_curve Logical value indicating whether the displacement curve should be displayed. Default is TRUE.
#' @param lambda Logical value indicating whether the lambda value for the exponential decay function should be displayed. Default is TRUE.
#' @param hdr_label Logical value indicating whether the numeric values for the highest density regions should be displayed. Default is TRUE.
#'
#' @return A plot displaying the provided shoreline date.
#' @seealso \code{\link{shoreline_date}}
#' @export
#'
#' @import ggplot2
#' @import ggridges
#' @import grid
#' @import patchwork
#'
#' @examples
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#' target_date <- shoreline_date(site = target_pt, elevation = 65)
#' shoredate_plot(target_date)
shoredate_plot <- function(shorelinedate,
                           site_elevation = TRUE,
                           displacement_curve = TRUE,
                           lambda = TRUE,
                           isobase_direction = TRUE,
                           hdr_label = TRUE){

  plts <- list()
  for(k in 1:length(shorelinedate)){

    nshoredate <- shorelinedate[[k]]

    hdrdates <- shoredate_hdr(nshoredate)
    # Visualisation cut-off
    dategrid <- nshoredate$date[nshoredate$date$probability > 0.000009,]

    plt <- ggplot2::ggplot(data = dategrid) +
    ggridges::geom_ridgeline(ggplot2::aes(x = .data$bce, y = 1,
                                          height = .data$probability * 15000),
                             colour = NA, fill = "darkgrey", alpha = 0.7) +
    ggplot2::geom_segment(data = hdrdates, ggplot2::aes(x = .data$start,
                                                        xend = .data$end,
                                      y = 0, yend = 0), col = "black") +
    ggplot2::labs(y = "Meters above present sea-level",
                  x = "Shoreline date (BCE)") +
      ggplot2::theme_bw()

    if(lambda == TRUE & isobase_direction = TRUE){
      plt <- plt +
        ggplot2::ggtitle(paste0("\U03BB = ", as.numeric(nshoredate$expratio),
              ", Isobase direction = ", unique(nshoredate$dispcurve$direction),
                                              "°"))
    } else if(lambda == TRUE){
      plt <- plt +
        ggplot2::ggtitle(paste("\U03BB =", as.numeric(nshoredate$expratio)))
    }

    if(displacement_curve == TRUE){
      plt <- plt +
        ggplot2::theme(legend.position = "None") +
        ggplot2::geom_ribbon(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          ymin = .data$lowerelev,
                                          ymax = .data$upperelev),
                             fill = "red", alpha = 0.2) +
        ggplot2::geom_line(data = nshoredate$dispcurve,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$upperelev, col = "red")) +
        ggplot2::geom_line(data = nshoredate$dispcurve,
                           ggplot2::aes(x = .data$bce,
                                        y = .data$lowerelev, col = "red"))+
        ggplot2::scale_x_continuous(expand = c(0,0),
                                    limits = c(min(dategrid$bce) - 1000,
                                               max(dategrid$bce) + 1000)) +
        ggplot2:: coord_cartesian(ylim = c(0,
                                           as.numeric(nshoredate$elev) + 30))
    }

    if(site_elevation == TRUE){

      # For plotting purposes to close the geom_polygon on the y-axis
      expdatg <- rbind(c(0, 0, 0), nshoredate$expdat)

      # Code taken from oxcAAR to plot density to y-axis
      x_extent <-
        ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
      expdatg$probs_scaled <- expdatg$probs / max(expdatg$probs) *
        diff(x_extent)/6 + x_extent[1]

      plt <- plt + ggplot2::geom_polygon(data = expdatg,
                                          ggplot2::aes(x =.data$probs_scaled,
                                      y = as.numeric(nshoredate$elev) -
                                        .data$offset),
                               fill = "#046c9a", alpha = 0.6) +
      ggplot2::geom_hline(yintercept = as.numeric(nshoredate$elev),
                          linetype = "dashed",
                 col = "#046c9a", alpha = 0.6)
    }

    if(hdr_label == TRUE){

      label_text <- "95% HDR:\n"
      for(i in 1:nrow(hdrdates)){
        label_text <- paste(label_text, round(hdrdates$start[i]),"to",
                            round(hdrdates$end[i]), "BCE\n")
      }

      grob <- grid::grobTree(grid::textGrob(label_text, x = 0.7,
                                            y = 0.7, hjust = 0,
                              gp = grid::gpar(fontsize = 8,
                                              fontface = "italic")))
      plt <- plt + ggplot2::annotation_custom(grob)
    }

    plts[[k]] <- plt
  }
  patchwork::wrap_plots(plts, ncol = 1)
}
