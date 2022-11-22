#' Plot shoreline date
#'
#' Function for plotting shoreline date along with associated metadata.
#'
#' @param shorelinedates A list of objects returned from shoreline_date().
#' @param elevation_distribution Logical value indicating whether the gamma distribution should be displayed. Default is TRUE.
#' @param displacement_curve Logical value indicating whether the displacement curve should be displayed. Default is TRUE.
#' @param parameters Logical value indicating whether the parameters of the gamma distribution should be displayed. Default is FALSE.
#' @param isobase_direction  Logical value indicating whether the direction of the isobases should be displayed. Default is FALSE.
#' @param highest_density_region Logical value indicating whether the 95\% highest density region should be displayed. Defaults to TRUE.
#' @param hdr_label Logical value indicating whether the numeric values for the highest density regions should be displayed. Default is TRUE.
#' @param greyscale Logical value indicating whether the plot should be in greyscale or not. Defaults to FALSE.
#'
#' @return A plot displaying the provided shoreline date.
#' @seealso \code{\link{shoreline_date}}, \code{\link{annotate_custom}}
#' @export
#'
#' @import ggplot2
#' @import ggridges
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
#'
#' target_date <- shoreline_date(site = target_point, elevation = 46,
#'                               isobase_direction = c(327,338))
#'
#' shoredate_plot(target_date, isobase_direction = TRUE)
shoredate_plot <- function(shorelinedates,
                           elevation_distribution = TRUE,
                           displacement_curve = TRUE,
                           parameters = FALSE,
                           isobase_direction = FALSE,
                           highest_density_region = TRUE,
                           hdr_label = TRUE,
                           greyscale = FALSE){


  if(greyscale){
    dispcol <- sitedistcol <- "black"
    dispfill <- NA
  } else{
    dispcol <- dispfill <- "red"
    sitedistcol <- "#046c9a"
  }


  plts <- list()
  for(k in 1:length(shorelinedates)){

    iso_shoredate <- shorelinedates[[k]]

    # List to hold plots per site in case of multiple isobase directions
    iso_plts <- list()
    for(j in 1:length(iso_shoredate)){

      # Check if results are passed listed or not.
      if(inherits(iso_shoredate[[j]], "list")){
        nshoredate <- iso_shoredate[[j]]
      } else{
        nshoredate <- iso_shoredate[j]
      }

      # Remove zeroes for visualisation
      dategrid <- nshoredate$date[nshoredate$date$probability > 0,]

      plt <- ggplot2::ggplot(data = dategrid) +
      ggridges::geom_ridgeline(ggplot2::aes(x = .data$bce, y = 1,
                       height = .data$probability * 10000/nshoredate$cal_reso),
                               colour = NA, fill = "darkgrey", alpha = 0.7) +
      ggplot2::labs(y = "Meters above present sea-level",
                    x = "Shoreline date (BCE)") +
        ggplot2::theme_bw()

      if(highest_density_region){
        hdrs <- shoredate_hdr(nshoredate)

        # hdr_list <- vector("list", length(hdrs))
        # for (i in 1:length(hdrs)) {
        #   hdr_list[[i]] <- data.frame(
        #     min = min(hdrs[[i]]),
        #     max = max(hdrs[[i]]),
        #     hdr = i
        #   )
        # }
        # hdr_df <- do.call(rbind, hdr_list)

        plt <- plt +
          ggplot2::geom_segment(data = hdrs, ggplot2::aes(x = .data$start,
                                                              xend = .data$end,
                                                              y = 0, yend = 0),
                                col = "black")

      }


      paramval <- as.numeric(nshoredate$model_parameters)
      dirval <- unique(nshoredate$dispcurve$direction)
      if(parameters & isobase_direction){
        plt <- plt +
          ggplot2::ggtitle(bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2]) ~
                                    "Isobase direction =" ~ .(dirval)))
      } else if(parameters){
        plt <- plt +
          ggplot2::ggtitle(bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2])))
      } else if(isobase_direction){
        plt <- plt +
          ggplot2::ggtitle(bquote("Isobase direction =" ~ .(dirval)))
      }

      if(displacement_curve){
        plt <- plt +
          ggplot2::theme(legend.position = "None") +
          ggplot2::geom_ribbon(data = nshoredate$dispcurve,
                               ggplot2::aes(x = .data$bce,
                                            ymin = .data$lowerelev,
                                            ymax = .data$upperelev),
                               fill = dispfill, alpha = 0.2) +
          ggplot2::geom_line(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          y = .data$upperelev),
                             colour = dispcol) +
          ggplot2::geom_line(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          y = .data$lowerelev),
                             colour = dispcol) +
          ggplot2::scale_x_continuous(expand = c(0,0),
                                      limits = c(min(dategrid$bce) - 1250,
                                                 max(dategrid$bce) + 250)) +
          ggplot2:: coord_cartesian(ylim = c(0,
                                             as.numeric(nshoredate$elev) + 10))
      }

      if(elevation_distribution){

        # For plotting purposes to close the geom_polygon on the y-axis
        gammadatg <- rbind(c(0, 0, 0), nshoredate$gammadat)

        # Code taken from oxcAAR to plot density to y-axis
        x_extent <-
          ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
        gammadatg$probs_scaled <- gammadatg$probs / max(gammadatg$probs) *
          diff(x_extent) * 20 + x_extent[1]


        plotdat <- data.frame(x = c(gammadatg$probs_scaled,
                                    x_extent[1]:x_extent[2]),
                              y = c(as.numeric(
                                nshoredate$elev) - gammadatg$offset,
                                    rep(as.numeric(nshoredate$elev),
                                        length(x_extent[1]:x_extent[2]))))

        plt <- plt + ggplot2::geom_polygon(data = plotdat,
                                            ggplot2::aes(x = .data$x,
                                        y = .data$y), col = sitedistcol,
                                 fill = sitedistcol, alpha = 0.6)
      }

      if(hdr_label & highest_density_region){


        label_hdrs <- ""
        for(i in 1:nrow(hdrs)){
          label_hdrs <- paste0(label_hdrs, hdrs$start[i]," to ",
                              hdrs$end[i], " BCE\n")
        }
        # for(i in 1:nrow(hdrs)){
        #   label_hdrs <- paste0(label_hdrs, round(hdrs$start[i])," to ",
        #                        round(hdrs$end[i]), " BCE\n")
        # }
        label_text <- paste0("95% HDR:\n", label_hdrs)

       plt <- plt + annotate_custom(label_text, x = 0.9, y = 0.9, hjust = 0)
      }
      iso_plts[[j]] <- plt
    }
    plts[[k]] <- iso_plts
  }

  suppressWarnings(print(patchwork::wrap_plots(flattenlist(plts), ncol = 1)))
}
