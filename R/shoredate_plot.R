#' Plot shoreline date
#'
#' Function for plotting shoreline dates along with associated metadata.
#'
#' @param shorelinedates Object of class `shoreline_date`.
#' @param elevation_distribution Logical value indicating whether the gamma
#'  distribution should be displayed. Default is TRUE.
#' @param displacement_curve Logical value indicating whether the displacement
#'  curve should be displayed. Default is TRUE.
#' @param parameters Logical value indicating whether the parameters of the
#'  gamma distribution should be displayed. Default is FALSE.
#' @param isobase_direction  Logical value indicating whether the direction of
#'  the isobases should be displayed. Default is FALSE.
#' @param highest_density_region Logical value indicating whether the 95%
#'  highest density region should be displayed. Defaults to TRUE.
#' @param hdr_label Logical value indicating whether the numeric values for the
#'  highest density regions should be displayed. Default is TRUE.
#' @param multiplot Logical value indicating whether multiple dates should be
#'  plotted individually, or be collapsed into a single plot. Default is FALSE.
#' @param greyscale Logical value indicating whether the plot should be in
#'  greyscale or not. Defaults to FALSE.
#'
#' @details `shoredate_plot()` returns a plot displaying the provided shoreline
#'  dates. A single plot is created for each date, where a range of settings can
#'  be adjusted to display or hide various parameters and results. Setting the
#'  parameter `multiplot` to `TRUE` returns a sparser version for multiple
#'  dates, where the only option is whether or not to display the highest
#'  density region in addition to each date. `multiplot` does not allow for
#'  multiple isobase directions. Negative values denote years BCE while positive
#'  values denote CE.
#'
#' @export
#'
#' @import ggplot2
#' @import ggridges
#' @importFrom patchwork wrap_plots
#'
#'
#' @examples
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
#'
#' target_date <- shoreline_date(sites = target_point, elevation = 46,
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
                           multiplot = FALSE,
                           greyscale = FALSE){


  if (greyscale) {
    dispcol <- sitedistcol <- "black"
    dispfill <- NA
  } else {
    dispcol <- dispfill <- "red"
    sitedistcol <- "#046c9a"
  }

  # Keep track of number of dates with all NA values (out of bounds)
  no_plots <- 0

  if (!multiplot) {
  plts <- list()
  for(k in 1:length(shorelinedates)){

    iso_shoredate <- shorelinedates[[k]]
    # List to hold plots per site in case of multiple isobase directions
    iso_plts <- list()
    for(j in 1:length(iso_shoredate)){

      nshoredate <- iso_shoredate[[j]]

      if (all(is.na(nshoredate$date$probability))) {
        no_plots <- no_plots + 1
        next
      }

      # Remove zeroes for visualisation
      dategrid <- nshoredate$date[nshoredate$date$probability > 0, ]

      plt <- ggplot2::ggplot(data = dategrid) +
      ggridges::geom_ridgeline(ggplot2::aes(x = .data$bce, y = 1,
                       height = .data$probability * 10000/nshoredate$cal_reso),
                               colour = NA, fill = "darkgrey", alpha = 0.7) +
      ggplot2::labs(y = "Meters above present sea-level",
                    x = "Shoreline date (BCE)") +
        ggplot2::theme_bw()

      if (highest_density_region) {
        hdrs <- data.frame(start = nshoredate$hdr_start,
                           end = nshoredate$hdr_end)

        plt <- plt +
          ggplot2::geom_segment(data = hdrs,
                                ggplot2::aes(x = .data$start,
                                            xend = .data$end,
                                            y = 0, yend = 0),
                                            col = "black")
      }

      paramval <- as.numeric(nshoredate$model_parameters)
      dirval <- nshoredate$dispcurve_direction
      if (parameters & isobase_direction) {
        plt <- plt +
          ggplot2::ggtitle(bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2]) ~
                                    "Isobase direction =" ~ .(dirval)))
      } else if (parameters) {
        plt <- plt +
          ggplot2::ggtitle(bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2])))
      } else if (isobase_direction) {
        plt <- plt +
          ggplot2::ggtitle(bquote("Isobase direction =" ~ .(dirval)))
      }

      if (displacement_curve) {
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
                             colour = dispcol, na.rm = TRUE) +
          ggplot2::geom_line(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          y = .data$lowerelev),
                             colour = dispcol, na.rm = TRUE) +
          ggplot2::scale_x_continuous(expand = c(0,0),
                                      limits = c(min(dategrid$bce) - 1250,
                                                 max(dategrid$bce))) +
          ggplot2:: coord_cartesian(ylim = c(0,
                                    as.numeric(nshoredate$site_elev) + 10))
      }

      if (elevation_distribution) {

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
                                nshoredate$site_elev) - gammadatg$offset,
                                    rep(as.numeric(nshoredate$site_elev),
                                        length(x_extent[1]:x_extent[2]))))

        plt <- plt + ggplot2::geom_polygon(data = plotdat,
                                            ggplot2::aes(x = .data$x,
                                        y = .data$y), col = sitedistcol,
                                 fill = sitedistcol, alpha = 0.6)
      }

      if (hdr_label & highest_density_region) {

        label_hdrs <- ""
        for(i in 1:nrow(hdrs)){
          label_hdrs <- paste0(label_hdrs, hdrs$start[i]," to ",
                              hdrs$end[i], " \n")
        }
        # for(i in 1:nrow(hdrs)){
        #   label_hdrs <- paste0(label_hdrs, round(hdrs$start[i])," to ",
        #                        round(hdrs$end[i]), " BCE\n")
        # }
        label_text <- paste0("95% HDR:\n", label_hdrs)

       plt <- plt + annotate_custom(label_text, x = 0.9, y = 0.9, hjust = 0)
      }

      if (!all(is.na(nshoredate$date$probability))) {
        iso_plts[[j]] <- plt
      }
    }

    if (length(iso_plts) > 0) {
      plts[[k]] <- iso_plts
    }

  }

    # Could not use [[]] when passing a vector of indexes to a
    # list of ggplot objects, hence the for loop
    plts_index <- which(unlist(lapply(plts,  length)) != 0)
    plts_nonull <- list()
    for(i in 1:length(plts_index)){
      plts_nonull[[i]] <- plts[[plts_index[i]]]
    }

    print(patchwork::wrap_plots(flatten_list(plts_nonull), ncol = 1))

  } else {

    if (length(unique(shorelinedates[[1]])) > 1) {
       stop("The parameter setting multiplot = TRUE is not compatible with more than one isobase direction.")
    }

    # Find cal_reso
    cal_reso <- shorelinedates[[1]][[1]]$cal_reso

    # Unpack shoreline dates and HDRs
    dates_df <- data.frame()
    hdrs <- data.frame()
    for (i in 1:length(shorelinedates)){
      tmp_date <- shorelinedates[[i]][[1]]$date
      tmp_date$site_name <- shorelinedates[[i]][[1]]$site_name
      dates_df <- rbind(dates_df, tmp_date)

      tmp_hdr <- data.frame(start = shorelinedates[[i]][[1]]$hdr_start)
      tmp_hdr$end <- shorelinedates[[i]][[1]]$hdr_end
      tmp_hdr$site_name <- shorelinedates[[i]][[1]]$site_name
      hdrs <- rbind(hdrs, tmp_hdr)

    }

    # Identify if any site have date probability of NA
    probsums <- aggregate(dates_df$probability, by =
                          list(dates_df$site_name),
                          FUN = sum)

    # Subset based on this (excluding dates with all NA in column probability)
    dates_dfna <-  dates_df[!(dates_df$site_name %in%
                              probsums[which(is.na(probsums$x)), 1]), ]

    # Find number of dates that were excluded
    no_plots <- sum(is.na(probsums$x))

    # Exclude NA dates from the HDRs to be plotted
    hdrs <- hdrs[hdrs$site_name %in% unique(dates_dfna$site_name),]

    # Compile plot without HDRS, but using these to order the sites
    plt <- ggplot2::ggplot(data = hdrs,
      ggplot2::aes(x = .data$start, y = stats::reorder(.data$site_name,
                                                .data$start, FUN = min,
                                                decreasing = TRUE))) +
      ggplot2::geom_segment(data = hdrs, ggplot2::aes(x = .data$start,
                                                      xend = .data$end,
                                       yend = .data$site_name), col = NA) +
      ggridges::geom_ridgeline(data = dates_dfna,
        ggplot2::aes(x = .data$bce,
                     y = .data$site_name,
                      height = .data$probability * 100/cal_reso),
                     colour = NA, fill = "darkgrey", alpha = 0.7) +
      ggplot2::labs(y = "", x = "Shoreline date (BCE)") +
      ggplot2::theme_bw()

    if (highest_density_region) {

      # Add HDR segments
      plt <- plt +
             ggplot2::geom_linerange(data = hdrs,
                          ggplot2::aes(xmin = .data$start, xmax = .data$end,
                          y = .data$site_name), linewidth = 0.5,
                          col = "black",
                          # Parameter preserve = "single" causes warning
                          # that I can not make sense of or find any info on
                       position = ggplot2::position_dodge(width = 0.05,
                                                 preserve = "single"),
                       inherit.aes = FALSE)
    }

    # Suppress warnings if highest_density_region = TRUE, as I have not found a
    # way to handle the warning from preserve = "single"
    if (highest_density_region) {
      suppressWarnings(print(plt))
    } else {
      print(plt)
    }
  }

  if (no_plots == 1) {
    warning(paste("Skipped one date that was out of bounds."))
  } else if (no_plots > 1) {
    warning(paste("Skipped", no_plots, "dates that were out of bounds."))
  }
}
