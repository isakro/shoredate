#' Plot shoreline dates
#'
#' Function for plotting shoreline dates along with associated metadata.
#'
#' @param shorelinedates Object of class `shoreline_date`.
#' @param date_probability Logical value indicating whether the
#'  probability distribution of the shoreline date should be plotted.
#'  Defaults to TRUE.
#' @param elevation_distribution Logical value indicating whether the
#'  distribution describing the distance between site and shoreline should be
#'  displayed. Default is TRUE.
#' @param displacement_curve Logical value indicating whether the displacement
#'  curve should be displayed. Default is TRUE.
#' @param site_name Logical value indicating whether the name of the site should
#'  be printed in the header of the plot. Defaults to FALSE.
#' @param parameters Logical value indicating whether the parameters of the
#'  statistical function should be displayed. Default is FALSE.
#' @param isobase_direction  Logical value indicating whether the direction of
#'  the isobases should be printed. Default is FALSE.
#' @param highest_density_region Logical value indicating whether the 95%
#'  highest density region should be displayed. Defaults to TRUE.
#' @param hdr_label Logical value indicating whether the numeric values for the
#'  highest density regions should be displayed. Default is TRUE.
#' @param multiplot Logical value indicating whether multiple dates should be
#'  plotted individually, or be collapsed into a single plot. The only other
#'  graphical option with `multiplot` set to TRUE is `highest_density_region`.
#'  Default is FALSE.
#' @param date_col Character value specifying the outline colour of the
#'  probability distribution of the shoreline date.
#' @param date_fill Character value specifying the fill colour of the
#'  probability distribution of the shoreline date.
#' @param displacement_col Character value specifying the outline colour of the
#'  displacement curve.
#' @param displacement_fill Character value specifying the fill colour of the
#'  displacement curve.
#' @param site_elevation_col Character value specifying the outline colour of
#'  the distribution describing the likely distance between site and shoreline.
#' @param site_elevation_fill Character value specifying the fill colour of
#'  the distribution describing the likely distance between site and shoreline.
#' @param hdr_col Character value specifying the colour of the line
#'  segment giving the highest density region of the shoreline date.
#' @param hdr_label_xadj Numerical value between 0 and 1 specifying the position
#'  of the HDR label on the x-axis. Increasing the value moves the label further
#'  from the plot border. Defaults to 0.2.
#' @param hdr_label_yadj Numerical value between 0 and 1 specifying the position
#'  of the HDR label on the y-axis. Increasing the value moves the label further
#'  from the plot border. Defaults to 0.3.
#' @param greyscale Logical value indicating whether the plot should be in
#'  greyscale or not. If TRUE, overrides other colour parameters.
#'  Defaults to FALSE.
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
#' @return Plot(s) displaying shoreline dates and associated metadata.
#'
#' @export
#'
#' @importFrom ggplot2 .data
#'
#' @examples
#' # Create example point with correct coordinate reference system
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
#'
#' # Reduce date resolution with cal_reso and elevation_reso for speed
#' target_date <- shoreline_date(sites = target_point, elevation = 80,
#'                               elev_reso = 10,
#'                               cal_reso = 500)
#'
#' shoredate_plot(target_date)
shoredate_plot <- function(shorelinedates,
                           date_probability = TRUE,
                           elevation_distribution = TRUE,
                           displacement_curve = TRUE,
                           site_name = FALSE,
                           parameters = FALSE,
                           isobase_direction = FALSE,
                           highest_density_region = TRUE,
                           hdr_label = TRUE,
                           multiplot = FALSE,
                           date_col = NA,
                           date_fill = "darkgrey",
                           displacement_col = "red",
                           displacement_fill = "red",
                           site_elevation_col = "#046c9a",
                           site_elevation_fill = "#046c9a",
                           hdr_col = "black",
                           hdr_label_xadj = 0.2,
                           hdr_label_yadj = 0.3,
                           greyscale = FALSE){


  if (greyscale) {
    displacement_col <- site_elevation_col <- site_elevation_fill <-
        hdr_col <-"black"
    date_fill <- "darkgrey"
    displacement_fill <- date_col <- NA
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

      # If there is a displacement curve (this is not returned from
      # shoreline_date() with sum_isobase_directions = TRUE), find earliest
      # start date of displacement curve for adjusting x-axis limit
      mindisp <- ifelse(!all(is.na(nshoredate$dispcurve)),
                        min(nshoredate$dispcurve$bce),  min(dategrid$bce)-1251)

      plt <- ggplot2::ggplot() +
         ggplot2::labs(y = "Meters above present sea-level",
                    x = "Shoreline date (BCE/CE)") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(expand = c(0,0),
                                    limits = c(
                                      ifelse(mindisp > min(dategrid$bce) - 1250,
                                             mindisp, min(dategrid$bce) - 1250),
                                      ifelse(
                                        (max(nshoredate$hdr_end) + 1250) < 1950,
                                    max(nshoredate$hdr_end) + 1250, 1950))) +
        ggplot2:: coord_cartesian(ylim = c(0,
                                  as.numeric(nshoredate$site_elev) + 10))

      if(date_probability){

        # Scaling constant for the probability of the date
        dr <- 10000

        # If there is a displacement curve, scale the probability by the range
        # of elevation values
        if(!all(is.na(nshoredate$dispcurve))){
        dr <- 100 * (max(nshoredate$dispcurve$upperelev,
                  na.rm = TRUE) - min(nshoredate$dispcurve$upperelev,
                                      na.rm = TRUE))
        }

        plt <- plt +
          ggridges::geom_ridgeline(
            data = dategrid,
            ggplot2::aes(x = .data$bce, y = 1,
            height = .data$probability * dr/nshoredate$cal_reso),
            colour = date_col,
            fill = date_fill,
            alpha = 0.7)
      }

      if (highest_density_region) {
        hdrs <- data.frame(start = nshoredate$hdr_start,
                           end = nshoredate$hdr_end,
                           prob = nshoredate$hdr_prob * 100)

        plt <- plt +
          ggplot2::geom_segment(data = hdrs,
                                ggplot2::aes(x = .data$start,
                                            xend = .data$end,
                                            y = 0, yend = 0),
                                            col = hdr_col)
      }

      paramval <- as.numeric(nshoredate$model_parameters)
      dirval <- nshoredate$dispcurve_direction
      if(length(dirval) > 1){
        dirval <- paste("Sum of isobase directions:",
                        paste(dirval, collapse = ", "))
      } else {
        dirval <- paste("Isobase direction =", dirval)
      }

      if (site_name & parameters & isobase_direction) {
        plt <- plt +
          ggplot2::labs(title = nshoredate$site_name,
            subtitle = bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2]) ~
                                .(dirval)))

      } else if (site_name & parameters) {
        plt <- plt +
          ggplot2::labs(title = nshoredate$site_name,
            subtitle = bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                    sigma ~ "=" ~ .(paramval[2])))
      } else if (site_name & isobase_direction) {
        plt <- plt +
          ggplot2::labs(title = nshoredate$site_name,
            subtitle = bquote(.(dirval)))

      } else if (parameters & isobase_direction) {
        plt <- plt +
          ggplot2::labs(subtitle =
                          bquote(alpha ~ "=" ~ .(paramval[1]) ~
                          sigma ~ "=" ~ .(paramval[2]) ~
                          .(dirval)))

      } else if (parameters){
        plt <- plt +
          ggplot2::labs(subtitle = bquote(alpha ~ "=" ~ .(paramval[1]) ~
                                            sigma ~ "=" ~ .(paramval[2])))

      } else if (isobase_direction) {
        plt <- plt +
          ggplot2::labs(subtitle = bquote(.(dirval)))

      } else if (site_name) {
        plt <- plt +
          ggplot2::labs(title = nshoredate$site_name)
      }

      if (displacement_curve) {
        if(!all(is.na(nshoredate$dispcurve))){
        plt <- plt +
          ggplot2::theme(legend.position = "None") +
          ggplot2::geom_ribbon(data = nshoredate$dispcurve,
                               ggplot2::aes(x = .data$bce,
                                            ymin = .data$lowerelev,
                                            ymax = .data$upperelev),
                               fill = displacement_fill, alpha = 0.2) +
          ggplot2::geom_line(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          y = .data$upperelev),
                             colour = displacement_col, na.rm = TRUE) +
          ggplot2::geom_line(data = nshoredate$dispcurve,
                             ggplot2::aes(x = .data$bce,
                                          y = .data$lowerelev),
                             colour = displacement_col, na.rm = TRUE)
        }
      }

      if (elevation_distribution) {
        # If a sum of multiple isobase directions, elevation_distribution can
        # not be plotted.
        if(!all(is.na(nshoredate$dispcurve))){

        # Check if there is a statitical function account for distance between
        # site and sea.
        if(nrow(nshoredate$modeldat) > 1){

          # For plotting purposes to close the geom_polygon on the y-axis
          modeldatg <- rbind(c(0, 0, 0), nshoredate$modeldat)

          # Code adapted from oxcAAR to plot density to y-axis
          x_extent <-
            ggplot2::ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
          modeldatg$probs_scaled <- modeldatg$probs / max(modeldatg$probs) *
            diff(x_extent) * 20 + x_extent[1]


          plotdat <- data.frame(x = c(modeldatg$probs_scaled,
                                      x_extent[1]:x_extent[2]),
                                y = c(as.numeric(
                                  nshoredate$site_elev) - modeldatg$offset,
                                      rep(as.numeric(nshoredate$site_elev),
                                          length(x_extent[1]:x_extent[2]))))

          plt <- plt + ggplot2::geom_polygon(data = plotdat,
                                              ggplot2::aes(x = .data$x,
                                          y = .data$y),
                                          col = site_elevation_col,
                                          fill = site_elevation_fill,
                                          alpha = 0.6)
          # If no statistical model was in use, i.e. shoreline_date run with
          # model = "none", plot the elevation as a line
        } else {
          plt <- plt + ggplot2::geom_hline(
            ggplot2::aes(yintercept = nshoredate$site_elev),
                         col = site_elevation_col)
        }
        }
      }

      if (hdr_label & highest_density_region) {

        label_hdrs <- ""
        for(i in 1:nrow(hdrs)){
          label_hdrs <- paste0(label_hdrs, hdrs$start[i]," to ",
                              hdrs$end[i], " \n")
        }

        label_text <- paste0(hdrs$prob, "% HDR:\n", label_hdrs)

        yrange <- plt$coordinates$limits$y
        xrange <- ggplot2::layer_scales(plt)$x$range$range
        yadj <-  yrange[2] - (yrange[2] - yrange[1]) * hdr_label_yadj
        xadj <- xrange[2] - (xrange[2] - xrange[1]) * hdr_label_xadj

        plt <- plt +
          ggplot2::annotate("text", x = xadj, y = yadj, label = label_text)

        # plt <- plt + annotate_custom(label_text, x = 0.5, y = 0.5, hjust = 0)
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

    # print(patchwork::wrap_plots(flatten_list(plts_nonull), ncol = 1))
    plts <- flatten_list(plts_nonull)
    for(i in 1:length(plts)){
      print(plts[[i]])
    }

  } else {

    if (length(unique(shorelinedates[[1]])) > 1) {
       stop("The parameter setting multiplot = TRUE is not compatible with more than one isobase direction.")
    }

    # Retrieve cal_reso
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
    probsums <- stats::aggregate(dates_df$probability, by =
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
      ggplot2::labs(y = "", x = "Shoreline date (BCE/CE)") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0,0),
                                  limits = c(min(hdrs$start) - 1250,
                                             ifelse(
                                               (max(hdrs$end) + 1250) < 1950,
                                               max(hdrs$end) + 1250, 1950)))
    if(date_probability){
      plt <- plt +
        ggridges::geom_ridgeline(data = dates_dfna,
                        ggplot2::aes(x = .data$bce,
                                     y = .data$site_name,
                                     height = .data$probability * 100/cal_reso),
                                 colour = date_col,
                                 fill = date_fill,
                                 alpha = 0.7)
    }


    if (highest_density_region) {
      # Add HDR segments
      plt <- plt +
             ggplot2::geom_linerange(data = hdrs,
                          ggplot2::aes(xmin = .data$start, xmax = .data$end,
                          y = .data$site_name), linewidth = 0.5,
                          col = hdr_col,
                          # Parameter preserve = "single" causes a warning
                          # that I can not make sense of.
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
