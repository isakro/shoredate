#' Target plot
#'
#' Function to plot the centroids of one or more sites to be dated, relative to
#'  the shoreline isobases of the employed displacement curves. The default
#'  basemap is a simplified representation of the coastline within the study
#'  area. Calling the function without providing a target displays the map with
#'  the isobases. This can be combined with `create_isobases()` to visualise
#'  isobases with a different direction than the default of 327.
#'
#' @param targets Objects of class `sf` representing the sites to be dated. The
#'  first column beyond geom is taken as site name.
#' @param isobases Spatial lines as object of class `sf` representing the
#'  shoreline isobases. Defaults to isobases with a direction of 327, but
#'  `create_isobases()` can be used to create isobases with other directions
#'  that can then be passed to `target_plot()`.
#' @param basemap Object of class `sf` representing the study area. Defaults to
#'  a light-weight basemap from https://www.naturalearthdata.com/
#' @param greyscale Logical value indicating whether the plot should include
#'  colours or not. Defaults to FALSE.
#'
#' @return A map displaying the location of the shoreline isobases, and, if
#'  provided, the position of target locations represented as centroids.
#'
#' @export
#'
#' @import ggrepel
#' @importFrom ggspatial annotation_scale
#'
#' @examples
#' # Display the background map and default isobases
#' target_plot()
#'
target_plot <- function(targets = NA,
                        isobases = NA,
                        basemap = NA,
                        greyscale = FALSE){

  if (greyscale) {

    target_fill <- "black"

    isobase_col <- c("Arendal" = "black",
                    "Porsgrunn" = "black",
                    "Tvedestrand" = "black",
                    "Horten" = "black")

    isobase_line <- c("Horten" = "twodash",
                      "Porsgrunn" = "dashed",
                      "Tvedestrand" = "dotted",
                      "Arendal" = "longdash")

  } else {

    target_fill <- "red"

    isobase_col <- c("Arendal" = "black",
                    "Porsgrunn" = "darkgreen",
                    "Tvedestrand" = "blue",
                    "Horten" = "darkorange")

    isobase_line <- c("Horten" = "solid",
                      "Porsgrunn" = "solid",
                      "Tvedestrand" = "solid",
                      "Arendal" = "solid")
  }

  # Load required background map distributed with the package
  if(is.na(basemap)){
    basemap <- sf::st_read(system.file("extdata/naturalearth_basemap.gpkg",
                                     package = "shoredate",
                                     mustWork = TRUE), quiet = TRUE)
  }

  # Use default isobases if none are provided
  if (all(is.na(isobases))) {
    isobases <- sf::st_read(system.file("extdata/isobases.gpkg",
                            package = "shoredate",
                            mustWork = TRUE), quiet = TRUE)
  }

  # Create baseplot with isobases
  plt <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = basemap, fill = "grey", colour = NA) +
    ggplot2::geom_sf(data = isobases, ggplot2::aes(colour = .data$name,
                                                   linetype = .data$name)) +
    ggplot2::scale_colour_manual(values = isobase_col) +
    ggplot2::scale_linetype_manual(values = isobase_line) +
    ggspatial::annotation_scale(location = 'br',
                                width_hint = 0.4, style = "ticks") +
    ggplot2::theme_bw() + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      rect = ggplot2::element_rect(),
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "none")

  # If one or more targets are provided
  if (!any(is.na(targets))) {

    # Make sure the geometries are represented as a sf data frame,
    # and not for example sfc
    if (!inherits(targets, c("sf", "data.frame"))) {
      targets <- sf::st_as_sf(targets, crs = sf::st_crs(targets))
    }

    if (ncol(targets) == 1) {
      targets$site_name <- as.character(1:nrow(targets))
    } else {
      targets$site_name <- as.character(st_drop_geometry(targets)[, 1])
    }

    # Check that the target locations has the correct CRS and is located within
    # the study area
    for(i in 1:nrow(targets)){
    check_target_location(targets[i, ])
    }

    if(!("geom" %in% names(targets))){
      st_geometry(targets) <- "geom"
    }

    plt <- plt +
      ggplot2::geom_sf(data = suppressWarnings(sf::st_centroid(targets)),
                           fill = target_fill,
                           size = 2.25, shape = 21,
                           colour = "black") +
      ggrepel::geom_text_repel(data = targets,
                               aes(label = .data$site_name,
                                   geometry = .data$geom),
                               stat = "sf_coordinates")
  }
  # Create bounding box for constraining plot
  base_bbox <- sf::st_bbox(basemap)

  # Call to plot
  plt + ggplot2::coord_sf(xlim = c(base_bbox[1], base_bbox[3]),
                          ylim = c(base_bbox[2], base_bbox[4]),
                          expand = FALSE, datum = sf::st_crs(32632))
}
