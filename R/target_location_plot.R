#' Target location plot
#'
#' Function to plot the centroids of one or more sites to be dated, relative to the shoreline isobases of the employed displacement curves.
#' The basemap is a simplified representation of the coastline within the study area.
#'
#' @param targets Objects of class `sf` representing the sites to be dated. The first column beyond geom is taken as site name.
#'
#' @return A plot displaying the target locations relative to the shoreline isobases.
#' @export
#'
#' @import ggrepel
#'
#' @examples
#' target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#'
#' target_location_plot(target_point)
target_location_plot <- function(targets){

  # Load required data distributed with the package
  isobases <- sf::st_read(system.file("extdata/isobases.gpkg",
                                      package = "shoredate",
                                      mustWork = TRUE), quiet = TRUE)

  basemap <- sf::st_read(system.file("extdata/naturalearth_basemap.gpkg",
                                     package = "shoredate",
                                     mustWork = TRUE), quiet = TRUE)


  # Make sure the geometries are represented as a sf data frame
  # (and not for example sfc).
  if(!inherits(targets, c("sf", "data.frame"))){
    targets <- sf::st_as_sf(targets, crs = sf::st_crs(targets))
  }

  if(ncol(targets) == 1){
    targets$site_name <- as.character(1:nrow(targets))
  } else{
    targets$site_name <- as.character(st_drop_geometry(targets)[,1])
  }

  # Check that the target locations has the correct CRS and is located within
  # the study area
  for(i in 1:nrow(targets)){
  check_target_location(targets[i,])
  }

  # Create bounding box for plotting
  base_bbox <- sf::st_bbox(basemap)

  # Call to plot
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = basemap, fill = "grey", colour = NA) +
    ggplot2::geom_sf(data = isobases, ggplot2::aes(colour = .data$name)) +
    ggplot2::geom_sf(data = suppressWarnings(sf::st_centroid(targets)),
                     fill = "red",
                     size = 2.25, shape = 21,
                     colour = "black") +
    ggrepel::geom_text_repel(data = targets,
                             aes(label = .data$site_name, geometry = .data$x),
                                          stat = "sf_coordinates") +
    ggplot2::coord_sf(xlim = c(base_bbox[1], base_bbox[3]),
                      ylim = c(base_bbox[2], base_bbox[4]),
                      expand = FALSE, datum = sf::st_crs(32632)) +
    ggplot2::scale_colour_manual(values = c("Arendal" = "black",
                                            "Larvik" = "darkgreen",
                                            "Tvedestrand" = "blue",
                                            "Horten" = "darkorange")) +
    ggplot2::theme_bw() + ggplot2::theme(
                                    axis.title = ggplot2::element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.ticks = element_blank(),
                                    rect = ggplot2::element_rect(),
                                    panel.grid.major = ggplot2::element_blank(),
                                    legend.position = "none")
}
