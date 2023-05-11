#' Plot a map with target locations
#'
#' Function to plot the centroids of sites to be dated and shoreline isobases of
#'  employed displacement curves on a basemap. Defaults to displaying a
#'  light-weight version of the spatial coverage in south-eastern Norway.
#'  However, spatial geometries covering other regions can also be provided or
#'  temporarily downloaded with the function.
#'
#' @param targets Objects of class `sf` representing the sites to be dated. The
#'  first column beyond geom is taken as site name.
#' @param isobases Spatial lines as object of class `sf` representing the
#'  shoreline isobases. Defaults to isobases with a direction of 327 within the
#'  spatial limit in SE Norway, but `create_isobases()` can be used to create
#'  isobases with other directions that can then be passed to `target_plot()`.
#' @param basemap Object of class `sf` representing a background map. Defaults
#'  to a light-weight basemap for the spatial limit in SE Norway.
#' @param crs_epsg Numeric value specifying the EPSG code of the coordinate
#'  reference system (CRS) to be used. Geometries with a different CRS will be
#'  re-projected. Defaults to 32632, which is WGS 84 / UTM zone 32N
#'  (EPSG:32632).
#' @param naturalearth_basemap Logical value specifying if a background map
#'  should be downloaded to be used as a basemap. Downloaded files are stored
#'  with `base::tempdir()` and deleted when the R session is closed. If `TRUE`,
#'  overrides the `basemap` argument. Defaults to `FALSE`.
#' @param naturalearth_zoom A vector of two numerical values specifying the
#'  amount of cropping that is done around provided `targets` when
#'  `naturalearth_basemap` is set to
#'  `TRUE`. Be aware of whether a projected or geographical CRS is specified in
#'  `crs_epsg`. Defaults to `c(20000, 20000)`.
#' @param target_labels Logical value specifying whether the targets should be
#'  labelled in the plot. Takes the first column beyond the one holding the
#'  geometries to represent names. If this is not present the targets are
#'  labelled by row number. Defaults to `TRUE`.
#' @param scalebar Logical specifying whether a scalebar should be added to the
#'  plot. Defaults to `TRUE`.
#' @param scalebar_width Numerical value specifying the width of the scalebar by
#'  passing it to the `width_hint` argument of `ggspatial::annotation:scale()`.
#'  Defaults to `0.4`.
#' @param scalebar_style Character value specifying the style of the scalebar by
#'  passing it to the `style` argument of `ggspatial::annotation:scale()`.
#'  Defaults to `"ticks"`.
#' @param scalebar_location Character value specifying the location of the
#'  scalebar on the plot by passing it to the `location` argument of
#'  `ggspatial::annotation:scale()`. Defaults to `"br"`.
#' @param base_fill Character value specifying the fill colour of the basemap.
#'  Defaults to `"grey"`.
#' @param base_col Character value specifying the outline colour of the
#'  basemap. Defaults to `NA`.
#' @param target_shape Numerical value specifying the point shape that represent
#'  the centroids of the targets. Defaults to `21`.
#' @param target_col Character value specifying the colour parameter for the
#'  points that represent the centroids of the targets. Defaults to `"black"`.
#' @param target_fill Character value specifying the fill parameter for the
#'  points that represent the centroids of the targets. Defaults to `"red"`.
#' @param target_size Numerical value specifying the size of the points that
#'  represent the centroids of the targets. Defaults to `2.25`.
#' @param isobase_line Vector of character values specifying the linetype that
#'  is used to represent the isobases of the geologically derived displacement
#'  curves. Defaults to `c("Horten" = "solid", "Porsgrunn" = "solid",
#'  "Tvedestrand" = "solid", "Arendal" = "solid")`.`
#' @param isobase_col  Vector of character values specifying the colours used
#'  for the lines that represent the isobases of the geologically derived
#'  displacement curves. Defaults to `c("Arendal" = "black",
#'  "Porsgrunn" = "darkgreen", "Tvedestrand" = "blue",
#'  "Horten" = "darkorange")`.`
#' @param greyscale Logical value indicating whether the plot should include
#'  colours or not. Overrides other graphical parameters When set to `TRUE`.
#'  Defaults to `FALSE`.
#'
#' @return A ggplot that displays a background map with the location of the
#'  shoreline isobases within the spatial coverage in south-eastern Norway,
#'  unless geometries for other regions are provided. If provided, the function
#'  also plots the position of target locations represented as centroids.
#'
#' @export
#'
#' @examples
#' # Display the background map and default isobases
#' target_plot()
#'
target_plot <- function(targets = NA,
                        isobases = sf::st_read(
                          system.file("extdata/isobases.gpkg",
                                      package = "shoredate",
                                      mustWork = TRUE), quiet = TRUE),
                        basemap = sf::st_read(
                          system.file("extdata/naturalearth_basemap.gpkg",
                                      package = "shoredate",
                                      mustWork = TRUE), quiet = TRUE),
                        crs_epsg = 32632,
                        naturalearth_basemap = FALSE,
                        naturalearth_zoom = c(20000, 20000),
                        target_labels = TRUE,
                        scalebar = TRUE,
                        scalebar_width = 0.4,
                        scalebar_style = "ticks",
                        scalebar_location = "br",
                        base_fill = "grey",
                        base_col = NA,
                        target_shape = 21,
                        target_col = "black",
                        target_fill = "red",
                        target_size = 2.25,
                        isobase_line = c("Horten" = "solid",
                                         "Porsgrunn" = "solid",
                                         "Tvedestrand" = "solid",
                                         "Arendal" = "solid"),
                        isobase_col = c("Arendal" = "black",
                                         "Porsgrunn" = "darkgreen",
                                         "Tvedestrand" = "blue",
                                         "Horten" = "darkorange"),
                        greyscale = FALSE) {

  if (greyscale) {

    target_shape <- 16
    target_col <- "black"
    isobase_col <- c("Arendal" = "black",
                    "Porsgrunn" = "black",
                    "Tvedestrand" = "black",
                    "Horten" = "black")
    isobase_line <- c("Horten" = "twodash",
                      "Porsgrunn" = "dashed",
                      "Tvedestrand" = "dotted",
                      "Arendal" = "longdash")

  }

  # Check that provided geometries have a CRS (sf would throw an error, but
  # doesn't distinguish between mismatching and no CRS).
  if (!all(is.na(basemap))) {
    if (is.na(sf::st_crs(basemap))) {
      stop("The provided basemap does not have a defined coordinate reference system.")
    }
  }

  if (!all(is.na(isobases))) {
    if (is.na(sf::st_crs(isobases))) {
      stop("The provided isobases do not have a defined coordinate reference system.")
    }
  }

  if (!all(is.na(targets))) {
    if (is.na(sf::st_crs(targets))) {
      stop("The provided targets do not have a defined coordinate reference system.")
    }
  }

  if(naturalearth_basemap){

    basemap <- rnaturalearth::ne_download(scale = "large",
                                     type = "countries",
                                     category = "cultural",
                                     returnclass = "sf")
  }

  # Make sure CRS matches the crs_epsg argument
  if (!all(is.na(basemap)) & crs_epsg != sf::st_crs(basemap)$epsg) {

    warning(paste("Reprojecting basemap from CRS with EPSG code",
                  sf::st_crs(basemap)$epsg, "to EPSG", crs_epsg), ".")
    basemap <- sf::st_transform(basemap, sf::st_crs(crs_epsg))
  }

  if (!all(is.na(isobases)) & crs_epsg != sf::st_crs(isobases)$epsg) {

    warning(paste("Reprojecting isobases from CRS with EPSG code",
            sf::st_crs(isobases)$epsg, "to EPSG", crs_epsg), ".")
    isobases <- sf::st_transform(isobases, sf::st_crs(crs_epsg))
  }

  if (!all(is.na(targets)) & crs_epsg != sf::st_crs(targets)$epsg) {

    warning(paste("Reprojecting targets from CRS with EPSG code",
                  sf::st_crs(targets)$epsg, "to EPSG", crs_epsg), ".")
    targets <- sf::st_transform(targets, sf::st_crs(crs_epsg))
  }

  # Check that provided geometries intersect (except sites and isobases,
  # which is not required). Give warning if not.

  # If only one element is present, skip checks
  if(sum(!all(is.na(basemap)),
         !all(is.na(isobases)),
         !all(is.na(targets))) > 1) {

    # If basemap and isobases are provided
    if (!all(is.na(basemap)) & !all(is.na(isobases))) {

      if (!any(sf::st_intersects(basemap, isobases, sparse = FALSE))) {
        warning("Basemap and isobases do not intersect.")
      }
    }

    # If basemap and targets are provided
    if (!all(is.na(basemap)) & !all(is.na(targets))) {
      if (!any(sf::st_intersects(basemap, targets, sparse = FALSE))) {
        warning("Basemap and targets do not intersect.")
      }
    }
  }

  # Create baseplot
  plt <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      rect = ggplot2::element_rect(),
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "none")

  if(naturalearth_basemap & !any(is.na(naturalearth_zoom))){

    targetsbbox <- sf::st_bbox(targets)
    targetsbbox[1:2] <- targetsbbox[1:2] - naturalearth_zoom[1]
    targetsbbox[3:4] <- targetsbbox[3:4] + naturalearth_zoom[2]
    croppoly <- sf::st_as_sf(sf::st_as_sfc(targetsbbox))

    # Suppress warning on attributes from sf
    basemap <- suppressWarnings(sf::st_crop(basemap, croppoly))
    # plt <- plt +
    #   ggplot2::coord_sf(xlim = c(targetsbbox[1], targetsbbox[3]),
    #                     ylim = c(targetsbbox[2], targetsbbox[4]),
    #                     expand = FALSE, sf::st_crs(crs_epsg))
  }

  # Add basemap, if not set to NA
  if (!all(is.na(basemap))) {

    plt <- plt + ggplot2::geom_sf(data = basemap,
                                  fill = base_fill,
                                  colour = base_col)
  }

  # Add isobases, if not set to NA
  if (!all(is.na(isobases))) {

    plt <- plt + ggplot2::geom_sf(data = isobases,
                                  ggplot2::aes(colour = .data$name,
                                               linetype = .data$name)) +
                 ggplot2::scale_colour_manual(values = isobase_col) +
                 ggplot2::scale_linetype_manual(values = isobase_line)

  }



  # If targets are provided
  if (!all(is.na(targets))) {

    # Make sure the geometries are represented as a sf data frame,
    # and not for example sfc
    if (!inherits(targets, c("sf", "data.frame"))) {
      targets <- sf::st_as_sf(targets, crs = sf::st_crs(targets))
    }

    # Make sure the column representing geometries is present and named "geom"
    if(!("geom" %in% names(targets))){
      targets <- sf::st_set_geometry(targets, sf::st_geometry(targets))
      # sf::st_geometry(targets) <- "geom"
    }

    # Add target centroids, suppressing the warning from sf::st_centroid()
    plt <- plt +
      ggplot2::geom_sf(data = suppressWarnings(sf::st_centroid(targets)),
                           fill = target_fill,
                           size = target_size,
                           shape = target_shape,
                           colour = target_col)

   # Add labels
   if (target_labels) {

      # If only a single column is present, name the targets by row number
      if (ncol(targets) == 1) {

        targets$site_name <- as.character(1:nrow(targets))

      # If there are more columns present, take the first one to represent names
      } else {

        targets$site_name <- as.character(sf::st_drop_geometry(targets)[, 1])

      }
      plt <- plt + ggrepel::geom_text_repel(
                              data = targets,
                              ggplot2::aes(label = .data$site_name,
                              geometry = sf::st_geometry(targets)),
                              stat = "sf_coordinates")
   }
  }

  # Unless naturalearth background map is provided, create bounding box for
  # constraining the plot
  if (!all(is.na(basemap))) {
    base_bbox <- sf::st_bbox(basemap)

    # Adjust to bounding box
    plt <- plt +
      ggplot2::coord_sf(xlim = c(base_bbox[1], base_bbox[3]),
                                 ylim = c(base_bbox[2], base_bbox[4]),
                                 expand = FALSE,
                                 datum = sf::st_crs(crs_epsg))

  }


  # Add scalebar
  if(scalebar) {
    plt <- plt + ggspatial::annotation_scale(
                                location = scalebar_location,
                                width_hint = scalebar_width,
                                style = scalebar_style)
  }

  plt
}
