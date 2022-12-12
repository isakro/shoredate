#' Check if a target site is set to the correct CRS and is located within the
#'  spatial limit
#'
#' Checks if a target sites is set to the correct coordinate reference system
#'  WGS84 UTM32N (EPSG: 32632). Throws an error if this is not the case. Checks
#'  if the location intersects the study area for which the method was derived
#'  (see Roalkvam 2022). Returns a warning if this is not the case.
#'
#' @param target A spatial object of class `sf`, representing a site location.
#'
#' @return A warning if the site does not intersect the study area.
#' @export
#'
#' @examples
#' # Create example point using the required coordinate system
#' # WGS84 UTM32N (EPSG: 32632)
#' target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#'
#' # Check the CRS and the location of the site
#' check_target_location(target_point)
#'
check_target_location <- function(target){

  spatial_limit = sf::st_read(
    system.file("extdata/spatial_limit.gpkg",
                package = "shoredate",
                mustWork = TRUE), quiet = TRUE)

  if (is.na(sf::st_crs(target))) {
    stop("Undefined coordinate reference system. This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632).")
  }

  if (sf::st_crs(target)$epsg != 32632) {
    stop(paste0("Target has coordinate reference system with EPSG ",
                sf::st_crs(target)$epsg,
                ". This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632)."))
  }

  if (!(sf::st_intersects(target, spatial_limit, sparse = FALSE))) {
    warning(paste("Target location is not within the study area for which the method was derived."))
  }
}
