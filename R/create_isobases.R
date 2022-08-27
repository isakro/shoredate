#' Create isobases
#'
#' Function to create isobases for interpolating shoreline displacement curves. This is done from the centre points of the supplied displacement curves.
#'
#' @param isobase_direction A vector holding a single or multiple directions for the isobases.
#'
#' @return A simple feature holding the isobases represented as lines
#' @export
#'
#' @examples
#' # Create isobases in a specified direction
#' isobases <- create_isobases(327)
#' plot(sf::st_geometry(isobases))
#'
#' # Create isobases using a range of directions. Useful for testing the sensitivity of dates to the isobase direction.
#' isobases <- create_isobases(seq(327, 338, 1))
#' plot(sf::st_geometry(isobases), col  = "red")
create_isobases <- function(isobase_direction){
  centrepoints <- sf::st_read(
    system.file("extdata/isobase_centrepts.gpkg",
                package = "shoredate",
                mustWork = TRUE))

  # Arbitrarily long length of lines to represent the isobases.
  isobase_length = 9000000

  # Create empty sf object to hold the isobases
  nrows <- length(isobase_direction)*nrow(centrepoints)
  isobases <- sf::st_sf(id = 1:nrows,
                    name = NA, direction = NA,
                    crs = 32632,
                    geometry = sf::st_sfc(lapply(1:nrows,
                     function(x) sf::st_linestring())))

  # Loop over isobase directions
  for(i in 1:length(isobase_direction)){

    deg <- isobase_direction[i]

    # Create empty sf object to hold isobases for the current direction
    isobases_temp <- sf::st_sf(id = 1:nrow(centrepoints),
                               name = NA, direction = NA,
                          crs = 32632,
                      geometry = sf::st_sfc(lapply(1:nrow(centrepoints),
                                        function(x) sf::st_linestring())))

    # Loop over crentre points and create isobases
    for(j in 1:nrow(centrepoints)){
      # Find x and y coords
      x <- sf::st_coordinates(centrepoints[j, ])[1]
      y <- sf::st_coordinates(centrepoints[j, ])[2]

      # Find coords at the specified distance from the point at deg degree angle
      # adding isobase_length
      xx <- x + isobase_length * (cos(deg * pi / 180))
      yy <- y + isobase_length * (sin(deg * pi / 180))

      # Find coords at the specified distance from the point at deg degree angle
      # subtracting isobase_length
      xx2 <- x - isobase_length * (cos(deg * pi / 180))
      yy2 <- y - isobase_length * (sin(deg * pi / 180))

      # Create points from the identified coordinates
      pts <- sf::st_sfc(
        sf::st_multipoint(
          rbind(
            sf::st_coordinates(centrepoints[j,])[1,],
                                        c(xx, yy), c(xx2, yy2))))
      pts <-  sf::st_sf(sf::st_set_crs(pts, 32632)) # WGS 84 / UTM 32N

      # Combine these into a line and update the temporary data frame
      isobases_temp[j, "geometry"] <- sf::st_cast(pts, to = 'LINESTRING')
    }
    isobases_temp$name <- centrepoints$name
    isobases_temp$direction <- deg
    isobases[(nrow(centrepoints)*i-(nrow(centrepoints)-1)):
               (nrow(centrepoints)*i),] <- isobases_temp
  }
  return(isobases)
}

