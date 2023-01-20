#' Interpolate displacement curve using IDW
#'
#' Interpolate the trajectory of past shoreline displacement to a single
#'  location based on the distance of the location to the shoreline isobases of
#'  the geologically derived displacement curves. This is done using inverse
#'  distance weighting.
#'
#' @param target A spatial target location to where the new displacement curve
#'  is interpolated.
#' @param isobases 4 spatial lines representing the shoreline isobases of the
#'  existing displacement curves. Multiple sets of 4 isobases with different
#'  isobase dirctions can be provided (see [create_isobases()]).
#' @param power A numerical value indicating the inverse distance power for IDW.
#'  Defaults to 2.
#' @param cal_reso Numeric value specifying the resolution to use on the
#'  calendar scale. Defaults to 10.
#' @param verbose Logical value indicating whether progress should be printed to
#'  console. Defaults to FALSE.
#'
#' @return A list holding an interpolated displacement curve for each isobase
#'  direction. Each displacement curve is represented by a data frame with
#'  the columns `bce` where negative values indicate years BCE and positive CE,
#'  `lowerelev`, representing the lower limit for the elevation of the shoreline
#'  for each year. `upperelev`, the upper limit for elevation of the shoreline
#'  for each year, and `direction` which indicates the direction of the isobases
#'  used when interpolating the curve.
#'
#' @export
#'
#' @import sf
#'
#' @examples
#' # Create example point using the required coordinate system
#' # WGS84 / zone UTM32N (EPSG: 32632)
#' target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#'
#' # Interpolate shoreline displacement curve to the target point location
#' # setting the resolution on the calendar scale to 1000 years for speed.
#' target_curve <- interpolate_curve(target_point, cal_reso = 1000)
#'
#' # Call to plot
#' displacement_plot(target_curve)
interpolate_curve <- function(target,
                              isobases = NA,
                              power = 2,
                              cal_reso = 10,
                              verbose = FALSE){

  # Check that the target location is set to correct CRS (causes error if it
  # is not) and is located within the study area (prints warning if it is not)
  check_target_location(target)

  # Load existing displacement curves
  displacement_curves <- get(load(system.file("extdata/displacement_curves.rda",
                  package = "shoredate",
                  mustWork = TRUE)))

  bce <- seq(-1950, 10550,  cal_reso) * -1 # Sequence of years to
                                          # match displacement data

  # Use default isobases unless others are provided
  if (any(is.na(isobases))) {
    isobases <- sf::st_read(
                system.file("extdata/isobases.gpkg",
                  package = "shoredate",
                  mustWork = TRUE), quiet = TRUE)
  }

  interpolated_curves <- list()

  for(i in 1:length(unique(isobases$direction))){
    isobases_dir <- isobases[isobases$direction ==
                               unique(isobases$direction)[i], ]
    dists <- as.data.frame(sf::st_distance(target, isobases_dir))
    names(dists) <- isobases_dir$name

    values <- data.frame(matrix(ncol = 3, nrow = length(bce)))
    names(values) <- c("bce", "lowerelev", "upperelev")

    # In the case that a site is on the isobase of a
    # displacement curve, simply return that displacement curve
    if (any(as.numeric(dists) == 0)) {
      values <- displacement_curves[displacement_curves$name ==
                                  names(dists)[which(as.numeric(dists) == 0)], ]

    } else {
      if (verbose) {
        # If a site is not on a isobase, the displacement curve needs to be
        # interpolated, in which case printing progress might be of interest
        print("Interpolating displacement curve")
        pb <- utils::txtProgressBar(min = 0,
                                    max = length(bce),
                                    style = 3,
                                    char = "=")
      }

      for(j in 1:length(bce)){
      for(k in 1:ncol(dists)){
        le <- displacement_curves[which(displacement_curves$name ==
                                  names(dists)[k] & displacement_curves$bce ==
                                  bce[j]), "lowerelev"]

        ue <- displacement_curves[which(displacement_curves$name ==
                                  names(dists)[k] & displacement_curves$bce ==
                                    bce[j]), "upperelev"]

        dists[2, k] <- le
        dists[3, k] <- ue
      }
      distdat <- as.data.frame(t(dists))
      names(distdat) <- c("distance", "lower", "upper")

      # No sites are older than the lowest limit of any displacement curve
      # so in case of NA values, simply assign NA
      if (any(is.na(distdat))) {
        lowerval <- upperval <- NA
      } else {
        # Inverse distance weighting
        lowerval <- sum(apply(distdat, 1,
                              function(x) x["lower"] * x["distance"]^-power)) /
          sum(apply(distdat, 1, function(x) x["distance"] ^-power))
        upperval <- sum(apply(distdat, 1,
                              function(x) x["upper"] * x["distance"]^-power)) /
          sum(apply(distdat, 1, function(x) x["distance"] ^-power))

      }

      values[j, 1:3] <- c(bce[j], lowerval, upperval)

      if (verbose) {
        utils::setTxtProgressBar(pb, j)
      }
      }
    }
    if (verbose) {
      close(pb)
    }
    values$direction <- unique(isobases$direction)[i]
    interpolated_curves[[i]] <- values
  }

  interpolated_curves
}
