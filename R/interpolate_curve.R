#' Interpolate displacement curve using IDW
#'
#' Interpolate the trajectory of past shoreline displacement to a single location based on the. This is done using inverse distance weighting.
#'
#' @param target A spatial target location to where the new displacement curve is interpolated
#' @param dispdat Load existing displacement curves.
#' @param isobases Spatial lines representing the isobases of the existing displacement curves
#' @param cal_reso Numeric value specifying the resolution to use on the calendar scale. Defaults to 1.
#'
#' @return A data frame holding the interpolated displacement curve
#' @export
#'
#' @import sf
#'
#' @examples
#' # Create example point using the required coordinate system WGS84 UTM32N (EPSG: 32632).
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#'
#' # Interpolate shoreline displacement curve to the target point location.
#' target_curve <- interpolate_curve(target_pt)
#'
#' # Call to plot
#' displacement_plot(target_curve)
interpolate_curve <- function(target,
                              dispdat =
                                load(
                                  system.file("extdata/displacement_curves.rda",
                                                    package = "shoredate",
                                              mustWork = TRUE)),
                              isobases = NA,
                              cal_reso = 1){

  bce <- seq(-1950, 10550,  cal_reso) * -1 # Sequence of years to
                                          # match displacement data

  # Use default isobases unless others are provided
  if(any(is.na(isobases))){
    isobases <- sf::st_read(
      system.file("extdata/isobases.gpkg",
                  package = "shoredate",
                  mustWork = TRUE), quiet = TRUE)
  }

  displacement_curves <- get(dispdat)

  interpolated_curves <- list()

  for(i in 1:length(unique(isobases$direction))){
    isobases_dir <- isobases[isobases$direction ==
                               unique(isobases$direction)[i],]
    dists <- as.data.frame(sf::st_distance(target, isobases_dir))
    names(dists) <- isobases_dir$name

    values <- data.frame(matrix(ncol = 3, nrow = length(bce)))
    names(values) <- c("bce", "lowerelev", "upperelev")

    # In the case that a site is on the isobase of a
    # displacement curve, simply return that displacement curve
    if(any(as.numeric(dists) == 0)){
      values <- displacement_curves[displacement_curves$name ==
                                  names(dists)[which(as.numeric(dists) == 0)],]

    } else { for(j in 1:length(bce)){
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
      if(any(is.na(distdat))){
        lowerval <- upperval <- NA
      } else {
        # Inverse distance weighting
        lowerval <- sum(apply(distdat, 1,
                              function(x) x["lower"] * x["distance"]^-2)) /
          sum(apply(distdat, 1, function(x) x["distance"] ^-2))
        upperval <- sum(apply(distdat, 1,
                              function(x) x["upper"] * x["distance"]^-2)) /
          sum(apply(distdat, 1, function(x) x["distance"] ^-2))

      }
      values[j, 1:3] <- c(bce[j], lowerval, upperval)

    }
    }
    values$direction <- unique(isobases$direction)[i]
    interpolated_curves[[i]] <- values
  }
  return(interpolated_curves)
}
