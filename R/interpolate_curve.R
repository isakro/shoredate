#' Interpolate displacement curve using IDW
#'
#' @param target A spatial target location to where the new displacement curve is interpolated
#' @param dispdat Load existing displacement curves
#' @param isobases Load spatial lines representing the isobases of the existing displacement curves
#'
#' @return A data frame holding the interpolated displacement curve
#' @export
#'
#' @examples
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#' target_curve <- interpolate_curve(target_pt)
#' plot(target_curve$years, target_curve$upperelev, type = "l")
#' lines(target_curve$years, target_curve$lowerelev)
interpolate_curve <- function(target,
                              dispdat =
                                load(
                                  system.file("extdata/displacement_curves.rda",
                                                    package = "shoredate")),
                              isobases =
                                sf::st_read(
                                  system.file("extdata/isobases.gpkg",
                                                   package = "shoredate"))){

  years <- seq(-1950, 10550,  1) * -1 # Sequence of years to match displacement
                                      # data

  displacement_curves <- get(dispdat)
  dists <- as.data.frame(sf::st_distance(target, isobases))
  names(dists) <- isobases$name

  values <- data.frame(matrix(ncol = 3, nrow = length(years)))
  names(values) <- c("years", "lowerelev", "upperelev")

  # In the case that a site is on the isobase of a
  # displacement curve, simply return that displacement curve
  if(any(as.numeric(dists) == 0)){
    values <- displacement_curves[displacement_curves$name ==
                                  names(dists)[which(as.numeric(dists) == 0)],]

  } else { for(i in 1:length(years)){
    for(j in 1:ncol(dists)){
      le <- displacement_curves[which(displacement_curves$name ==
                                names(dists)[j] & displacement_curves$years ==
                                years[i]), "lowerelev"]

      ue <- displacement_curves[which(displacement_curves$name ==
                                names(dists)[j] & displacement_curves$years ==
                                  years[i]), "upperelev"]

      dists[2, j] <- le
      dists[3, j] <- ue
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
    values[i,] <- c(years[i], lowerval, upperval)
  }
  }
  return(values)
}
