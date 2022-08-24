# Function to interpolate displacement curve using IDW
interpolate_curve <- function(years, target, dispdat, isodat){

  dists <- as.data.frame(st_distance(target, isobases))
  names(dists) <- isobases$name

  values <- data.frame(matrix(ncol = 3, nrow = length(years)))
  names(values) <- c("years", "lowerelev", "upperelev")

  # In the case that a site is on the isobase of a
  # displacement curve (e.g. Alveberget 8), simply return that displacement
  # curve
  if(any(as.numeric(dists) == 0)){
    values <-
      dispdat[dispdat$name == names(dists)[which(as.numeric(dists) == 0)],]

  } else { for(i in 1:length(years)){
    for(j in 1:ncol(dists)){
      le <- dispdat[which(dispdat$name == names(dists)[j] &
                            dispdat$years == years[i]),
                    "lowerelev"]

      ue <- dispdat[which(dispdat$name == names(dists)[j] &
                            dispdat$years == years[i]),
                    "upperelev"]

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
