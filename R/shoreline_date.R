#' Shoreline date
#'
#' A function for shoreline dating a Stone Age site based on its present-day elevation and the trajectory of past shoreline displacement on the Norwegian Skagerrak coast.
#'
#' @param site A spatial target representing the site to be dated.
#' @param elev Elevation raster to be input if the elevation values are not provided manually.
#' @param reso Numeric value specifying the resolution with which to step through the elevation distance between site and shoreline. Defaults to 0.1m.
#' @param isobase_direction A single numeric value defining the direction of the isobases, or a vector of direction values to iterate over. Defaults to.
#' @param expratio Numeric value specifying the ratio with which the exponential function decays. Defaults to 0.168
#' @param elevavg Specified statistic to define elevation if this is to be derived from elevation raster.
#' @param elevation Numeric elevation value to inform shoreline date unless an elevation raster is provided.
#' @param interpolated_curve Data frame holding shoreline displacement curve derived from interpolate_curve(). The interpolation function will be run if this is not provided.
#'
#' @return A list containing the shoreline date and associated parameters
#' @export
#'
#' @import sf
#' @import terra
#'
#' @examples
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#' target_date <- shoreline_date(site = target_pt, elevation = 65)
#' shoredate_plot(target_date)
shoreline_date <- function(site,
                           elev = NA,
                           reso = 0.1,
                           isobase_direction = 327,
                           expratio = 0.168,
                           elevavg = "mean",
                           elevation = NA,
                           interpolated_curve = NA){

  bce <- seq(-1950, 10550,  1) * -1 # Sequence of years to match displacement
                                    # data

  if(is.na(interpolated_curve) & isobase_direction != 327){
      isobases <- create_isobases(isobase_direction)
  }

  if(is.na(interpolated_curve)){
    sitecurve <- interpolate_curve(target = site)
  } else{
    sitecurve <- interpolated_curve
  }

  if(is.na(elev)){
    if(is.na(elevation)){
      return(NA)
    } else{
      siteelev <- elevation
    }
  } else{
    siteelev <- terra::extract(elev, terra::vect(site), fun = elevavg)[,-1]
  }

  # Elevation offsets to step through by increments of reso
  inc <- seq(0, siteelev, reso)

  # Set up data frame and assign
  expdat <- data.frame(
    offset = inc,
    px = stats::pexp(inc, rate = expratio))
  expdat$probs <- c(diff(expdat$px), 0)
  expdat <- expdat[expdat$px < 0.99999,]

  dategrid <- data.frame(
    bce = bce,
    probability = 0)

  for(i in 1:nrow(expdat)){
    # Subtract offset
    adjusted_elev <- as.numeric(siteelev - expdat$offset[i])

    # Find lower date
    lowerd <- round(stats::approx(sitecurve[,"lowerelev"],
                           bce, xout = adjusted_elev)[['y']])

    # Find upper date
    upperd <- round(stats::approx(sitecurve[,"upperelev"],
                           bce, xout = adjusted_elev)[['y']])

    # Find youngest and oldest date
    earliest <- min(c(lowerd, upperd))
    latest <- max(c(lowerd, upperd))

    # Add probability to each year in range
    if(!is.na(latest) && !is.na(earliest)){

      year_range <- seq(earliest, latest, 1)
      prob <- 1/length(year_range) * expdat$probs[i]

      dategrid[dategrid$bce %in% year_range, "probability"] <-
        dategrid[dategrid$bce %in% year_range, "probability"] + prob
    }
  }

  if(class(site)[1] == "sf") {
    dategrid$site_name <- as.character(sf::st_drop_geometry(site[1]))
  } else{
    dategrid$site_name <- site
  }

  # Normalise to sum to unity
  dategrid$probability <- dategrid$probability / sum(dategrid$probability)

  return(list(date = dategrid,
              dispcurve = sitecurve,
              elev = siteelev,
              expratio = expratio,
              expdat = expdat))
}
