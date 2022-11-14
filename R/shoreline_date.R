#' Shoreline date
#'
#' A function for shoreline dating a Stone Age site based on its present-day elevation, it's likely elevation above sea-level when in use and the trajectory of past shoreline displacement on the Norwegian Skagerrak coast.
#'
#' @param site Vector giving site name, or, if displacement curve is to be interpolated, an object of class `sf` representing the site to be dated.
#' @param elev_raster Elevation raster to be input if the elevation values are not provided manually.
#' @param elev_reso Numeric value specifying the resolution with which to step through the elevation distance between site and shoreline. Defaults to 0.01m.
#' @param cal_reso Numeric value specifying the resolution to use on the calendar scale. Defaults to 1.
#' @param isobase_direction A single numeric value or a vector of values defining the direction(s) of the isobases. Defaults to 327.
#' @param model_parameters Vector of numeric values specifying the shape \eqn{\alpha} and rate \eqn{\sigma} of the gamma distribution. Defaults to \eqn{\alpha} = 0.286 and \eqn{\sigma} = 0.048.
#' @param elevavg Specified statistic to define elevation if this is to be derived from elevation raster. Defaults to mean.
#' @param elevation Numeric elevation value to inform shoreline date unless an elevation raster is provided.
#' @param interpolated_curve List holding shoreline displacement curve. interpolate_curve() will be run if this is not provided.
#' @param normalise Logical value specifying whether the shoreline date should be normalised to sum to unity. Defaults to TRUE.
#' @param sparse Logical value specifying if only site name and shoreline date should be returned. Defaults to FALSE.
#'
#' @return A list containing the shoreline date and associated parameters.
#' @export
#'
#' @import sf
#' @import terra
#'
#' @examples
#' # Create example point using the required coordinate system WGS84 UTM32N (EPSG: 32632).
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
#'
#' # Date target point, manually specifying the elevation instead of providing an elevation raster.
#' target_date <- shoreline_date(site = target_point, elevation = 46)
#'
#' # Call to plot
#' shoredate_plot(target_date)
shoreline_date <- function(site,
                           elev_raster = NA,
                           elev_reso = 0.01,
                           cal_reso = 1,
                           isobase_direction = 327,
                           model_parameters = c(0.286, 0.048),
                           elevavg = "mean",
                           elevation = NA,
                           interpolated_curve = NA,
                           normalise = TRUE,
                           sparse = FALSE){

  bce <- seq(-1950, 10550,  cal_reso) * -1 # Sequence of years to match
                                          # displacement data

  if(all(is.na(interpolated_curve)) & any(isobase_direction != 327)){
      isobases <- create_isobases(isobase_direction)
  } else{
    isobases <- sf::st_read(
      system.file("extdata/isobases.gpkg",
                  package = "shoredate",
                  mustWork = TRUE), quiet = TRUE)
  }

  if(all(is.na(interpolated_curve)) & length(unique(isobases$direction)) > 1){
    sitecurve <- interpolate_curve(target = site,
                                   isobases = isobases, cal_reso = cal_reso)
  } else if(all(is.na(interpolated_curve))){
    sitecurve <- interpolate_curve(target = site,
                                   isobases = isobases, cal_reso = cal_reso)
  } else{
    sitecurve <- interpolated_curve
  }

  if(inherits(sitecurve, "list")){
    sitecurve <- do.call(rbind.data.frame, sitecurve)
  }

  if(!(inherits(elev_raster, c("raster","SpatRaster")))){
    if(is.na(elevation)){
      return(NA)
    } else{
      siteelev <- elevation
    }
  } else{
    siteelev <- terra::extract(elev_raster,
                               terra::vect(site), fun = elevavg)[,-1]
  }

  # Elevation offsets to step through by increments of elev_reso
  inc <- seq(0, siteelev, elev_reso)

  shorelinedate <- list()
  for(k in 1:length(unique(sitecurve$direction))){

    temp_curve <- sitecurve[sitecurve$direction ==
                              unique(sitecurve$direction)[k],]
    # Set up data frame and assign probability to the offset increments

    gammadat <- data.frame(
      offset = inc,
      px = stats::pgamma(inc, shape = model_parameters[1],
                  rate =  model_parameters[2]))
    gammadat$probs <- c(diff(gammadat$px), 0)
    gammdat <- gammadat[gammadat$px < 0.99999,]

    dategrid <- data.frame(
      bce = bce,
      probability = 0)

    for(i in 1:nrow(gammadat)){
      # Subtract offset
      adjusted_elev <- as.numeric(siteelev - gammadat$offset[i])

      # Find lower date
      lowerd <- round(stats::approx(temp_curve[,"lowerelev"],
                             bce, xout = adjusted_elev)[['y']])

      # Find upper date
      upperd <- round(stats::approx(temp_curve[,"upperelev"],
                             bce, xout = adjusted_elev)[['y']])

      # Find youngest and oldest date
      earliest <- min(c(lowerd, upperd))
      latest <- max(c(lowerd, upperd))

      # Add probability to each year in range
      if(!is.na(latest) && !is.na(earliest)){

        year_range <- seq(earliest, latest, 1)
        prob <- 1/length(year_range) * gammadat$probs[i]

        dategrid[dategrid$bce %in% year_range, "probability"] <-
          dategrid[dategrid$bce %in% year_range, "probability"] + prob
      }
    }

    if(inherits(site, c("sf", "sfc"))) {
      dategrid$site_name <- as.character(sf::st_drop_geometry(site[1]))
    } else{
      dategrid$site_name <- site
    }

    # Normalise to sum to unity
    if(normalise){
      dategrid$probability <- dategrid$probability / sum(dategrid$probability)
    }

    # Update list holding results
    if(!sparse){
      shorelinedate[[k]] <- list(date = dategrid,
                  dispcurve = temp_curve,
                  elev = siteelev,
                  model_parameters = model_parameters,
                  gammadat = gammadat)
    } else {
      shorelinedate[[k]] <- dategrid
    }
  }
  return(shorelinedate)
}
