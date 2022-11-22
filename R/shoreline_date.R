#' Shoreline date
#'
#' A function for shoreline dating Stone Age sites based on their present-day elevation, likely elevation above sea-level when in use and the trajectory of past shoreline displacement on the Norwegian Skagerrak coast.
#'
#' @param sites Vector giving one or more site names, or, if displacement curves are to be interpolated, objects of class `sf` representing the sites to be dated. In the case of a spatial geometry, the first column is taken as site name.
#' @param elevation Numeric elevation values to inform shoreline dates unless an elevation raster is provided.
#' @param elev_raster Elevation raster to be input if the elevation values are not provided manually.
#' @param elev_reso Numeric value specifying the resolution with which to step through the elevation distance between site and shoreline. Defaults to 0.001m.
#' @param cal_reso Numeric value specifying the resolution to use on the calendar scale. Must be a power of 10. Defaults to 10.
#' @param isobase_direction A single numeric value or a vector of values defining the direction(s) of the isobases. Defaults to 327.
#' @param model_parameters Vector of numeric values specifying the shape
#' @param elevavg Specified statistic to define elevation if this is to be derived from elevation raster. Defaults to mean.
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
#' # Create example points
#' target_points <- sf::st_sfc(sf::st_point(c(538310, 65442551)), sf::st_point(c(538300, 65442500)))
#'
#' Set these to the required coordinate system WGS84 UTM32N (EPSG: 32632).
#' target_points <- st_set_crs(target_points, 32632)
#'
#' # Date target points, manually specifying the elevations instead of providing an elevation raster.
#' target_dates <- shoreline_date(sites = target_points, elevation = c(46, 60))
#'
#' # Call to plot
#' shoredate_plot(target_dates)
shoreline_date <- function(sites,
                           elevation = NA,
                           elev_raster = NA,
                           elev_reso = 0.01,
                           cal_reso = 10,
                           isobase_direction = 327,
                           model_parameters = c(0.286, 0.048),
                           elevavg = "mean",
                           interpolated_curve = NA,
                           normalise = TRUE,
                           sparse = FALSE){

  if(cal_reso %% 10 != 0) {
    stop("Resolution on calendar scale must be powers of 10 (including 1).")
  }

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

  # Make sure the geometries are represented as a sf data frame
  # (and not for example sfc)
  if(!inherits(sites, c("sf", "data.frame"))){
    sites <- st_as_sf(sites, crs = st_crs(sites))
  }

  shorelinedates <- list()
  for(i in 1:nrow(sites)){
    print(paste("Dating site", i, "of", nrow(sites)))

    if(all(is.na(interpolated_curve)) & length(unique(isobases$direction)) > 1){
      sitecurve <- interpolate_curve(target = sites[i,],
                                     isobases = isobases, cal_reso = cal_reso)
    } else if(all(is.na(interpolated_curve))){
      sitecurve <- interpolate_curve(target = sites[i,],
                                     isobases = isobases, cal_reso = cal_reso)
    } else{
      sitecurve <- interpolated_curve
    }

    if(inherits(sitecurve, "list")){
      sitecurve <- do.call(rbind.data.frame, sitecurve)
    }

    if(!(inherits(elev_raster, c("raster","SpatRaster")))){
      if(any(is.na(elevation))){
        return(NA)
      } else{
        siteelev <- elevation[i]
      }
    } else{
      siteelev <- terra::extract(elev_raster,
                                 terra::vect(sites[i,]), fun = elevavg)[,-1]
    }

    # Elevation offsets to step through by increments of elev_reso
    inc <- seq(0, siteelev, elev_reso)

    # Perform one shoreline date per isobase direction
    date_isobases <- list()
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

      for(j in 1:nrow(gammadat)){
        # Subtract offset
        adjusted_elev <- as.numeric(siteelev - gammadat$offset[j])

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

          # Identify range, rounding to closest cal_reso (requires powers of 10)
          year_range <- round(seq(earliest, latest, cal_reso),
                              digits = (floor(log10(cal_reso / 10)) + 1)* -1)
          # Identify probability to be distributed across range
          prob <- 1/length(year_range) * gammadat$probs[j]

          # Add probability to calendar years
          dategrid[dategrid$bce %in% year_range, "probability"] <-
            dategrid[dategrid$bce %in% year_range, "probability"] + prob
        }
      }

      if(inherits(sites, c("sf", "sfc"))) {
        if(ncol(sites) == 1){
        dategrid$site_name <- paste("Site", i)
        } else{
        dategrid$site_name <- as.character(st_drop_geometry(sites[i,1]))
        }
      } else{
        dategrid$site_name <- sites[i, 1]
      }

      print(sum(dategrid$probability))

      # Normalise to sum to unity
      if(normalise){
        dategrid$probability <- dategrid$probability / sum(dategrid$probability)
      }

      # Update list holding results
      if(!sparse){
        date_isobases[[k]] <- list(date = dategrid,
                    dispcurve = temp_curve,
                    elev = siteelev,
                    model_parameters = model_parameters,
                    gammadat = gammadat,
                    cal_reso = cal_reso)
      } else {
        date_isobases[[k]] <- dategrid
      }
    }
  shorelinedates[[i]] <- date_isobases
  }
  return(shorelinedates)
}
