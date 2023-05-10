#' Shoreline date
#'
#' A function for shoreline dating Stone Age sites based on their present-day
#'  elevation, their likely elevation above sea-level when in use and the
#'  trajectory of past shoreline displacement. Details and caveats pertaining to
#'  the implemented method is given in Roalkvam (2023).
#'
#' @param sites Vector giving one or more site names, or, if displacement curves
#'   are to be interpolated, objects of class `sf` representing the sites to be
#'   dated. In the case of a spatial geometry, the first column is taken as the
#'   site name.
#' @param elevation Vector of numeric elevation values for each site or a
#'  an elevation raster of class `SpatRaster` from the package
#'  `terra` from where the elevation values are to be derived.
#' @param elev_reso Numeric value specifying the resolution with which to step
#'  through the distribution representing the distance between site and
#'  shoreline. Defaults to 0.01m.
#' @param cal_reso Numeric value specifying the resolution to use on the
#'   calendar scale. Defaults to 10.
#' @param isobase_direction A vector of numeric values defining the direction(s)
#'   of the isobases. Defaults to 327.
#' @param sum_isobase_directions Logical value indicating that if multiple
#'  isobase directions are specified in `isobase_direction` the results should
#'  be summed for each site using `sum_shoredates`. Defaults to FALSE.
#' @param model Character vector specifying the statistical model with which to
#'  model the distance from site to shoreline. Currently accepts either "none"
#'  or "gamma". Defaults to "gamma".
#' @param model_parameters Vector of numeric values specifying the parameters
#'  for the statistical model describing the distance between site and
#'  shoreline. Defaults to c(0.286, 20.833), denoting the shape and scale of the
#'   default gamma function, respectively.
#' @param elev_fun Statistic to define site elevation if this is to be derived
#'  from an elevation raster. Uses `terra::extract()`. Defaults to mean.
#' @param upper_temp_limit Numerical value giving the upper temporal limit.
#'  Dates with a start date after the limit are returned as NA. Defaults to
#'  -2500, i.e. 2500 BCE.
#' @param target_curve Data frame holding precomputed shoreline displacement
#'  curve. This has to have the same or higher resolution on the calendar scale
#'  as that specified with `cal_reso`. [interpolate_curve()] will be run if
#'  nothing is is not provided to `target_curve`. Defaults to NA.
#' @param hdr_prob Numeric value specifying the coverage of the highest density
#'  region. Defaults to 0.95.
#' @param normalise Logical value specifying whether the shoreline date should
#'  be normalised to sum to unity. Defaults to TRUE.
#' @param sparse Logical value specifying if only site name and shoreline date
#'  should be returned. Defaults to FALSE. Note that of the functions for
#'  further treatment, sparse dates are only compatible with
#'  [sum_shoredates()].
#' @param verbose Logical value indicating whether progress should be printed to
#'  console. Defaults to FALSE.
#'
#' @return A nested list of class `shoreline_date` holding the shoreline date
#'  results and associated metadata for each dated site for each isobase
#'  direction. The elements of each date is:
#'
#'  * `site_name` name of the site.
#'  * `site_elev` elevation of the site.
#'  * `date` data frame with the columns `bce` where negative values
#'  indicate years BCE and positive CE, as well as `probability`, which gives
#'  the probability mass for each year.
#'  * `weighted_mean` the weighted mean date.
#'  * `hdr_start` start values for the HDR ranges.
#'  * `hdr_end` end values for the HDR ranges.
#'  * `hdr_prob` probability level for the HDR.
#'  * `dispcurve` data frame holding the displacement curve used for dating
#'  the site. This has the columns `bce`, giving years BCE/CE. `lowerelev`,
#'  the lower limit for the elevation of the shoreline for each year.
#'  `upperelev`, the upper limit for elevation of the shoreline for each year.
#'  * `dispcurve_direction` direction of the isobases in use.
#'  * `model_parameters` parameters for the statistical model.
#'  * `modeldat` data frame holding the model distribution. The column
#'  `offset` denotes the vertical distance (m) from the shoreline, as specified
#'  by the `elev_reso` argument. `px` is the cumulative probability at each step
#'  of `offset`, and `probs` is the probability of each step found by
#'  subtracting the preceding value from each value of `px`.
#'  * `cal_reso` resolution on the calendar scale.
#'
#' @export
#'
#' @references
#' Roalkvam, I. 2023. A simulation-based assessment of the relation between
#'  Stone Age sites and relative sea-level change along the Norwegian Skagerrak
#'  coast. \emph{Quaternary Science Reviews} 299:107880. DOI:
#'  https://doi.org/10.1016/j.quascirev.2022.107880
#'
#' @examples
#' # Create example point using the required CRS WGS84 UTM32N (EPSG: 32632)
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
#'
#' # Date target point, manually specifying the elevation instead of providing
#' # an elevation raster. Reducing elev_reso and cal_reso for speed.
#' shoreline_date(sites = target_point,
#'                elevation = 80,
#'                elev_reso = 1,
#'                cal_reso = 400)
shoreline_date <- function(sites,
                           elevation = NA,
                           elev_reso = 0.01,
                           cal_reso = 10,
                           isobase_direction = 327,
                           sum_isobase_directions = FALSE,
                           model = "gamma",
                           model_parameters = c(0.286, 20.833),
                           elev_fun = "mean",
                           upper_temp_limit = -2500,
                           target_curve = NA,
                           hdr_prob = 0.95,
                           normalise = TRUE,
                           sparse = FALSE,
                           verbose = FALSE){

  # Make sfc geometries be represented as a sf data frame
  if(!inherits(sites, c("sf", "data.frame"))){
    if (all(is.na(target_curve)) & inherits(sites, "sfc")) {
      sites <- sf::st_as_sf(sites, crs = sf::st_crs(sites))
      # Make vector of site names a data frame for nrow() below
    } else {
      sites <- as.data.frame(sites)
    }
  }

  if (!(is.numeric(elevation) |
        (inherits(elevation, c("raster","SpatRaster"))))){
    stop("Numeric values specifying the site elevations or an elevation raster must be provided.")
  }

  if(is.numeric(elevation) & length(elevation) != nrow(sites)){
    stop(paste("Specify one elevation value per site.", length(elevation),
               "elevation values and", nrow(sites), "sites were provided."))
  }

  # If isobase directions other than the default is provided, create these
  if (all(is.na(target_curve)) & any(isobase_direction != 327)) {
    isobases <- create_isobases(isobase_direction)
    # If the default is used, load the precompiled isobases
  } else{
    isobases <- sf::st_read(
      system.file("extdata/isobases.gpkg",
                  package = "shoredate",
                  mustWork = TRUE), quiet = TRUE)
  }

  # List to hold dates
  shorelinedates <- list()
  for (i in 1:nrow(sites)) {

    if (verbose) {
      print(paste("Site", i, "of", nrow(sites)))
    }

    # If a target curve is not provided and multiple isobase directions have
    # been specified, interpolate these
    if (all(is.na(target_curve)) &
        length(unique(isobases$direction)) > 1) {

      sitecurve <- interpolate_curve(target = sites[i,],
                                     isobases = isobases, cal_reso = cal_reso,
                                     verbose = verbose)
      # If default isobase direction is to be used, but no interpolated curve
      # is provided
    } else if (all(is.na(target_curve))) {
      sitecurve <- interpolate_curve(target = sites[i,],
                                     isobases = isobases, cal_reso = cal_reso,
                                     verbose = verbose)
    } else {
      # If pre-compiled/interpolated curve(s) are provided
      sitecurve <- target_curve
    }

    if (inherits(sitecurve, "list")) {
      sitecurve <- do.call(rbind.data.frame, sitecurve)
    }

    # If there is no direction to the provided target curve, specify that this
    # is NA.
    if (!("direction" %in% names(sitecurve))) {
      sitecurve$direction <- NA
    }

    # Create sequence of years BCE matching displacement curves
    # (suppressWarsnings() as different lengths should also return FALSE)
    if (suppressWarnings(all((seq(-1950, 10550,
                                  cal_reso) * -1) ==  sitecurve$bce))) {

      bce <- seq(-1950, 10550,  cal_reso) * -1
    } else {
    # A provided target_curve could cover a different time-span, in which
    # case an adjusted bce element is created
      bce <- seq(min(sitecurve$bce), max(sitecurve$bce), cal_reso)
    }

    # Make sure the resolution of the provided curve matches cal_reso.
    # If not, downsample so that it does.
    if ((sitecurve$bce[1]-sitecurve$bce[2]) != cal_reso) {

      upperelev <- stats::approx(sitecurve[,"bce"],
                                 sitecurve[,"upperelev"],
                                 xout = bce)[["y"]]
      lowerelev <- stats::approx(sitecurve[,"bce"],
                          sitecurve[,"lowerelev"],
                          xout = bce)[["y"]]

      sitecurve <- data.frame(bce,
                              upperelev,
                              lowerelev,
                              "direction" = unique(sitecurve$direction))
    }

    if (!inherits(elevation, c("raster", "SpatRaster"))) {
      # if (any(is.na(elevation))) {
      #   return(NA)
      # } else {
      siteelev <- elevation[i]
    } else {
      siteelev <- terra::extract(elevation,
                                 terra::vect(sites[i,]), fun = elev_fun)[,-1]
    }

    # Elevation offsets to step through by increments of elev_reso
    inc <- seq(0, siteelev, elev_reso)

    # Perform one shoreline date per isobase direction.
    date_isobases <- list()
    for (k in 1:length(unique(sitecurve$direction))) {

      if(!(is.na(unique(sitecurve$direction)[k]))){
        temp_curve <- sitecurve[sitecurve$direction ==
                                unique(sitecurve$direction)[k],]
      } else {
        temp_curve <- sitecurve
      }

      # Make sure the displacement curve is sorted descending
      # temp_curve <- temp_curve[order(temp_curve$bce, decreasing = TRUE),]

      # Set up data frame to hold results
      dategrid <- data.frame(
        bce = bce,
        probability = 0)

      # Assign site name
      if (inherits(sites, "sf") &
          ncol(as.data.frame(sites)) == 1) {
        site_name <- as.character(i)
      } else if (inherits(sites, "sf")){
        site_name <- as.character(sf::st_drop_geometry(sites)[i,1])
      } else {
        site_name <- as.character(sites[i, 1])
      }

      # Find oldest possible date
      mdate <- temp_curve[which(temp_curve[,"lowerelev"] ==
                                  max(temp_curve[,"lowerelev"],
                                      na.rm = TRUE)), "bce"]

      # Check that site date is not above this limit
      msdate <- stats::approx(temp_curve[,"lowerelev"],
                              bce, xout = siteelev, method = "constant")[['y']]

      # If it is msdate will be NA and the date is returned as NA with a
      # warning. Do not print isobase direction if this is
      # the default.
      if (is.na(msdate) & is.na(unique(temp_curve$direction))) {
        warning(paste0("The elevation of site ", site_name,
                      " implies an earliest possible date older than ", mdate,
                      " BCE and is out of bounds. The date is returned as NA."))
        dategrid$probability <- NA
        modeldat <- NA
      } else if(is.na(msdate) & unique(temp_curve$direction) == 327) {
        warning(paste0("The elevation of site ", site_name,
                       " implies an earliest possible date older than ", mdate,
                       " BCE and is out of bounds. The date is returned as NA."))
        dategrid$probability <- NA
        modeldat <- NA
      } else if (is.na(msdate)) {
        warning(paste0("The elevation of site ", site_name,
                       " with an isobase direction of ",
                       unique(temp_curve$direction),
                       " implies an earliest possible date older than ", mdate,
                       " BCE and is out of bounds. The date is returned as NA."))
        dategrid$probability <- NA
        modeldat <- NA
      } else {

        if(model == "gamma"){
          # Set up data frame and assign probability to the offset increments
          modeldat <- data.frame(
            offset = inc,
            px = stats::pgamma(inc,
                               shape = model_parameters[1],
                               scale =  model_parameters[2]))
          modeldat$probs <- c(diff(modeldat$px), 0)
          modeldat <- modeldat[modeldat$px < 0.99999,]
        } else if(model == "none"){
          modeldat <- data.frame(
            offset = 0,
            probs = 1
          )
        }

        if (verbose) {
          print("Performing shoreline dating")
          pb <- utils::txtProgressBar(min = 0,
                                      max = nrow(modeldat),
                                      style = 3,
                                      char = "=")
        }

        for (j in 1:nrow(modeldat)) {

          # Subtract offset
          adjusted_elev <- as.numeric(siteelev - modeldat$offset[j])

          # Find lower date
          lowerd <- stats::approx(temp_curve[,"lowerelev"], bce,
                                  xout = adjusted_elev,
                                  method = "constant")[['y']]

          # Find upper date
          upperd <- stats::approx(temp_curve[,"upperelev"], bce,
                                  xout = adjusted_elev,
                                  method = "constant")[['y']]

          # Find youngest and oldest date
          earliest <- min(c(lowerd, upperd))
          latest <- max(c(lowerd, upperd))

          # Add probability to each year in range
          if (!is.na(latest) && !is.na(earliest)) {

            # Identify range, rounding to closest cal_reso
            year_range <- seq(earliest, latest, cal_reso)

            # Identify probability to be distributed across range
            prob <- 1/length(year_range) * modeldat$probs[j]

            # Add probability to calendar years
            dategrid[dategrid$bce %in% year_range, "probability"] <-
              dategrid[dategrid$bce %in% year_range, "probability"] + prob
          }
          if (verbose) {
            utils::setTxtProgressBar(pb, j)
          }
        }
        if (verbose) {
          close(pb)
        }
      }

      # Make the date NA and return a warning if it's latest possible start is
      # younger than upper limit (defaults to 2500 BCE). Also return isobase
      # direction if this is not the default.
      if (!all(is.na(dategrid$probability))) {
        if (min(dategrid$bce[dategrid$probability > 0]) > upper_temp_limit) {
          dategrid$probability <- NA

          if (unique(temp_curve$direction) == 327) {
            warning(paste("Site", site_name,
                          "has a younger possible start date than 2500 BCE and is returned as NA."))
          } else {
            warning(paste("Site", site_name,
                          "has a younger possible start date than 2500 BCE",
                          "with an isobase direction of",
                          unique(temp_curve$direction),
                          "and is returned as NA."))
          }
        }
      }


      # Normalise to sum to unity
      if (!(all(is.na(dategrid$probability))) & normalise) {
        dategrid$probability <- dategrid$probability / sum(dategrid$probability)
      }

      # Update list holding results
      if (!(all(is.na(dategrid$probability))) & !sparse) {

        hdr <- shoredate_hdr(dategrid$bce, dategrid$probability,
                             site_name, cal_reso, hdr_prob)

        date_isobases[[k]] <- list(
          site_name = site_name,
          site_elev = siteelev,
          date = dategrid,
          weighted_mean = hdr$weighted_mean,
          hdr_start = hdr$start,
          hdr_end = hdr$end,
          hdr_prob = hdr_prob,
          dispcurve = temp_curve[, names(temp_curve) %in%
                                   c("bce", "lowerelev", "upperelev")],
          dispcurve_direction = unique(temp_curve$direction),
          model_parameters = model_parameters,
          modeldat = modeldat,
          cal_reso = cal_reso)

      } else if (all(is.na(dategrid$probability)) & !sparse){
        date_isobases[[k]] <- list(
          site_name = site_name,
          site_elev = siteelev,
          date = dategrid,
          weighted_mean = NA,
          hdr_start = NA,
          hdr_end = NA,
          hdr_prob = hdr_prob,
          dispcurve = temp_curve[, names(temp_curve) %in%
                                   c("bce", "lowerelev", "upperelev")],
          dispcurve_direction = unique(temp_curve$direction),
          model_parameters = model_parameters,
          modeldat = modeldat,
          cal_reso = cal_reso)
      } else {
        date_isobases[[k]] <- dategrid
      }
    }

    if (sum_isobase_directions) {
      sum_isobases <- sum_shoredates(date_isobases, normalise = normalise)

      # Find the HDR for the sum
      hdr <- shoredate_hdr(sum_isobases$sum$bce, sum_isobases$sum$probability,
                           site_name, cal_reso, hdr_prob)

      date_isobases <- list(list(
        site_name = site_name,
        site_elev = siteelev,
        date = sum_isobases$sum,
        weighted_mean = hdr$weighted_mean,
        hdr_start = hdr$start,
        hdr_end = hdr$end,
        hdr_prob = hdr_prob,
        dispcurve = NA,
        dispcurve_direction = unlist(lapply(date_isobases,
                              function(x) unique(x["dispcurve_direction"]))),
        model_parameters = model_parameters,
        modeldat = modeldat,
        cal_reso = cal_reso
      ))

    }

    shorelinedates[[i]] <- date_isobases

  }

  class(shorelinedates) <- c("shoreline_date", class(shorelinedates))

  shorelinedates
}
