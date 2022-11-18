#' Highest density region of shoreline date
#'
#' Function to find 95% highest density region (HDR) for a provided shoreline date. If the resolution of the shoreline date is too low, this can break down. Code is taken from Parnell
#'
#' @param shorelinedate A list of objects returned from shoreline_date().
#' @param prob A numerical value indicating the probability coverage of the HDR. Defaults to 0.95.
#'
#' @return A data frame holding  start and end points for segments of the 95% highest density region
#' @export
#'
#' @examples
#' # Create point to shoreline date
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
#'
#' # Perform shoreline dating
#' target_date <- shoreline_date(site = target_point, elevation = 65)
#'
#' # Retrieve and print HDR for the shoreline date
#' (shoredate_hdr(target_date))
shoredate_hdr <- function(shorelinedate, prob = 0.95){

  # Check if results are passed listed or not.
  if(is.null(names(shorelinedate))){
    date <- shorelinedate[[1]]$date
    cal_reso <- shorelinedate[[1]]$cal_reso
  } else{
    date <- shorelinedate$date
    cal_reso <- shorelinedate$cal_reso
  }

  # Extend the calendar scale to individual years for the HDR to work
  # irrespective of calendar resolution
  date <- data.frame(bce = head(seq(max(date$bce), min(date$bce)), -1),
                      probability = rep(head(date$probability, -1),
                                        each = cal_reso))
  # Re-normalise probability to sum to unity
  date$probability <- date$probability/sum(date$probability)

  # Code (and comments to code) to follow is taken from Parnell's Bchron
  # package: https://github.com/andrewcparnell/Bchron/blob/master/R/hdr.R

  ag <- date$bce
  de <- date$probability

  # Put the probabilities in order of density
  o <- order(de)
  cu <- cumsum(de[o])

  # Find which ones are above the threshold
  good_cu <- which(cu > 1 - prob)
  good_ag <- sort(ag[o][good_cu])

  # Pick out the extremes of each range
  breaks <- diff(good_ag) > 1
  where_breaks <- which(diff(good_ag) > 1)
  n_breaks <- sum(breaks) + 1
  # Store output
  # [Slightly modified this to use a data frame instead of list]
  out <- data.frame()
  low_seq <- 1
  high_seq <- ifelse(length(where_breaks) == 0, length(breaks), where_breaks[1])
  for (i in 1:n_breaks) {
    out <- rbind(out, c(good_ag[low_seq], good_ag[high_seq]))
    # curr_dens <- round(100 * sum(de[o][seq(good_cu[low_seq],
    #                                        good_cu[high_seq])]), 1)
    # names(out)[[i]] <- paste0(as.character(curr_dens), "%")
    low_seq <- high_seq + 1
    high_seq <- ifelse(i < (n_breaks - 1), where_breaks[i + 1], length(good_ag))
  }
  names(out) <- c("start", "end")

  # Round up to nearest cal_reso
  out <- out + (cal_reso - out %% cal_reso)

  return(out)
}

# hdrdat <- hdrcde::hdr(den = list("x" = date$bce,
#                                  "y" = date$probability), prob = 95)
# segdat <- data.frame(t(hdrdat$hdr))
# hdrsegs <- data.frame(matrix(nrow = length(segdat[seq(1, nrow(segdat), 2),]),
#                              ncol = 5))
# names(hdrsegs) <- c("site_name", "start", "end", "group", "year_median")
#
# hdrsegs$site_name <-  unique(date$site_name)
# hdrsegs$start <- segdat[seq(1, nrow(segdat), 2), "X95."]
# hdrsegs$end <- segdat[seq(2, nrow(segdat), 2), "X95."]
# hdrsegs$group <- seq(1:length(hdrdat$hdr[c(TRUE, FALSE)]))
# hdrsegs$year_median <- median(as.numeric(hdrdat$mode))
#
# return(hdrsegs)







