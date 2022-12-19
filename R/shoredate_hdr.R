#' Highest density region of shoreline dates
#'
#' Function to find 95% highest density region (HDR) for a provided shoreline
#'  date. Negative values denote years BCE while positive values denote CE.
#'
#' @param bce A vector holding calendar years associated with a date
#' @param probability A vector holding the probability corresponding to each
#'  year in `date`.
#' @param site_name A vector holding the name of the site that has been dated.
#' @param cal_reso Resolution on the calendar scale used when dating the site.
#' @param prob A numerical value indicating the probability coverage of the HDR.
#'  Defaults to 0.95.
#'
#' @return A data frame holding start and end points for segments of the highest
#'  density region.
#' @export
#'
#' @examples
#' # Create point to shoreline date
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
#'
#' # Perform shoreline dating
#' target_date <- shoreline_date(sites = target_point, elevation = 65)
#'
#' # shoredate_hdr() is already called under the hood with shoreline_date, and
#' # is printed with the date
#' target_date
#'
#' # However, shoredate_hdr() can be applied separately by pulling the necessary
#' # data from the date
#' (shoredate_hdr(target_date[[1]][[1]]$date$bce,
#'                target_date[[1]][[1]]$date$probability,
#'                target_date[[1]][[1]]$site_name,
#'                target_date[[1]][[1]]$cal_reso))
shoredate_hdr <- function(bce, probability, site_name, cal_reso, prob = 0.95){

  # Extend the calendar scale to individual years for the HDR to work
  # irrespective of calendar resolution
  date <- data.frame(bce = head(seq(max(bce), min(bce)), -1),
                      probability = rep(head(probability, -1),
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

  # [Code below does not follow Parnell]

  # Round to nearest cal_reso
  out <- round(out, digits = (floor(log10(cal_reso / 10)) + 1)* -1)

  # Collapse HDRs that adjoin on the resolution of cal_reso

  # Check if end value is cal_reso from next start value, except last row.
  # This returns indices of end values more than cal_reso from next start.
  end_indices <- which(abs(out$end -  c(tail(out$start, -1), 0)) > abs(cal_reso))
  end_breaks <- out$end[end_indices]

  # First select start breaks of HDRs. First row is always included, last row
  # could be, but is here made NA.
  start_breaks <- out$start[c(1, end_indices + 1)]
  # Remove the NA resulting from the last row
  start_breaks <- start_breaks[!is.na(start_breaks)]

  # # We know that the start value of the last row is included if the
  # # second to last end break is at the index nrow()-1
  # if(head(tail(end_indices, 2), 1) == nrow(out) - 1 &&
  #    out$start[nrow(out)] != out$end[nrow(out)]){
  #   start_breaks <- c(start_breaks, out$start[nrow(out)])
  # }

  shdr <- list(start = start_breaks,
                     end = end_breaks,
                     prob = prob,
                     site_name = site_name)
  shdr
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
