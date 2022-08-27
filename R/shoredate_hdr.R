#' Find 95% highest density region of shoreline date
#'
#' Function to find 95% highest density region (HDR) for a provided shoreline date.
#'
#' @param shorelinedate A list of objects returned from shoreline_date().
#'
#' @return A data frame holding  start and end points for segments of the 95% highest density region
#' @export
#'
#' @import hdrcde
#'
#' @examples
#' # Create point to shoreline date
#' target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
#' # Perform shoreline dating
#' target_date <- shoreline_date(site = target_pt, elevation = 65)
#' # Retrieve and print HDR for the shoreline date
#' (shoredate_hdr(target_date))
shoredate_hdr <- function(shorelinedate){

  dategrid <- shorelinedate$date

  dathdr <- hdrcde::hdr(den = list("x" = dategrid$bce,
                                   "y" = dategrid$probability), prob = 95)
  segdat <- data.frame(t(dathdr$hdr))
  datedat <- data.frame(matrix(nrow = length(segdat[seq(1, nrow(segdat), 2),]),
                                ncol = 4))

  names(datedat) <- c("start", "end", "group", "year_median")
  datedat$start <- segdat[seq(1, nrow(segdat), 2), "X95."]
  datedat$end <- segdat[seq(2, nrow(segdat), 2), "X95."]
  datedat$hdr_segment <- seq(1:length(segdat$hdr[c(TRUE, FALSE)]))

  return(datedat)

}
