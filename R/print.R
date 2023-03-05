#' print.shoreline_date
#'
#' Print the dates in a `shoreline_date` object. Each date is printed
#'  with site name, elevation and highest density region. If the isobase
#'  direction is different or more are provided than the default, the
#'  directions and dates associated with these are printed seperately.
#'
#' @param x Object of class `shoreline_date`.
#' @param ... Additional arguments.
#'
#' @return Print the site names, elevations, non-default isobase directions and
#'  HDRs contained in a `shoreline_date` object to console.
#'
#' @export
#'
#' @examples
#' target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
#'
#' # Reduce date resolution with cal_reso and elevation_reso for speed
#' target_date <- shoreline_date(site = target_point,
#'                               elevation = 70,
#'                               elev_reso = 1,
#'                               cal_reso = 400)
#'
#' # Print to console
#' target_date
print.shoreline_date <- function(x, ...){

  for (i in 1:length(x)) {
    if (all(is.na(x[[i]][[1]]$date$probability))) {

      cat("===============")
      cat("\nSite: ", x[[i]][[1]]$site_name)
      cat("\nElevation: ", x[[i]][[1]]$site_elev, "\n")
      cat("\n",  x[[i]][[1]]$hdr_prob*100, "% HDR:\nNA\n", sep = "")

    } else if (length(x[[i]][[1]]$dispcurve_direction) > 1) {

      cat("===============")
      cat("\nSite: ", x[[i]][[1]]$site_name)
      cat("\nElevation: ", x[[i]][[1]]$site_elev, "\n")
      cat("\nSum of isobase directions: ",
          x[[i]][[1]]$dispcurve_direction, "\n")
      cat("\n",  x[[i]][[1]]$hdr_prob*100, "% HDR:\n", sep = "")
      cat(rbind(paste0(ifelse(grepl("^[-]", x[[i]][[1]]$hdr_start),
                              paste(gsub("^[-]", "",
                                         x[[i]][[1]]$hdr_start), "BCE"),
                              gsub("$", " CE", x[[i]][[1]]$hdr_start)), "-"),
                ifelse(grepl("^[-]", x[[i]][[1]]$hdr_end),
                       gsub("^[-](.*)", "\\1 BCE\n", x[[i]][[1]]$hdr_end),
                       gsub("$", " CE\n", x[[i]][[1]]$hdr_end))), sep = "")

    } else if (length(x[[i]]) == 1 & x[[i]][[1]]$dispcurve_direction == 327) {

      cat("===============")
      cat("\nSite: ", x[[i]][[1]]$site_name)
      cat("\nElevation: ", x[[i]][[1]]$site_elev, "\n")
      cat("\n",  x[[i]][[1]]$hdr_prob*100, "% HDR:\n", sep = "")
      cat(rbind(paste0(ifelse(grepl("^[-]", x[[i]][[1]]$hdr_start),
                              paste(gsub("^[-]", "",
                                         x[[i]][[1]]$hdr_start), "BCE"),
                              gsub("$", " CE", x[[i]][[1]]$hdr_start)), "-"),
                ifelse(grepl("^[-]", x[[i]][[1]]$hdr_end),
                       gsub("^[-](.*)", "\\1 BCE\n", x[[i]][[1]]$hdr_end),
                       gsub("$", " CE\n", x[[i]][[1]]$hdr_end))), sep = "")

    } else if (length(x[[i]]) == 1 & x[[i]][[1]]$dispcurve_direction != 327) {

      cat("===============")
      cat("\nSite: ", x[[i]][[1]]$site_name)
      cat("\nElevation: ", x[[i]][[1]]$site_elev, "\n")
      cat("\nIsobase direction: ", x[[i]][[1]]$dispcurve_direction, "\n")
      cat("\n",  x[[i]][[1]]$hdr_prob*100, "% HDR:\n", sep = "")
      cat(rbind(paste0(ifelse(grepl("^[-]", x[[i]][[1]]$hdr_start),
                              paste(gsub("^[-]", "",
                                         x[[i]][[1]]$hdr_start), "BCE"),
                              gsub("$", " CE", x[[i]][[1]]$hdr_start)), "-"),
                ifelse(grepl("^[-]", x[[i]][[1]]$hdr_end),
                       gsub("^[-](.*)", "\\1 BCE\n", x[[i]][[1]]$hdr_end),
                       gsub("$", " CE\n", x[[i]][[1]]$hdr_end))), sep = "")

    } else if (length(x[[i]]) > 1) {
      cat("===============")
      cat("\nSite: ", x[[i]][[1]]$site_name)
      cat("\nElevation: ", x[[i]][[1]]$site_elev, "\n")
      cat("\nIsobase direction: ", x[[i]][[1]]$dispcurve_direction, "\n")
      cat("\n",  x[[i]][[1]]$hdr_prob*100, "% HDR:\n", sep = "")
      cat(rbind(paste0(ifelse(grepl("^[-]", x[[i]][[1]]$hdr_start),
                              paste(gsub("^[-]", "",
                                         x[[i]][[1]]$hdr_start), "BCE"),
                              gsub("$", " CE", x[[i]][[1]]$hdr_start)), "-"),
                ifelse(grepl("^[-]", x[[i]][[1]]$hdr_end),
                       gsub("^[-](.*)", "\\1 BCE\n", x[[i]][[1]]$hdr_end),
                       gsub("$", " CE\n", x[[i]][[1]]$hdr_end))), sep = "")

      for (j in 2:length(x[[i]])) {
        cat("\nIsobase direction: ", x[[i]][[j]]$dispcurve_direction, "\n")
        cat("\n",  x[[i]][[j]]$hdr_prob*100, "% HDR:\n", sep = "")
        cat(rbind(paste0(ifelse(grepl("^[-]", x[[i]][[j]]$hdr_start),
                                paste(gsub("^[-]", "",
                                           x[[i]][[j]]$hdr_start), "BCE"),
                                gsub("$", " CE", x[[i]][[j]]$hdr_start)), "-"),
                  ifelse(grepl("^[-]", x[[i]][[j]]$hdr_end),
                         gsub("^[-](.*)", "\\1 BCE\n", x[[i]][[1]]$hdr_end),
                         gsub("$", " CE\n", x[[i]][[j]]$hdr_end))), sep = "")
      }
    }
  }
}
