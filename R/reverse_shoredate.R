#' Reverse a shoreline date to an elevation
#'
#' Go from a single calendar date to a corresponding elevation by reversing the
#' procedure for shoreline dating. Th ecode is in large part repurposed from Bchron::unCalibrate()
#'
#' @param shoreline_date
#' @param displacement_curve
#'
#' @return
#' @export
#'
#' @examples
reverse_shoredate <- function(shoreline_date,
                              displacement_curve){

  lwelev <- round(stats::approx(displacement_curve$years,
                                displacement_curve$lowerelev,
                                xout = shoreline_date,
                                rule = 1
  )$y, 1)

  upelev <- round(stats::approx(displacement_curve$years,
                                displacement_curve$upperelev,
                                xout = shoreline_date,
                                rule = 1
  )$y, 1)

  if(!(is.na(lwelev) | is.na(upelev))){
    # In case the displacement curves intersect
    minelev <- min(lwelev, upelev)
    maxelev <- max(lwelev, upelev)

    # Make sure elevation is never 0 (i.e. present day sea-level)
    if(minelev <= 0){
      minelev <- 0.05
    }

    return(sample(seq(minelev, maxelev, 0.05), 1))
  } else{
    return(NA)
  }
}
