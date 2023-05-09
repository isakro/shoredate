#' Internal helper function for unpacking nested lists of plots
#'
#' Utility function to extract a nested list of ggplots. Code taken from answers
#'  on Stackoverflow. From user \@divibsan here:
#'  https://stackoverflow.com/a/55733001 who in turn based their answer on user
#'  \@Michael here: https://stackoverflow.com/a/41882883
#'
#' @param x Nested list of `ggplot` objects.
#'
#' @return List of ggplot objects
#' @keywords internal
#' @noRd
flatten_list <- function(x){
  morelists <- sapply(x, function(xprime) {
    'list' %in% class(xprime) & !('gg' %in% class(xprime))
  })

  out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
  if (sum(morelists)) {
    Recall(out)
  } else{
    return(out)
  }
}
