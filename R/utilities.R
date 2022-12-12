#' Internal helper function for unpacking nested lists of plots
#'
#' Utility function to extract a nested list of ggplots. Code taken from answers
#'  on Stackoverflow. From user \@divibsan here:
#'  https://stackoverflow.com/a/55733001 who in turn based their answer on user
#'  \@Michael here: https://stackoverflow.com/a/41882883
#'
#' @param x Nested list of ggplot .
#'
#' @return List of ggplot objects
#' @keywords internal
flatten_list <- function(x){
  morelists <- sapply(x, function(xprime) {
    'list' %in% class(xprime) & !('gg' %in% class(xprime))
  })
  out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}

#' Internal helper function for custom text annotation
#'
#' The code for this function is taken from an answer on SO given by user
#'  Rosen Matev: https://stackoverflow.com/a/22500252/11376454
#'  This allows for the placement of a multi-line text-label using `Inf`,
#'  which can be problematic with `ggplot2::annotate`.
#'
#'
#' @param label Text label.
#' @param x Numerical value to adjust label position relative to the extremes of
#'  the x-axis.
#' @param y Numerical value to adjust label position relative to the extremes of
#'  the y-axis.
#' @param hjust Numerical value indicating the horizontal justification of the
#'  text label.
#' @param vjust Numerical value indicating the vertical justification of the
#'  text label.
#' @param thm Get thematic parameters from the ggplot object to which the
#'  annotation is added.
#'
#' @import ggplot2
#' @importFrom grid textGrob gpar unit.c viewport grobWidth grobHeight editGrob grobTree
#' @importFrom scales squish_infinite
#'
#' @return A ggplot layer holding text label
#' @keywords internal
annotate_custom <- function(label, x, y, hjust = 0,
                            vjust = 0, thm = ggplot2::theme_get()) {

  x <- scales::squish_infinite(x)
  y <- scales::squish_infinite(y)
  data <- data.frame(x = NA)


  size = thm$text$size
  margin = ggplot2::unit(size/2, 'pt')
  alpha = NA
  color = 'black'
  box_just = ifelse(c(x, y) < 0.5, 0,1)

  tg <- grid::textGrob(
    label, x = 0, y = 0, hjust = 0, vjust = 0,
    gp = grid::gpar(col = alpha(color, alpha),
                    fontsize = thm$text$size, fontfamily = thm$text$family,
                    fontface = 1, lineheight = 1.0)
  )
  ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
  vp <- grid::viewport(x = x, y = y, width = ts[1],
                       height = ts[2], just = box_just)
  tg <- grid::editGrob(tg, x = ts[1]*hjust, y = ts[2]*vjust, vp = vp)
  inner <- grid::grobTree(tg,
                          vp = grid::viewport(width = unit(1, 'npc')-margin*2,
                                              height = unit(1, 'npc')-margin*2))

  ggplot2::layer(
    data = NULL,
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = TRUE,
    params = list(
      grob = grid::grobTree(inner),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    )
  )
}

