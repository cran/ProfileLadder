# zzz.R

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("profileLadder.colors"))) {
    default_colors <- list(
      col.known = crayon::make_style("#333333"), ### run-off triangle
      col.predicted = crayon::make_style("#CC00CC"),
      col.unknown = crayon::make_style("#999999"), 
      col.info = crayon::make_style("#CC00CC")
    )
    options(profileLadder.colors = default_colors)
  }
}