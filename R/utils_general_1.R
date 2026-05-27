#' nav spacer stack
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

nav_spacers <- function(n = 1) {
  lapply(1:n, function(x) nav_spacer())
}
