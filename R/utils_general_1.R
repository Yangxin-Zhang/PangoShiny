#' nav_spacers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

nav_spacers <- function(n = 1) {
  lapply(1:n, function(x) nav_spacer())
}

#' theme_publish_pango_r
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

theme_publish_pango_r <- function(){
  
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
  
}