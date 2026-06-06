#' transcriptome_point_plot
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import ggplot2
#' @import ggiraph
#' @noRd

transcriptome_point_plot <- function(plt_dt){
  
  cat(class(plt_dt),"\n")
  
  # plt_dt <- data.frame("spatial_x" = c(1:10),
  #                      "spatial_y" = c(1:10))
  pt_plt <- ggplot() +
    geom_point_interactive(data = plt_dt,
                           mapping = aes(x = -transfered_spatial_x,
                                         y = transfered_spatial_y,
                                         colour = leiden,
                                         data_id = barcode,
                                         tooltip = barcode),
                           size = 1)+
    coord_cartesian(ratio = 1) +
    theme(panel.background = element_rect(colour = "grey90",
                                          fill = "white"),
          plot.background = element_rect(colour = "white",
                                         fill = "white"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  return(pt_plt)
  
}

#' transcriptome_tile_plot
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import ggplot2
#' @import ggiraph
#' @noRd

transcriptome_tile_plot() <- function(plt_dt){
  
  tl_plt <- ggplot() +
    geom_tile_interactive(data = plt_dt,
                          mapping = aes()) +
    coord_cartesian(ratio = 1) +
    theme(panel.background = element_rect(colour = "grey90",
                                          fill = "white"),
          plot.background = element_rect(colour = "white",
                                         fill = "white"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  return(tl_plt)
  
}