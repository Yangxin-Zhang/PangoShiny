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
  
  # cat(class(plt_dt),"\n")
  
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

#' transcriptome_point_plot_plotly
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import ggplot2
#' @import plotly
#' @noRd

transcriptome_point_plot_plotly <- function(plt_dt){
  
  # cat(class(plt_dt),"\n")
  
  # plt_dt <- data.frame("spatial_x" = c(1:10),
  #                      "spatial_y" = c(1:10))
  pt_plt <- ggplot() +
    geom_point(data = plt_dt,
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
          axis.ticks.y = element_blank(),
          legend.location = "none")
  
  return(ggplotly(pt_plt))
  
}

#' transcriptome_tile_plot
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import ggplot2
#' @import ggiraph
#' @import reshape2
#' @noRd

transcriptome_tile_plot <- function(img_mat){
  
  plt_dt <- melt(img_mat)
  colnames(plt_dt) <- c("x", "y", "value")
  bk_dt <- data.frame("bk_x" = c(plt_dt$x,(plt_dt$x+max(plt_dt$x))),
                      "bk_y" = c(plt_dt$y,plt_dt$y))
  
  tl_plt <- ggplot(data = bk_dt,
                   mapping = aes(x = bk_x,
                                 y = bk_y)) +
    scale_x_continuous(limits = c(1,max(bk_dt$bk_x)),
                       expand = expansion()) +
    scale_y_continuous(limits = c(1,max(bk_dt$bk_y)),
                       expand = expansion()) +
    geom_tile(data = plt_dt,
              mapping = aes(x = x,
                            y = y,
                            fill = value)) +
    scale_fill_continuous_interactive(low = "black",
                                      high = "white") +
    coord_fixed() +
    theme(panel.background = element_rect(colour = "grey90",
                                          fill = "white"),
          plot.background = element_rect(colour = "white",
                                         fill = "white"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "none")
  
  return(tl_plt)
  
}

#' transcriptome_combined_plot
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import ggplot2
#' @import ggiraph
#' @import reshape2
#' @noRd

transcriptome_combined_plot <- function(pt_df,img_mt){
  
  suppressWarnings({
    downsampled_img_mt <- downsample_greyscale_image_pangor(greyscale_img = img_mt,
                                                            width = 78*6,
                                                            height = 64*6)
    img_df <- melt(downsampled_img_mt)
    colnames(img_df) <- c("x", "y", "value")
    
    img_df$img_x <- scales::rescale(img_df$x,to = c(1,78))
    img_df$img_y <- scales::rescale(-img_df$y,to = c(1,64))
    
    pt_df$pt_x <- scales::rescale(-pt_df$transfered_spatial_x, to = c(1,78))
    pt_df$pt_y <- scales::rescale(pt_df$transfered_spatial_y, to = c(65,128))
    pt_df$tl_x <- scales::rescale(-pt_df$transfered_spatial_x,to = c(1.5,77.5))
    pt_df$tl_y <- scales::rescale(pt_df$transfered_spatial_y,to = c(1.5,63.5))
    
    plt_df <- data.frame("bk_x" = rep(seq(1:78),times = 64*2),
                         "bk_y" = rep(seq(1:(64*2)),each = 78))
    
    cmb_plt <- ggplot(data = plt_df,
                      mapping = aes(x = bk_x,
                                    y = bk_y)) +
      scale_x_continuous(limits = c(0,max(plt_df$bk_x)+1),
                         expand = expansion(0)) +
      scale_y_continuous(limits = c(0,max(plt_df$bk_y)+1),
                         expand = expansion(0)) +
      geom_point_interactive(data = pt_df,
                             mapping = aes(x = pt_x,
                                           y = pt_y,
                                           colour = leiden,
                                           data_id = barcode,
                                           tooltip = barcode),
                             size = 1) +
      geom_tile(data = img_df,
                mapping = aes(x = img_x,
                              y = img_y,
                              fill = value)) +
      scale_fill_continuous(low = "black",
                            high = "white") +
      geom_tile_interactive(data = pt_df,
                            mapping = aes(x = tl_x,
                                          y = tl_y,
                                          data_id = barcode,
                                          tooltip = barcode),
                            alpha = 0,
                            fill = "white") +
      # coord_fixed() +
      theme(panel.background = element_rect(colour = "grey90",
                                            fill = "white"),
            plot.background = element_rect(colour = "white",
                                           fill = "white"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            legend.position = "none")
  })
  
  return(cmb_plt)
  
}


#' theme_pangoshiny
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @noRd
#' @export

theme_pangoshiny <- function(){
  
  theme_shiny <- theme(plot.background = element_rect(),
                       panel.background = element_rect(),
                       plot.margin = margin(5,5,5,5),
                       plot.title = element_text(vjust = 0.5,
                                                 hjust = 0.5,
                                                 margin = margin(5,0,0,0),
                                                 size = 12,
                                                 face = "plain"),
                       axis.title = element_text(family = "Arial",
                                                 vjust = 0.5,
                                                 hjust = 0.5,
                                                 margin = margin(0,0,0,0)),
                       plot.tag = element_text())
  
  return(theme_shiny)
  
}