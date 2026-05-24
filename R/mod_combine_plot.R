#' combine_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib layout_sidebar page_fluid
mod_combine_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    page_fluid(layout_sidebar(fillable = TRUE,
                              sidebar = combine_plots_sidebar(id),
                              combine_plots_layout_columns(id)))
    
  )
}

#' combine_plot Server Functions
#'
#' @importFrom shiny renderPlot renderImage
#' @importFrom patchwork plot_spacer
#' @importFrom ggplot2 theme
#' @noRd 
mod_combine_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$Combined_Plot <- renderImage({
      list(
        src = "/home/youngxin/Documents/PangoICH/Graph/background_v2.png",
        contentType = "image/png",
        width = "100%",
        height = "100%"
      )
    },
    deleteFile = FALSE)
    
  })
}

## To be copied in the UI
# mod_combine_plot_ui("combine_plot_1")

## To be copied in the server
# mod_combine_plot_server("combine_plot_1")
