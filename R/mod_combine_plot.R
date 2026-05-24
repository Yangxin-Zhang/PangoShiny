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
#' @importFrom shiny renderPlot renderImage isolate renderTable
#' @importFrom patchwork plot_spacer
#' @importFrom ggplot2 theme
#' @importFrom dplyr bind_rows
#' @noRd 
mod_combine_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    Upload_Times <- reactiveVal(0)
    Upload_Files <- reactiveVal(NULL)
    
    upload_file <- reactive({
      req(input$upload_files)
      input$upload_files
    })
    
    observeEvent(eventExpr = input$upload_files,
                 handlerExpr = {
                   if (is.null(Upload_Files())) {
                     Upload_Files(upload_file())
                     Upload_Times(Upload_Times()+1)
                   } else{
                     Upload_Files(bind_rows(Upload_Files(),upload_file()))
                     Upload_Times(Upload_Times()+1)
                   }
                 })
    
    output$Combined_Plot <- renderImage({
      list(
        src = "/home/youngxin/Documents/PangoICH/Graph/background_v2.png",
        contentType = "image/png",
        width = "100%",
        height = "100%"
      )
    },
    deleteFile = FALSE)
    
    output$Test_Text <- renderText({
      req(Upload_Files())
      paste("Upload Time",Upload_Times(),sep = ":")
    })
    
    output$Test_Table <- renderTable({
      req(Upload_Files())
      Upload_Files()[c("name","type")]
    })
    
  })
}

## To be copied in the UI
# mod_combine_plot_ui("combine_plot_1")

## To be copied in the server
# mod_combine_plot_server("combine_plot_1")
