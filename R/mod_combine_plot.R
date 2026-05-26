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
    
    ## reactive
    Upload_Times <- reactiveVal(0)
    Upload_Files <- reactiveVal(NULL)
    Loaded_Subplots <- reactiveVal(NULL)
    
    upload_file <- reactive({
      req(input$upload_files)
      input$upload_files
    })
    
    Plots_Information <- reactive({
      req(Upload_Files())
      plots_na <- paste("Plot",seq(nrow(Upload_Files())),sep = "_")
      return(data.frame("comb_na" = plots_na,
                        "name" = Upload_Files()["name"]))
    })
    
    ## observeEvent
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
    
    observeEvent(eventExpr = input$upload_files,
                 handlerExpr = {
                   req(Plots_Information())
                   updateSelectizeInput(
                     inputId = "choose_subplots",
                     choices = Plots_Information()[,"comb_na"],
                     server = FALSE,
                     selected = NULL
                   )
                 })
    
    observeEvent(eventExpr = input$choose_subplots,
                 handlerExpr = {
                   output$Add_Subplots_Button <- renderUI({
                     actionButton(inputId = ns("add_subplots"),
                                  label = "Add")
                   })
                 })
    
    observeEvent(eventExpr = input$add_subplots,
                 handlerExpr = {
                   output$Add_Subplots_Button <- renderUI({NULL})
                   if (is.null(Loaded_Subplots())) {
                     Loaded_Subplots(input$choose_subplots)
                   } else {
                     Loaded_Subplots(unique(c(Loaded_Subplots(),input$choose_subplots)))
                   }
                 })
    
    observeEvent(eventExpr = Loaded_Subplots(),
                 handlerExpr = {
                   req(Loaded_Subplots())
                   
                   choosed_id <- Plots_Information()[,"comb_na"] %in% Loaded_Subplots()
                   output$Chosed_Subplots_Info <-renderTable({
                     Plots_Information()[choosed_id,]
                   })
                   
                   insertUI(selector = paste0("#",ns("subplot_param")),
                            where = "beforeEnd",
                            ui = textInput(inputId = ns("test"),label = "TEst"))
                   
                 })
    
    ## output
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
      req(Loaded_Subplots())
      class(Loaded_Subplots())
    })
    
    output$Upload_Time <- renderText({
      req(Upload_Files())
      paste("Upload Time",Upload_Times(),sep = ":")
    })
    
    output$Test_Table <- renderTable({
      req(Plots_Information())
      Plots_Information()
    })
    
    output$Subplots_Info_Table <- renderTable({
      req(Plots_Information())
      Plots_Information()
    })
    
    output$SubPlot_Param <- renderUI({
      card(
        id = ns("subplot_param"),
        card_title("Subplot Param")
      )
    })
    
    output$Add_Subplots_Button <- renderUI({NULL})
    
    output$Chosed_Subplots_Info <- renderTable({NULL})
    
  })
}

## To be copied in the UI
# mod_combine_plot_ui("combine_plot_1")

## To be copied in the server
# mod_combine_plot_server("combine_plot_1")
