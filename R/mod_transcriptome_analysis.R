#' transcriptome_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_transcriptome_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
    page_fluid(layout_sidebar(fillable = TRUE,
                              sidebar = transcriptome_analysis_sidebar(id),
                              transcriptome_analysis_main_panel(id)))
    
  )
}
    
#' transcriptome_analysis Server Functions
#'
#' @importFrom ggiraph renderGirafe
#' @noRd 
mod_transcriptome_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    Anndata_Obs <- reactiveVal(NULL)
    Selected_Points <- reactiveVal(NULL)
    Point_Graph_ggplot <- reactiveVal(NULL)
    
    # observeevnt
    
    observeEvent(eventExpr = input$h5ad_file_path,
                 handlerExpr = {
                   req(input$h5ad_file_path)

                   shinyjs::hide("h5ad_file_path_js")
                   shinyjs::show("uploaded_h5ad_js")
                   
                   Anndata_Obs(get_h5ad_obs(File_Path = input$h5ad_file_path))
                   
                   cat("File Path: ",input$h5ad_file_path,"\n")
                   cat(class(Anndata_Obs()),"\n")
                   cat(paste(colnames(Anndata_Obs()),collapse = " "),"\n")
                   
                 })
    
    observeEvent(eventExpr = input$reload_h5ad,
                 handlerExpr = {
                   
                   shinyjs::hide("uploaded_h5ad_js")
                   shinyjs::show("h5ad_file_path_js")
                   
                 })
    
    observeEvent(eventExpr = input$Point_Graph_selected,
                 handlerExpr = {
                   
                   selected_adata_obs <- Anndata_Obs()[Anndata_Obs()$barcode %in% input$Point_Graph_selected,]
                   Selected_Points(selected_adata_obs)
                   
                   # cat(length(Anndata_Obs()$barcode),"\n")
                   # cat(length(input$Point_Graph_selected),"\n")
                   # cat(length(Anndata_Obs()$barcode %in% input$Point_Graph_selected),"\n")
                   cat("update selected points \n")
                   
                 })
    
    observeEvent(eventExpr = Anndata_Obs(),
                 handlerExpr = {
                   req(Anndata_Obs())
                   
                   Point_Graph_ggplot(transcriptome_point_plot(plt_dt = Anndata_Obs()))
                   
                   cat("create point graph \n")
                   
                 })
    # output
    output$Point_Graph <- renderGirafe({
      req(Point_Graph_ggplot())
      
      ggiraph_obj <- isolate(Point_Graph_ggplot())
      
      return(girafe(ggobj = ggiraph_obj))
      
    })
    
    output$test_ggiraph <- renderGirafe({
      req(Point_Graph_ggplot())
      
      ggiraph_obj <- isolate(Point_Graph_ggplot())
      
      return(girafe(ggobj = ggiraph_obj))
      
    })
    
    output$test_text <- renderText({
      req(Point_Graph_ggplot())
      text1 <- paste0(class(Anndata_Obs()$barcode),"\n")
      # text2 <- paste0(ncol(Selected_Points())," : ",nrow(Selected_Points()),"\n")
      cat("print brushed points \n")
      return(paste(text1,sep = "\n"))
    })
    
    output$test_table <- renderTable({
      req(Selected_Points())
      Selected_Points()
    })
    
    output$text_table_card_ui <- renderUI({
      if (isTRUE(input$test_table_card_full_screen)) {
        card_body(tableOutput(outputId = ns("test_table")))
      }else{
        card_title("Test Table")
      }
    })
 
  })
}
    
## To be copied in the UI
# mod_transcriptome_analysis_ui("transcriptome_analysis_1")
    
## To be copied in the server
# mod_transcriptome_analysis_server("transcriptome_analysis_1")
