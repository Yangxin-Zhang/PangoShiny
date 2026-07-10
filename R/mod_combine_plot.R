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
#' @import patchwork
#' @importFrom shiny renderPlot renderImage isolate renderTable
#' @importFrom patchwork plot_spacer
#' @importFrom ggplot2 theme
#' @importFrom dplyr bind_rows
#' @importFrom shinyjs show useShinyjs hidden hide
#' @importFrom data.table data.table
#' @noRd 
mod_combine_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    Background_Plot <- magick::image_read(create_Background_Plot()) %>%
      grid::rasterGrob()
    
    ## reactiveVal
    Upload_Times <- reactiveVal(0)
    Upload_Files <- reactiveVal(NULL)
    Uploaded_Plots <- reactiveVal(NULL)
    Loaded_Subplots <- reactiveVal(NULL)
    Inserted_UI_Subplots <- reactiveVal(NULL)
    Add_Times <- reactiveVal(0)
    Remove_Button_Pre_State <- reactiveVal(NULL)
    Remove_Times <- reactiveVal(0)
    Update_Button_Pre_State <- reactiveVal(NULL)
    Update_Times <- reactiveVal(0)
    Param_Info_Datatable <- reactiveVal(NULL)
    Loaded_RAM_plots <- reactiveVal(NULL)
    Area_List <- reactiveVal(NULL)
    Main_Plot_Path <- reactiveVal(tempfile(fileext = "_main.png"))
    Combined_Plot_Main <- reactiveVal(wrap_plots(list("plot_spacer" = plot_spacer()),
                                                 design = do.call(c,list("plot_spacer" = area(1,1,297,297)))))
    Combined_Plot_Main_Show <- reactiveVal(wrap_plots(list("background_plot" = Background_Plot),
                                                      design = do.call(c,list("background_plot" = area(1,1,297,297))))+
                                             plot_annotation(theme = theme(plot.background = element_rect(colour = "black"),
                                                                           plot.margin = margin(0,0,0,0),
                                                                           margins = margin(0,0,0,0))))
    
    ## reactive
    Remove_Buttons_Clicked <- reactive({
      req(Remove_Button_List(),Remove_Button_Pre_State())
      Remove_Button_Pre_State() == Remove_Button_List()
    })
    
    Update_Buttons_Clicked <- reactive({
      req(Update_Button_List(),Update_Button_Pre_State())
      Update_Button_Pre_State() == Update_Button_List()
    })
    
    Plots_Information <- reactive({
      req(Upload_Files())
      # cat("## Plots_Information() \n")
      
      plots_na <- paste("Plot",seq(nrow(Upload_Files())),sep = "_")
      # cat(plots_na,"\n")
      # cat("Update Plot_Information! \n")
      # cat("#### \n")
      # cat("\n")
      return(data.frame("comb_na" = plots_na,
                        "name" = Upload_Files()["name"],
                        "type" = Upload_Files()["type"],
                        "datapath" = Upload_Files()["datapath"]))
      
    })
    
    Remove_Button_List <- reactive({
      req(Plots_Information())
      
      # cat("## Remove_Button_List() \n")
      remove_buttons_na <- paste(isolate(Plots_Information()$comb_na),"remove",sep = "_")
      # cat(remove_buttons_na,"\n")
      remove_buttons_ls <- c()
      for (i in 1:length(remove_buttons_na)) {
        if (is.null(input[[remove_buttons_na[i]]])) {
          # cat("invalidatelater \n")
          invalidateLater(100)
        }else{
          remove_buttons_ls <- c(remove_buttons_ls,input[[remove_buttons_na[i]]])
          # cat(input[[remove_buttons_na[i]]],"\n")
        }
      }
      
      # cat(class(remove_buttons_ls),length(remove_buttons_ls),"\n")
      # cat("Update Remove_Button_List! \n")
      # cat("#### \n")
      # cat("\n")
      
      return(remove_buttons_ls)
    })
    
    Update_Button_List <- reactive({
      
      req(Plots_Information())
      update_buttons_na <- paste(isolate(Plots_Information()$comb_na),"update",sep = "_")
      
      update_buttons_ls <- c()
      for (i in 1:length(update_buttons_na)) {
        if (is.null(input[[update_buttons_na[i]]])) {
          invalidateLater(100)
        }else{
          update_buttons_ls <- c(update_buttons_ls,input[[update_buttons_na[i]]])
        }
      }
      
      return(update_buttons_ls)
      
    })
    
    ## observe
    
    ####
    observe({
      Remove_Times(sum(Remove_Button_List()))
    })
    
    observe({
      Update_Times(sum(Update_Button_List()))
    })
    ####
    
    observe({
      
      # cat("## Update Remove_Button_Pre_State() \n")
      Plots_Information()
      Remove_Button_Pre_State(isolate(Remove_Button_List()))
      
      if (nrow(Plots_Information()) != length(Remove_Button_Pre_State())) {
        # cat("Refresh! \n")
        invalidateLater(100)
      }
      
      # cat("#### \n")
      # cat("\n")
      
    })
    
    observe({
      
      Plots_Information()
      Update_Button_Pre_State(isolate(Update_Button_List()))
      
      if (nrow(Plots_Information()) != length(Update_Button_Pre_State())) {
        invalidateLater(100)
      }
      
    })
    
    ## observeEvent
    
    observeEvent(eventExpr = input$upload_files,
                 handlerExpr = {
                   
                   
                   # cat("## observeEvent input$upload_files \n")
                   if (Upload_Times() != 0) {
                     Uploaded_Plots(Plots_Information())
                     # cat("Upload_Times != 0 /n")
                   }
                   
                   if (is.null(Upload_Files())) {
                     Upload_Files(input$upload_files)
                     Upload_Times(Upload_Times()+1)
                     # cat("Upload_Files():NULL -> Upload_Files()",Upload_Files()$name,"\n")
                   } else{
                     Upload_Files(bind_rows(Upload_Files(),input$upload_files))
                     Upload_Times(Upload_Times()+1)
                   }
                   
                   if (is.null(Loaded_Subplots())) {
                     updateSelectizeInput(
                       inputId = "choose_subplots",
                       choices = Plots_Information()$comb_na,
                       server = FALSE,
                       selected = NULL
                     )
                   } else {
                     updateSelectizeInput(
                       inputId = "choose_subplots",
                       choices = Plots_Information()$comb_na[!Plots_Information()$comb_na %in% Loaded_Subplots()],
                       server = FALSE,
                       selected = NULL
                     )
                   }
                   
                   # cat(Plots_Information()$comb_na,"\n")
                   
                   if (Upload_Times() == 1) {
                     
                     shinyjs::show(("subplot_param"))
                     
                     # cat("Show Subparam \n")
                     
                   }
                   
                   insertUI(selector = paste0("#",ns("subplot_param")),
                            where = "beforeEnd",
                            ui = subplot_param_ui(id = id,plots = Plots_Information()$comb_na[!Plots_Information()$comb_na %in% Uploaded_Plots()$comb_na]),
                            immediate = TRUE)
                   
                   # cat("#### \n")
                   # cat("\n")
                   
                 })
    
    observeEvent(eventExpr = input$choose_subplots,
                 handlerExpr = {
                   if (length(input$choose_subplots != 0)) {
                     
                     shinyjs::show("add_subplots_button")
                     
                   } else {
                     
                     shinyjs::hide("add_subplots_button")
                     
                   }
                 },
                 ignoreNULL = FALSE,
                 ignoreInit = FALSE)
    
    observeEvent(eventExpr = input$add_subplots,
                 handlerExpr = {
                   
                   Add_Times(Add_Times()+input$add_subplots)
                   # req(Add_Times())
                   
                   # cat("## Add Subplots! \n")
                   shinyjs::hide("add_subplots_button")
                   # cat("Hide Add Button! \n")
                   if (is.null(Loaded_Subplots())) {
                     Loaded_Subplots(input$choose_subplots)
                     # cat("Loaded_Subplots():",Loaded_Subplots(),"\n")
                     # cat("Load the first subplot! \n")
                   } else {
                     Loaded_Subplots(unique(c(Loaded_Subplots(),input$choose_subplots)))
                     # cat(Loaded_Subplots(),"\n")
                     # cat("Update Loaded_Subplots! \n")
                   }
                   
                   if (is.null(Inserted_UI_Subplots())) {
                     
                     # cat("Inserted_UI_Subplots is Null! \n")
                     
                     for (i in 1:length(Loaded_Subplots())) {
                       shinyjs::show(paste(Loaded_Subplots()[i],"_param_div",sep = ""))
                     }
                     
                     Inserted_UI_Subplots(Loaded_Subplots())
                     
                     # cat("Inserted_UI_Subplots: ",Inserted_UI_Subplots(),"\n")
                     
                   }
                   
                   if (length(Loaded_Subplots()[!Loaded_Subplots() %in% Inserted_UI_Subplots()]) != 0) {
                     
                     for (i in 1:length(Loaded_Subplots())) {
                       shinyjs::show(paste0(Loaded_Subplots()[i],"_param_div"))
                       # cat("Show: ",Loaded_Subplots()[i])
                     }
                     
                     Inserted_UI_Subplots(Loaded_Subplots())
                     # cat("Update Inserted_UI_Subplots! \n")
                   }
                   
                   output$Chosed_Subplots_Info <-renderTable({
                     Param_Info_Datatable()[(Param_Info_Datatable()$comb_na %in% Loaded_Subplots()),]
                   })
                   
                   updateSelectizeInput(
                     inputId = "choose_subplots",
                     choices = Plots_Information()$comb_na[!Plots_Information()$comb_na %in% Loaded_Subplots()],
                     server = FALSE,
                     selected = NULL
                   )
                   
                   if (is.null(Loaded_RAM_plots())) {
                     Loaded_RAM_plots(load_plots_to_RAM(Plots_Information()[Plots_Information()$comb_na %in% Loaded_Subplots(),]))
                     # Loaded_RAM_plots(load_plots_to_RAM(Plots_Information()))
                   }else{
                     # cat("refresh Loaded_RAM_plots \n")
                     new_comb_na <- setdiff(Loaded_Subplots(),names(Loaded_RAM_plots()))
                     
                     # cat(new_comb_na,"\n")
                     if (length(new_comb_na) != 0) {
                       new_RAM_plots <- load_plots_to_RAM(Plots_Information()[Plots_Information()$comb_na %in% new_comb_na,])
                       Loaded_RAM_plots(c(Loaded_RAM_plots(),new_RAM_plots))
                     }
                     
                   }
                   
                   # cat("#### Completely! \n")
                   # cat("\n")
                 },
                 ignoreInit = FALSE)
    
    observeEvent(eventExpr = Remove_Times(),
                 handlerExpr = {
                   
                   # cat("## Observe Remove_Times() Changing! \n")
                   
                   if (sum(Remove_Button_Pre_State())==Remove_Times()) {
                     
                     # cat("Remove Button were not been clicked! \n")
                     
                   } else {
                     
                     # cat("Remove Buttons were clicked! \n")
                     
                     Loaded_Subplots(Loaded_Subplots()[Loaded_Subplots() %in% Plots_Information()$comb_na[Remove_Buttons_Clicked()]])
                     # cat("Loaded_Subplots(): ",Loaded_Subplots(),"\n")
                     Inserted_UI_Subplots(Loaded_Subplots())
                     
                     # cat(paste0(Plots_Information()$comb_na[!Remove_Buttons_Clicked()],"_param_div"),"\n")
                     shinyjs::hide(paste0(Plots_Information()$comb_na[!Remove_Buttons_Clicked()],"_param_div"))
                     
                     
                     updateSelectizeInput(
                       inputId = "choose_subplots",
                       choices = Plots_Information()$comb_na[!Plots_Information()$comb_na %in% Loaded_Subplots()],
                       server = FALSE,
                       selected = NULL
                     )
                     
                     output$Chosed_Subplots_Info <-renderTable({
                       Param_Info_Datatable()[(Param_Info_Datatable()$comb_na %in% Loaded_Subplots()),]
                     })
                     
                     Remove_Button_Pre_State(isolate(Remove_Button_List()))
                     
                   }
                   
                   # cat("#### Completely! \n")
                   # cat("\n")
                   
                 })
    
    observeEvent(eventExpr = Update_Times(),
                 handlerExpr = {
                   
                   Param_Info_Datatable(update_plot_params(
                     input = input,
                     session = session,
                     subplot = isolate(Plots_Information())$comb_na[!isolate(Update_Buttons_Clicked())],
                     param_table = isolate(Param_Info_Datatable())
                   ))
                   
                   Update_Button_Pre_State(isolate(Update_Button_List()))
                   
                 })
    
    observeEvent(eventExpr = Plots_Information(),
                 handlerExpr = {
                   req(Plots_Information())
                   # cat("## Initiate Param_Info_Datatable() \n")
                   # cat(colnames(Plots_Information()),"\n")
                   if (is.null(Param_Info_Datatable())) {
                     param_info_df <- Plots_Information()
                     param_info_df$plot_na <- NA
                     param_info_df$plot_group <- NA
                     param_info_df$loc_top <- NA
                     param_info_df$loc_bottom <- NA
                     param_info_df$loc_left <- NA
                     param_info_df$loc_right <-NA
                     Param_Info_Datatable(param_info_df)
                     
                     # cat(nrow(Param_Info_Datatable()),":",ncol(Param_Info_Datatable()),"\n")
                     # cat(colnames(Param_Info_Datatable()), "\n")
                     # cat("init \n")
                   } else {
                     
                     exist_plt <- as.character(unlist(Param_Info_Datatable()["comb_na"]))
                     new_plt <- as.character(unlist(Plots_Information()["comb_na"]))
                     param_info_df <- Plots_Information()[!new_plt %in% exist_plt,]
                     
                     param_info_df$plot_na <- NA
                     param_info_df$plot_group <- NA
                     param_info_df$loc_top <- NA
                     param_info_df$loc_bottom <- NA
                     param_info_df$loc_left <- NA
                     param_info_df$loc_right <-NA
                     param_info_df <- bind_rows(Param_Info_Datatable(),param_info_df)
                     Param_Info_Datatable(param_info_df)
                     
                     # cat(paste((Param_Info_Datatable()["comb_na"]),collapse = " "),"\n")
                     # cat("revise \n")
                   }
                   
                   # cat("#### Complete! \n")
                 })
    
    observeEvent(eventExpr = input$upload_param_info_files,
                 handlerExpr = {
                   shinyjs::hide("param_file_input_button")
                   shinyjs::show("lock_param_file_input")
                 })
    
    observeEvent(eventExpr = input$reload_param_info_files,
                 handlerExpr = {
                   shinyjs::hide("lock_param_file_input")
                   shinyjs::show("param_file_input_button")
                 })
    
    observeEvent(eventExpr = Param_Info_Datatable(),
                 handlerExpr = {
                   req(Param_Info_Datatable())
                   Area_List(generate_area_list(Param_Info_Datatable()))
                 })
    
    observeEvent(eventExpr = input$refresh_main_comb_plot,
                 handlerExpr = {
                   req(Loaded_RAM_plots(),Area_List(),Loaded_Subplots())
                   
                   # cat("#### \n refresh main panel \n")
                   # cat(length(Area_List()[Loaded_Subplots()]),"\n")
                   # cat(class(Area_List()[Loaded_Subplots()][1]),"\n")
                   # cat(is.null(Area_List()[Loaded_Subplots()][[1]]),"\n")
                   # cat(paste0(!is.null(Area_List()[Loaded_Subplots()])),"\n")
                   
                   null_counts <- 0
                   for (subplot in Loaded_Subplots()) {
                     if (is.null(Area_List()[[subplot]])) {
                       null_counts <- null_counts+1
                     }
                   }
                   
                   # cat("null counts: ",null_counts,"\n")
                   
                   if (null_counts == 0) {
                     
                     comb_plot <- wrap_plots(c(list("plot_spacer" = plot_spacer()),
                                               Loaded_RAM_plots()[Loaded_Subplots()]),
                                             design = do.call(c,c(list("plot_spacer" = area(1,1,297,297)),
                                                                  Area_List()[Loaded_Subplots()]))) +
                       plot_annotation(theme = theme(plot.background = element_rect(colour = "black"),
                                                     plot.margin = margin(0,0,0,0),
                                                     margins = margin(0,0,0,0)))
                     comb_plot_show <- wrap_plots(c(list("plot_spacer" = plot_spacer(),
                                                         "background_plot" = Background_Plot),
                                               Loaded_RAM_plots()[Loaded_Subplots()]),
                                             design = do.call(c,c(list("plot_spacer" = area(1,1,297,297),
                                                                       "background_plot" = area(1,1,297,297)),
                                                                  Area_List()[Loaded_Subplots()]))) +
                       plot_annotation(theme = theme(plot.background = element_rect(colour = "black"),
                                                     plot.margin = margin(0,0,0,0),
                                                     margins = margin(0,0,0,0)),
                                       tag_levels = "a",
                                       tag_prefix = "(",
                                       tag_suffix = ")")
                     Combined_Plot_Main(comb_plot)
                     Combined_Plot_Main_Show(comb_plot_show)
                   }
                   
                 })
    
    observeEvent(eventExpr = Combined_Plot_Main(),
                 handlerExpr = {
                   req(Combined_Plot_Main(),Combined_Plot_Main_Show())
                   ggsave(filename = Main_Plot_Path(),
                          plot = Combined_Plot_Main_Show(),
                          device = "png",
                          width = 297,
                          height = 297,
                          units = "mm",
                          dpi = 300)
                 })
    ## output
    output$Combined_Plot <- renderImage({
      req(Combined_Plot_Main())
      list(
        src = Main_Plot_Path(),
        contentType = "image/png",
        width = "100%",
        height = "100%"
      )
    },
    deleteFile = FALSE)
    
    output$Test_Text <- renderText({
      req(Main_Plot_Path(),Combined_Plot_Main())
      paste0(Main_Plot_Path(),"\n")
    })
    
    output$Upload_Time <- renderText({
      if (is.null(Upload_Files())) {
        paste("Upload Time",0,sep = ":")
      } else {
        paste("Upload Time",Upload_Times(),sep = ":")
      }
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
      tagList(
        useShinyjs(),
        hidden(card(
          id = ns("subplot_param"),
          card_title("Subplot Param")
        )))
    })
    
    output$Add_Subplots_Button <- renderUI({
      
      tagList(
        useShinyjs(),
        hidden(div(
          id = ns("add_subplots_button"),
          actionButton(
            inputId = ns("add_subplots"),
            label = "Add"
          )
        )
        )
      )
    })
    
    output$Chosed_Subplots_Info <- renderTable({NULL})
    
    output$Chosed_Subplots_Info_Card <- renderUI({
      req(Loaded_Subplots())
      
      tagList(
        br(),
        card(
          id = ns("chosed_subplots_info_card"),
          full_screen = TRUE,
          uiOutput(outputId = ns("full_screen_chosed_subplots_info_card"))
        )
      )
    })
    
    output$full_screen_subplots_info_table_card <- renderUI({
      if (isTRUE(input$subplots_info_table_card_full_screen)) {
        card_body(tableOutput(outputId = ns("Subplots_Info_Table")))
      } else {
        card_title("Plots Information")
      }
    })
    
    output$full_screen_chosed_subplots_info_card <- renderUI({
      if (isTRUE(input$chosed_subplots_info_card_full_screen)) {
        card_body(tableOutput(outputId = ns("Chosed_Subplots_Info")))
      } else {
        card_title("Param Information")
      }
    })
    
    output$param_file_input <- renderUI({
      
      tagList(      
        useShinyjs(),
        div(
          id = ns("param_file_input_button"),
          fileInput(
            inputId = ns("upload_param_info_files"),
            label = "Upload Param File", 
            multiple = FALSE
          )
        ),
        hidden(div(
          id = ns("lock_param_file_input"),
          card_body(card_title("Param Is Upload")),
          actionButton(inputId = ns("reload_param_info_files"),
                       label = "Reload")
        ))
      )
    })
    
    output$Download_Plots <- downloadHandler(
      filename = "test_main.png",
      content = function(file){
        # cat(file,"\n")
        ggsave(file,
               plot = Combined_Plot_Main(),
               dpi = 600,
               device = "png",
               width = 297,
               height = 297,
               units = "mm")
      }
    )
    
  })
}

## To be copied in the UI
# mod_combine_plot_ui("combine_plot_1")

## To be copied in the server
# mod_combine_plot_server("combine_plot_1")
