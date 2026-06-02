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
#' @importFrom shinyjs show useShinyjs hidden hide
#' @importFrom data.table data.table
#' @noRd 
mod_combine_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ## reactiveVal
    Upload_Times <- reactiveVal(0)
    Upload_Files <- reactiveVal(NULL)
    Uploaded_Plots <- reactiveVal(NULL)
    Loaded_Subplots <- reactiveVal(NULL)
    Inserted_UI_Subplots <- reactiveVal(NULL)
    Add_Times <- reactiveVal(0)
    Remove_Button_Pre_State <- reactiveVal(NULL)
    Remove_Times <- reactiveVal(0)
    Param_Info_Datatable <- reactiveVal(NULL)
    
    ## reactive
    Remove_Buttons_Clicked <- reactive({
      req(Remove_Button_List(),Remove_Button_Pre_State())
      Remove_Button_Pre_State() == Remove_Button_List()
    })
    
    Chosed_Plots <- reactive({
      input$choose_subplots
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
                        "name" = Upload_Files()["name"]))
      
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
    
    Add_Subplots <- reactive({
      input$add_subplots
    })
    
    ## observe
    observe({
      Remove_Times(sum(Remove_Button_List()))
    })
    
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
    
    ## observeEvent
    observeEvent(eventExpr = Add_Subplots(),
                 handlerExpr = {
                   Add_Times(Add_Times()+Add_Subplots())
                 })
    
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
    
    observeEvent(eventExpr = Chosed_Plots(),
                 handlerExpr = {
                   if (length(Chosed_Plots()) != 0) {
                     
                     shinyjs::show("add_subplots_button")
                     
                   } else {
                     
                     shinyjs::hide("add_subplots_button")
                     
                   }
                 },
                 ignoreNULL = FALSE,
                 ignoreInit = FALSE)
    
    observeEvent(eventExpr = input$add_subplots,
                 handlerExpr = {
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
                     Plots_Information()[(Plots_Information()$comb_na %in% Loaded_Subplots()),]
                   })
                   
                   updateSelectizeInput(
                     inputId = "choose_subplots",
                     choices = Plots_Information()$comb_na[!Plots_Information()$comb_na %in% Loaded_Subplots()],
                     server = FALSE,
                     selected = NULL
                   )
                   
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
                     
                     Remove_Button_Pre_State(Remove_Button_List())
                     
                   }
                   
                   # cat("#### Completely! \n")
                   # cat("\n")
                   
                 })
    
    observeEvent(eventExpr = Plots_Information(),
                 handlerExpr = {
                   req(Plots_Information())
                   cat("## Initiate Param_Info_Datatable() \n")
                   cat(colnames(Plots_Information()),"\n")
                   if (is.null(Param_Info_Datatable())) {
                     param_info_df <- Plots_Information()
                     param_info_df$plot_na <- NA
                     param_info_df$loc_top <- NA
                     Param_Info_Datatable(param_info_df)
                     
                     cat(nrow(Param_Info_Datatable()),":",ncol(Param_Info_Datatable()),"\n")
                     cat(colnames(Param_Info_Datatable()), "\n")
                   } else {
                     param_info_df <- Param_Info_Datatable()[!Param_Info_Datatable()["comb_na"] %in% Param_Info_Datatable()["comb_na"]]
                     param_info_df$plot_na <- NA
                     param_info_df$loc_top <- NA
                     param_info_df <- bind_rows(Param_Info_Datatable(),param_info_df)
                     Param_Info_Datatable(param_info_df)
                     
                     cat(nrow(Param_Info_Datatable()),":",ncol(Param_Info_Datatable()),"\n")
                     cat(colnames(Param_Info_Datatable()), "\n")
                   }
                   
                   cat("#### Complete! \n")
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
      p1 <- paste(Remove_Button_List(),collapse = " ")
      p2 <- paste(Remove_Button_Pre_State(),collapse = " ")
      paste0(p1,"::",p2)
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
      Param_Info_Datatable()
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
    
  })
}

## To be copied in the UI
# mod_combine_plot_ui("combine_plot_1")

## To be copied in the server
# mod_combine_plot_server("combine_plot_1")
