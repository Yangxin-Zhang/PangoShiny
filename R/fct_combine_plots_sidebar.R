#' combine_plots_sidebar 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib sidebar accordion_panel accordion
#' @importFrom shiny fileInput selectInput br
#' @noRd
combine_plots_sidebar <- function(id) {
  ns <- NS(id)
  
  sidebar(
    fillable = TRUE,
    
    card(
      fileInput(
        inputId = ns("upload_files"),
        label = "Upload Files", 
        multiple = TRUE
      ),
      
      br(),
      
      textOutput(outputId = ns("Upload_Time")),
      
    ),
    
    selectizeInput(
      inputId = ns("choose_subplots"),
      label = "Choose Subplots",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Choose Subplots",
        maxOptions = 20,
        minLength = 1)
    ),
    
    uiOutput(outputId = ns("Add_Subplots_Button")),
    
    card(
      id = ns("subplots_info_table_card"),
      full_screen = TRUE,
      uiOutput(ns("full_screen_subplots_info_table_card"))
    ),
    
    card(
      id = ns("upload_parma_info_excel"),
      fileInput(
        inputId = ns("upload_param_info_files"),
        label = "Upload Param File", 
        multiple = FALSE
      )
    )
  )
  
}

#' combine_plots_layout_columns 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib layout_columns card accordion accordion_panel
#' @importFrom shiny textInput tableOutput textOutput br
#' @noRd

combine_plots_layout_columns <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    fill = TRUE,
    col_widths = c(9,3),
    
    tabsetPanel(
      id = ns("viewer_panel"),
      tabPanel(title = "test",
               card(
                 id = ns("viewer_card"),
                 plotOutput(
                   outputId = ns("Combined_Plot"),
                   height = "auto"
                 )
               )
      )
    ),
    
    card(
      id = ns("sidebar_card"),
      card(
        textInput(inputId = ns("page_size"),
                  label = "Page Size",
                  updateOn = "blur",
                  value = "210,210",
                  placeholder = "210X210(mm)"),
        
        textOutput(ns("Test_Text"))
      ),
      br(),
      uiOutput(outputId = ns("SubPlot_Param")),
      br(),
      uiOutput(outputId = ns("Chosed_Subplots_Info_Card"))
    ),
    
    tags$script(
      HTML(
        sprintf(
          "
    const viewer = document.getElementById('%s');
    new ResizeObserver(entries => {
        const w = viewer.offsetWidth;
        viewer.style.height = w + 'px';
    }).observe(viewer)
  ",
          ns("viewer_card")
        )))
  )
  
}

#' subplot_param_ui 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib layout_columns card accordion accordion_panel
#' @importFrom shiny textInput tableOutput textOutput br
#' @importFrom shinyjs useShinyjs hidden
#' @noRd

subplot_param_ui <- function(id,plots){
  ns <- NS(id)
  ui_ls <- vector("list",length = length(plots))
  names(ui_ls) <- plots
  
  for (i in 1:length(plots)) {
    ui_ls[i] <- list(
      
      tagList(
        useShinyjs(),
        hidden(div( id = ns(paste0(plots[i],"_param_div")),
                  card(
                    id = ns(paste0(plots[i],"_param")),
                    accordion(
                      id = ns(paste0(plots[i],"_accordion")),
                      open = FALSE,
                      accordion_panel(title = plots[i],
                                      textInput(inputId = ns(paste0(plots[i],"_textinput")),
                                                label = "",
                                                placeholder = "t,r,b,l"),
                                      br(),
                                      actionButton(inputId = ns(paste0(plots[i],"_remove")),
                                                   label = "Remove")
                      )
                    )
                  )
      )
      ))
    )
  }
  return(tagList(ui_ls))
}
