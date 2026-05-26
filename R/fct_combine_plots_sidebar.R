#' combine_plots_sidebar 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib sidebar br accordion_panel accordion
#' @importFrom shiny fileInput selectInput
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
      full_screen = TRUE,
      accordion(
        id = ns("file_infos_table"),
        open = FALSE,
        accordion_panel(
          value = "subplots_information_table",
          title = "Subplot Info",
          tableOutput(outputId = ns("Subplots_Info_Table")))
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
        full_screen = TRUE,
        textInput(inputId = ns("page_size"),
                  label = "Page Size",
                  updateOn = "blur",
                  value = "210,210",
                  placeholder = "210X210(mm)"),
        
        textOutput(ns("Test_Text"))
      ),
      card(
        full_screen = TRUE,
        tableOutput(ns("Chosed_Subplots_Info"))
      )),
    
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
