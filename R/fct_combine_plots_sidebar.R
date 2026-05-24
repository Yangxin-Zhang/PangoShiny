#' combine_plots_sidebar 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib sidebar
#' @noRd
combine_plots_sidebar <- function(id) {
  ns <- NS(id)
  
  sidebar(fillable = TRUE)
  
}

#' combine_plots_layout_columns 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom bslib layout_columns card
#' @importFrom shiny textInput
#' @noRd

combine_plots_layout_columns <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    fill = TRUE,
    col_widths = c(9,3),
    
    card(id = ns("viewer_card"),
         plotOutput(outputId = ns("Combined_Plot"),
                    height = "auto")),
    
    card(
      full_screen = TRUE,
      textInput(inputId = ns("page_size"),
                label = "Page Size",
                updateOn = "blur",
                value = "210,210",
                placeholder = "210X210(mm)")),
    
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
