#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import DT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  mod_welcome_page_server(id = "welcome")

  mod_combine_plot_server(id = "combine_plot")
  
}
