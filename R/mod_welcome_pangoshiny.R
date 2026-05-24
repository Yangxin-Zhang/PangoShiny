#' welcome_pangoshiny UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib page_fluid card card_header
mod_welcome_pangoshiny_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    page_fluid(
      card(card_header("Welcome to PangoShiny"))
    )
 
  )
}
    
#' welcome_pangoshiny Server Functions
#'
#' @noRd 
mod_welcome_pangoshiny_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_welcome_pangoshiny_ui("welcome_pangoshiny_1")
    
## To be copied in the server
# mod_welcome_pangoshiny_server("welcome_pangoshiny_1")
