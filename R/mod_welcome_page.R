#' welcome_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib page_navbar bs_theme nav_panel nav_menu nav_spacer nav_panel_hidden
#' @importFrom bsicons bs_icon 
mod_welcome_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    page_navbar(
      
      id = ns("welcome_nav"),
      fluid = TRUE,
      title = tags$div(
        class = "d-flex align-items-center",
        style = "cursor: pointer",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
          ns("home_click")
          ),
        span("PangoShiny", style = "font-weight: bold;font-size: 2rem")
      ),
      
      theme = bs_theme(
        version = 5,
        primary = "#18bc9c",
        bg = "#f5f5f5",
        fg = "#212529", 
      ),
      
      bg = "#ffffff",
      
      nav_panel_hidden(
        value = "home",
        mod_welcome_pangoshiny_ui(id = "welcome_pangoshiny")
      ),
      
      nav_spacer(),
      
      nav_menu(
        title = "Tools",
        icon = bs_icon("gear"),
        align = "left",
        nav_panel(title = "Combine Plots",
                  value = "CobPlt",
                  mod_combine_plot_ui(id = "combine_plot")),
        nav_panel("子页面2", "内容2")
      )
      
    ))
}

#' welcome_page Server Functions
#'
#' @importFrom bslib nav_hide nav_select
#' @importFrom shiny observeEvent 
#' @noRd 
mod_welcome_page_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$home_click, {
      nav_select(
        id = "welcome_nav",
        selected = "home"
        )
    })
    
  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
