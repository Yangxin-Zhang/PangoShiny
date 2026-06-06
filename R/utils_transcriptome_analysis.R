#' transcriptome_analysis_sidebar 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

transcriptome_analysis_sidebar <- function(id){
  ns <- NS(id)
  
  sidebar(
    fillable = TRUE,
    
    card(
      useShinyjs(),
      div(
        id = ns("h5ad_file_path_js"),
        textInput(inputId = ns("h5ad_file_path"),
                  label = "H5ad File Path",
                  updateOn = "blur",
                  value = "/home/youngxin/Desktop/协和小鼠/24H_1A/H5ad/filtered_feature_bc_matrix_version1.h5ad")
      ),
      hidden(
        div(
          id = ns("uploaded_h5ad_js"),
          card_title("H5ad has uploaded"),
          actionButton(inputId = ns("reload_h5ad"),
                       label = "Reload")
        )
      )
    ),
    
    card(
      card_title("Test Card"),
      card_body(
        textOutput(outputId = ns("test_text"))
      ),
      card(
        id = ns("test_table_card"),
        full_screen = TRUE,
        uiOutput(outputId = ns("text_table_card_ui"))
      )
    )
    
  )
  
}

#' transcriptome_analysis_main_panel
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

transcriptome_analysis_main_panel <- function(id){
  ns <- NS(id)
  
  layout_columns(
    fill = TRUE,
    col_widths = c(11,1),
    
    navset_card_tab(
      tags$head(
        tags$script(
          HTML(
            "
            function makeCardSquare(cardId) {
      const card = document.getElementById(cardId);
      if (!card) return;
      
      function setSquare() {
        const width = card.offsetWidth;
        if (width > 0 && card.offsetHeight !== width) {
          card.style.height = width + 'px';
        }
      }
      
      setSquare();
      const observer = new ResizeObserver(setSquare);
      observer.observe(card);
      window.addEventListener('resize', setSquare);
    }
            "
          )
        )
      ),
      nav_panel(
        "Panel 1",
        card(
          id = ns("viewer_card_1"),
          fill = TRUE,
          girafeOutput(outputId = ns("Point_Graph"),
                       height = "100%",
                       width = "100%"),
          tags$script(HTML(sprintf("makeCardSquare('%s');", ns("viewer_card_1"))))
        )
      ),
      nav_panel(
        "Panel 2",
        card(
          id = ns("viewer_card_2"),
          fill = TRUE
        )
      )
    )
  )
}