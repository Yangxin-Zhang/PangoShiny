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
      uiOutput(ns("param_file_input"))
    ),
    
   downloadButton(outputId = ns("Download_Plots"),
                  label = "Download") 
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
    
    div(
      navset_card_tab(
      id = ns("viewer_panel"),
      nav_panel(
        title = "test",
        card(
          id = ns("viewer_card"),
          plotOutput(
            outputId = ns("Combined_Plot"),
            height = "auto"
          )
        )
      )
    )),
    
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
      uiOutput(outputId = ns("Chosed_Subplots_Info_Card")),
      br(),
      actionButton(inputId = ns("refresh_main_comb_plot"),
                   label = "Refresh")
    )
    
  #   tags$script(
  #     HTML(
  #       sprintf(
  #         "
  #   const viewer = document.getElementById('%s');
  #   new ResizeObserver(entries => {
  #       const w = viewer.offsetWidth;
  #       viewer.style.height = w + 'px';
  #   }).observe(viewer)
  # ",
  #         ns("viewer_card")
  #       )))
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
                                        textInput(inputId = ns(paste0(plots[i],"_combine_id")),
                                                  label = "Plot ID",
                                                  placeholder = "enter the unique ID",
                                                  updateOn = "blur"),
                                        textInput(inputId = ns(paste0(plots[i],"_group_id")),
                                                  label = "Group ID",
                                                  placeholder = "enter the group ID",
                                                  updateOn = "blur"),
                                        textInput(inputId = ns(paste0(plots[i],"_plot_location")),
                                                  label = "Plot Location",
                                                  placeholder = "t,r,b,l",
                                                  updateOn = "blur"),
                                        br(),
                                        layout_columns(
                                          actionButton(inputId = ns(paste0(plots[i],"_remove")),
                                                       label = "Remove"),
                                          actionButton(inputId = ns(paste0(plots[i],"_update")),
                                                       label = "Update"),
                                          col_widths = c(6, 6)
                                        )
                        )
                      )
                    )
        )
        ))
    )
  }
  return(tagList(ui_ls))
}

#' update_plot_params 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

update_plot_params <- function(input,session,subplot,param_table){
  ns <- session$ns
  
  loc_plot <- param_table$comb_na == subplot
  
  if (!is.na(input[[(paste0(subplot,"_combine_id"))]])) {
    
    param_table[loc_plot,"plot_na"] <- input[[(paste0(subplot,"_combine_id"))]]
    
  }else{
    param_table[loc_plot,"plot_na"] <- NA
  }
  
  if (!is.na(input[[(paste0(subplot,"_group_id"))]])) {
    
    param_table[loc_plot,"plot_group"] <- input[[(paste0(subplot,"_group_id"))]]
    
  }else{
    param_table[loc_plot,"plot_group"] <- NA
  }
  
  plot_location <- strsplit(input[[(paste0(subplot,"_plot_location"))]],split = ",")
  plot_location <- plot_location[[1]]
  
  if (length(plot_location) == 4) {
    
    param_table[loc_plot,"loc_top"] <- plot_location[1] %>%
      as.integer()
    param_table[loc_plot,"loc_left"] <- plot_location[2] %>%
      as.integer()  
    param_table[loc_plot,"loc_bottom"] <- plot_location[3] %>%
      as.integer()
    param_table[loc_plot,"loc_right"] <- plot_location[4] %>%
      as.integer()
    
  }
  
  # cat("update_plot_params: ",param_table[loc_plot,"plot_na"],"\n")
  
  return(param_table)
  
}

#' load_plots_to_RAM 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import figpatch
#' @noRd

load_plots_to_RAM <- function(info_mat){
  
  plt_ls <- vector("list",length = nrow(info_mat))
  plt_na <- info_mat["comb_na"] %>%
    unlist() %>%
    as.character()
  names(plt_ls) <- plt_na
  
  # cat(class(plt_na),"\n")
  # cat(plt_na[1],"\n")
  # cat(paste0(info_mat[info_mat$comb_na %in% plt_na[1],"type"]),"\n")
  # cat(nrow(info_mat),"\n")
  # cat(paste0(colnames(info_mat)),"\n")
  
  for (i in 1:length(plt_na)) {

    if (info_mat[info_mat$comb_na %in% plt_na[i],"type"] == "image/png") {

      plt_ls[plt_na[i]] <- magick::image_read(info_mat[info_mat$comb_na %in% plt_na[i],"datapath"]) %>%
        grid::rasterGrob() %>%
        list()

    }
    
    if (tools::file_ext(info_mat[info_mat$comb_na %in% plt_na[i],"datapath"]) == "rds") {
      
      plt_ls[plt_na[i]] <- readRDS(info_mat[info_mat$comb_na %in% plt_na[i],"datapath"]) %>%
        list()
      
    }

  }
  
  # cat(class(plt_ls[[1]]),"\n")
  
  return(plt_ls)
  
}

#' generate_area_list 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

generate_area_list <- function(param_info_mat){
  
  # cat("generate area list \n")
  area_ls <- vector("list",length = nrow(param_info_mat))
  area_na <- param_info_mat["comb_na"] %>%
    unlist() %>%
    as.character()
  names(area_ls) <- area_na
  
  for (i in 1:length(area_na)) {
    area_vec <- param_info_mat[param_info_mat$comb_na %in% area_na[i],c("loc_top","loc_left","loc_bottom","loc_right")] %>%
      unlist()
    # cat(class(area_vec),"\n")
    # cat(length(area_vec),"\n")
    # cat(area_vec[1],"\n")
    # cat(paste0(is.na(area_vec)),"\n")
    if (sum((!is.na(area_vec))) == 4) {
      area_ls[area_na[i]] <- area(area_vec[1],area_vec[2],area_vec[3],area_vec[4]) %>%
        list()
    }else{
      area_ls[area_na[i]] <- list(NULL)
      # cat("Is null area: ",is.null(area_ls[[area_na[i]]]),"\n")
    }
  }
  
  return(area_ls)
  
}

#' create_Background_Plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

create_Background_Plot <- function(){
  
  bk_plt <- ggplot(data = data.frame(x = range(0,297),
                                     y = range(0,297)),
                   mapping = aes(x = x,
                                 y = y)) +
    scale_x_continuous(breaks = seq(0,297,by = 29.7),
                       expand = expansion(),
                       limits = c(0,297))+
    scale_y_continuous(breaks = seq(0,297,by = 29.7),
                       expand = expansion(),
                       limits = c(0,297)) +
    theme(panel.background = element_rect(fill = "grey90",
                                          colour = "white",
                                          linewidth = 1),
          plot.background = element_rect(colour = "black",
                                         linewidth = 1),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          aspect.ratio = 1,
          panel.grid.minor = element_line(colour = "white",
                                          linewidth = 0.5),
          panel.grid.major = element_line(colour = "white",
                                          linewidth = 0.5),
          plot.margin = margin(0,0,0,0),
          margins = margin(0,0,0,0))
  
  tmp_file <- tempfile(fileext = "_background_plot.png")
  
  ggsave(filename = tmp_file,
         plot = bk_plt,
         device = "png",
         dpi = 300,
         width = 297,
         height = 297,
         units = "mm")
  
  return(tmp_file)
  
}