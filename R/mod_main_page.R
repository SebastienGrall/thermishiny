#' main_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_ui <- function(id){
  ns <- NS(id)
  shinydashboard::dashboardBody(
   
   # hide icon
    tags$script(
      HTML(
        'var e = document.querySelector("body > div.wrapper > header > nav > div:nth-child(4) > ul > li > a > i");
           e.setAttribute("style", "display: none;");'
      )
    ),
    
    
    
    fluidRow(
    shinydashboard::tabItems(
      mod_import_stations_ui("import_stations_1"),
      mod_import_data_temp_ui("import_data_temp_1"),
      mod_import_data_releve_ui("import_data_releve_1"),
      mod_analysis_from_file_ui("analysis_from_file_1"),
      mod_analysis_from_bdd_ui("analysis_from_bdd_1"),
      mod_export_data_from_bdd_ui("export_data_from_bdd_1")
    
    )),
    sidebar = shinydashboardPlus::dashboardSidebar()
  )
}
    
#' main_page Server Functions
#'
#' @noRd 
mod_main_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_main_page_ui("main_page_ui_1")
    
## To be copied in the server
# mod_main_page_server("main_page_ui_1")
