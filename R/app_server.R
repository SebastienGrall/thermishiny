#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  DD <- reactiveValues()
  mod_main_page_server("main_page_ui_1")
  DD$login_button <- mod_header_server("header_ui_1") # login_button reactive variable récupérée depuis le module
  DD$tabs <- mod_left_side_bar_server("left_side_bar_ui_1",DD)
  
  # mod import stations
  
  mod_import_stations_server("import_stations_1",DD,mytab="import_stations")
  
  # mod import data temp
  
  mod_import_data_temp_server("import_data_temp_1",DD,mytab="import_data_temp")
  
  # mod import data releve
  
  mod_import_data_releve_server("import_data_releve_1",DD,mytab="import_data_releve")
  
  # mod analysis from file
  
  mod_analysis_from_file_server("analysis_from_file_1",DD)
  
  # mod analysis from bdd
  
  mod_analysis_from_bdd_server("analysis_from_bdd_1",DD,mytab="analysis_from_database")
  
  # mod export data from database
  
  mod_export_data_from_bdd_server("export_data_from_bdd_1",DD,mytab="export_data")
   
}
