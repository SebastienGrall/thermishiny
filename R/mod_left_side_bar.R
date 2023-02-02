#' left_side_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_left_side_bar_ui <- function(id){
  ns <- NS(id)
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = ns("tabs"),
                  shinydashboard::menuItem(tabName = "import_stations","Import des stations"
                                ),            
                  shinydashboard::menuItem(tabName = "import_data_temp","Import donn\u00e9es temp\u00e9rature"
                  ),
                  shinydashboard::menuItem(tabName = "import_data_releve","Import donn\u00e9es de rel\u00e8ve"
                  ),
                 shinydashboard::menuItem(tabName = "analysis_from_file","Analyse depuis un fichier import\u00e9"
                 ),
                shinydashboard::menuItem(tabName = "analysis_from_database","Analyse depuis la BDD"
                ),
                shinydashboard::menuItem(tabName = "export_data","Export des donn\u00e9es"
                )
                
          
    )       
 
  )
}
    
#' left_side_bar Server Functions
#'
#' @noRd 
mod_left_side_bar_server <- function(id,DD){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$tabs,{
     # cat(input$tabs)
    })
    return(reactive(input$tabs))
 
  })
}
    
## To be copied in the UI
# mod_left_side_bar_ui("left_side_bar_ui_1")
    
## To be copied in the server
# mod_left_side_bar_server("left_side_bar_ui_1")
