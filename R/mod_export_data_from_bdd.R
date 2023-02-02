#' export_data_from_bdd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_export_data_from_bdd_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="export_data",
                          shinydashboardPlus::box(title="Choix des donn\u00e9es \u00e0 exporter", solidHeader = T,
                                                     collapsible = T,width=5,
                                                  
                               selectizeInput(ns('choix_export'),"Type:",choices=c("donn\u00e9es temp\u00e9rature","donn\u00e9es d'analyse"),
                                              selected = NULL, multiple = F,
                                                                 options = NULL),                    
                              
                              selectizeInput(ns('station_export'),"Station:",choices="",selected = NULL, multiple = TRUE,
                                             options = NULL),
                              
                              sliderInput(ns("date_range"),
                                          "Choisir la p\u00e9riode:",
                                          min = as.POSIXct("2020-01-01",tz="UTC"),
                                          max = as.POSIXct(stringr::str_c(CY,"-12-31"),tz="UTC"),
                                          value = c(as.POSIXct("2020-01-01",tz="UTC"),as.POSIXct(stringr::str_c(CY,"-12-31"),tz="UTC")),
                                          timeFormat = "%Y-%m-%d", ticks = F)
                                                            ),
                          
                          shinydashboardPlus::box(title="Tableau d'export", solidHeader = T,
                                                     collapsible = T,width=10,
                              DT::DTOutput(ns("export_data_resultats"))
                              
                          )
  )
}
    
#' export_data_from_bdd Server Functions
#'
#' @noRd 
mod_export_data_from_bdd_server <- function(id,DD,mytab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(eventExpr={
      DD$login_button()
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        
        stations = dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"stations")) %>%
          dplyr::select(id_station,libelle_station) %>%
          as.data.frame()
        updateSelectizeInput(session,"station_export",choices=stations$id_station,selected = stations$id_station[1])
      },
      blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
    
    observeEvent(eventExpr={
      DD$login_button()
      input$choix_export
      input$station_export
      input$date_range
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        if(input$choix_export=="donn\u00e9es temp\u00e9rature"){
          data<-dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"temperature")) %>%
            as.data.frame()
             data<-subset(data, t_station %in% input$station_export)
            data<- data %>% dplyr::filter(date_heure >= input$date_range[1]) %>% 
             dplyr::filter(date_heure <= input$date_range[2]) %>%
             dplyr::mutate(date=as.Date(date_heure)) %>%
             dplyr::mutate(heure=format(as.POSIXct(date_heure), format = "%H:%M:%S")) %>%
             dplyr::relocate(1,2,4,5,3) %>%
             as.data.frame()
          
          output$export_data_resultats <- DT::renderDT(
            DT::datatable(
              data,
              rownames=FALSE,
              extensions = "Buttons",
              option=list(
                scroller = TRUE,
                scrollX = TRUE,
                lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
                "pagelength"=-1,
                dom= "Blfrtip",
                scrollX = T,
                buttons=list(
                  list(extend="excel",
                       filename = "table export"))
              )
            ))
          
          
        }else{
          
          data<-dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"resultats_analyse")) %>%
            as.data.frame()
          data<-subset(data, ra_station %in% input$station_export)
          data<- data %>% dplyr::filter(dd_periode >= input$date_range[1]) %>% 
            dplyr::filter(df_periode <= input$date_range[2]) %>%
            as.data.frame()
          
          output$export_data_resultats <- DT::renderDT(
            DT::datatable(
              data,
              rownames=FALSE,
              extensions = "Buttons",
              option=list(
                scroller = TRUE,
                scrollX = TRUE,
                lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
                "pagelength"=-1,
                dom= "Blfrtip",
                scrollX = T,
                buttons=list(
                  list(extend="excel",
                       filename = "table export"))
              )
            ))
          
          
        }
          
    
      },
      blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
  })
}
    
## To be copied in the UI
# mod_export_data_from_bdd_ui("export_data_from_bdd_1")
    
## To be copied in the server
# mod_export_data_from_bdd_server("export_data_from_bdd_1")
