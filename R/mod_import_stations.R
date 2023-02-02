#' import_stations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_stations_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="import_stations",
                          shinydashboardPlus::box(title = "Param\u00e8tres d'import :",
                                                  collapsible = TRUE,
                                                  width = 3,
                                                  fileInput(ns('file1'), 'Choisir le fichier CSV',
                                                            accept=c('text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv'))%>%
                                                    spsComps::bsPopover(			
                                                      "Cliquez ici pour importer un fichier", 
                                                      content= "l'id de station doit \u00eatre unique et les coordonn\u00e9es en Lambert93
                                                      (EPSG 2154). Se r\u00e9f\u00e9rer au script de cr\u00e9ation de la bdd
                                                      pour le d\u00e9tail des champs.",
                                                      placement = "right", 
                                                      bgcolor = "#00a65a",
                                                      titlecolor = "white", 
                                                      contentcolor = "black"),
                                                  tags$hr(),
                                                  checkboxInput(ns('header'), 'En-t\u00eate', TRUE),
                                                  radioButtons(ns('sep'), 'S\u00e9parateur de colonnes',
                                                               c(Comma=',',
                                                                 Semicolon=';',
                                                                 Tab='\t'),
                                                               ';'),
                                                  radioButtons(ns('dec'), 'D\u00e9cimal',
                                                               c(Point='.',
                                                                 Virgule=','),
                                                  ),
                                                 actionButton(inputId = ns("write_table_sql"),label = "Importer la table dans la BDD")
                                                  
                          ),
                          shinydashboardPlus::box(title = "Table d'import",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  DT::DTOutput(ns('contents'))
                                                  
                                                  
                          ),
                          shinydashboardPlus::box(title = "Table de destination",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  DT::DTOutput(ns('responses'))
                                                  
                          ),
                          shinydashboardPlus::box(title = "Cartographie",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  leaflet::leafletOutput(ns('carto'))
                          )
                          
  )
}
    
#' import_stations Server Functions
#'
#' @noRd 
mod_import_stations_server <- function(id,DD,mytab){
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
          
          getData <- reactive({
            
            inFile <- input$file1
            
            if (is.null(input$file1))
              return(NULL)
            
            df<-read.csv2(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec)
            validate(need(ncol(df)==19,"le s\u00e9parateur de colonnes ne doit pas etre le bon"))
            df[,16]<-as.Date(df[,16],format="%d/%m/%Y")
            validate(need(as.numeric(df[,7]),"la d\u00e9cimale dans la colonne X93 doit \u00eatre un point"))
            validate(need(as.numeric(df[,8]),"la d\u00e9cimale dans la colonne Y93 doit \u00eatre un point")) 
            colnames(df)<-c("id_station","libelle_station","bassin","cours_eau","commune","departemen","x_l93","y_l93",
                            "berge", "localisati","securite","contrainte","interet","reference","proprio","mise_en_fct",
                            "mise_arret","remarques","operateur")
            
            return(df)
          })
          
          observeEvent({input$file1
            input$header
            input$sep
            input$dec
          },{
            shinyCatch({
              if (is.null(input$file1))
                return(NULL)
              data<-getData()
              if(is.null(data))
                return(NULL)
           
                
              
          
          output$contents <- DT::renderDT(
            DT::datatable(
              data      ,
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
                       filename = "table import station"))
              )
              
            ))
          
            },  # fin shiny catch
          blocking_level = "error")},
          ignoreInit=T,
          ignoreNULL = F
          ) #fin observevent
          
          

          
          
          observeEvent(input$write_table_sql,{
            shinyCatch({
              station_import<-getData()
              db_connection<-thermish$db_connection
              user_schema<-thermish$schema
              validate(need(as.Date(station_import[,16]),"le format date de mise en fonctionnement n'est pas le bon"))
              validate(need(as.numeric(station_import[,7]),"la d\u00e9cimale dans la colonne X93 doit \u00eatre un point"))
              validate(need(as.numeric(station_import[,8]),"la d\u00e9cimale dans la colonne Y93 doit \u00eatre un point"))
              shinybusy::show_modal_spinner()
              
              station_import<-station_import %>%
                     dplyr::rowwise() %>%
                dplyr::mutate(geom = list(sf::st_point(c(x_l93,y_l93)))) %>%
                as.data.frame()
              
              station_import$geom<-sf::st_sfc(station_import$geom,crs=2154)
              
              sf::st_write(obj = station_import, dsn = db_connection, DBI::Id(schema=user_schema, table = "stations"),append=TRUE)
              shinybusy::remove_modal_spinner()
              ###shinyModal to show to user when the table is written to the database is successful
              shiny::showModal( shiny::modalDialog( title=paste0("RPostgreSQL table Updated"),
                                                    br(),
                                                    div(tags$b(paste0("Les donn\u00e9es ont bien \u00e9t\u00e9
                                                                      \u00e9crites dans la base de donn\u00e9es"),
                                                               style = "color: green;"))
              ))
             
            },  # fin shiny catch
            blocking_level = "error")},
            ignoreInit=T,
            ignoreNULL = F
          )
          
          thermoIcon <- leaflet::makeIcon(
            iconUrl = "www/logo_thermo.png",
            iconWidth = 15,
            
          )
          
          observeEvent({input$write_table_sql
                       DD$login_button()
                      } ,{
            shinyCatch({
              
              station_desti<-dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"stations")) %>%
                dplyr::select(-geom) %>%
                as.data.frame()
              
              
              output$responses <- DT::renderDT(
                DT::datatable(
                  station_desti      ,
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
                           filename = "table stations"))
                  )
                  
                ))
              
              
          
            stations_carto<-sf::st_read(db_connection, DBI::Id(schema=user_schema, table = "stations"))
            stations_carto<-sf::st_transform(stations_carto, 4326)
            output$carto <- leaflet::renderLeaflet(
              leaflet::leaflet(stations_carto) %>%
              leaflet::addTiles() %>%
              leaflet::addMarkers(label=~id_station,icon = thermoIcon,popup = paste("Station : ", stations_carto$id_station, "<br>",
                                                                  "Localisation : ", stations_carto$cours_eau))
          )
            },  # fin shiny catch
          blocking_level = "error")},
          ignoreInit=F,
          ignoreNULL = F
          )
          
        },  # fin shiny catch
        blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
  })
}
    
## To be copied in the UI
# mod_import_stations_ui("import_stations_1")
    
## To be copied in the server
# mod_import_stations_server("import_stations_1")
