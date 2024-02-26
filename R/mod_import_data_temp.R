#' import_data_temp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_data_temp_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="import_data_temp",
                          shinydashboardPlus::box(title = "Param\u00e8tres d'import :",
                                                  collapsible = TRUE,
                                                  width = 3,
                                                  fileInput(ns('file1'), 'Choisir le fichier CSV',
                                                            accept=c('text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv'))%>%
                                                    spsComps::bsPopover(			
                                                      "Cliquez ici pour importer un fichier", 
                                                      content= "le fichier doit contenir, soit une colonne
                                                      date heure au format '25/10/2021 23:22:00' ou deux colonnes date
                                                      et heure s\u00e9par\u00e9es;
                 une colonne avec les valeurs des temp\u00e9ratures.",
                                                      placement = "right", 
                                                      bgcolor = "#00a65a",
                                                      titlecolor = "white", 
                                                      contentcolor = "black"),
                                                  tags$hr(),
                                                  radioButtons(ns('ncoldate'),'format date heure',
                                                               c('1 colonne date_heure'='1col',
                                                                 '1 colonne date et 1 colonne heure'='2 col')
                                                  ),
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
                                                  selectizeInput(ns("stationInput"),"Stations",choices = NULL, selected = NULL),
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
                          shinydashboardPlus::box(title = "Graphiques des donn\u00e9es \u00e0 importer",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  plotly::plotlyOutput(outputId = ns("plot_import")))
                          
  )
}

#' import_data_temp Server Functions
#'
#' @noRd 
mod_import_data_temp_server <- function(id,DD,mytab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    getData <- reactive({
      
      inFile <- input$file1
      
      if (is.null(input$file1))
        return(NULL)
      
      if(input$ncoldate=='1col'){
        
        df<-read.csv2(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec)
        validate(need(ncol(df)==2,"le s\u00e9parateur de colonnes ne doit pas \u00eatre le bon"))
        
        df[,3]<-input$stationInput
        df<-dplyr::relocate(df,3,1,2)
        df[,2]<-as.POSIXct(df[,2],format="%d/%m/%Y %H:%M",tz="UTC")
        colnames(df)<-c("t_station","date_heure","temperature")
        return(df)}
      
      #if(input$ncoldate=='2col')
      else{
        df<-read.csv2(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec)
        validate(need(ncol(df)==3,"le s\u00e9parateur de colonnes ne doit pas \u00eatre le bon"))
        df[,2]<-as.POSIXct(df[,2],format="%d/%m/%Y",tz="UTC")
        df[,1]<-as.POSIXct(stringr::str_c(df[,2],df[,1],sep = " "),format="%Y-%m-%d %H:%M",tz="UTC")
        
        df[,2]<-input$stationInput
        
        
        df<-dplyr::relocate(df,2,1,3)
        colnames(df)<-c("t_station","date_heure","temperature")
        
        return(df)
      }
    })
    
    
    output$contents <- DT::renderDT(
      DT::datatable(
        getData()      ,
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
                 filename = "table import"))
        )
        
      ))
    
    
    
    
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
        updateSelectizeInput(session,"stationInput",choices=stations$id_station,selected = stations$id_station[1])
      },
      blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
    
    
    observeEvent({input$file1
      DD$login_button()
      input$ncoldate
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
        
        
        
        validate(need(as.numeric(data$temperature),"la d\u00e9cimale dans la colonne t\u00e9mperature doit \u00eatre un point"))
        validate(need(as.POSIXct(data$date_heure),"le format de la date doit \u00eatre de type jj/mm/aaaa hh:mm:ss"))
        output$plot_import <-plotly::renderPlotly({
          
          p<-ggplot2::ggplot(data)+ggplot2::geom_line(ggplot2::aes(x=date_heure,y=temperature))
          plotly::ggplotly(p)
        })
      },blocking_level = "error"
      )},  #fin ShinyCatch
    ignoreInit=T,
    ignoreNULL = F 
    ) # fin observeEvent
    
    
    observeEvent(eventExpr={
      DD$login_button()
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        table_desti<-dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"temperature")) %>%
          as.data.frame()
        output$responses <- DT::renderDT(
          DT::datatable(
            table_desti      ,
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
                     filename = "table temperature"))
            )
            
          ))
      }) # fin shiny catch
    }) # fin observe Event
    
    
    ##Update data in Rpostgresql table
    observeEvent(input$write_table_sql,{
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        shinybusy::show_modal_spinner()
        table_id<-DBI::Id(
          schema=user_schema,
          table="temperature"
        )
        DBI::dbWriteTable(con = db_connection,name = table_id,value = getData(),append = TRUE)
        
        shinybusy::remove_modal_spinner()
        ###shinyModal to show to user when the table is written to the database is successful
        shiny::showModal( shiny::modalDialog( title=paste0("RPostgreSQL table Updated"),
                                              br(),
                                              div(tags$b(paste0("Les donn\u00e9es ont bien \u00e9t\u00e9
                                                                \u00e9crites dans la base de donn\u00e9es"), style = "color: green;"))
        ))
      })
    })
    
    
  })
}

## To be copied in the UI
# mod_import_data_temp_ui("import_data_temp_ui_1")

## To be copied in the server
# mod_import_data_temp_server("import_data_temp_ui_1")
