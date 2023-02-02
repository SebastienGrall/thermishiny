#' import_data_releve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_data_releve_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="import_data_releve",
                          shinydashboardPlus::box(title = "Param\u00e8tres d'import :",
                                                  collapsible = TRUE,
                                                  width = 3,
                                                  fileInput(ns('file2'), 'Choisir le fichier CSV',
                                                            accept=c('text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv'))%>%
                                                    spsComps::bsPopover(			
                                                      "Cliquez ici pour importer un fichier", 
                                                      content= "le fichier doit contenir une colonne avec le
                                                      code station, une colonne date au format '25/10/2021' et une colonne avec
                                                      l'identit\u00e9 de la personne ayant relev\u00e9 les sondes.",
                                                      placement = "right", 
                                                      bgcolor = "#00a65a",
                                                      titlecolor = "white", 
                                                      contentcolor = "black"),
                                                  tags$hr(),
                                                  checkboxInput(ns('header2'), 'En-t\u00eate', TRUE),
                                                  radioButtons(ns('sep2'), 'S\u00e9parateur de colonnes',
                                                               c(Comma=',',
                                                                 Semicolon=';',
                                                                 Tab='\t'),
                                                               ';'),
                                                  
                                                  actionButton(inputId =ns("write_table_sql2"),label = "Importer la table dans la BDD")
                          ),
                          shinydashboardPlus::box(title = "Table d'import",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  DT::DTOutput(ns('contents2'))
                                                  
                                                  
                          ),
                          shinydashboardPlus::box(title = "Table de destination",
                                                  collapsible = TRUE,
                                                  width = 9,
                                                  DT::DTOutput(ns('responses2'))
                                                  
                          )
 
  )
}
    
#' import_data_releve Server Functions
#'
#' @noRd 
mod_import_data_releve_server <- function(id,DD,mytab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    getData2 <- reactive({
      
      inFile2 <- input$file2
      
      if (is.null(input$file2))
        return(NULL)
      
      df2<-read.csv2(inFile2$datapath, header=input$header2, sep=input$sep2,)
      validate(need(ncol(df2)==3,"le s\u00e9parateur de colonnes ne doit pas \u00eatre le bon"))
      
      df2[,2]<-as.Date(df2[,2],format="%d/%m/%Y")
      colnames(df2)<-c("r_station","r_date","r_operateur")
      return(df2)
      
    })
    
    
    output$contents2 <- DT::renderDT(
      DT::datatable(
        getData2()      ,
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
                 filename = "table releve"))
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
        table_releve<-dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"releve_sonde")) %>%
          as.data.frame()
        output$responses2 <- DT::renderDT(
          DT::datatable(
            table_releve      ,
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
                     filename = "table releve"))
            )
            
          ))
      }) # fin shiny catch
    })
    
    
    ##Update data in Rpostgresql table
    observeEvent(input$write_table_sql2,{
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        shinybusy::show_modal_spinner()
        table_id2<-DBI::Id(
          schema=user_schema,
          table="releve_sonde"
        )
        DBI::dbWriteTable(con = db_connection,name = table_id2,value = getData2(),append = TRUE)
        
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
# mod_import_data_releve_ui("import_data_releve_1")
    
## To be copied in the server
# mod_import_data_releve_server("import_data_releve_1")
