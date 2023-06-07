#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList textInput passwordInput
#' @importFrom shinydashboardPlus dashboardHeader notificationItem dropdownBlock userOutput
#' @importFrom shinydashboard dropdownMenu
#' @importFrom shinyWidgets actionBttn
#' @importFrom spsComps spsDepend
mod_header_ui <- function(id){
  ns <- NS(id)
  
  shinydashboardPlus::dashboardHeader(title=  tags$a(href="https://github.com/SebastienGrall/thermishiny", target="_blank", 
                                 tags$img(height = "40px", alt="THERMISHINY",  src="www/favicon.ico", width = "40px") ),
                  leftUi = tagList(
                    
                    dropdownBlock(
                      id = ns("base"),
                      title = "Options de connexion",
                      icon = icon("database"),
                      badgeStatus = NULL,
                      
                      # to have popup message on error
                      spsDepend("toastr"),
                      
                      textInput(
                        inputId = ns("host_login"), 
                        label= "host", 
                        value = "localhost", placeholder = "localhost"
                      ),
                      textInput(
                        inputId = ns("port_login"), 
                        label= "port", 
                        value = "5432", placeholder = "5432"
                      ),
                      textInput(
                        inputId = ns("dbname_login"), 
                        label= "base de  donn\u00e9es", 
                        value = "bd_thermie", 
                        placeholder = "db_name"
                      ),
                      textInput(
                        inputId = ns("name_login"), 
                        label= "utilisateur", 
                        value = "user_1",
                        placeholder = "Nom utilisateur utilise pour l'acc\u00e8s a la base de donn\u00e9es."
                      ),
                      passwordInput(
                        inputId = ns("pass_login"), 
                        label= "mot de passe", 
                        value = "", 
                        placeholder = "Mot de passe utilise pour l'acc\u00e8s a la base de donn\u00e9es."
                      ),
                      actionBttn(
                        inputId = ns("login_button"),
                        label = "OK",
                        style = "pill", 
                        color = "success"
                      )) %>%
                      spsComps::bsPopover(			
                        "1. Cliquez d'abord ici", 
                        content= "Info de connexion vers base puis OK",
                        placement = "right", 
                        bgcolor = "#00a65a",
                        titlecolor = "white", 
                        contentcolor = "black"),		  
                    userOutput(ns("statut_connection"))
                    
                  )
  )
  
}

#' header Server Functions, contains the code to connect to the database
#' upon trigger the code writes a "db_connection" in envir thermish
#'
#' @noRd 
#' @importFrom shiny observeEvent isolate 
#' @importFrom  spsComps shinyCatch

#' @importFrom shinydashboardPlus dashboardUser dashboardUserItem renderUser
#' @return The input input$login_button. 

mod_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Action lorsque le bouton connection est presse
    observeEvent(input$login_button, ignoreInit=TRUE, 
                 { 
                   shinyCatch({
                     
                     # utilisateur et mot de passe sont isoles
                     host <- isolate(input$host_login)
                     port <- isolate(input$port_login)
                     dbname <- isolate(input$dbname_login)
                     user <- isolate(input$name_login)
                     password <- isolate(input$pass_login)

                   db_connection<-  pool::dbPool(
                                    drv=RPostgres::Postgres(),
                                    dbname = dbname, 
                                    host = host, 
                                    port = port, 
                                    user = user,
                                    password = password
                     )
                   validate(need(!is.null(db_connection), "Pas de connexion, vérifiez les paramètres de connexion"))
                     #browser()
                     if (db_connection$valid == "TRUE") {
                       
                       
                        assign("db_connection",db_connection,envir=thermish)
                       assign("schema",user,envir=thermish)
                     
                      # pool::poolClose(db_connection)
                       
                       # on met à jour la boite du statut de la connection
                       output$statut_connection <- renderUser({
                         dashboardUser(
                           name = dbname, 
                           image = "https://e7.pngegg.com/pngimages/165/79/png-clipart-database-server
-computer-icons-database-connection-backup-database-miscellaneous-angle-thumbnail.png", 
                           #title = ,
                           subtitle = paste('Utilisateur :',user, sep=' '), 
                           footer = p(paste('Hote :',port, sep = ' '), class = "text-center")
                           
                         )
                       }) # fin output$statut_connection
                       
                     }# fin if
                     
                     
                   }, blocking_level = "error")
                 }) # fin observeEvent connection
    return(reactive(input$login_button))
    
    
    
    input$schema_user<-observeEvent(input$login_button, ignoreInit=TRUE, 
                                  { 
                                    shinyCatch({
                                      user <- isolate(input$name_login)
                                    }, blocking_level = "error")
                                  })
    
    return(reactive(input$schema_user))
    
  })
}

## To be copied in the UI
# mod_header_ui("header_ui_1")

## To be copied in the server
# mod_header_server("header_ui_1")
