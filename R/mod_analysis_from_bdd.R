#' analysis_from_bdd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_from_bdd_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="analysis_from_database",
                          shinydashboardPlus::box(title = "Choix de la station et calcul",solidHeader = TRUE,
                              collapsible = TRUE, width = 4,
                              selectizeInput(ns("station_BDD"),
                                             label = "Station",
                                             choices = NULL,selected = NULL, multiple = FALSE,
                                             options = NULL),
                              dateInput(ns("date_debut_analyse"), label = h4("Date de d\u00e9but de la p\u00e9riode"),
                                        value = (as.Date(stringr::str_c(format(Sys.Date(), "%Y"),"01-01",sep = "-")))-365),
                              verbatimTextOutput(ns("value_date_debut")),
                              verbatimTextOutput(ns("value_date_fin")),
                              verbatimTextOutput(ns("temperature_station_min_date")),
                              verbatimTextOutput(ns("temperature_station_max_date")),
                              dateInput(ns("date_ponte_BDD"), label = h4("Date de ponte"),
                                        value = (as.Date(stringr::str_c(format(Sys.Date(), "%Y"),"01-01",sep = "-")))-365),
                              actionButton(ns('run_BDD'), 'Run')
                              
                          ),
                          shinydashboardPlus::box(title = "R\u00e9sultats de l'analyse", solidHeader = TRUE,
                              collapsible = TRUE, width = 8,
                              DT::DTOutput(ns("resultats_BDD")),
                              actionButton(inputId = ns("write_table_sql4"),label = "Importer la table dans la BDD"),
                              DT::DTOutput(ns('t_mean_j'))
                          ),
                          shinydashboardPlus::box(title = "Tableau", solidHeader = TRUE,
                                                  collapsible = TRUE, width = 8,
                                                  DT::DTOutput(ns('data_analyse_bdd'))
                                                                            ),
                          
                          shinydashboardPlus::box(title = "Graphique", solidHeader = TRUE,
                                                  collapsible = TRUE, width = 8,
                                                  plotly::plotlyOutput(outputId = ns("plot_bdd"))
                          )
                          
  )
}
    
#' analysis_from_bdd Server Functions
#'
#' @noRd 
mod_analysis_from_bdd_server <- function(id,DD,mytab){
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
        updateSelectizeInput(session,"station_BDD",choices=stations$id_station,selected = stations$id_station[1])
      },
      blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
    
    output$value_date_debut <- renderText({ stringr::str_c("date d\u00e9but s\u00e9lectionn\u00e9e :",input$date_debut_analyse,sep = " ")})
    
    output$value_date_fin<- renderText({ stringr::str_c("date de fin :",input$date_debut_analyse +365,sep = " ")}) 
    
    output$value_date_ponte <- renderPrint({ input$date_ponte_BDD })
    
    observeEvent(eventExpr={
      DD$login_button()
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
      shinyCatch({
        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
        output$temperature_station_min_date = renderText({
          
          date_min<-DBI::dbGetQuery(db_connection, stringr::str_c("SELECT MIN(date_heure) FROM ",user_schema,
                                                                  ".temperature WHERE t_station=","'",input$station_BDD,"'",sep=""))
          
          validate(need(!is.na(date_min), "Pas de donn\u00e9es disponible pour cette station"))
          date_min<-date_min[,1]
          
          
          date_min<-stringr::str_c("date min dispo ",date_min,sep = " ")
          
          
          
          
        })
        
        output$temperature_station_max_date = renderText({
          
          date_max<-DBI::dbGetQuery(db_connection, stringr::str_c("SELECT MAX(date_heure) FROM ",user_schema,
                                                                  ".temperature WHERE t_station=","'",input$station_BDD,"'",sep=""))
          
           if(is.na(date_max))
             return()
          
          date_max<-date_max[,1]
          
          date_max<-stringr::str_c("date max dispo ",date_max,sep = " ")
          
          
        })
        
        
      },
      blocking_level = "error")},
      ignoreInit=T,
      ignoreNULL = F
    ) #fin observevent
  
    v_BDD <- reactiveValues(data = NULL)
    
    observeEvent(input$run_BDD,{
      v_BDD$data<-1})
    
    observeEvent(input$reset_BDD,{
      v_BDD$data<-NULL})
    

        data_BDD_reactive<-reactive({
          db_connection<-thermish$db_connection
          user_schema<-thermish$schema
          
          if(is.null(db_connection))
            return(NULL)
          
          date_debut_analyse<-input$date_debut_analyse
          date_fin_analyse<-date_debut_analyse+365
          
          data = dplyr::tbl(db_connection, dbplyr::in_schema(user_schema,"temperature")) %>%
            dplyr::select(t_station,date_heure,temperature) %>%
            dplyr::filter(t_station==!!input$station_BDD) %>% 
            dplyr::filter(date_heure >= date_debut_analyse) %>% 
            dplyr::filter(date_heure < date_fin_analyse) %>%
            as.data.frame()
          
          
          data$date<-as.Date(data$date_heure)
          
          data<-data %>% dplyr::arrange(date_heure)
          
          return(data)
          
        })
        

       
    observeEvent(eventExpr={
      DD$login_button()
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
                shinyCatch({
                  db_connection<-thermish$db_connection
                 user_schema<-thermish$schema
                  validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
                  
                 
        
        output$data_analyse_bdd <- DT::renderDT(
                  DT::datatable(
                    data_BDD_reactive(),
                    rownames=TRUE,
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
                             filename = "data_bdd"))
                    )
                  ))
        
        
        
           },
           blocking_level = "error")},
           ignoreInit=T,
           ignoreNULL = F
         ) #fin observevent
        
        
    observeEvent(eventExpr={
      input$station_BDD
      input$run_BDD
      input$date_debut_analyse
      DD$tabs()},
      handlerExpr={
        req(DD$tabs()==mytab)
    

          shinyCatch({
            db_connection<-thermish$db_connection
            user_schema<-thermish$schema
            validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))
            data<-data_BDD_reactive()
            rown<-nrow(data)
            
            if(rown<1) {
               
                 output$plot_bdd <-plotly::renderPlotly({
                   
                   p<-ggplot2::ggplot() +
                     ggplot2::theme_void() +
                     ggplot2::geom_text(ggplot2::aes(0,0,label='pas de donn\u00e9es disponible')) +
                     ggplot2::xlab(NULL)
                   plotly::ggplotly(p)
                 })
                 
                 
                 }
            
         else {
             output$plot_bdd <-plotly::renderPlotly({
               
               p<-ggplot2::ggplot(data)+ggplot2::geom_line(ggplot2::aes(x=date_heure,y=temperature))+
                 ggplot2::theme_bw()
               plotly::ggplotly(p)
             })
             }
            
            validate(need(!is.na(data),"pas de donn\u00e9es disponible"))
  
            
  },
  blocking_level = "error")},
ignoreInit=T,
ignoreNULL = F
) #fin observevent
    
    observeEvent({
      
      input$run_BDD
    },{
      shinyCatch({

        
        data<-data_BDD_reactive()

        if (is.null(data))
          return(NULL)

        db_connection<-thermish$db_connection
        user_schema<-thermish$schema
        validate(need(!is.null(db_connection), "Pas de connexion, cliquez sur le bouton dans la barre de titre"))


        validate(need(input$date_ponte_BDD>=min(data$date_heure),"la date de ponte est inf\u00e9rieure
                      au d\u00e9but de la p\u00e9riode \u00e0 analyser"))
        validate(need(input$date_ponte_BDD<=max(data$date_heure),"la date de ponte est sup\u00e9rieure
                      \u00e0 la fin de la p\u00e9riode \u00e0 analyser"))
        
        if(nrow(data)<8760){(warning("il manque des donn\u00e9es dans la base pour cette p\u00e9riode, les r\u00e9sultats de
                                     l'analyse sont \u00e0 prendre avec pr\u00e9caution"))}
        if(nrow(data)>8760){(warning("il y a plus d'une ann\u00e9e de donn\u00e9es dans le fichier,
                                     les r\u00e9sultats de l'analyse sont \u00e0 prendre avec pr\u00e9caution"))}

        
        T_mean_j<-dplyr::summarise(dplyr::group_by(data,date),mean=mean(temperature)) %>%
          as.data.frame()
        T_mean_j$mean<-round(T_mean_j$mean,digits = 1)
        
        output$t_mean_j <- DT::renderDT(
          DT::datatable(
            T_mean_j,
            rownames=TRUE,
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
                     filename = "temperature moyenne journaliere"))
            )
          ))
        
        
        rappel<-fnrappel(data)

        var_therm_gen<-fnvarthergen(data)

        var_therm_truite<-fnvarthertruite(data)

        cond_fav_MRP<-fncondfavMRP(data)

        cond_embry_larv<-fncondembryolarv(data,input$date_ponte_BDD)

        resultats<-cbind(rappel,var_therm_gen,var_therm_truite,cond_fav_MRP,cond_embry_larv)


        colnames(resultats)<-c("dd_periode","df_periode","duree","ti_min","ti_max","ati","ajmax_ti","d_ajmax_ti",
                               "tmj_min","tmj_max","atmj","d_tmj_max","tmp","tm30j_max","dd_tm30j_max","df_tm30j_max",
                               "nbj_tmj_4_19","percent_j_tmj_4_19","dd_tmj_4","df_tmj_4","percent_j_tmj_inf4","percent_j_tmj_sup19",
                               "nb_ti_sup19","nb_sq_ti_sup19","nbmax_ti_csf_sup19","nb_ti_sup25","nb_sq_ti_sup25","nbmax_ti_csf_sup25",
                               "nb_ti_sup15","nb_sq_ti_sup15","nbmax_ti_csf_sup15","d50_ponte","nbj_inc","d50_ecl","nbj_rsp",
                               "nbj_pel","d50_emg","nb_ti_sup15_pel","nb_sq_ti_sup15_pel","nbmax_ti_csf_sup15_pel",
                               "nb_ti_inf1_5_pel","nb_sq_ti_inf1_5_pel","nbmax_ti_csf_inf1_5_pel")

        resultats$ti_min<-round(resultats$ti_min,digits = 1)
        resultats$ti_max<-round(resultats$ti_max,digits = 1)
        resultats$ati<-round(resultats$ati,digits = 1)
        resultats$ajmax_ti<-round(resultats$ajmax_ti,digits = 1)
        resultats$tmj_min<-round(resultats$tmj_min,digits = 1)
        resultats$tmj_max<-round(resultats$tmj_max,digits = 1)
        resultats$atmj<-round(resultats$atmj,digits = 1)
        resultats$tmp<-round(resultats$tmp,digits = 1)
        resultats$tm30j_max<-round(resultats$tm30j_max,digits = 1)
        resultats$dd_periode<-as.Date(resultats$dd_periode,format="%Y-%m-%d",tz="UTC")
        resultats$df_periode<-as.Date(resultats$df_periode,format="%Y-%m-%d",tz="UTC")
        resultats$d_ajmax_ti<-as.Date(resultats$d_ajmax_ti,format="%Y-%m-%d",tz="UTC")
        resultats$d_tmj_max<-as.Date(resultats$d_tmj_max,format="%Y-%m-%d",tz="UTC")
        resultats$dd_tm30j_max<-as.Date(resultats$dd_tm30j_max,format="%Y-%m-%d",tz="UTC")
        resultats$df_tm30j_max<-as.Date(resultats$df_tm30j_max,format="%Y-%m-%d",tz="UTC")
        resultats$dd_tmj_4<-ifelse(is.na(resultats$dd_tmj_4),"NULL",as.Date(resultats$dd_tmj_4,format="%Y-%m-%d",tz="UTC"))
        resultats$df_tmj_4<-ifelse(is.na(resultats$df_tmj_4),"NULL",as.Date(resultats$df_tmj_4,format="%Y-%m-%d",tz="UTC"))
        resultats$d50_ponte<-as.Date(resultats$d50_ponte,format="%Y-%m-%d",tz="UTC")
        resultats$d50_ecl<-as.Date(resultats$d50_ecl,format="%Y-%m-%d",tz="UTC")
        resultats$d50_emg<-as.Date(resultats$d50_emg,format="%Y-%m-%d",tz="UTC")

        resultats<-cbind(data.frame(ra_station=c(input$station_BDD)),resultats)



        output$resultats_BDD <- DT::renderDT(
          DT::datatable(
            resultats,
            rownames=TRUE,
            extensions = "Buttons",
            option=list(
              scroller = TRUE,
              scrollX = TRUE,
              #lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
              #"pagelength"=-1,
              dom= "Blfrtip",
              scrollX = T,
              buttons=list(
                list(extend="excel",
                     filename = "table resultats"))
            )
          ))
        
        observeEvent(input$write_table_sql4,{
          shinyCatch({
            db_connection<-thermish$db_connection
            user_schema<-thermish$schema
            validate(need(!is.null(resultats), "Pas de r\u00e9sultats a importer"))
            shinybusy::show_modal_spinner()
            table_id<-DBI::Id(
              schema=user_schema,
              table="resultats_analyse"
            )
            DBI::dbWriteTable(con = db_connection,name = table_id,value = resultats,append = TRUE)
            
            shinybusy::remove_modal_spinner()
            ###shinyModal to show to user when the table is written to the database is successful
            shiny::showModal( shiny::modalDialog( title=paste0("RPostgreSQL table Updated"),
                                                  br(),
                                                  div(tags$b(paste0("Les donn\u00e9es ont bien \u00e9t\u00e9
                                                                    \u00e9crites dans la base de donn\u00e9es"), style = "color: green;"))
            ))
          })
        })


    },
    blocking_level = "error")},
    ignoreInit=T,
    ignoreNULL = F
  ) #fin observevent
    
    

      
    })
}
    
## To be copied in the UI
# mod_analysis_from_bdd_ui("analysis_from_bdd_1")
    
## To be copied in the server
# mod_analysis_from_bdd_server("analysis_from_bdd_1")
