#' analysis_from_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom utils read.csv2
#' @importFrom shiny NS tagList 
#' @importFrom stringr str_c
mod_analysis_from_file_ui <- function(id){
  ns <- NS(id)
  spsDepend("toastr")
  shinydashboard::tabItem(tabName="analysis_from_file",
                          shinydashboardPlus::box( title = "Import et Calcul", solidHeader = TRUE,
                               collapsible = TRUE, width = 3,
                               fileInput(ns('file3'), 'Choisir le fichier CSV',
                                         accept=c('text/csv',
                                                  'text/comma-separated-values,text/plain',
                                                  '.csv'))%>%
                               spsComps::bsPopover(			
                                                    "Cliquez ici pour importer un fichier", 
                                                    content= "le fichier doit contenir, soit une colonne date heure
                                                    au format '25/10/2021 23:22:00' ou deux colonnes date et heure s\u00e9par\u00e9es;
                 une colonne avec les valeurs des temp\u00e9ratures.",
                                                    placement = "right", 
                                                    bgcolor = "#00a65a",
                                                    titlecolor = "white", 
                                                    contentcolor = "black"),
                               tags$hr(),
                               checkboxInput(ns('header3'), 'En-t\u00eate', TRUE),
                               radioButtons(ns('ncoldate3'),'format date heure',
                                            c('1 colonne date_heure'='1col',
                                              '1 colonne date et 1 colonne heure'='2 col')
                               ),
                               
                               radioButtons(ns('sep3'), 'S\u00e9parateur de colonnes',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t'),
                                            ';'),
                               radioButtons(ns('dec3'), 'D\u00e9cimal',
                                            c(Point='.',
                                              Virgule=','),
                               ),
                               dateInput(ns("date_ponte"), label = h4("Date de ponte"),
                                         value = (as.Date(stringr::str_c(format(Sys.Date(), "%Y"),"01-01",sep = "-")))-365),
                               
                               verbatimTextOutput(ns("value")),
                               actionButton(ns('run_csv'), 'Run')
                           
                          ),
                          
                          shinydashboardPlus::box(title = "R\u00e9sultats de l'analyse", solidHeader = TRUE,
                                                  collapsible = TRUE, width =9,
                                                  DT::DTOutput(ns('resultats_csv')),
                                                  DT::DTOutput(ns('t_mean_j'))
                          ),
                        
                          shinydashboardPlus::box(title = "Tableau des donn\u00e9es import\u00e9es", solidHeader = TRUE,
                                                  collapsible = TRUE, width = 9,
                                                  DT::DTOutput(ns('data_analyse_csv'))
                          ),
                          
                          shinydashboardPlus::box(title = "Graphique des donn\u00e9es import\u00e9es", solidHeader = TRUE,
                                                  collapsible = TRUE, width = 9,
                                                  plotly::plotlyOutput(outputId = ns("plot_csv"))
                          )
                          
                          
                          
                          
                          
                          
                          
  )
}
    
#' analysis_from_file Server Functions
#'
#' @noRd 
mod_analysis_from_file_server <- function(id,DD){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    getData3 <- reactive({
      
      
      inFile3 <- input$file3
      
      if (is.null(input$file3))
        return(NULL)
      
      if(input$ncoldate3=='1col'){
        
        df3<-read.csv2(inFile3$datapath, header=input$header3, sep=input$sep3, dec=input$dec3)
        validate(need(ncol(df3)==2,"le s\u00e9parateur de colonnes ne doit pas \u00eatre le bon"))
        colnames(df3)<-c("date_heure","temperature")
        
        
        df3$date_heure<-as.POSIXct(df3$date_heure,format="%d/%m/%Y %H:%M",tz="UTC")
        
        return(df3)}
      
      
      else{
        df3<-read.csv2(inFile3$datapath, header=input$header3, sep=input$sep3, dec=input$dec3)
        validate(need(ncol(df3)==3,"le s\u00e9parateur de colonnes ne doit pas \u00eatre le bon"))
        
        df3[,2]<-as.POSIXct(df3[,2],format="%d/%m/%Y",tz="UTC")
        df3[,1]<-as.POSIXct(stringr::str_c(df3[,2],df3[,1],sep = " "),format="%Y-%m-%d %H:%M",tz="UTC")
        
        df3[,2]<-df3[,3]
        
        df3<-df3[,-3]
        
        colnames(df3)<-c("date_heure","temperature")
        
        return(df3)
        
      }
    })
      
    output$data_analyse_csv <- DT::renderDT(
      DT::datatable(
        getData3()      ,
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
    
        observeEvent({input$file3
          DD$login_button()
          input$ncoldate3
          input$header3
          input$sep3
          input$dec3
        },{
          shinyCatch({
            if (is.null(input$file3))
              return(NULL)
            data<-getData3()
            if(is.null(data))
              return(NULL)
           
           
            validate(need(as.numeric(data$temperature),"la d\u00e9cimale dans la colonne temp\u00e9rature doit \u00eatre un point"))
            validate(need(as.POSIXct(data$date_heure),"le format de la date doit \u00eatre de type jj/mm/aaaa hh:mm:ss"))
            
            
            output$plot_csv <-plotly::renderPlotly({

              p<-ggplot2::ggplot(data)+ggplot2::geom_line(ggplot2::aes(x=date_heure,y=temperature))
              plotly::ggplotly(p)
            })
          },blocking_level = "error"
          )},  #fin ShinyCatch
        ignoreInit=T,
        ignoreNULL = F
        ) # fin observeEvent

        observeEvent({#input$file3
         # DD$login_button()
       # input$date_ponte
        input$run_csv
        },{
          shinyCatch({


            data<-getData3()
            if(is.null(data))
              return(NULL)

            data$date<-as.Date(data$date_heure)

            data<-data %>% dplyr::arrange(date_heure)

            
            validate(need(as.numeric(data$temperature),"la d\u00e9cimale dans la colonne temp\u00e9rature doit \u00eatre un point"))
            validate(need(as.POSIXct(data$date_heure),"le format de la date doit \u00eatre de type jj/mm/aaaa hh:mm:ss"))
            validate(need(input$date_ponte>=min(data$date_heure),"la date de ponte est inf\u00e9rieure
                          au d\u00e9but de la p\u00e9riode \u00e0 analyser"))
            validate(need(input$date_ponte<=max(data$date_heure),"la date de ponte est sup\u00e9rieure
                          \u00e0 la fin de la p\u00e9riode \u00e0 analyser"))
            
            
            if(nrow(data)<8760){(warning("il manque des donn\u00e9es dans le fichier, les r\u00e9sultats
                                         de l'analyse sont \u00e0 prendre avec pr\u00e9caution"))}
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

            cond_embry_larv<-fncondembryolarv(data,input$date_ponte)

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
            
            

            output$resultats_csv <- DT::renderDT(
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

          },blocking_level = "error") # fin shiny catch
          },
    ignoreInit=T,
    ignoreNULL = F
    ) # fin observe event

        output$value <- renderPrint({ input$date_ponte })
    
  })
}

## To be copied in the UI
# mod_analysis_from_file_ui("analysis_from_file_1")

## To be copied in the server
# mod_analysis_from_file_server("analysis_from_file_1")
