#' @import dplyr

CY<-lubridate::year(Sys.Date())

# thermish<-environment()

thermish<-new.env()

unlockEnvironment <- function (env) {
  return (new.env(parent=env))
}

thermish<-unlockEnvironment(thermish)

require(magrittr)
require(rle)


################ Variables rappel  ###########

fnrappel<-function(data){
  
  Dd_Periode<-min(data$date)
  
  Df_Periode<-max(data$date)
  
  Duree<-length(unique(data$date))
  
  Rappel<-data.frame(Dd_Periode=Dd_Periode,
                     Df_Periode=Df_Periode,
                     Duree=Duree
  )
  return(Rappel)
}


############## Variables thermiques générales du milieu  #########################


fnvarthergen<-function(data){
  
  ###### Temperature instantanée minimale
  
  Ti_min<-min(data$temperature)
  
  ##### Temperature instantanée maximale
  
  Ti_max<-max(data$temperature)
  
  ###### Amplitude thermique 
  
  ATi<-Ti_max-Ti_min
  
  ##### Amplitude thermique journalière maximale et date de celle-ci
  
  Tmaxj<-dplyr::summarise(dplyr::group_by(data,date),max=max(temperature)) %>%
    as.data.frame()
  
  Tminj<-dplyr::summarise(dplyr::group_by(data,date),min=min(temperature)) %>%
    as.data.frame()
  
  Tminmaxj<-merge(Tmaxj,Tminj,by="date",all = T)
  
  
  Tminmaxj$amplitude<-Tminmaxj$max-Tminmaxj$min
  
  Ajmax_Ti<- Tminmaxj %>% dplyr::slice_max(amplitude) %>% dplyr::select(4)
  Ajmax_Ti<- Ajmax_Ti[1,]
  
  D_Ajmax_Ti<-Tminmaxj %>% dplyr::slice_max(amplitude) %>% dplyr::select(1)
  D_Ajmax_Ti<-D_Ajmax_Ti[1,]
  
  ###### Température moyenne journalière minimale
  
  Tmj<-dplyr::summarise(dplyr::group_by(data,date),mean=mean(temperature)) %>%
    as.data.frame()
  
  Tmj_min<-min(Tmj$mean)  
  
  ###### Température moyenne journalière maximale
  
  Tmj_max<-max(Tmj$mean) 
  
  ###### Amplitude thermique des moyennes journalières
  
  ATmj<-Tmj_max-Tmj_min
  
  ###### Date à laquelle la température moyenne journalière maximale a été observée
  
  D_Tmj_max<- Tmj%>% dplyr::slice_max(mean) %>% dplyr::select(1)
  D_Tmj_max<-D_Tmj_max[1,]
  
  ##### Température moyenne de la période 
  
  Tmp<-mean(data$temperature)
  
  #### Température moyenne des 30 jours consécutifs les plus chauds  
  
  # Tmj$rollsum<-frollapply(Tmj$mean, 30, sum)
  
  Tmj$rollmean<-data.table::frollapply(Tmj$mean, 30, mean)
  
  Tm30j_max<-max(Tmj$rollmean,na.rm = T)
  
  Df_Tm30j_max<-Tmj %>% dplyr::slice_max(rollmean) %>% dplyr::select(1)
  Df_Tm30j_max<-Df_Tm30j_max[1,]
  
  Dd_Tm30j_max<- Df_Tm30j_max-29
  
  Var_therm_gener<-data.frame(Ti_min=Ti_min,
                              Ti_max=Ti_max,
                              ATi=ATi,
                              Ajmax_Ti=Ajmax_Ti,
                              D_Ajmax_Ti=D_Ajmax_Ti,
                              Tmj_min=Tmj_min,
                              Tmj_max=Tmj_max,
                              ATmj=ATmj,
                              D_Tmj_max=D_Tmj_max,
                              Tmp=Tmp,
                              Tm30j_max=Tm30j_max,
                              Dd_Tm30j_max=Dd_Tm30j_max,
                              Df_Tm30j_max=Df_Tm30j_max
  )
  
  return(Var_therm_gener)
}

############## Variables en rapport avec le preferendum thermique de la truite commune  #########################

fnvarthertruite<-function(data){
  
  ####### Nombre total de jours où T° moyenne est >= à 4°C et <= à 19°C
  
  Tmj<-dplyr::summarise(dplyr::group_by(data,date),mean=mean(temperature)) %>%
    as.data.frame()
  
  Nbj_Tmj_4_19 <- Tmj %>% dplyr::filter(mean>=4 & mean <=19)
  Nbj_Tmj_4_19 <- nrow(Nbj_Tmj_4_19)
  
  
  ######  pourcentage de jours où la T° moyenne journalière est comprise entre 4 et 19°C 
  
  percent_j_Tmj_4_19<- round(Nbj_Tmj_4_19/nrow(Tmj)*100,digits = 2)
  
  
  #####  Date à laquelle la T° moyenne journalière (Tmj) est pour la première fois sur la période <4°C
  
  Tmj_4<- Tmj %>% dplyr::filter(mean<4)
  Dd_Tmj_4<-ifelse(nrow(Tmj_4)==0,"NA",as.character(min(Tmj_4$date)))
  
  #####  Date à laquelle la T° moyenne journalière (Tmj) est pour la dernière fois sur la période <4°C
  
  Df_Tmj_4<-ifelse(nrow(Tmj_4)==0,"NA",as.character(max(Tmj_4$date)))
  
  ##### % de jours au cours de la période où la T° moyenne journalière est < 4°C
  
  Nbj_Tmj_inf4 <- Tmj %>% dplyr::filter(mean<4)
  Nbj_Tmj_inf4 <- nrow(Nbj_Tmj_inf4)
  
  percent_j_Tmj_inf4<- round(Nbj_Tmj_inf4/nrow(Tmj)*100,digits = 2)
  
  
  ##### % de jours au cours de la période où la T° moyenne journalière est > 19°C
  
  Nbj_Tmj_sup19 <- Tmj %>% dplyr::filter(mean>19)
  Nbj_Tmj_sup19 <- nrow(Nbj_Tmj_sup19)
  
  percent_j_Tmj_sup19<- round(Nbj_Tmj_sup19/nrow(Tmj)*100,digits = 2)
  
  ##### Nombre d'heures totales où la T° est > 19°C 
  
  Nb_Ti_sup19 <-  data %>% dplyr::filter(temperature>19)
  Nb_Ti_sup19 <- nrow(Nb_Ti_sup19)
  
  ##### Nbre de séquences pdt la période durants lesquels les T° instantanée restent > 19°C
  
  data$statut<-ifelse(data$temperature>19,"TRUE","FALSE")
  
  succ.count.19 <- rle(data$statut)$lengths[rle(data$statut)$values=="TRUE"]
  
  Nb_sq_Ti_sup19 <- length(succ.count.19)
  
  ##### Nb d'heures maximales consécutives durant lesquels les T° instantanées restent >19°C
  
  Nbmax_Ti_csf_sup19 <- ifelse(Nb_sq_Ti_sup19==0,0,max(succ.count.19))
  
  ##### Date de début et de fin de chaque séquence où la Ti est >19°C 
  
  # rle1 <- rle(data$temperature)
  # rle19 <- rle1
  # rle19$values <- ifelse(rle19$values>19, cumsum(rle19$values),0)
  # rle19<-tapply(data$date_heure, inverse.rle(rle19), range)[-1]
  # date_seq_sup19 <- if(Nb_Ti_sup19==0){"NA"}else{data.frame(matrix(unlist(rle19), nrow=length(rle19), byrow=TRUE))}
  #date_seq_sup19$X1<-as.POSIXct(date_seq_sup19$X1,origin="1970-01-01",tz="UTC")
  #date_seq_sup19$X2<-as.POSIXct(date_seq_sup19$X2,origin="1970-01-01",tz="UTC")
  
  ##### Nombre d'heures totales où la T° est > 25°C 
  
  Nb_Ti_sup25 <-  data %>% dplyr::filter(temperature>=25)
  Nb_Ti_sup25 <- nrow(Nb_Ti_sup25)
  
  ##### Nbre de séquences pdt la période durants lesquels les T° instantanée restent > 25°C
  
  data$statut<-ifelse(data$temperature>=25,"TRUE","FALSE")
  
  succ.count.25 <- rle(data$statut)$lengths[rle(data$statut)$values=="TRUE"]
  
  
  
  Nb_sq_Ti_sup25 <- length(succ.count.25)
  
  ##### Nb d'heures maximales consécutives durant lesquels les T° instantanées restent >25°C
  
  Nbmax_Ti_csf_sup25 <- ifelse(Nb_sq_Ti_sup25==0,0,max(succ.count.25))
  
  ##### Date de début et de fin de chaque séquence où la Ti est >25°C 
  
  
  # rle25 <- rle1
  # rle25$values <- ifelse(rle25$values>=25, cumsum(rle25$values),0)
  # rle25<-tapply(data$date_heure, inverse.rle(rle25), range)[-1]
  # date_seq_sup25 <- if(Nb_Ti_sup25==0){"NA"}else{data.frame(matrix(unlist(rle25), nrow=length(rle25), byrow=TRUE))}
  # date_seq_sup25$X1<-as.POSIXct(date_seq_sup25$X1,origin="1970-01-01",tz="GMT")
  # date_seq_sup25$X2<-as.POSIXct(date_seq_sup25$X2,origin="1970-01-01",tz="GMT")
  
  ######
  
  Var_therm_truite<-data.frame(Nbj_Tmj_4_19=Nbj_Tmj_4_19,
                               percent_j_Tmj_4_19=percent_j_Tmj_4_19,
                               Dd_Tmj_4=Dd_Tmj_4,
                               Df_Tmj_4=Df_Tmj_4,
                               percent_j_Tmj_inf4=percent_j_Tmj_inf4,
                               percent_j_Tmj_sup19=percent_j_Tmj_sup19,
                               Nb_Ti_sup19=Nb_Ti_sup19,
                               Nb_sq_Ti_sup19=Nb_sq_Ti_sup19,
                               Nbmax_Ti_csf_sup19=Nbmax_Ti_csf_sup19,
                               Nb_Ti_sup25=Nb_Ti_sup25,
                               Nb_sq_Ti_sup25=Nb_sq_Ti_sup25,
                               Nbmax_Ti_csf_sup25=Nbmax_Ti_csf_sup25
                               
  )
  
  return(Var_therm_truite)
}

############## Conditions thermiques potentiellement favorable au développement de la MRP (ou PKD)  #########################

fncondfavMRP<-function(data){
  
  #####  Nb d'heures totales où la T° instantanée est >= 15°C 
  
  Nb_Ti_sup15 <-  data %>% dplyr::filter(temperature>=15)
  Nb_Ti_sup15 <- nrow(Nb_Ti_sup15)
  
  ##### Nbre de séquences pdt la période durants lesquels les T° instantanée restent >= 15°C
  
  data$statut<-ifelse(data$temperature>=15,"TRUE","FALSE")
  
  succ.count.15 <- rle(data$statut)$lengths[rle(data$statut)$values=="TRUE"]
  
  
  
  Nb_sq_Ti_sup15 <- length(succ.count.15)
  
  ##### Nb d'heures maximales consécutives durant lesquels les T° instantanées restent >= 15°C
  
  Nbmax_Ti_csf_sup15 <- ifelse(Nb_sq_Ti_sup15==0,0,max(succ.count.15)) 
  
  ####
  
  condfavMRP<-data.frame(Nb_Ti_sup15=Nb_Ti_sup15,
                         Nb_sq_Ti_sup15=Nb_sq_Ti_sup15,
                         Nbmax_Ti_csf_sup15=Nbmax_Ti_csf_sup15
                         
  )
  
  return(condfavMRP)
  
}



############## Conditions thermiques au cours de la phase de vie embryo-larvaire (intra-graviers)  #########################

fncondembryolarv<-function(data,date_ponte){
  
  ###### Date médiane de ponte 
  
  D50_ponte<-date_ponte
  
  
  ##### Date médiane d'éclosion
  
  Tmj<-dplyr::summarise(dplyr::group_by(data,date),mean=mean(temperature)) %>%
    as.data.frame()
  
  Tmj<-subset(Tmj,Tmj$date>date_ponte)
  
  Tmj$log10D2<-(-13.9306*log10(Tmj$mean+80))+28.8392
  Tmj$D2<-10^Tmj$log10D2
  Tmj$a100D2<-100/Tmj$D2
  Tmj$cumsum100D2<-cumsum(Tmj$a100D2)
  Tmj$D3<-((1.660*Tmj$D2)+5.4)
  Tmj$a100D3<-100/Tmj$D3
  Tmj$cumsum100D3<-cumsum(Tmj$a100D3)
  
  
  TmjD2<-Tmj %>% 
    dplyr::filter(cumsum100D2>=100) %>%
    dplyr::arrange(date) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  D50_Ecl<-TmjD2[,1]
  
  
  #####  Nombre de jours d'incubation 
  
  Nbj_Inc<-as.numeric(difftime(D50_Ecl,D50_ponte,units = "days"))+1
  
  ##### Date médiane d'émergence
  
  TmjD3<-Tmj %>%
    dplyr::filter(cumsum100D3>=100) %>%
    dplyr::arrange(date) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  D50_Emg<-TmjD3[,1]
  
  ##### Nombre de jours de la phase embryo larvaire = Nbre de jours jusqu'à l'émergence
  
  Nbj_Emg<-as.numeric(difftime(D50_Emg,D50_ponte,units = "days"))+1
  Nbj_PEL<-Nbj_Emg
  
  ##### Nombre de jours de résorption
  
  Nbj_Rsp<- Nbj_Emg-Nbj_Inc
  
  ###### Sélection des données pendant la PEL
  
  PEL<-data %>% 
    dplyr::filter(date >= D50_ponte) %>%
    dplyr::filter(date <= D50_Emg)
  
  #####  Nb d'heures totales où la T° instantanée est > 15°C  pendant la PEL
  
  Nb_Ti_sup15_PEL <-  PEL %>% dplyr::filter(temperature>15)
  Nb_Ti_sup15_PEL <- nrow(Nb_Ti_sup15_PEL)
  
  ##### Nbre de séquences pdt la période durants lesquels les T° instantanée restent > 15°C
  
  PEL$statut<-ifelse(PEL$temperature>15,"TRUE","FALSE")
  
  succ.count.15.PEL <- rle(PEL$statut)$lengths[rle(PEL$statut)$values=="TRUE"]
  
  
  Nb_sq_Ti_sup15_PEL <- length(succ.count.15.PEL)
  
  ##### Nb d'heures maximales consécutives durant lesquels les T° instantanées restent > 15°C
  
  Nbmax_Ti_csf_sup15_PEL <- ifelse(Nb_sq_Ti_sup15_PEL==0,0,max(succ.count.15.PEL)) 
  
  
  #####  Nb d'heures totales où la T° instantanée est < 1.5°C  pendant la PEL
  
  Nb_Ti_inf1.5_PEL <-  PEL %>% dplyr::filter(temperature<1.5)
  Nb_Ti_inf1.5_PEL <- nrow(Nb_Ti_inf1.5_PEL)
  
  ##### Nbre de séquences pdt la période durants lesquels les T° instantanée restent > 15°C
  
  PEL$statut<-ifelse(PEL$temperature<1.5,"TRUE","FALSE")
  
  succ.count.1.5.PEL <- rle(PEL$statut)$lengths[rle(PEL$statut)$values=="TRUE"]
  
  
  Nb_sq_Ti_inf1.5_PEL <- length(succ.count.1.5.PEL)
  
  ##### Nb d'heures maximales consécutives durant lesquels les T° instantanées restent > 15°C
  
  Nbmax_Ti_csf_inf1.5_PEL <- ifelse(Nb_sq_Ti_inf1.5_PEL==0,0,max(succ.count.1.5.PEL)) 
  
  #######
  
  condembryolarv<-data.frame(D50_ponte=D50_ponte,
                             Nbj_Inc=Nbj_Inc,
                             D50_Ecl=D50_Ecl,
                             Nbj_Rsp=Nbj_Rsp,
                             Nbj_PEL=Nbj_PEL,
                             D50_Emg=D50_Emg,
                             Nb_Ti_sup15_PEL=Nb_Ti_sup15_PEL,
                             Nb_sq_Ti_sup15_PEL=Nb_sq_Ti_sup15_PEL,
                             Nbmax_Ti_csf_sup15_PEL=Nbmax_Ti_csf_sup15_PEL,
                             Nb_Ti_inf1.5_PEL=Nb_Ti_inf1.5_PEL,
                             Nb_sq_Ti_inf1.5_PEL=Nb_sq_Ti_inf1.5_PEL,
                             Nbmax_Ti_csf_inf1.5_PEL=Nbmax_Ti_csf_inf1.5_PEL
  )
  
  return(condembryolarv)
  
}
