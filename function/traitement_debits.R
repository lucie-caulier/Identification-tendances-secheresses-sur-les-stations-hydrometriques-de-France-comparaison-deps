#### Fonction de (custom Benoit Richard) pour l'importation des données de débit depuis Hub'eau pour UNE station ####

get_serie_hydro <- function(code_station, 
                            date_debut, 
                            date_fin, 
                            param){
  get_hydrometrie_obs_elab(
    # fonction d'importation du package hubeau
    list(
      code_entite = code_station,
      # code de la station
      date_debut_obs_elab = date_debut,
      #date de début de l'importation
      date_fin_obs_elab = date_fin,
      # date de fin de l'importation
      grandeur_hydro_elab = param
    )
  ) %>%  # grandeur importée (QmJ= débits moyens journaliers, QmM= débits moyens mensuels)
    select(code_station:resultat_obs_elab) %>%  #sélection des colonnes d'intêret
    mutate(annee = lubridate::ymd(date_obs_elab),
           # création d'une colonne année au format date
           annee = lubridate::year(annee))
}

#### Fonction (Benoit Richard) pour vérifer si les imports Hub'eau (de plusieurs stations) sont possibles" ####

series_stations_tot <- function (hydro_stations, date_debut, date_fin, param) {
  
get_serie_hydro_possible <- purrr::possibly(get_serie_hydro, otherwise = "Aucune data")



  map(.x = hydro_stations, 
      .f = function(x) {
        get_serie_hydro_possible(code_station = x,
                                 date_debut = date_debut,
                                 date_fin = date_fin,
                                 param = param
        )
        
      }
      
  )

}

#### Fonction de calcul d'UN seuil de sécheresse à partir de la FDC #### 

# arguments : 
## q_jr = data frame contenant les débits journaliers pour la station à étudier, importé depuis l'API hydrometrie de Hub'eau 
## seuil = valeur de la fréquence au dépassement choisie pour déterminer le seuil de sécheresse (exemple : pour un seuil au Q95, seuil <- 0.95) 
## libelle_sta = nom de la station étudiée (caractères) (exemple : pour l'Authie à Dompierre, libelle_sta <- "L'Authie à Dompierre" )

# return : 
## FDC (graphique)avec le seuil de sécheresse choisi 
## valeur du seuil de sécheresse choisi 


calcul_1seuil_sech <- function (q_jr, seuil) {

  x  <- q_jr$resultat_obs_elab  # débits journaliers 
  y <- fdc(x, lQ.thr=seuil, hQ.thr=NA
           ) # courbe des débits classés (fonction fdc du packahe HydroTSM)
  
  f <- splinefun(y,x) 
  

  return(f(seuil)) # renvoi de la valeur du seuil de sécheresse pour la fréquence au dépassement choisie 
  
}

### remarque : warning message (à creuser ?) 



############# Brouillon à partir d'ici 

#calcul_1seuil_sech_v2 <- function (code_sta, date_debut, date_fin, seuil) {
  
#  q_jr <- get_hydrometrie_obs_elab(     # fonction d'importation du package hubeau 
  #   list(code_entite = code_sta,   # code de la station 
   #      date_debut_obs_elab = date_debut,  #date de début de l'importation 
    #     date_fin_obs_elab= date_fin,  # date de fin de l'importation 
       #  grandeur_hydro_elab = "QmJ")) 
  
 # x  <- q_jr$resultat_obs_elab  # débits journaliers 
  
  #y <- fdc(x, lQ.thr=seuil, hQ.thr=NA, plot= FALSE
   #        ) # courbe des débits classés (fonction fdc du packahe HydroTSM)
  
  #f <- splinefun(y,x) 
  
  
  #return(f(seuil)) # renvoi de la valeur du seuil de sécheresse pour la fréquence au dépassement choisie 
  
#}

calcul_1seuil_sech_v2 <- function (vecteur_debit, seuil) {
  
  y <- fdc(vecteur_debit, lQ.thr=seuil, hQ.thr=NA, plot= FALSE
  ) # courbe des débits classés (fonction fdc du packahe HydroTSM)
  
  f <- splinefun(y,vecteur_debit) 
  
  df<- data.frame(debit_seuil = f(seuil), Q_seuil = seuil)
  return(df) 
  
}
#### Fonction de calcul de plusieurs seuils de sécheresse à partir de la FDC #### 

# arguments : 
## codes_sta = vecteur de codes stations (format caractère) pour lesquels calculer les seuils de sécheresse 
## date_debut
## seuil = valeur de la fréquence au dépassement choisie pour déterminer le seuil de sécheresse (exemple : pour un seuil au Q95, seuil <- 0.95) 
## libelle_sta = nom de la station étudiée (caractères) (exemple : pour l'Authie à Dompierre, libelle_sta <- "L'Authie à Dompierre" )

# return : 
## FDC (graphique)avec le seuil de sécheresse choisi 
## valeur du seuil de sécheresse choisi 
 
#calcul_seuils_sech <- function (q_jr_totaux_def, seuil) {

  #codes_sta <-unique(q_jr_totaux_def$code_station)
  
  #seuils_sech <- map(.x = codes_sta
   #   , .f = function (x) {
   #     calcul_1seuil_sech_v2 (q_jr_totaux_def = q_jr_totaux_def, code_sta = x, seuil=seuil)
        
   #   } ) 
  
 #df_seuils <- as.data.frame (cbind (codes_sta, seuils_sech))
 
 #return (df_seuils)
  
#} # marche pas pour l'instant 


#### Fonction Benoir Richard calcul de seuils UNE station #### 

fdc_custom_1 <-
  function(vecteur_debit, 
           valeur_seuil = 0.95) {
    
    Qposition <- function(x, Q) {
      Q.dist  <- abs(x - Q)
      Q.index <- which.min( Q.dist )
      return(Q.index)
    }
    
    Fn <- ecdf(sort(vecteur_debit))
    dc <- 1 - Fn(vecteur_debit) + 1/length(vecteur_debit)
    
    df_seuil <- data.frame(seuil = valeur_seuil, 
                           debit = vecteur_debit[Qposition(dc, valeur_seuil)])
    
    return(df_seuil)
    
  }


#### calcul de seuils pour plusieurs stations #### 


#df_seuil <- data.frame()
#stations_ciblees <- unique(q_jr_totaux_def$code_station)
#for (i in 1:length(stations_ciblees)) {

  #q_jr_totaux_sub <- q_jr_totaux_def %>% 
   # filter(code_station== stations_ciblees[i])
  
  #seuil <-calcul_1seuil_sech_v2(q_jr_totaux_sub$resultat_obs_elab, 0.95)
  
  #df_seuil <- rbind(df_seuil, seuil)
  
  
#}
#df_seuil <-cbind(df_seuil, stations_ciblees)  



calcul_seuils<- function (q_jr_totaux_def, valeur_seuil) {
df_seuil <- data.frame()
stations_ciblees <- unique(q_jr_totaux_def$code_station)
for (i in 1:length(stations_ciblees)) {
  
  q_jr_totaux_sub <- q_jr_totaux_def %>% 
    filter(code_station== stations_ciblees[i])
  
  seuil <-calcul_1seuil_sech_v2(q_jr_totaux_sub$resultat_obs_elab, valeur_seuil)
  
  df_seuil <- rbind(df_seuil, seuil)
  
  
}
df_seuil <-cbind(df_seuil, stations_ciblees) 

return(df_seuil)
}



#### Fonction pour calculer les VCN10 pour UNE station du data frame de débits (avec débits spécifiques) #### 

VCNx_1sta <- function (vecteur_debits_spe, vecteur_dates, jours_glissants, code_station) {
  
  dates<-sort(vecteur_dates, decreasing = FALSE)
  
  VCNx<-
    mean_run(vecteur_debits_spe,   # calcul d'une moyenne glissante des débits spécifiques sur le nombre de jours choisis  
             k = jours_glissants, 
             idx = dates
    )
  
  VCNx<-data.frame(VCNx_spe = VCNx, annee=substr(dates, 1,4), jours_glissants = rep(jours_glissants, each=length(VCNx)),
                   code_sta = rep(code_station, each=length(VCNx))) # création d'un data frame avec les moyennes glissantes, les dates et les années 
  
  VCNx<-VCNx %>% 
    dplyr::group_by(annee, jours_glissants, code_sta) %>% 
    dplyr::summarise(VCNx_annuel_spe=min(VCNx_spe)) # calcul du minimum par années des moyennes glissantes -> VCN10 annuel 
  
  return(VCNx)
}  

#### Fonction pour calculer les VCNx de plusieurs stations #### 

VCNx_sta_mult <- function (q_jr_totaux_def, jours_glissant) {
  
  map(.x = unique(q_jr_totaux_def$code_station),
         .f = function (x){
           q_jr<-q_jr_totaux_def %>% 
             filter(code_station == x) %>% 
             arrange(date_obs_elab)
           
           vecteur_debits_spe <- q_jr$resultat_obs_elab_spe
           vecteur_dates <- q_jr$date_obs_elab 
           
          VCNx <- VCNx_1sta(vecteur_debits_spe, vecteur_dates, jours_glissant, code_station = unique(q_jr$code_station)) 
          
          return(VCNx)
         })
}

#### Fonction pour calculer les VCN3 pour UNE station du data frame de débits (avec débits spécifiques) #### 
VCN3_1sta <- function (vecteur_debits_spe, vecteur_dates, jours_glissants_2, code_station) {
  
  dates<-sort(vecteur_dates, decreasing = FALSE)
  
  VCN3<-
    mean_run(vecteur_debits_spe,   # calcul d'une moyenne glissante des débits spécifiques sur le nombre de jours choisis  
             k = jours_glissants_2, 
             idx = dates
    )
  
  VCN3<-data.frame(VCN3_spe = VCN3, annee=substr(dates, 1,4), jours_glissants_2 = rep(jours_glissants_2, each=length(VCN3)),
                   code_sta = rep(code_station, each=length(VCN3))) # création d'un data frame avec les moyennes glissantes, les dates et les années 
  
  VCN3<-VCN3 %>% 
    dplyr::group_by(annee, jours_glissants_2, code_sta) %>% 
    dplyr::summarise(VCN3_annuel_spe=min(VCN3_spe)) # calcul du minimum par années des moyennes glissantes -> VCN3 annuel 
  
  return(VCN3)
}


#### Fonction pour calculer les VCN3 de plusieurs stations #### 

VCN3_sta_mult <- function (q_jr_totaux_def, jours_glissant_2) {
  
  map(.x = unique(q_jr_totaux_def$code_station),
      .f = function (x){
        q_jr<-q_jr_totaux_def %>% 
          filter(code_station == x) %>% 
          arrange(date_obs_elab)
        
        vecteur_debits_spe <- q_jr$resultat_obs_elab_spe
        vecteur_dates <- q_jr$date_obs_elab 
        
        VCN3 <- VCN3_1sta(vecteur_debits_spe, vecteur_dates, jours_glissant_2, code_station = unique(q_jr$code_station)) 
        
        return(VCN3)
      })
}
