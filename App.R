library(shiny)
library(shiny.router)
library(shinydashboard)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(DT)
library(highcharter)
library(lubridate, warn.conflicts = FALSE)
library(shinyjs)
library(shinycssloaders)
library(rintrojs)
library(glue)
library(stringi)
library(plyr)
library(stringr)



source("Fonctions.R",encoding="UTF-8")
source("Homicides.R",encoding="UTF-8")
source("Violencesphysiques.R",encoding="UTF-8")
source("Violencessexuelles.R",encoding="UTF-8")
source("Arnaquesetescroqueries.R",encoding="UTF-8")
source("Volssurpersonne.R",encoding="UTF-8")
source("Cambriolages.R",encoding="UTF-8")
source("Stupefiants.R",encoding="UTF-8")
source("TraiteDesEtresHumains.R",encoding="UTF-8")
source("QPV.R",encoding="UTF-8")
source("AtteintesEnvAnimaux.R",encoding="UTF-8")
source("Vols_vehicules.R",encoding="UTF-8")
source("DelinquanceTransports.R",encoding="UTF-8")
source("Cyberdelinquance.R",encoding="UTF-8")
source("SentimentInsecurite.R",encoding="UTF-8")
source("Comparaison_europeenne.R",encoding="UTF-8")
source("Elucidation_faits_delinquance.R",encoding="UTF-8")
source("Caracteristiques_victimes.R",encoding="UTF-8")
source("Caracteristiques_mec.R",encoding="UTF-8")
source("Discriminations_crimes_de_haine.R",encoding="UTF-8")
source("Atteintes_probite.R",encoding="UTF-8")
source("Destructions_degradation.R",encoding="UTF-8")
source("Relations_population_fs.R",encoding="UTF-8")
source("Delinquance_intrafamiliale.R",encoding="UTF-8")
source("Policiersmunicipaux.R",encoding="UTF-8")
source("Policiersetgendarmes.R",encoding="UTF-8")
source("Conditions_de_travail_des_fs.R",encoding="UTF-8")
source("Delinquance_par_UU.R",encoding="UTF-8")
source("depense_securite.R",encoding="UTF-8")

#source("informations.R",encoding="UTF-8")

source("admin_informations.R",encoding="UTF-8")

source("enq_informations.R",encoding="UTF-8")





ListeSentimentInsecurite<-c("Sentiment d'insécurité")
ListePanoramaDelinquance<-c("Homicides","Violences physiques","Traite et exploitation des êtres humains","Violences sexuelles","Discriminations et crimes de haine","Arnaques et escroqueries",
                            "Atteintes à la probité","Infractions à la légisation sur les stupéfiants","Vols sur personne","Vols liés aux véhicules","Cambriolages","Atteintes à l'environnement et aux animaux",
                            "Destructions et dégradations des biens")

ListeMilieuxCommission<-c("Violences intrafamiliales","Quartiers prioritaires de la politique de la ville","Délinquance dans les transports","Cyberdélinquance","Délinquance par taille d'unité urbaine")
ListeProfessionnelsSecurite<-c("Relations population et forces de sécurité","Policiers et gendarmes","Policiers municipaux","Conditions de travail des policiers et des gendarmes","Dépenses en matière de sécurité")



FunctionBox<-function(title,list){

  liste_des_boutons<-lapply(list, function(x){

    f=Unaccent(x)

    y=gsub(" ","",f)

    actionButton(y,x,onclick = glue("window.open('./#!/{y}', '_parent')"),icon = NULL)  # class="home-btn"


  })

  tagList(

    div(class="card card-page-accueil",

        h5(class="card-header card-header-page-accueil",title),

        div(class="card-body",

          lapply(seq(1,length(liste_des_boutons),2), function(i){

          Resultat<-div(class="row",

          div(class="col-md-6",liste_des_boutons[i]),

          div(class="col-md-6",liste_des_boutons[i+1]))})

        )))}




home_page<-div(class="container home-container",style="background-color:#fff;flex-direction:column;justify-content:center;",

    div(class="row text-center",style="border:1px solid transparent;margin-top:4%;",h2("LES CHIFFRES CLÉS DE LA SÉCURITÉ INTÉRIEURE")),

    div(class="row text-start",style="border:1px solid transparent;",

      div(class="col-md-12",

      p("Le Service statistique ministériel de la sécurité intérieure lance un outil interactif pour visualiser les chiffres clés de la sécurité intérieure : 
        de multiples thématiques et indicateurs développés pour mieux mesurer la délinquance et l'insécurité, actualisés et enrichis régulièrement, au service de tous."),
      br(),
      p("Découvrez, ci-dessous, les principales statistiques relatives à la sécurité intérieure issues des :"))),
      br(),
      div(class="card",
          
      div(class="card-body",style="border:1px solid transparent;background-color:#fff;",

      div(class="row",

      div(class="col-md-6",

      actionButton("admin-button",HTML("Données enregistrées par la police et la gendarmerie nationales ou autres données administratives"),onclick = glue("window.open('./#!/informations-donnees-administratives', '_parent')"))),

      div(class="col-md-6",

      actionButton("enq-button",HTML("Données issues d&apos;enquêtes en population générale ou d&apos;autres enquêtes"),onclick = glue("window.open('./#!/informations-donnees-enquetes', '_parent')")))

          ))),

      br(),
      FunctionBox("PANORAMA DE LA DÉLINQUANCE",ListePanoramaDelinquance),
      FunctionBox("LIEUX DE COMMISSION DE LA DÉLINQUANCE",ListeMilieuxCommission),
      FunctionBox("PROFESSIONNELS DE LA SÉCURITÉ",ListeProfessionnelsSecurite),
      FunctionBox("CARACTERISTIQUES DES VICTIMES ET DES MIS EN CAUSE",c("Victimes","Mis en cause")),
      FunctionBox("SENTIMENT D'INSECURITÉ",ListeSentimentInsecurite),
      FunctionBox("ÉLÉMENT DE PROCÉDURE",c("Elucidation des faits de délinquance")),
      FunctionBox("COMPARAISONS INTERNATIONALES",c("Comparaisons européennes")),
      br(),
      br(),
      br())



router <- make_router(
  route("/", home_page),
  route("Homicides", homicides_ui,homicides_server),
  route("Violencesphysiques",violences_physiques_ui,violences_physiques_server),
  route("Violencessexuelles",Violencessexuelles_ui,Violencessexuelles_server),
  route("Arnaquesetescroqueries",Arnaquesetescroqueries_ui,Arnaquesetescroqueries_server),
  route("Volssurpersonne",Volssurpersonne_ui,Volssurpersonne_server),
  route("Cambriolages",Cambriolages_ui,Cambriolages_server),
  route("Infractionsalalegisationsurlesstupefiants",stupefiants_ui,stupefiants_server),
  route("Traiteetexploitationdesetreshumains",teh_ui,teh_server),
  route("Quartiersprioritairesdelapolitiquedelaville",qpv_ui,qpv_server),
  route("Atteintesalenvironnementetauxanimaux",atteintes_env_animaux_ui,atteintes_env_animaux_server),
  route("Volsliesauxvehicules",vols_vehicule_ui,vols_vehicule_server),
  route("Delinquancedanslestransports",delinquance_transports_ui,delinquance_transports_server),
  route("Cyberdelinquance",cyberdelinquance_ui,cyberdelinquance_server),
  route("Sentimentdinsecurite",insecurite_ui,insecurite_server),
  route("Comparaisonseuropeennes",comparaison_europe_ui,comparaison_europe_server),
  route("Elucidationdesfaitsdedelinquance",elucidation_ui,elucidation_server),
  route("Victimes",caracteristiques_victimes_ui,caracteristiques_victimes_server),
  route("Misencause",caracteristiques_mec_ui,caracteristiques_mec_server),
  route("Discriminationsetcrimesdehaine",dicrimination_crimes_haine_ui,dicrimination_crimes_haine_server),
  route("Atteintesalaprobite",atteintes_probite_ui,atteintes_probite_server),
  route("Destructionsetdegradationsdesbiens",Destructions_degradataion_ui,Destructions_degradataion_server),
  route("Relationspopulationetforcesdesecurite",Relations_population_ui,Relations_population_server),
  route("Violencesintrafamiliales",Delinquance_intrafamiliale_ui,Delinquance_intrafamiliale_server),
  route("Policiersmunicipaux",Policiersmunicipaux_ui,Policiersmunicipaux_server),
  route("Policiersetgendarmes",Policiersetgendarmes_ui,Policiersetgendarmes_server),
  route("Conditionsdetravaildespoliciersetdesgendarmes",Conditionsdetravail_fs_ui,Conditionsdetravail_fs_server),
  route("Delinquancepartailleduniteurbaine",Delinquance_par_UU_ui,Delinquance_par_UU_server),
  route("Depensesenmatieredesecurite",depense_securite_ui,depense_securite_server),
  route("informations-donnees-administratives",admin_informations),
  route("informations-donnees-enquetes",enq_informations)


  )



ui <- shinyUI(fluidPage(
  
tags$head(includeScript("analytics_cc.js")),

tags$head(includeHTML("analytics_cc.html")),
  
tags$style('
           
           .list-infos{list-style:none;font-family:Marianne;color:#666666;font-size:11px;fill:#666666;}
           
           body{font-family:Marianne;}'),
  
includeHTML('css/nav.html'),
useShinyjs(),
includeCSS("css/styles.css"),
includeCSS("css/footerDV.css"),
router$ui,
br(),
br(),
br(),
includeHTML('css/SSMSIFooterDV.html')


)

)

server <- shinyServer(function(input, output, session) {
  
  router$server(input, output, session)
  
})


shinyApp(ui, server)
