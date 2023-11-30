

load("data/List_nationalite_victime.RData") 
load("data/ListeIndicateursProfilVictimesAge.RData")
load("data/listePourTaux100Victimes.RData")
#load("data/Listepart_femmes_victimes.RData")

caracteristiques_victimes_ui<-PageLayoutFunction("Caractéristiques des victimes",
  
  tagList(
    
    outside_container(
      
       div(class="col-md-6",
          
          div(class="col-lg-12 card-container",
              
              tags$div(class="card cards",style="height:600px;border:1px solid transparent;background-color:#fff;",
                             
              div(class="card-header cards-header",
                  
              style="display:flex;justify-content:space-between;align-items:center;padding:4px 4px;color:#000;background-color:#ecf0f1;",
                                 
              tags$h5(class="card-title cards-title-admin",
      
              HTML("Données issues d'enquêtes en population générale ou données enregistrées par la police et la gendarmerie nationales")),
              
              tags$div(class='infobutton',Button('caracteristiques_victimes_a'))),
                             
              tags$div(class="card-body cards-body-admin",style="background-color:#fff;",
                       
              div(class="col-md-6 col-md-offset-3 border",selectInput("repart_nationalite_victimes","",choices=List_nationalite_victime,width="100%")),
                       
              highchartOutput('repart_nationalite_victimes_chart') ))
              
              
          )), ### Fin premier col-md-6
       
       
       
       
       ############### Deuxième figure ######################
       
       div(class="col-md-6",
           
           div(class="col-lg-12 card-container",
               
               tags$div(class="card cards",style="height:600px;border:1px solid transparent;background-color:#fff;",
                        
                  div(class="card-header cards-header",
                      
                  style="display:flex;justify-content:space-between;align-items:center;padding:4px 4px;color:#000;background-color:#ecf0f1;",
                            
                  tags$h5(class="card-title cards-title-admin",
                                    
                  HTML("Données issues d'enquêtes en population générale ou données enregistrées par la police et la gendarmerie nationales")),
                            

                  tags$div(class='infobutton',Button('caracteristiques_victimes_b'))),
                        
                  tags$div(class="card-body cards-body-admin",style="background-color:#fff;",
                                 
                  div(class="col-md-6 col-md-offset-3 border",selectInput("ProfilVictimesAgeID","",choices=ListeIndicateursProfilVictimesAge,width="100%")),
                                 
                  highchartOutput('repart_par_age_victimes') ))
               
               
           )) ### Fin second col-md-6
       
      
      
     
             ), 
    br(),
    br(),
    outside_container(
      
      BoxForChart(Button('caracteristiques_victimes_c'),highchartOutput('Taux100hbtsProfilVictimes'),
               Selector=TRUE,FullPage=TRUE,selectInput("ProfilTaux100Victimes","",choices=listePourTaux100Victimes,width="50%"))
      
    ),
    
    outside_container(
      
      BoxForChart(Button('caracteristiques_victimes_d'),highchartOutput('chart_victimes_parmi_les_forces_de_securite'),source="enq"),
      
      BoxForChart(Button('caracteristiques_victimes_e'),highchartOutput('chart_victimes_de_violences_dans_l_enfance'),source="enq")
      
    ),
    
    br(),
    br() 

  )
  
)

caracteristiques_victimes_server<-function(input,output,session){
  profil_victimes = eventReactive(input$Victimes,{  
    load("data/nationalite_victimes.RData")
    load("data/age_profil_victimes.RData")
    load("data/tx_1000_profil_victimes.RData")
    
    load("data/victimes_parmi_les_forces_de_securite.RData")
    load("data/victimes_de_violences_dans_l_enfance.RData")
    
    return(list(
      "nationalite_victimes" = nationalite_victimes,
      "age_profil_victimes"=age_profil_victimes,
      "tx_1000_profil_victimes"=tx_1000_profil_victimes,
      "victimes_parmi_les_forces_de_securite"=victimes_parmi_les_forces_de_securite,
      "victimes_de_violences_dans_l_enfance"=victimes_de_violences_dans_l_enfance

    ))

},ignoreNULL=FALSE)
  
### REACTIVES  
  
Nationalite_Victimes_Reactives<-reactive({
  profil_victimes()$nationalite_victimes %>% filter(Indicateurs==input$repart_nationalite_victimes)

})
  

age_profil_victimes_reactive<-reactive({
    profil_victimes()$age_profil_victimes %>% filter(Indicateurs==input$ProfilVictimesAgeID)

})
  

pv_taux100hbt<-reactive({
profil_victimes()$tx_1000_profil_victimes %>% filter(Indicateurs==input$ProfilTaux100Victimes)
    
})
  
  
output$repart_nationalite_victimes_chart<-renderHighchart({  
  
  highchartFunction(Nationalite_Victimes_Reactives(),'pie',ChoixCouleurs,    
                    
                    hcaes(x=as.character(sous_indicateurs),y=lapply(as.numeric(value),function(x){round(x)})),"PPIE",
                    
                    HEADER=unique(Nationalite_Victimes_Reactives()$titre),
                    
                    FormatLabelsData="sous_indicateurs")
  
  
})


output$repart_par_age_victimes<-renderHighchart({
  
  highchartFunction(age_profil_victimes_reactive(),'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(age_profil_victimes_reactive()$titre),
                    
                    FormatLabelsData="sous_indicateurs")
  
  
  
})



output$Taux100hbtsProfilVictimes<-renderHighchart({
  
  highchartFunction(pv_taux100hbt(),'line',ChoixCouleursBar[1:length(unique(pv_taux100hbt()$sous_indicateurs))],
                    
                    hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"N",
                    
                    HEADER=unique(pv_taux100hbt()$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
  
  
})





output$chart_victimes_parmi_les_forces_de_securite<-renderHighchart({
  
  highchartFunction(profil_victimes()$victimes_parmi_les_forces_de_securite,'column',ChoixCouleursBar[1:length(unique(profil_victimes()$victimes_parmi_les_forces_de_securite$sous_indicateurs))],
                    
                    hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
                    
                    HEADER=unique(profil_victimes()$victimes_parmi_les_forces_de_securite$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
  
  
})



output$chart_victimes_de_violences_dans_l_enfance<-renderHighchart({
  
  highchartFunction(profil_victimes()$victimes_de_violences_dans_l_enfance,'column',
                    
                    ChoixCouleursBar[1:length(unique(profil_victimes()$victimes_de_violences_dans_l_enfance$sous_indicateurs))],
                    
                    hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
                    
                    HEADER=unique(profil_victimes()$victimes_de_violences_dans_l_enfance$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
  
  
})  

  
  
# observeEvent(input$Victimes,{ 
# 
# output$repart_nationalite_victimes_chart<-renderHighchart({  
# 
#   highchartFunction(Nationalite_Victimes_Reactives(),'pie',ChoixCouleurs,    
#                      
#                      hcaes(x=as.character(sous_indicateurs),y=lapply(as.numeric(value),function(x){round(x)})),"PPIE",
#                      
#                      HEADER=unique(Nationalite_Victimes_Reactives()$titre),
#                      
#                      FormatLabelsData="sous_indicateurs")
#   
#       
# })
#     
#     
# output$repart_par_age_victimes<-renderHighchart({
# 
#   highchartFunction(age_profil_victimes_reactive(),'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(age_profil_victimes_reactive()$titre),
#                      
#                      FormatLabelsData="sous_indicateurs")
#   
#   
#       
# })
#     
# 
#     
# output$Taux100hbtsProfilVictimes<-renderHighchart({
#  
#   highchartFunction(pv_taux100hbt(),'line',ChoixCouleursBar[1:length(unique(pv_taux100hbt()$sous_indicateurs))],
#                      
#                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"N",
#                      
#                      HEADER=unique(pv_taux100hbt()$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
#   
#   
# })
# 
# 
# 
# 
# 
# output$chart_victimes_parmi_les_forces_de_securite<-renderHighchart({
#   
#   highchartFunction(profil_victimes()$victimes_parmi_les_forces_de_securite,'column',ChoixCouleursBar[1:length(unique(profil_victimes()$victimes_parmi_les_forces_de_securite$sous_indicateurs))],
#                      
#                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
#                      
#                      HEADER=unique(profil_victimes()$victimes_parmi_les_forces_de_securite$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
#   
#   
# })
# 
# 
# 
# output$chart_victimes_de_violences_dans_l_enfance<-renderHighchart({
#   
#   highchartFunction(profil_victimes()$victimes_de_violences_dans_l_enfance,'column',
#                      
#                      ChoixCouleursBar[1:length(unique(profil_victimes()$victimes_de_violences_dans_l_enfance$sous_indicateurs))],
#                      
#                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
#                      
#                      HEADER=unique(profil_victimes()$victimes_de_violences_dans_l_enfance$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
#   
#   
# })
# 
#    
#     
# },ignoreNULL=FALSE)
  
  

# Victimes
  
  
observeEvent(input$caracteristiques_victimes_a, {  showInfo(Nationalite_Victimes_Reactives())  })

observeEvent(input$caracteristiques_victimes_b, {  showInfo(age_profil_victimes_reactive() ) })

observeEvent(input$caracteristiques_victimes_c, {  showInfo(pv_taux100hbt() )  })

observeEvent(input$caracteristiques_victimes_d, {  showInfo(profil_victimes()$victimes_parmi_les_forces_de_securite)  })

observeEvent(input$caracteristiques_victimes_e, {  showInfo(profil_victimes()$victimes_de_violences_dans_l_enfance) })
  






  
}