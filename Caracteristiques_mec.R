
load("data/ListRepartProfil_mec.RData")

load("data/List_nationalite_mec.RData")

load("data/ListProfilEvol_mec.RData")

load("data/Liste_repart_mec_violences_conjugales.RData")

load("data/Liste_contribution_min_maj_mec_etrangers.RData")


caracteristiques_mec_ui<-PageLayoutFunction("Caracteristiques des mis en cause",
  
  tagList(

    outside_container(
      
      div(class="col-md-6",
          
          div(class="col-lg-12 card-container",
              
              tags$div(class="card cards",style="height:600px;border:1px solid transparent;background-color:#fff;",
                       
                       div(class="card-header cards-header",
                           
                           style="display:flex;justify-content:space-between;align-items:center;padding:4px 4px;color:#000091;background-color:#ececfe;",
                           
                           tags$h5(class="card-title cards-title-admin",
                                   
                                   HTML("Données enregistrées par la police et la gendarmerie nationales ou autres données administratives")),
                           
                           tags$div(class='infobutton',Button('profil_a'))),
                       
                       tags$div(class="card-body cards-body-admin",style="background-color:#fff;",
                                
                                div(class="col-md-6 col-md-offset-3 border",selectInput("repart_age_et_part_homme_mec","",choices=ListRepartProfil_mec,width="100%")),
                                
                                highchartOutput('repart_age_et_part_homme_mec_chart') ))
              
              
          )), ### Fin premier col-md-6
      
      
      
      div(class="col-md-6",
          
          div(class="col-lg-12 card-container",
              
              tags$div(class="card cards",style="height:600px;border:1px solid transparent;background-color:#fff;",
                       
                       div(class="card-header cards-header",
                           
                           style="display:flex;justify-content:space-between;align-items:center;padding:4px 4px;color:#000091;background-color:#ececfe;",
                           
                           tags$h5(class="card-title cards-title-admin",
                                   
                                   HTML("Données enregistrées par la police et la gendarmerie nationales ou autres données administratives")),
                           
                           
                           tags$div(class='infobutton',Button('profil_b'))),
                       
                       tags$div(class="card-body cards-body-admin",style="background-color:#fff;",
                                
                                div(class="col-md-6 col-md-offset-3 border",selectInput("nationalite_mec","",choices=List_nationalite_mec,width="100%")),
                                
                                highchartOutput('nationalite_mec_chart') ))
              
              
          )) ### Fin second col-md-6
      

    ),
    
    br(),
    br(),
    outside_container(BoxForChart(Button('profil_c'),highchartOutput('evol_profil_mec'),Selector=TRUE,FullPage=TRUE,selectInput("ProfilMecEvolBase100","",choices=ListProfilEvol_mec,width="60%"))),
    br(),
    br(),
    outside_container(

      # BoxForChart(Button('profil_c'),highchartOutput('evol_profil_mec'),Selector=TRUE,FullPage=TRUE,selectInput("ProfilMecEvolBase100","",choices=ListProfilEvol_mec,width="60%")),
      
      BoxForChart(Button('profil_d'),highchartOutput('contribution_min_maj_mec_etrangers_chart'),Selector=TRUE,FullPage=TRUE,selectInput("select_contribution_min_maj_mec_etrangers","",choices=Liste_contribution_min_maj_mec_etrangers,width="60%"))
      
      
    ),
    
    br(),
    br() 
    
    
  )
  
)


caracteristiques_mec_server<-function(input,output,session){
  
  profil_des_mec = eventReactive(input$Misencause,{  
    
    load("data/repart_age_et_part_homme_mec.RData")
    
    load("data/profil_mec_evol.RData")
    
    load("data/repart_nationalite_mec.RData")
    
    load("data/repart_mec_violences_conjugales.RData")
    
    
    load("data/contribution_min_maj_mec_etrangers.RData")
    
    return(list(
      
      "repart_age_et_part_homme_mec" = repart_age_et_part_homme_mec,
      
      "profil_mec_evol"=profil_mec_evol,
      
      "repart_nationalite_mec" = repart_nationalite_mec,
      
      "repart_mec_violences_conjugales"=repart_mec_violences_conjugales,
      
      
      "contribution_min_maj_mec_etrangers"=contribution_min_maj_mec_etrangers
      
      
    ))
    
    
  },ignoreNULL=FALSE)
  
  
  
  
  
  repart_age_et_part_homme_mec_react<-reactive({
    
    profil_des_mec()$repart_age_et_part_homme_mec %>% filter(Indicateurs==input$repart_age_et_part_homme_mec)
    
  })
  
  
  
  ### MEC ##  
  
  
  profil_mec_evol_reactive<-reactive({
    
    profil_des_mec()$profil_mec_evol %>% filter(Indicateurs==input$ProfilMecEvolBase100)
    
  })
  
  
  
  
  repart_nationalite_mec_reactive<-reactive({
    
    profil_des_mec()$repart_nationalite_mec %>% filter(Indicateurs==input$nationalite_mec)
    
  })
  
  
  
  
  Reactive_repart_mec_violences_conjugales<-reactive({
    
    profil_des_mec()$repart_mec_violences_conjugales %>% filter(Indicateurs==input$Select_repart_mec_violences_conjugales)
    
  })
  

Reactive_contribution_min_maj_mec_etrangers<-reactive({
  
  profil_des_mec()$contribution_min_maj_mec_etrangers %>% filter(statistiques==input$select_contribution_min_maj_mec_etrangers)
  
})
  


output$repart_age_et_part_homme_mec_chart<-renderHighchart({
  
  highchartFunction(repart_age_et_part_homme_mec_react(),'column',ChoixCouleursBar[1:length(unique(repart_age_et_part_homme_mec_react()$statistiques))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=statistiques),"P",
                    
                    HEADER=unique(repart_age_et_part_homme_mec_react()$titre),
                    
                    FormatLabelsData="statistiques",InfosPlus="Catégorie")
  
  
  
})


output$nationalite_mec_chart<-renderHighchart({
  
  highchartFunction(repart_nationalite_mec_reactive(),'pie',ChoixCouleurs, 
                    
                    hcaes(x=as.character(sous_indicateurs),y=lapply(as.numeric(value),function(x){round(x)})),"PPIE",
                    
                    HEADER=unique(repart_nationalite_mec_reactive()$titre),
                    
                    FormatLabelsData="sous_indicateurs")
  
})




output$evol_profil_mec<-renderHighchart({
  
  highchartFunction(profil_mec_evol_reactive(),'line',c("#77B5FE"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"N",
                    
                    HEADER=unique(profil_mec_evol_reactive()$titre),
                    
                    FormatLabelsData="unite_temps")
  
  
  
})




output$contribution_min_maj_mec_etrangers_chart<-renderHighchart({
  
  if(length(unique(Reactive_contribution_min_maj_mec_etrangers()$sous_indicateurs))>2  ){
    
    codesCouleurs<-c("#0078f3","#6a6af4","#cacafb","#fc5d00","#ffded9")
    
  } else {
    
    codesCouleurs<-c("#0078f3","#6a6af4")
    
  }
  
  highchartFunction(Reactive_contribution_min_maj_mec_etrangers(),'bar',codesCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stackingP",EnableLabels=TRUE,
                    
                    HEADER=unique(Reactive_contribution_min_maj_mec_etrangers()$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Pays")
  
  
  
  
  
})  
  
  
# observeEvent(input$Misencause,{
#   
# 
#     output$repart_age_et_part_homme_mec_chart<-renderHighchart({
# 
#       highchartFunction(repart_age_et_part_homme_mec_react(),'column',ChoixCouleursBar[1:length(unique(repart_age_et_part_homme_mec_react()$statistiques))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=statistiques),"P",
#                          
#                          HEADER=unique(repart_age_et_part_homme_mec_react()$titre),
#                          
#                          FormatLabelsData="statistiques",InfosPlus="Catégorie")
#       
#       
#       
#     })
#     
#     
#     output$nationalite_mec_chart<-renderHighchart({
#       
#       highchartFunction(repart_nationalite_mec_reactive(),'pie',ChoixCouleurs, 
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=lapply(as.numeric(value),function(x){round(x)})),"PPIE",
#                          
#                          HEADER=unique(repart_nationalite_mec_reactive()$titre),
#                          
#                          FormatLabelsData="sous_indicateurs")
#       
#     })
#     
#     
#     
#     
#     output$evol_profil_mec<-renderHighchart({
#       
#       highchartFunction(profil_mec_evol_reactive(),'line',c("#77B5FE"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value)),"N",
#                          
#                          HEADER=unique(profil_mec_evol_reactive()$titre),
#                          
#                          FormatLabelsData="unite_temps")
#       
#       
#       
#     })
#     
#     
# 
#     
# output$contribution_min_maj_mec_etrangers_chart<-renderHighchart({
#   
#       if(length(unique(Reactive_contribution_min_maj_mec_etrangers()$sous_indicateurs))>2  ){
#         
#         codesCouleurs<-c("#0078f3","#6a6af4","#cacafb","#fc5d00","#ffded9")
#         
#       } else {
#         
#         codesCouleurs<-c("#0078f3","#6a6af4")
#         
#       }
#       
#       highchartFunction(Reactive_contribution_min_maj_mec_etrangers(),'bar',codesCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stackingP",EnableLabels=TRUE,
#                          
#                          HEADER=unique(Reactive_contribution_min_maj_mec_etrangers()$titre),
#                          
#                          FormatLabelsData="sous_indicateurs",InfosPlus="Pays")
#       
#       
#       
#       
#       
#     })
#     
#     
#     
# 
#        
#     
# },ignoreNULL=FALSE)
  

observeEvent(input$profil_a, {showInfo( repart_age_et_part_homme_mec_react())})
  
observeEvent(input$profil_b, {showInfo(repart_nationalite_mec_reactive())})

observeEvent(input$profil_c, {howInfo(profil_mec_evol_reactive())})

observeEvent(input$profil_d, {showInfo(Reactive_contribution_min_maj_mec_etrangers())})
  
  
  
}