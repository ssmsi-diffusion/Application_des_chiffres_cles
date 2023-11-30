

atteintes_env_animaux_ui<-PageLayoutFunction("Atteintes à l'environnement et aux animaux",
  tagList(
    radioGroupButtons(inputId="AtteintesNatureID","",choices=c("Atteintes à l'environnement"="atteinteEnv","Atteintes envers les animaux"="atteinteAnimaux"),direction="horizontal",individual=TRUE,status="boutonGroup"),
    uiOutput("atteintesNatureUI"),br(),br()
  )
)



atteintes_env_animaux_server<-function(input,output,session){
  AtteinteEnv<-eventReactive(input$Atteintesalenvironnementetauxanimaux,{
    ## Atteintes à l'environnement
    load("data/Nb_atteinte_env.RData")
    load("data/part_atteinte_env.RData")
    load("data/repart_moyenne_att_env.RData")
    ## Atteintes aux animaux
    load("data/repart_atteinte_animaux.RData")
    load("data/nb_atteinte_animaux.RData")
    load("data/repart_moy_atteinte_animaux.RData")
    load("data/repart_moy_par_type_maltraitance.RData") 
    return(list(
      ## Atteintes à l'environnement
      "Nb_atteinte_env"=Nb_atteinte_env,    
      "part_atteinte_env"=part_atteinte_env,
      "repart_moyenne_att_env"=repart_moyenne_att_env,   
      ## Atteintes aux animaux
      "repart_atteinte_animaux"=repart_atteinte_animaux,
      "nb_atteinte_animaux"=nb_atteinte_animaux,
      "repart_moy_atteinte_animaux"=repart_moy_atteinte_animaux,
      "repart_moy_par_type_maltraitance"=repart_moy_par_type_maltraitance
    ))
  },ignoreNULL=FALSE)
  
  
output$atteintesNatureUI<-renderUI({
    if(input$AtteintesNatureID=="atteinteEnv"){
      tagList(
        outside_container(BoxForChart(Button('nature_a'),highchartOutput('Nb_atteinte')),
                BoxForChart(Button('nature_b'),highchartOutput('part_atteinte'))),
        br(),
        outside_container(
          BoxForChart(Button('nature_c'),highchartOutput('repart_moyenne_att_env'),Selector=FALSE,FullPage=TRUE) 
        ),
      )
    } else if(input$AtteintesNatureID=="atteinteAnimaux"){
      tagList(
        outside_container(
          BoxForChart(Button('nature_d'),highchartOutput('nb_atteinte_animaux')),
          BoxForChart(Button('nature_e'),highchartOutput('repart_atteinte_animaux'))
        ),
        br(),
        outside_container(
          BoxForChart(Button('nature_f'),highchartOutput('repart_moyenne_att_animaux')), 
          BoxForChart(Button('nature_g'),highchartOutput('Chart_repart_moy_par_type_maltraitance'))
        )
      )
    }
    
  })
  
  
  
  
  
  Reactive_repart_moyenne_att_animaux<-reactive({
    
    AtteinteEnv()$repart_moy_atteinte_animaux %>% filter(sous_indicateurs==input$Select_repart_moyenne_att_animaux)
    
  })
  
  
  
  
  
  Reactive_repart_moy_par_type_maltraitance<-reactive({
    
    
    AtteinteEnv()$repart_moy_par_type_maltraitance %>% filter(sous_indicateurs==input$Select_repart_moy_par_type_maltraitance)
    
  })
  
  
  output$Nb_atteinte<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$Nb_atteinte_env,'line',c("#1283f3"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                      
                      HEADER=unique(AtteinteEnv()$Nb_atteinte_env$titre),
                      
                      FormatLabelsData="unite_temps")
    
    
    
  })
  
  
  
  
  output$part_atteinte<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$part_atteinte_env,'bar',c("#0daaf3","#1560BD"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stacking",
                      
                      HEADER=unique(AtteinteEnv()$part_atteinte_env$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Délit")
    
    
    
  })
  
  
  
  
  output$repart_moyenne_att_env<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$repart_moyenne_att_env,'column',c("#1560BD"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
                      
                      HEADER=unique(AtteinteEnv()$repart_moyenne_att_env$titre),
                      
                      FormatLabelsData="unite_temps")
    
    
    
  })
  
  
  
  #### Partie II : Atteinte envers les animaux   
  
  
  output$nb_atteinte_animaux<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$nb_atteinte_animaux,'line',c("#1283f3"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                      
                      HEADER=unique(AtteinteEnv()$nb_atteinte_animaux$titre),
                      
                      FormatLabelsData="unite_temps")
    
    
    
  })    
  
  
  
  output$repart_atteinte_animaux<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$repart_moyenne_att_env,'pie',ChoixCouleurs,
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                      
                      HEADER=unique(AtteinteEnv()$repart_atteinte_animaux$titre),
                      
                      FormatLabelsData="Indicateurs")
    
    
  })
  
  
  
  
  
  output$repart_moyenne_att_animaux<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$repart_moy_atteinte_animaux,'column',c("#FF7F00","#1283f3"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
                      
                      HEADER=unique(AtteinteEnv()$repart_moy_atteinte_animaux$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Espèce victime")
    
    
    
    
    
  })
  
  
  
  
  
  
  output$Chart_repart_moy_par_type_maltraitance<-renderHighchart({
    
    highchartFunction(AtteinteEnv()$repart_moy_par_type_maltraitance,'column',c("#FF7F00","#1283f3"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
                      
                      HEADER=unique(AtteinteEnv()$repart_moy_par_type_maltraitance$titre), 
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Type de maltraitance")
    
    
  })    
  
  
  
  
# observeEvent(input$Atteintesalenvironnementetauxanimaux,{
#     
#     output$Nb_atteinte<-renderHighchart({
# 
#       highchartFunction(AtteinteEnv()$Nb_atteinte_env,'line',c("#1283f3"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                          
#                          HEADER=unique(AtteinteEnv()$Nb_atteinte_env$titre),
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
#     output$part_atteinte<-renderHighchart({
#       
#       highchartFunction(AtteinteEnv()$part_atteinte_env,'bar',c("#0daaf3","#1560BD"),
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stacking",
#                          
#                          HEADER=unique(AtteinteEnv()$part_atteinte_env$titre),
#                          
#                          FormatLabelsData="sous_indicateurs",InfosPlus="Délit")
#       
#       
#       
#     })
#     
#     
#     
#     
#     output$repart_moyenne_att_env<-renderHighchart({
#       
#       highchartFunction(AtteinteEnv()$repart_moyenne_att_env,'column',c("#1560BD"),
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
#                          
#                          HEADER=unique(AtteinteEnv()$repart_moyenne_att_env$titre),
#                          
#                          FormatLabelsData="unite_temps")
#       
#       
#       
#     })
#     
#     
# 
# #### Partie II : Atteinte envers les animaux   
#     
# 
# output$nb_atteinte_animaux<-renderHighchart({
#       
#       highchartFunction(AtteinteEnv()$nb_atteinte_animaux,'line',c("#1283f3"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                          
#                          HEADER=unique(AtteinteEnv()$nb_atteinte_animaux$titre),
#                          
#                          FormatLabelsData="unite_temps")
#       
#       
#       
#     })    
#     
#     
#     
# output$repart_atteinte_animaux<-renderHighchart({
#    
#       highchartFunction(AtteinteEnv()$repart_moyenne_att_env,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                          
#                          HEADER=unique(AtteinteEnv()$repart_atteinte_animaux$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
#       
#   })
#     
#     
#      
#     
#  
# output$repart_moyenne_att_animaux<-renderHighchart({
# 
#   highchartFunction(AtteinteEnv()$repart_moy_atteinte_animaux,'column',c("#FF7F00","#1283f3"),
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
#                      
#                      HEADER=unique(AtteinteEnv()$repart_moy_atteinte_animaux$titre),
#                      
#                     FormatLabelsData="sous_indicateurs",InfosPlus="Espèce victime")
#   
#   
#   
#   
#       
# })
#     
#     
# 
# 
# 
# 
# output$Chart_repart_moy_par_type_maltraitance<-renderHighchart({
#       
#   highchartFunction(AtteinteEnv()$repart_moy_par_type_maltraitance,'column',c("#FF7F00","#1283f3"),
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
#                      
#                      HEADER=unique(AtteinteEnv()$repart_moy_par_type_maltraitance$titre), 
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Type de maltraitance")
#       
#       
# })  
#     
# 
# },ignoreNULL=FALSE)
  
  
  
observeEvent(input$nature_a, {showInfo(AtteinteEnv()$Nb_atteinte_env)})
observeEvent(input$nature_b, {showInfo(AtteinteEnv()$part_atteinte_env)})
observeEvent(input$nature_c, {showInfo(AtteinteEnv()$repart_moyenne_att_env)})
observeEvent(input$nature_d, {showInfo(AtteinteEnv()$nb_atteinte_animaux)})
observeEvent(input$nature_e, {showInfo(AtteinteEnv()$repart_atteinte_animaux)})
observeEvent(input$nature_f, {showInfo(AtteinteEnv()$repart_moy_atteinte_animaux)})
observeEvent(input$nature_g,{showInfo(AtteinteEnv()$repart_moy_par_type_maltraitance)})
  
  
}