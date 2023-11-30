

Cambriolages_ui<-PageLayoutFunction("Cambriolages",
  tagList(
    uiOutput("cambriolage_ui"),br(),br()
  )
)




Cambriolages_server<-function(input,output,session){
cambriolagesData<-eventReactive(input$Cambriolages,{
    load("data/Nb_cambriolages_logements.RData") 
    load("data/Menages_victimes_cambriolages.RData")
    load("data/mec_cambriolages_logements.RData")
    load("data/Prop_menages_cambriolages.RData")
    load("data/repartition_cambriolages.RData")
    load("data/taux_plainte_cambriolages.RData")
    load("data/pro_et_nb_menages_cambriolages.RData")
    load("data/Nb_menages_camb.RData")
    load("data/prop_menages_camb.RData")

    return(
      list(
        "Nb_cambriolages_logements"=Nb_cambriolages_logements,
        "Menages_victimes_cambriolages"=Menages_victimes_cambriolages,
        "mec_cambriolages_logements"=mec_cambriolages_logements,
        "Prop_menages_cambriolages"=Prop_menages_cambriolages,
        "repartition_cambriolages"=repartition_cambriolages,
        "taux_plainte_cambriolages"=taux_plainte_cambriolages,
        "pro_et_nb_menages_cambriolages"=pro_et_nb_menages_cambriolages,
        "Nb_menages_camb"=Nb_menages_camb,
        "prop_menages_camb"=prop_menages_camb
      )
    )
},ignoreNULL=FALSE)
  
  
output$cambriolage_ui<-renderUI({
  tagList(
    
    outside_container(
      
      BoxForChart(Button('cambriolage_a'),highchartOutput("Nb_cambriolages_logements")),  
      
      SingleValueChart(Button('cambriolage_b'),cambriolagesData()$mec_cambriolages_logements)
      
      
    ),
    
    
    
    outside_container(
      
      BoxForChart(Button('cambriolage_c'),highchartOutput("repartition_cambriolages"),source="enq"),
      
      BoxForChart(Button('cambriolage_d'),highchartOutput("menages_cambriolage"),source="enq")
      
      
      
    ),
    
    outside_container(
      
      SingleValueChart(Button('cambriolage_e'),cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[1]),source="enq"),
      
      SingleValueChart(Button('cambriolage_f'),cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[2]),source="enq")
      
      
    ))
})


output$Nb_cambriolages_logements<-renderHighchart({
  
  highchartFunction(cambriolagesData()$Nb_cambriolages_logements,'line',c("#3497db"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                    
                    HEADER=unique(cambriolagesData()$Nb_cambriolages_logements$titre), 
                    
                    FormatLabelsData="unite_temps")
  
})





output$repartition_cambriolages<-renderHighchart({
  
  highchartFunction(cambriolagesData()$repartition_cambriolages,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(cambriolagesData()$repartition_cambriolages$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
  
  
  
})



output$menages_cambriolage<-renderHighchart({
  Multiaxe(
    cambriolagesData()$pro_et_nb_menages_cambriolages,
    cambriolagesData()$Nb_menages_camb,
    cambriolagesData()$prop_menages_camb,
    cambriolagesData()$pro_et_nb_menages_cambriolages$unite_temps,
    c("column","line"),
    c("#3497db","#FF7F00"))
  
})  
  




# observeEvent(input$Cambriolages,{
#   
#     output$cambriolage_ui<-renderUI({
#       tagList(
#         
#         outside_container(
# 
#           BoxForChart(Button('cambriolage_a'),highchartOutput("Nb_cambriolages_logements")),  
# 
#           SingleValueChart(Button('cambriolage_b'),cambriolagesData()$mec_cambriolages_logements)
#           
#           
#           ),
#         
#         
#         
#         outside_container(
# 
#           BoxForChart(Button('cambriolage_c'),highchartOutput("repartition_cambriolages"),source="enq"),
#           
#           BoxForChart(Button('cambriolage_d'),highchartOutput("menages_cambriolage"),source="enq")
#           
#           
#           
#           ),
#         
#         outside_container(
# 
#         SingleValueChart(Button('cambriolage_e'),cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[1]),source="enq"),
#         
#         SingleValueChart(Button('cambriolage_f'),cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[2]),source="enq")
#         
#           
#         ))
#  })
#     
#     
# output$Nb_cambriolages_logements<-renderHighchart({
#       
#   highchartFunction(cambriolagesData()$Nb_cambriolages_logements,'line',c("#3497db"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                      
#                      HEADER=unique(cambriolagesData()$Nb_cambriolages_logements$titre), 
#                      
#                      FormatLabelsData="unite_temps")
# 
# })
#     
# 
# 
#     
#     
# output$repartition_cambriolages<-renderHighchart({
# 
#   highchartFunction(cambriolagesData()$repartition_cambriolages,'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(cambriolagesData()$repartition_cambriolages$titre),
#                      
#                     FormatLabelsData="Indicateurs")
#   
#   
#   
#   
#   
#     })
#     
#     
# 
# output$menages_cambriolage<-renderHighchart({
#       Multiaxe(
#         cambriolagesData()$pro_et_nb_menages_cambriolages,
#         cambriolagesData()$Nb_menages_camb,
#         cambriolagesData()$prop_menages_camb,
#         cambriolagesData()$pro_et_nb_menages_cambriolages$unite_temps,
#         c("column","line"),
#         c("#3497db","#FF7F00"))
#   
#   })
#     
# 
# 
# 
# 
# 
# 
# },ignoreNULL=FALSE)
  
  

observeEvent(input$cambriolage_a, {showInfo(cambriolagesData()$Nb_cambriolages_logements)})
observeEvent(input$cambriolage_b, {showInfo(cambriolagesData()$mec_cambriolages_logements)})
observeEvent(input$cambriolage_c, {showInfo(cambriolagesData()$repartition_cambriolages)})
observeEvent(input$cambriolage_d, {showInfo(cambriolagesData()$pro_et_nb_menages_cambriolages)})
observeEvent(input$cambriolage_e, {showInfo(cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[1]))})
observeEvent(input$cambriolage_f, {showInfo(cambriolagesData()$taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(cambriolagesData()$taux_plainte_cambriolages$Indicateurs))[2]) )})
  
  
  
  
}