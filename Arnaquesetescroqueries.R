

Arnaquesetescroqueries_ui<-PageLayoutFunction("Arnaques et escroqueries",
  tagList(
    radioGroupButtons(inputId="arnaques_escroqueries","",choices=c("Arnaques","Escroqueries bancaires","Escroqueries et autres infractions assimilées"),
                      direction="horizontal",individual=TRUE,status="boutonGroup"), 
    uiOutput("arnaques_escroqueries_UI")
  )
)



Arnaquesetescroqueries_server<-function(input,output,session){
  
  ArnaquesEscroqueriesData = eventReactive(input$Arnaquesetescroqueries,{
    load("data/prop_menages_escroqueries.RData")
    load("data/Nb_arnaques.RData")
    load("data/Nb_arnaques_et_Prop_menages_victimes.RData")
    load("data/Part_arnaques_sur_plus_de_14_ans.RData")
    load("data/Nombre_escroqueries.RData")
    load("data/taux_plainte_arnaques.RData")
    load("data/Escroqueries_bancaires_nb_part_menages.RData")
    load("data/Part_escroqueries_bancaires.RData")
    load("data/taux_plainte_escroqueries_bancaires.RData")
    load("data/Nb_victime_autres_escroqueries.RData")
    load("data/Nb_mec_autres_escroqueries.RData")
    load("data/proportion_arnaque.RData")
  return( 
      list(
        "prop_menages_escroqueries"=prop_menages_escroqueries,
        "Nb_arnaques"=Nb_arnaques,
        "Nb_arnaques_et_Prop_menages_victimes"=Nb_arnaques_et_Prop_menages_victimes,
        "Part_arnaques_sur_plus_de_14_ans"=Part_arnaques_sur_plus_de_14_ans,
        "Nombre_escroqueries"=Nombre_escroqueries,
        "taux_plainte_arnaques"=taux_plainte_arnaques,
        "Escroqueries_bancaires_nb_part_menages"=Escroqueries_bancaires_nb_part_menages,
        "Part_escroqueries_bancaires"=Part_escroqueries_bancaires,
        "taux_plainte_escroqueries_bancaires"=taux_plainte_escroqueries_bancaires,
        "Nb_victime_autres_escroqueries"=Nb_victime_autres_escroqueries,
        "Nb_mec_autres_escroqueries"=Nb_mec_autres_escroqueries,
        "proportion_arnaque"=proportion_arnaque
      )
    )
},ignoreNULL=FALSE) 
  
  
  output$arnaques_escroqueries_UI<-renderUI({
    
    if(input$arnaques_escroqueries=="Arnaques"){
      tagList(
        
        outside_container(
          
          BoxForChart(Button('arnaque_a'),highchartOutput("Nb_et_par_menages_arnaques"),source="enq"),
          
          SingleValueChart(Button('arnaque_b'),ArnaquesEscroqueriesData()$Part_arnaques_sur_plus_de_14_ans,source="enq")
          
          
        ),
        br(),
        
        outside_container(
          
          SingleValueChart(Button('arnaque_c'),ArnaquesEscroqueriesData()$taux_plainte_arnaques,source="enq")
        )
      ) 
      
    } else if(input$arnaques_escroqueries=="Escroqueries bancaires"){
      tagList(
        outside_container(BoxForChart(Button('arnaque_d'),highchartOutput("Escroqueries_bancaires_nb_part_menages_chart"),Selector=FALSE,FullPage=TRUE,source="enq")),
        br(),
        outside_container(
          
          SingleValueChart(Button('arnaque_e'),ArnaquesEscroqueriesData()$Part_escroqueries_bancaires,source="enq"),
          
          SingleValueChart(Button('arnaque_f'),ArnaquesEscroqueriesData()$taux_plainte_escroqueries_bancaires,source="enq")
          
          
        )
      )
      
    } else if(input$arnaques_escroqueries=="Escroqueries et autres infractions assimilées") {
      
      tagList(
        
        outside_container(
          
          BoxForChart(Button('arnaque_g'),highchartOutput("victime_autres_escroqueries")),
          
          SingleValueChart(Button('arnaque_h'),ArnaquesEscroqueriesData()$Nb_mec_autres_escroqueries)
          
        ),
      )
      
    }})
  
  ####### Page I     
  
  output$Nb_et_par_menages_arnaques<-renderHighchart({
    Multiaxe(
      ArnaquesEscroqueriesData()$Nb_arnaques_et_Prop_menages_victimes,
      ArnaquesEscroqueriesData()$Nb_arnaques %>% mutate(value=RoundValuesFunction(value)),
      ArnaquesEscroqueriesData()$proportion_arnaque,
      ArnaquesEscroqueriesData()$Nb_arnaques_et_Prop_menages_victimes$unite_temps,
      c("bar","bar"),
      c("#77B5FE","#FF7F00"))
  })
  
  
  
  ####### Page II  
  
  output$Escroqueries_bancaires_nb_part_menages_chart<-renderHighchart({
    Multiaxe(
      ArnaquesEscroqueriesData()$Escroqueries_bancaires_nb_part_menages,
      ArnaquesEscroqueriesData()$Nombre_escroqueries,
      ArnaquesEscroqueriesData()$prop_menages_escroqueries,
      ArnaquesEscroqueriesData()$Escroqueries_bancaires_nb_part_menages$unite_temps,
      c("column","column"),
      c("#77B5FE","#FF7F00"))
  })
  
  
  
  
  ####### Page III
  output$victime_autres_escroqueries<-renderHighchart({
    
    highchartFunction(ArnaquesEscroqueriesData()$Nb_victime_autres_escroqueries,'line',c("#77B5FE"),
                      
                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"IntegerValues",
                      
                      HEADER=unique(ArnaquesEscroqueriesData()$Nb_victime_autres_escroqueries$titre),
                      
                      FormatLabelsData="unite_temps") 
    
    
  })

  
  
  
#   observeEvent(input$Arnaquesetescroqueries,{
#   
#     output$arnaques_escroqueries_UI<-renderUI({
#       
#       if(input$arnaques_escroqueries=="Arnaques"){
#         tagList(
#           
#           outside_container(
#             
#             BoxForChart(Button('arnaque_a'),highchartOutput("Nb_et_par_menages_arnaques"),source="enq"),
#             
#             SingleValueChart(Button('arnaque_b'),ArnaquesEscroqueriesData()$Part_arnaques_sur_plus_de_14_ans,source="enq")
#             
#             
#             ),
#           br(),
#           
#           outside_container(
# 
#             SingleValueChart(Button('arnaque_c'),ArnaquesEscroqueriesData()$taux_plainte_arnaques,source="enq")
#           )
#         ) 
# 
#       } else if(input$arnaques_escroqueries=="Escroqueries bancaires"){
#         tagList(
#           outside_container(BoxForChart(Button('arnaque_d'),highchartOutput("Escroqueries_bancaires_nb_part_menages_chart"),Selector=FALSE,FullPage=TRUE,source="enq")),
#           br(),
#           outside_container(
# 
#             SingleValueChart(Button('arnaque_e'),ArnaquesEscroqueriesData()$Part_escroqueries_bancaires,source="enq"),
#                   
#             SingleValueChart(Button('arnaque_f'),ArnaquesEscroqueriesData()$taux_plainte_escroqueries_bancaires,source="enq")
#             
#             
#             )
#         )
#         
#       } else if(input$arnaques_escroqueries=="Escroqueries et autres infractions assimilées") {
#         
#         tagList(
#           
#           outside_container(
#             
#             BoxForChart(Button('arnaque_g'),highchartOutput("victime_autres_escroqueries")),
# 
#             SingleValueChart(Button('arnaque_h'),ArnaquesEscroqueriesData()$Nb_mec_autres_escroqueries)
#             
#             ),
#         )
#         
#       }})
#     
#     
# ####### Page I     
#     
# output$Nb_et_par_menages_arnaques<-renderHighchart({
#       Multiaxe(
#       ArnaquesEscroqueriesData()$Nb_arnaques_et_Prop_menages_victimes,
#       ArnaquesEscroqueriesData()$Nb_arnaques %>% mutate(value=RoundValuesFunction(value)),
#       ArnaquesEscroqueriesData()$proportion_arnaque,
#       ArnaquesEscroqueriesData()$Nb_arnaques_et_Prop_menages_victimes$unite_temps,
#       c("bar","bar"),
#       c("#77B5FE","#FF7F00"))
# })
#     
#  
#     
# ####### Page II  
#     
# output$Escroqueries_bancaires_nb_part_menages_chart<-renderHighchart({
#    Multiaxe(
#    ArnaquesEscroqueriesData()$Escroqueries_bancaires_nb_part_menages,
#    ArnaquesEscroqueriesData()$Nombre_escroqueries,
#    ArnaquesEscroqueriesData()$prop_menages_escroqueries,
#    ArnaquesEscroqueriesData()$Escroqueries_bancaires_nb_part_menages$unite_temps,
#    c("column","column"),
#    c("#77B5FE","#FF7F00"))
#   })
# 
# 
# 
# 
# ####### Page III
# output$victime_autres_escroqueries<-renderHighchart({
#   
#   highchartFunction(ArnaquesEscroqueriesData()$Nb_victime_autres_escroqueries,'line',c("#77B5FE"),
#                      
#                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"IntegerValues",
#                      
#                      HEADER=unique(ArnaquesEscroqueriesData()$Nb_victime_autres_escroqueries$titre),
#                      
#                      FormatLabelsData="unite_temps") 
#   
#   
#   })
#     
#     
# 
# 
# },ignoreNULL=FALSE) 
  


observeEvent(input$arnaque_a, {showInfo(ArnaquesEscroqueriesData()$Nb_arnaques_et_Prop_menages_victimes)})
observeEvent(input$arnaque_b, {showInfo(ArnaquesEscroqueriesData()$Part_arnaques_sur_plus_de_14_ans)})
observeEvent(input$arnaque_c, {showInfo(ArnaquesEscroqueriesData()$taux_plainte_arnaques)})
observeEvent(input$arnaque_d, {showInfo(ArnaquesEscroqueriesData()$Escroqueries_bancaires_nb_part_menages)})
observeEvent(input$arnaque_e, {showInfo(ArnaquesEscroqueriesData()$Part_escroqueries_bancaires)})
observeEvent(input$arnaque_f, {showInfo(ArnaquesEscroqueriesData()$taux_plainte_escroqueries_bancaires)})
observeEvent(input$arnaque_g, {showInfo(ArnaquesEscroqueriesData()$Nb_victime_autres_escroqueries)})
observeEvent(input$arnaque_h, {showInfo(ArnaquesEscroqueriesData()$Nb_mec_autres_escroqueries)})

  
  
}