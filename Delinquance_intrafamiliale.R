
Delinquance_intrafamiliale_ui<-PageLayoutFunction("Violences Intrafamiliales",
  tagList(
    uiOutput("DelinquanceIntrafamiliale_UI"),
    br(),
    br()
  )
)



Delinquance_intrafamiliale_server<-function(input,output,session){
  
  DataDelinquanceIntrafamiliale= eventReactive(input$Violencesintrafamiliales,{  
    
    load("data/Nb_victimes_violences_conjugales.RData")
    
    load("data/Nb_mec_violences_conjugales.RData")
    
    load("data/Nb_victimes_violences_intra_non_conjugales.RData")
    
    load("data/Nb_mec_violences_intra_non_conjugales.RData")
    
    load("data/taux_plainte_violences_intrafam.RData")
  
    return(
      
      list(
       
        "Nb_victimes_violences_conjugales"=Nb_victimes_violences_conjugales,
        
        "Nb_mec_violences_conjugales"=Nb_mec_violences_conjugales,
        
        "Nb_victimes_violences_intra_non_conjugales"=Nb_victimes_violences_intra_non_conjugales,
        
        "Nb_mec_violences_intra_non_conjugales"=Nb_mec_violences_intra_non_conjugales,
        
        "taux_plainte_violences_intrafam"=taux_plainte_violences_intrafam
        
      )
    )
},ignoreNULL=FALSE)
 
  
  
output$DelinquanceIntrafamiliale_UI<-renderUI({
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('df_a'),highchartOutput("chart_Nb_victimes_violences_conjugales")),
        BoxForChart(Button('df_c'),highchartOutput("chart_Nb_mec_violences_conjugales"))
        
      ),
      br(),
      outside_container(
        BoxForChart(Button('df_b'),highchartOutput("chart_Nb_victimes_violences_intra_non_conjugales")),
        BoxForChart(Button('df_d'),highchartOutput("chart_Nb_mec_violences_intra_non_conjugales"))
      ),
      
      br(),
      
      outside_container(
        
        SingleValueChart(Button('df_e'),DataDelinquanceIntrafamiliale()$taux_plainte_violences_intrafam,source="enq")
        
      )
      
      
      
    )
    
    
    
    
    
    
  })  
  
  
  
  
  output$chart_Nb_victimes_violences_conjugales<-renderHighchart({
    
    highchartFunction(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales,'column',ChoixCouleursBar[1:length(unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales$sous_indicateurs))],
                      
                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
                      
                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Année")
    
    
  })
  
  
  
  output$chart_Nb_mec_violences_conjugales<-renderHighchart({
    
    highchartFunction(DataDelinquanceIntrafamiliale()$Nb_mec_violences_conjugales,'pie',ChoixCouleurs,
                      
                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
                      
                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_mec_violences_conjugales$titre),
                      
                      FormatLabelsData="sous_indicateurs") 
    
    
  })
  
  
  
  
  output$chart_Nb_victimes_violences_intra_non_conjugales<-renderHighchart({
    
    highchartFunction(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales,'column',
                      
                      ChoixCouleursBar[1:length(unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales$sous_indicateurs))],
                      
                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
                      
                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Année")
    
    
  })
  
  
  
  output$chart_Nb_mec_violences_intra_non_conjugales<-renderHighchart({
    
    highchartFunction(DataDelinquanceIntrafamiliale()$Nb_mec_violences_intra_non_conjugales,'pie',ChoixCouleurs,
                      
                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
                      
                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_mec_violences_intra_non_conjugales$titre),
                      
                      FormatLabelsData="sous_indicateurs") 
    
    
  })  
  
  
   
  
  
# observeEvent(input$Violencesintrafamiliales,{
# 
# output$DelinquanceIntrafamiliale_UI<-renderUI({
# 
#       tagList(
# 
#         outside_container(
#         
#           BoxForChart(Button('df_a'),highchartOutput("chart_Nb_victimes_violences_conjugales")),
#           BoxForChart(Button('df_c'),highchartOutput("chart_Nb_mec_violences_conjugales"))
# 
#         ),
#         br(),
#         outside_container(
#           BoxForChart(Button('df_b'),highchartOutput("chart_Nb_victimes_violences_intra_non_conjugales")),
#           BoxForChart(Button('df_d'),highchartOutput("chart_Nb_mec_violences_intra_non_conjugales"))
#         ),
#         
#         br(),
#         
#         outside_container(
#           
#           SingleValueChart(Button('df_e'),DataDelinquanceIntrafamiliale()$taux_plainte_violences_intrafam,source="enq")
#           
#         )
#         
# 
# 
#       )
#       
#       
#       
#     
#     
#     
#   })  
#   
#     
#  
#     
# output$chart_Nb_victimes_violences_conjugales<-renderHighchart({
#     
#     highchartFunction(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales,'column',ChoixCouleursBar[1:length(unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales$sous_indicateurs))],
#                        
#                        hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
#                        
#                        HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales$titre),
#                        
#                        FormatLabelsData="sous_indicateurs",InfosPlus="Année")
#     
#     
#   })
#   
#   
#   
# output$chart_Nb_mec_violences_conjugales<-renderHighchart({
#     
#   highchartFunction(DataDelinquanceIntrafamiliale()$Nb_mec_violences_conjugales,'pie',ChoixCouleurs,
#                        
#                        hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
#                        
#                        HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_mec_violences_conjugales$titre),
#                        
#                        FormatLabelsData="sous_indicateurs") 
#     
#     
#   })
# 
# 
# 
# 
# output$chart_Nb_victimes_violences_intra_non_conjugales<-renderHighchart({
#   
#   highchartFunction(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales,'column',
#                      
#                      ChoixCouleursBar[1:length(unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales$sous_indicateurs))],
#                      
#                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
#                      
#                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Année")
#   
#   
# })
# 
# 
# 
# output$chart_Nb_mec_violences_intra_non_conjugales<-renderHighchart({
#   
#   highchartFunction(DataDelinquanceIntrafamiliale()$Nb_mec_violences_intra_non_conjugales,'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
#                      
#                      HEADER=unique(DataDelinquanceIntrafamiliale()$Nb_mec_violences_intra_non_conjugales$titre),
#                      
#                      FormatLabelsData="sous_indicateurs") 
#   
#   
# })
# 
#   
#     
# },ignoreNULL=FALSE)
  
  

observeEvent(input$df_a,{showInfo(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_conjugales)})

observeEvent(input$df_b,{showInfo(DataDelinquanceIntrafamiliale()$Nb_victimes_violences_intra_non_conjugales)})

observeEvent(input$df_c,{showInfo(DataDelinquanceIntrafamiliale()$Nb_mec_violences_conjugales)})

observeEvent(input$df_d,{showInfo(DataDelinquanceIntrafamiliale()$Nb_mec_violences_intra_non_conjugales)})

observeEvent(input$df_e,{showInfo(DataDelinquanceIntrafamiliale()$taux_plainte_violences_intrafam )})
  

  
  

  
  
  
  
  
}