

Violencessexuelles_ui<-PageLayoutFunction("Violences sexuelles",uiOutput("violences_Sexuelles_ui"))
  

Violencessexuelles_server<-function(input,output,session){
  
  ViolencesSexuelles = eventReactive(input$Violencessexuelles,{  
    load("data/Nb_violences_sexuelles.RData")
    load("data/mec_VS.RData")
    load("data/repartition_VS.RData")
    load("data/taux_plainte_VS.RData")
    return(list("Nb_violences_sexuelles" = Nb_violences_sexuelles,  
                "mec_VS"=mec_VS, 
                "repartition_VS"=repartition_VS,
                "taux_plainte_VS"=taux_plainte_VS
                
))
    
    
},ignoreInit=FALSE,ignoreNULL=FALSE) 
  

output$violences_Sexuelles_ui<-renderUI({
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('vs_a'),highchartOutput("Nb_victimes_VS")),
        
        BoxForChart(Button('vs_b'),highchartOutput('mec_VS'))
        
      ),
      
      br(),
      
      outside_container(
        
        BoxForChart(Button('vs_c'),highchartOutput("repartition_VS"),source="enq"),
        
        SingleValueChart(Button('vs_d'),ViolencesSexuelles()$taux_plainte_VS,source="enq")
        
        
      ),
      br()
      
    )
    
    
})


output$Nb_victimes_VS<-renderHighchart({
  
  highchartFunction(ViolencesSexuelles()$Nb_violences_sexuelles,'column',ChoixCouleursBar[1:length(unique(ViolencesSexuelles()$Nb_violences_sexuelles$Indicateurs))],
                    
                    hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=Indicateurs),"stacking",EnableLabels=TRUE,
                    
                    HEADER=unique(ViolencesSexuelles()$Nb_violences_sexuelles$titre),
                    
                    FormatLabelsData="Indicateurs",InfosPlus="Année")
  
  
})



output$mec_VS<-renderHighchart({
  
  highchartFunction(ViolencesSexuelles()$mec_VS,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
                    
                    HEADER=unique(ViolencesSexuelles()$mec_VS$titre),
                    
                    FormatLabelsData="Indicateurs",InfosPlus="Année")  
  
  
})


output$repartition_VS<-renderHighchart({
  
  highchartFunction(ViolencesSexuelles()$repartition_VS,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(ViolencesSexuelles()$repartition_VS$titre),
                    
                    FormatLabelsData="Indicateurs") 
  
  
})  
  
  
# observeEvent(input$Violencessexuelles,{
# 
# output$Nb_victimes_VS<-renderHighchart({
#   
#   highchartFunction(ViolencesSexuelles()$Nb_violences_sexuelles,'column',ChoixCouleursBar[1:length(unique(ViolencesSexuelles()$Nb_violences_sexuelles$Indicateurs))],
#                          
#                          hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=Indicateurs),"stacking",EnableLabels=TRUE,
#                          
#                          HEADER=unique(ViolencesSexuelles()$Nb_violences_sexuelles$titre),
# 
#                          FormatLabelsData="Indicateurs",InfosPlus="Année")
#       
#       
#   })
#     
#     
#     
# output$mec_VS<-renderHighchart({
# 
#     highchartFunction(ViolencesSexuelles()$mec_VS,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
#                          
#                          HEADER=unique(ViolencesSexuelles()$mec_VS$titre),
#                          
#                          FormatLabelsData="Indicateurs",InfosPlus="Année")  
#       
#       
#   })
#     
#     
# output$repartition_VS<-renderHighchart({
# 
#       highchartFunction(ViolencesSexuelles()$repartition_VS,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                          
#                          HEADER=unique(ViolencesSexuelles()$repartition_VS$titre),
#                          
#                          FormatLabelsData="Indicateurs") 
#       
#       
# })
#     
# 
# 
# },ignoreInit=FALSE,ignoreNULL=FALSE)
  

  
observeEvent(input$vs_a,{showInfo(ViolencesSexuelles()$Nb_violences_sexuelles)})
  
observeEvent(input$vs_b,{showInfo(ViolencesSexuelles()$mec_VS)})

observeEvent(input$vs_c,{showInfo(ViolencesSexuelles()$repartition_VS)})
  
observeEvent(input$vs_d, {showInfo(ViolencesSexuelles()$taux_plainte_VS)})

  
}