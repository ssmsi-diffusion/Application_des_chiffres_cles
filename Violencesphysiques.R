

violences_physiques_ui<-PageLayoutFunction("Violences physiques",uiOutput("violences_physiques_UI"))


violences_physiques_server<-function(input,output,session){

  ViolencesPhysiques = eventReactive(input$Violencesphysiques,{
    
    load("data/Nb_victime_cbv.RData")
    
    load("data/Nb_mec_cbv.RData")
    
    load("data/repartition_CBV.RData")
    
    load("data/data_taux_plainte_cbv.RData")
    
    
    return(list(
      
      "Nb_victime_cbv"=Nb_victime_cbv,
      
      "Nb_mec_cbv"=Nb_mec_cbv,
      
      "repartition_CBV"=repartition_CBV,
      
      "data_taux_plainte_cbv"=data_taux_plainte_cbv
      
      
      
      
))
    
},ignoreInit=FALSE,ignoreNULL=FALSE) 
  
  
output$violences_physiques_UI<-renderUI({
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('vf_a'),highchartOutput('Nb_victimes_CBV')),
        
        BoxForChart(Button('vf_b'),highchartOutput("Nb_mec_cbv"))
        
      ),
      
      br(),
      
      outside_container(
        
        BoxForChart(Button('vf_c'),highchartOutput("repartition_CBV"),source="enq"),
        
        SingleValueChart(Button('vf_d'),ViolencesPhysiques()$data_taux_plainte_cbv,source="enq")
        
        
        
      )
      
    )
    
    
  })  
  

  
  
observeEvent(input$Violencesphysiques,{

output$Nb_victimes_CBV<-renderHighchart({
      
      highchartFunction(ViolencesPhysiques()$Nb_victime_cbv,'column',ChoixCouleursBar[1:length(unique(ViolencesPhysiques()$Nb_victime_cbv$Indicateurs))],
                         
                         hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value),group=Indicateurs),"stacking",EnableLabels=TRUE,
                         
                         HEADER=unique(ViolencesPhysiques()$Nb_victime_cbv$titre),
                         
                         FormatLabelsData="Indicateurs",InfosPlus="Année")
      
      
    })
    
    
output$Nb_mec_cbv<-renderHighchart({
      
      highchartFunction(ViolencesPhysiques()$Nb_mec_cbv,'pie',ChoixCouleurs,
                         
                         hcaes(x=as.character(unite_temps),y=RoundValuesFunction(value)),"NDonut",
                         
                         HEADER=unique(ViolencesPhysiques()$Nb_mec_cbv$titre),
                         
                         FormatLabelsData="Indicateurs")
      
      
    })
    
    
    
    
output$repartition_CBV<-renderHighchart({
      
  highchartFunction(ViolencesPhysiques()$repartition_CBV,'pie',list("#77B5FE","#FF7F00"),
                     
                     hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                     
                     HEADER=unique(ViolencesPhysiques()$repartition_CBV$titre),
                     
                     FormatLabelsData="Indicateurs")
      
      
})
    
    
    
    
output$taux_plainte_cbv<-renderHighchart({

      highchartFunction(ViolencesPhysiques()$data_taux_plainte_cbv,'pie',list("#77B5FE"),
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"SampleDonut",
                         
                         HEADER=unique(ViolencesPhysiques()$data_taux_plainte_cbv$titre),
                         
                         FormatLabelsData="unite_temps")
      
      
    })
    
    
    
    
    
},ignoreInit=FALSE,ignoreNULL=FALSE)
  
  
  
  
observeEvent(input$vf_a, {showInfo(ViolencesPhysiques()$Nb_victime_cbv)})
  
observeEvent(input$vf_b, {showInfo(ViolencesPhysiques()$Nb_mec_cbv)})
  
observeEvent(input$vf_c, {showInfo(ViolencesPhysiques()$repartition_CBV)})
  
observeEvent(input$vf_d, {showInfo(ViolencesPhysiques()$data_taux_plainte_cbv)})
  

  
}