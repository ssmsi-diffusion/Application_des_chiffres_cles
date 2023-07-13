
Destructions_degradataion_ui<-PageLayoutFunction("Destructions et dégradations volontaires",uiOutput("Degradation_UI"))


Destructions_degradataion_server<-function(input,output,session){
  
  DegradationData = eventReactive(input$Destructionsetdegradationsdesbiens,{

    load("data/Nb_degradations.RData")
    
    load("data/mec_degradations.RData")

    return(

      list(

        "Nb_degradations"=Nb_degradations,
        
        "mec_degradations"=mec_degradations



      ))


},ignoreNULL=FALSE)
  
  
output$Degradation_UI<-renderUI({
    
  outside_container(
      
    BoxForChart(Button('degradation_a'),highchartOutput('nb_degradation')),

    SingleValueChart(Button('degradation_b'),DegradationData()$mec_degradations)
      
  )
    
    
  })
  
  

  
  observeEvent(input$Destructionsetdegradationsdesbiens,{
  
    output$nb_degradation<-renderHighchart({

      highchartFunction(DegradationData()$Nb_degradations,'line',ChoixCouleursBar[1:length(unique(DegradationData()$Nb_degradations$sous_indicateurs))],
                         
                         hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
                         
                         HEADER=unique(DegradationData()$Nb_degradations$titre),
                         
                         FormatLabelsData="sous_indicateurs")
      
      
    })
    
    
    
    
    
    
},ignoreNULL=FALSE)


observeEvent(input$degradation_a, {showInfo(DegradationData()$Nb_degradations)})


observeEvent(input$degradation_b, {showInfo(DegradationData()$mec_degradations)})
  
  
}