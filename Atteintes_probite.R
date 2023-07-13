

atteintes_probite_ui<-PageLayoutFunction("Atteintes à la probité",
  tagList(
    uiOutput("atteinte_probite_UI")
  )
)



atteintes_probite_server<-function(input,output,session){
  
  probiteData = eventReactive(input$Atteintesalaprobite,{
    load("data/Nb_probite.RData")
    load("data/repartition_probite.RData")
    return(
      list(
        "Nb_probite"=Nb_probite,
        "repartition_probite"=repartition_probite
      ))
  },ignoreNULL=FALSE)
  

  output$atteinte_probite_UI<-renderUI({
    tagList(
      outside_container(
        BoxForChart(Button('probite_a'),highchartOutput('nb_probite')),
        BoxForChart(Button('probite_b'),highchartOutput('repartition_probite'))
      )       
    )
  })
  
  output$nb_probite<-renderHighchart({  
    
    highchartFunction(probiteData()$Nb_probite,'line',c("#1283f3"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                      
                      HEADER=unique(probiteData()$Nb_probite$titre),
                      
                      FormatLabelsData="unite_temps")
    
    
    
    
  })
  
  
  output$repartition_probite<-renderHighchart({
    
    highchartFunction(probiteData()$repartition_probite,'pie',ChoixCouleurs,
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PDonut",
                      
                      HEADER=unique(probiteData()$repartition_probite$titre),
                      
                      FormatLabelsData="Indicateurs")
    
    
    
    
  })
  
  
  
  # observeEvent(input$Atteintesalaprobite,{
  #   
  #   output$atteinte_probite_UI<-renderUI({
  #     tagList(
  #       outside_container(
  #         BoxForChart(Button('probite_a'),highchartOutput('nb_probite')),
  #         BoxForChart(Button('probite_b'),highchartOutput('repartition_probite'))
  #       )       
  #     )
  #   })
  #   
  #   output$nb_probite<-renderHighchart({  
  #     
  #     highchartFunction(probiteData()$Nb_probite,'line',c("#1283f3"),
  #                       
  #                       hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
  #                       
  #                       HEADER=unique(probiteData()$Nb_probite$titre),
  #                       
  #                       FormatLabelsData="unite_temps")
  #     
  #     
  #     
  #     
  #   })
  #   
  #   
  #   output$repartition_probite<-renderHighchart({
  #     
  #     highchartFunction(probiteData()$repartition_probite,'pie',ChoixCouleurs,
  #                       
  #                       hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PDonut",
  #                       
  #                       HEADER=unique(probiteData()$repartition_probite$titre),
  #                       
  #                       FormatLabelsData="Indicateurs")
  #     
  #     
  #     
  #     
  #   })
  #   
  #   
  # },ignoreNULL=FALSE)
  

observeEvent(input$probite_a, {showInfo(probiteData()$Nb_probite)})
observeEvent(input$probite_b, {showInfo(probiteData()$repartition_probite)})

  
}