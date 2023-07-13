

elucidation_ui<-PageLayoutFunction("Elucidation des faits de délinquance",uiOutput("taux_elucidation_UI"))


elucidation_server<-function(input,output,session){
  
  elucidation<-eventReactive(input$Elucidationdesfaitsdedelinquance,{
    
    load("data/tx_elucidation.RData")
    
    return(list("tx_elucidation"=tx_elucidation
                
                
    ))
    
},ignoreNULL=FALSE)
  
  

  

  

Reactive_tx_elucidation<-reactive({
  
  elucidation()$tx_elucidation %>% filter(Indicateurs==input$tx_elucidation_select)
  
})
  

 
output$taux_elucidation_UI<-renderUI({
  
  tagList(
    
    outside_container(
      
      BoxForChart(Button('elucidation_a'),highchartOutput('tx_elucidation'),Selector=TRUE,FullPage=TRUE,
                  
                  
                  selectInput("tx_elucidation_select","",choices=unique(elucidation()$tx_elucidation$Indicateurs),width="50%")
                  
                  
      )
      
      
    ),
    
    br()
    
  )
  
  
  
})




observeEvent(input$Elucidationdesfaitsdedelinquance,{

output$tx_elucidation<-renderHighchart({

      highchartFunction(Reactive_tx_elucidation(),'bar',c("#6b93f6"),
                         
                         hcaes(x=as.character(unite_temps),y=as.numeric(value)),"P",
                         
                         HEADER=unique(Reactive_tx_elucidation()$titre),
                         
                         FormatLabelsData="unite_temps")
      

    })
    
},ignoreNULL=FALSE)
  
  
  
  
observeEvent(input$elucidation_a, {showInfo( Reactive_tx_elucidation() )})
  
  
}