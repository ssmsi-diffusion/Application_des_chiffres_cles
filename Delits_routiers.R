


delits_routiers_ui<-PageLayoutFunction("Délits routiers",
  
  uiOutput("delits_routiers_page_ui")
  
)


delits_routiers_server<-function(input,output,session){
  
  DelitsRoutiers = eventReactive(input$Delitsroutiers,{
    
    load("data/Nb_DelitsRoutiers.RData")
    
    return(list("Nb_DelitsRoutiers"=Nb_DelitsRoutiers))
    
    
  },ignoreNULL=FALSE)
  
  
  observeEvent(input$Delitsroutiers,{
    
    output$delits_routiers_page_ui<-renderUI({
      
      div(class='delits-routiers',
          
          tagList(
            
            outside_container(BoxForChart(Button('dr_a'),highchartOutput('Nb_delitsRoutiers',width = "100%",height = "450px"),Selector=FALSE,FullPage=TRUE),
                    
            )
            
          )
          
      )
      
    })
    
    
    output$Nb_delitsRoutiers<-renderHighchart({

      highchartFunction(DelitsRoutiers()$Nb_DelitsRoutiers,'column',c("#77B5FE"),

                         hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"N",
                         
                         HEADER=unique(DelitsRoutiers()$Nb_DelitsRoutiers$titre),
                         
                         FormatLabelsData="Indicateurs")
      
      
      
  })
    
    
   
    
},ignoreNULL=FALSE)
  
  
  
  
observeEvent(input$dr_a, {showInfo(DelitsRoutiers()$Nb_DelitsRoutiers)})
  

}