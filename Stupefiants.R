
stupefiants_ui<-PageLayoutFunction("Infractions à la législation sur les stupéfiants",
  
  tagList(
    outside_container(BoxForChart(Button('ils_a'),highchartOutput("Nb_trafic_stup")),BoxForChart(Button('ils_b'),highchartOutput("ils_part"),source="enq")),
    br()
    
  )
  
  
)


stupefiants_server<-function(input,output,session){
  
  
  Stupefiants = eventReactive(input$Infractionsalalegisationsurlesstupefiants,{  
    
    load("data/Nb_trafic_stup.RData")
    
    load("data/Evol_trafic_stup.RData")
    
    load("data/ils_part.RData")
    
    
    return(list("Nb_trafic_stup" = Nb_trafic_stup,  
                
                "Evol_trafic_stup"=Evol_trafic_stup,
                
                "ils_part"=ils_part
                
                
                ))
    
    
  },ignoreNULL=FALSE) 
  

  
  output$Nb_trafic_stup<-renderHighchart({
    
    highchartFunction(Stupefiants()$Nb_trafic_stup,'line',c("#77B5FE","#FF7F00"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"IntegerValues",
                      
                      HEADER=unique(Stupefiants()$Nb_trafic_stup$titre),
                      
                      FormatLabelsData="Indicateurs",InfosPlus="Année")
    
    
    
  })
  
  
  
  
  
  output$ils_part<-renderHighchart({
    
    highchartFunction(Stupefiants()$ils_part,'bar',c("#77B5FE"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
                      
                      HEADER=unique(Stupefiants()$ils_part$titre),
                      
                      FormatLabelsData="Indicateurs")
    
    
    
  })
  
  

  
  
  
  
# observeEvent(input$Infractionsalalegisationsurlesstupefiants,{
#     
# output$Nb_trafic_stup<-renderHighchart({
#       
#       highchartFunction(Stupefiants()$Nb_trafic_stup,'line',c("#77B5FE","#FF7F00"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"IntegerValues",
#                          
#                          HEADER=unique(Stupefiants()$Nb_trafic_stup$titre),
#                          
#                          FormatLabelsData="Indicateurs",InfosPlus="Année")
#       
#       
#       
#     })
#     
#     
#     
#     
#     
# output$ils_part<-renderHighchart({
#       
#       highchartFunction(Stupefiants()$ils_part,'bar',c("#77B5FE"),
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
#                          
#                          HEADER=unique(Stupefiants()$ils_part$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
#       
#       
#     })
#     
#     
#     
#     
# output$ils_repartition<-renderHighchart({  
# 
#       
#       highchartFunction(Stupefiants()$ils_repartition,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                          
#                          HEADER=unique(Stupefiants()$ils_part$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
#       
#       
#       
# })
#     
#     
# },ignoreInit=FALSE,ignoreNULL=FALSE)  
  
  
observeEvent(input$ils_a, {showInfo(Stupefiants()$Nb_trafic_stup)})

observeEvent(input$ils_b, {showInfo(Stupefiants()$ils_part)})

observeEvent(input$ils_c, {showInfo(Stupefiants()$ils_repartition)})
  

}