

delinquance_transports_ui<-PageLayoutFunction("Délinquance dans les transports",
  uiOutput("delinquance_transports_page_ui")                               
)


delinquance_transports_server<-function(input,output,session){
  Transports = eventReactive(input$Delinquancedanslestransports,{
    load("data/Evol_delinquanceTransport.RData")
    load("data/DelinquanceTransportsMecSeries.RData")   
    load("data/NombreVictimesTransports.RData")
    load("data/Nb_victime_delinq_transport.RData")
    load("data/part_faits_delinq_transport.RData")
    
    return(list("Evol_delinquanceTransport" = Evol_delinquanceTransport,
                "DelinquanceTransportsMecSeries" = DelinquanceTransportsMecSeries,
                "NombreVictimesTransports" = NombreVictimesTransports, 
                "Nb_victime_delinq_transport" = Nb_victime_delinq_transport,
                "part_faits_delinq_transport" = part_faits_delinq_transport))
    
    
  },ignoreNULL=FALSE)  
  

  output$delinquance_transports_page_ui<-renderUI({
    tagList(
      outside_container(BoxForChart(Button('transport_a'),highchartOutput('evolutionVictimes')),
                        
                        BoxForChart(Button('transport_b'),highchartOutput('nombre_part_victimes'))),
      br(),
      outside_container(BoxForChart(Button('transport_c'),highchartOutput('DelinquanceTransportsMEC'),Selector=FALSE,FullPage=TRUE))
      
    )})
  
  output$evolutionVictimes<-renderHighchart({
    
    highchartFunction(Transports()$Evol_delinquanceTransport,'line',c("#77B5FE", "#FF7F00","#1560BD","#000000"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"N",
                      
                      HEADER=unique(Transports()$Evol_delinquanceTransport$titre),
                      
                      FormatLabelsData="Indicateurs",InfosPlus="Année")
    
    
    
    
  })
  
  output$nombre_part_victimes<-renderHighchart({
    
    Multiaxe(
      Transports()$NombreVictimesTransports,
      Transports()$Nb_victime_delinq_transport %>% mutate(value=RoundValuesFunction(value)),
      Transports()$part_faits_delinq_transport,
      Transports()$NombreVictimesTransports$Indicateurs,
      c("column","column"),
      c("#3497db","#FF7F00"))
    
  })
  
  
  output$DelinquanceTransportsMEC<-renderHighchart({
    
    highchartFunction(Transports()$DelinquanceTransportsMecSeries,'pie',ChoixCouleurs,  
                      
                      hcaes(x=as.character(Indicateurs),y=RoundValuesFunction(value)),"NDonut",
                      
                      HEADER=unique(Transports()$DelinquanceTransportsMecSeries$titre), 
                      
                      FormatLabelsData="Indicateurs")
    
    
    
  })   
  
  
  
  
  
  
  
  
    
  
# observeEvent(input$Delinquancedanslestransports,{
# output$delinquance_transports_page_ui<-renderUI({
#       tagList(
#           outside_container(BoxForChart(Button('transport_a'),highchartOutput('evolutionVictimes')),
#                             
#                             BoxForChart(Button('transport_b'),highchartOutput('nombre_part_victimes'))),
#           br(),
#           outside_container(BoxForChart(Button('transport_c'),highchartOutput('DelinquanceTransportsMEC'),Selector=FALSE,FullPage=TRUE))
#           
#     )})
#     
# output$evolutionVictimes<-renderHighchart({
#   
#   highchartFunction(Transports()$Evol_delinquanceTransport,'line',c("#77B5FE", "#FF7F00","#1560BD","#000000"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"N",
#                      
#                      HEADER=unique(Transports()$Evol_delinquanceTransport$titre),
#                      
#                      FormatLabelsData="Indicateurs",InfosPlus="Année")
#   
#   
#   
# 
# })
#     
#     output$nombre_part_victimes<-renderHighchart({
#       
#       Multiaxe(
#         Transports()$NombreVictimesTransports,
#         Transports()$Nb_victime_delinq_transport %>% mutate(value=RoundValuesFunction(value)),
#         Transports()$part_faits_delinq_transport,
#         Transports()$NombreVictimesTransports$Indicateurs,
#         c("column","column"),
#         c("#3497db","#FF7F00"))
# 
#     })
#     
#     
# output$DelinquanceTransportsMEC<-renderHighchart({
#   
#   highchartFunction(Transports()$DelinquanceTransportsMecSeries,'pie',ChoixCouleurs,  
#                      
#                      hcaes(x=as.character(Indicateurs),y=RoundValuesFunction(value)),"NDonut",
#                      
#                      HEADER=unique(Transports()$DelinquanceTransportsMecSeries$titre), 
#                      
#                      FormatLabelsData="Indicateurs")
#   
#   
#   
#     }) 
# 
#   },ignoreNULL=FALSE)
  
  
observeEvent(input$transport_a, {showInfo(Transports()$Evol_delinquanceTransport)})
observeEvent(input$transport_b, {showInfo(Transports()$NombreVictimesTransports)})
observeEvent(input$transport_c, {showInfo(Transports()$DelinquanceTransportsMecSeries)})
 
  
}



















