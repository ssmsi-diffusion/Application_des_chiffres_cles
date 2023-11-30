
Relations_population_ui<-PageLayoutFunction("Relations population et forces de sécurité",uiOutput("opinion_force_securite_page_ui"))


Relations_population_server<-function(input,output,session){
  
  RelationPopulationData = eventReactive(input$Relationspopulationetforcesdesecurite,{
    
    load("data/RelationPopulation.RData")
    
    return(list("RelationPopulation"=RelationPopulation))
    
    
  },ignoreNULL=FALSE)
  
  
  output$opinion_force_securite_page_ui<-renderUI({
    
    tagList(
      outside_container(BoxForChart(Button('opinions_a'),highchartOutput('satisfactionPopulation'),Selector=FALSE,FullPage=TRUE,source="enq")),
    )
    
  })  
  
  
  output$satisfactionPopulation<-renderHighchart({
    
    highchartFunction(RelationPopulationData()$RelationPopulation,'column',c("#77B5FE", "#FF7F00","#1560BD"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"P",
                      
                      HEADER=unique(RelationPopulationData()$RelationPopulation$titre),
                      
                      FormatLabelsData="Indicateurs",InfosPlus="Année")
    
    
  })
  
  
#   observeEvent(input$Relationspopulationetforcesdesecurite,{
#     
#     output$opinion_force_securite_page_ui<-renderUI({
#       
#       tagList(
#           outside_container(BoxForChart(Button('opinions_a'),highchartOutput('satisfactionPopulation'),Selector=FALSE,FullPage=TRUE,source="enq")),
#       )
#       
#     })  
#     
#     
# output$satisfactionPopulation<-renderHighchart({
# 
#       highchartFunction(RelationPopulationData()$RelationPopulation,'column',c("#77B5FE", "#FF7F00","#1560BD"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"P",
#                          
#                          HEADER=unique(RelationPopulationData()$RelationPopulation$titre),
#                          
#                          FormatLabelsData="Indicateurs",InfosPlus="Année")
#       
#       
# })
#     
#     
# },ignoreNULL=FALSE)
  
  
  
  
observeEvent(input$opinions_a, {showInfo(RelationPopulationData()$RelationPopulation)})
  
  
}