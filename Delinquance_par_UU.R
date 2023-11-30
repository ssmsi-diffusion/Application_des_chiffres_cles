

Delinquance_par_UU_ui<-PageLayoutFunction("Délinquance par taille d'unité urbaine",
  
  tagList(
    
    uiOutput("delinquance_par_UU_UI") 
    
  )
  
  
)




Delinquance_par_UU_server<-function(input,output,session){
  
  DelinquanceUUData = eventReactive(input$Delinquancepartailleduniteurbaine,{
    load("data/delinquance_par_uu_taux.RData")
    
    return(list(
      "delinquance_par_uu_taux"=delinquance_par_uu_taux
      
    ))
    
},ignoreNULL=FALSE) 
  
  
  Reactive_delinquance_par_UU_taux<-reactive({
    DelinquanceUUData()$delinquance_par_uu_taux %>% filter(Indicateurs==input$Select_delinquance_par_UU_taux)
  })
  
  
  output$delinquance_par_UU_UI<-renderUI({
    
    tagList(
      
      outside_container(
        BoxForChart(Button('delinquance_uu_a'),highchartOutput('delinquance_par_UU_taux'),Selector=TRUE,FullPage=TRUE,
                    selectInput("Select_delinquance_par_UU_taux","",sort(unique(DelinquanceUUData()$delinquance_par_uu_taux$Indicateurs)),width="60%"))
        
        
      ),
      
      
      
      
      br(),
      br()
    )
  })
  
  output$delinquance_par_UU_taux<-renderHighchart({
    
    
    if(input$Select_delinquance_par_UU_taux=="Homicides"){
      
      DataTable<-Reactive_delinquance_par_UU_taux() %>% filter(Indicateurs=="Homicides")
      
      chart<-highchartFunction(DataTable,'bar',ChoixCouleursBar[1:length(unique(DataTable$sous_sous_indicateurs))],
                               
                               hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"IntegerStackingP",EnableLabels=TRUE,
                               
                               HEADER=unique(DataTable$titre),
                               
                               FormatLabelsData="sous_sous_indicateurs")
      
    } else {
      
      DataTable<-Reactive_delinquance_par_UU_taux() 
      
      chart<-highchartFunction(DataTable,'column',ChoixCouleursBar[1:length(unique(DataTable$sous_sous_indicateurs))],
                               
                               hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"N",
                               
                               HEADER=unique(DataTable$titre),
                               
                               FormatLabelsData="sous_indicateurs")
      
    }
    
    
    
  })
  
  
# observeEvent(input$Delinquancepartailleduniteurbaine,{
#   
#     output$delinquance_par_UU_UI<-renderUI({
#       
#       tagList(
#         
#         outside_container(
#           BoxForChart(Button('delinquance_uu_a'),highchartOutput('delinquance_par_UU_taux'),Selector=TRUE,FullPage=TRUE,
#                       selectInput("Select_delinquance_par_UU_taux","",sort(unique(DelinquanceUUData()$delinquance_par_uu_taux$Indicateurs)),width="60%"))
#           
#           
#         ),
#         
#         
#         
#         
#         br(),
#         br()
#       )
#     })
#     
#     output$delinquance_par_UU_taux<-renderHighchart({
#       
#       
#       if(input$Select_delinquance_par_UU_taux=="Homicides"){
#         
#         DataTable<-Reactive_delinquance_par_UU_taux() %>% filter(Indicateurs=="Homicides")
#         
#         chart<-highchartFunction(DataTable,'bar',ChoixCouleursBar[1:length(unique(DataTable$sous_sous_indicateurs))],
#                           
#                           hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"IntegerStackingP",EnableLabels=TRUE,
#                           
#                           HEADER=unique(DataTable$titre),
#                           
#                           FormatLabelsData="sous_sous_indicateurs")
#         
#       } else {
#         
#         DataTable<-Reactive_delinquance_par_UU_taux() 
# 
#         chart<-highchartFunction(DataTable,'column',ChoixCouleursBar[1:length(unique(DataTable$sous_sous_indicateurs))],
#                           
#                           hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"N",
#                           
#                           HEADER=unique(DataTable$titre),
#                           
#                           FormatLabelsData="sous_indicateurs")
#         
#       }
#       
# 
#       
# })
# 
#     
# },ignoreNULL=FALSE)
  
  
  
  
  observeEvent(input$delinquance_uu_a, {showInfo(Reactive_delinquance_par_UU_taux())})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}