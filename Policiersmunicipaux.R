

Policiersmunicipaux_ui<-PageLayoutFunction("Policiers Municipaux",uiOutput("PoliciersMunicipauxUI"))

  
Policiersmunicipaux_server<-function(input,output,session){
  
  PoliciersMunicipauxData<-eventReactive(input$Policiersmunicipaux,{
    
    load("data/repart_policiers_municipaux.RData")
    
    load("data/Nb_policiers_municipaux.RData")
    
    load("data/Liste_NB_policiers_Municipaux.RData")
    
    return(list(
      
      "repart_policiers_municipaux"=repart_policiers_municipaux,
      
      "Nb_policiers_municipaux"=Nb_policiers_municipaux,
      
      "Liste_NB_policiers_Municipaux"=Liste_NB_policiers_Municipaux
      
    ))
    
},ignoreNULL=FALSE)
  
  
  
  
  NB_policiers_reactive<-reactive({
    
    PoliciersMunicipauxData()$Nb_policiers_municipaux %>% filter(sous_indicateurs==input$NBpoliciersMunicipauxID)
    
  })

  
  
  output$PoliciersMunicipauxUI<-renderUI({
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('policiers_municipaux_a'),highchartOutput('repart_policiers_municipaux')),
        
        BoxForChart(Button('policiers_municipaux_b'),highchartOutput('Nb_policiers_municipaux'),
                    
                    Selector=TRUE,selectInput("NBpoliciersMunicipauxID","",PoliciersMunicipauxData()$Liste_NB_policiers_Municipaux)
                    
        )
        
      ),
      
      
      
      
    )
    
    
  })
  
  
  output$repart_policiers_municipaux<-renderHighchart({
    
    highchartFunction(PoliciersMunicipauxData()$repart_policiers_municipaux,'column',ChoixCouleursBar[1:3],
                      
                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
                      
                      HEADER=unique(PoliciersMunicipauxData()$repart_policiers_municipaux$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
    
    
  })
  
  
  
  output$Nb_policiers_municipaux<-renderHighchart({
    
    highchartFunction(NB_policiers_reactive(),'column',ChoixCouleursBar[1],  
                      
                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value)),"N",
                      
                      HEADER=unique(NB_policiers_reactive()$titre),
                      
                      FormatLabelsData="sous_sous_indicateurs")
    
    
  })  
  
  
    

# observeEvent(input$Policiersmunicipaux,{
#     
#     output$PoliciersMunicipauxUI<-renderUI({
#       
#       tagList(
#         
#         outside_container(
# 
#           BoxForChart(Button('policiers_municipaux_a'),highchartOutput('repart_policiers_municipaux')),
# 
#           BoxForChart(Button('policiers_municipaux_b'),highchartOutput('Nb_policiers_municipaux'),
# 
#                       Selector=TRUE,selectInput("NBpoliciersMunicipauxID","",PoliciersMunicipauxData()$Liste_NB_policiers_Municipaux)
# 
#           )
# 
#         ),
#         
#         
#         
#         
#       )
#       
#       
#     })
#     
#     
#     output$repart_policiers_municipaux<-renderHighchart({
#       
#       highchartFunction(PoliciersMunicipauxData()$repart_policiers_municipaux,'column',ChoixCouleursBar[1:3],
#                          
#                          hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
#                          
#                          HEADER=unique(PoliciersMunicipauxData()$repart_policiers_municipaux$titre),
#                          
#                          FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
#       
#       
#     })
#     
#     
#     
#     output$Nb_policiers_municipaux<-renderHighchart({
#       
#       highchartFunction(NB_policiers_reactive(),'column',ChoixCouleursBar[1],  
#                          
#                          hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value)),"N",
# 
#                          HEADER=unique(NB_policiers_reactive()$titre),
#                          
#                          FormatLabelsData="sous_sous_indicateurs")
#       
#       
#   })
#     
#     
#     
#     
#     
# },ignoreNULL=FALSE)
  
  
  
  
observeEvent(input$policiers_municipaux_a, {showInfo(PoliciersMunicipauxData()$repart_policiers_municipaux)})

observeEvent(input$policiers_municipaux_b, {showInfo(NB_policiers_reactive())}) 
  
  
}