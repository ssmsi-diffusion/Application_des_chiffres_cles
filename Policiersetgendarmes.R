

Policiersetgendarmes_ui<-PageLayoutFunction("Policiers et Gendarmes",uiOutput("PoliciersGendarmesUI"))


Policiersetgendarmes_server<-function(input,output,session){
  
  PoliciersGendarmesData<-eventReactive(input$Policiersetgendarmes,{  
    
    load("data/repart_policiers_gendarmes.RData")
    
    load("data/repart_age_sexe_policiers_gendarmes.RData")
    
    return(list(
      
      "repart_policiers_gendarmes"=repart_policiers_gendarmes,
      
      "repart_age_sexe_policiers_gendarmes"=repart_age_sexe_policiers_gendarmes
      
    ))
    
},ignoreNULL=FALSE)
  
  
  output$PoliciersGendarmesUI<-renderUI({
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('policiers_gendarmes_a'),highchartOutput('rapart_cat_policiers_gendarmes')),
        
        BoxForChart(Button('policiers_gendarmes_b'),highchartOutput('age_sexe_policiers_gendarmes'))
        
      ),
      
    )
    
    
  })
  
  
  
  
  output$rapart_cat_policiers_gendarmes<-renderHighchart({
    
    highchartFunction(PoliciersGendarmesData()$repart_policiers_gendarmes,'pie',ChoixCouleurs,
                      
                      hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"PPIE",
                      
                      HEADER=unique(PoliciersGendarmesData()$repart_policiers_gendarmes$titre),
                      
                      FormatLabelsData="sous_indicateurs")
    
    
    
    
  })
  
  
  output$age_sexe_policiers_gendarmes<-renderHighchart({
    
    highchartFunction(PoliciersGendarmesData()$repart_age_sexe_policiers_gendarmes,'column',ChoixCouleursBar[1:3],
                      
                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
                      
                      HEADER=unique(PoliciersGendarmesData()$repart_age_sexe_policiers_gendarmes$titre),
                      
                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
    
    
  }) 
  
  
  
   
  
# observeEvent(input$Policiersetgendarmes,{
#     
#     output$PoliciersGendarmesUI<-renderUI({
# 
#       tagList(
# 
#         outside_container(
# 
#           BoxForChart(Button('policiers_gendarmes_a'),highchartOutput('rapart_cat_policiers_gendarmes')),
# 
#           BoxForChart(Button('policiers_gendarmes_b'),highchartOutput('age_sexe_policiers_gendarmes'))
# 
#         ),
# 
#       )
# 
# 
#   })
#     
#   
#    
#     
# output$rapart_cat_policiers_gendarmes<-renderHighchart({
# 
#   highchartFunction(PoliciersGendarmesData()$repart_policiers_gendarmes,'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(PoliciersGendarmesData()$repart_policiers_gendarmes$titre),
#                      
#                      FormatLabelsData="sous_indicateurs")
#   
#   
#       
#       
# })
#     
#     
# output$age_sexe_policiers_gendarmes<-renderHighchart({
# 
#   highchartFunction(PoliciersGendarmesData()$repart_age_sexe_policiers_gendarmes,'column',ChoixCouleursBar[1:3],
#                      
#                      hcaes(x=as.character(sous_sous_indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
#                      
#                      HEADER=unique(PoliciersGendarmesData()$repart_age_sexe_policiers_gendarmes$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie")
#       
#       
# })
#     
#     
#     
# },ignoreNULL=FALSE)
  
  
  
observeEvent(input$policiers_gendarmes_a,{showInfo(PoliciersGendarmesData()$repart_policiers_gendarmes)})
  
observeEvent(input$policiers_gendarmes_b,{showInfo(PoliciersGendarmesData()$repart_age_sexe_policiers_gendarmes)})
  
  
  
  
}