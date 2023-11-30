
ListeValueDiscriminations<-c("Atteintes anti LGBT+","Outrages sexistes","Atteintes à caractère raciste et antireligieux")

ListeLabelsDiscriminations<-c("LGBT","outrages","racistes")




dicrimination_crimes_haine_ui<-PageLayoutFunction("Discriminations et crimes de haine",
                                    
    tagList(

      radioGroupButtons(inputId="discriminationsID","",choices=setNames(ListeLabelsDiscriminations,ListeValueDiscriminations),direction="horizontal",individual=TRUE,status="boutonGroup"),
      
      uiOutput("discriminationsUi")
      
      
    )                                                              
  
  
  
)



dicrimination_crimes_haine_server<-function(input,output,session){
  
  DiscriminationsData = eventReactive(input$Discriminationsetcrimesdehaine,{

    ### LGBT
    
    load("data/Nb_atteintes_LGBT.RData")  

    load("data/crimes_delits_contraventions_LGBT.RData")

    #### Outrages

    load("data/Nb_outrages_sexistes.RData")

    ##### Atteintes à caractères racistes

    load("data/Nb_atteintes_racistes.RData")

    load("data/repart_atteintes.RData")


    return(

      list(

        ### LGBT
        
        "Nb_atteintes_LGBT" = Nb_atteintes_LGBT,

        "crimes_delits_contraventions_LGBT" = crimes_delits_contraventions_LGBT,  

        #### Outrages

        "Nb_outrages_sexistes"=Nb_outrages_sexistes,

        #### Atteintes à caractères racistes

        "Nb_atteintes_racistes"=Nb_atteintes_racistes,

        "repart_atteintes"=repart_atteintes

      ))


},ignoreNULL=FALSE)
  
  
  


Reactive_Crimes_Delits_Contraventions_LGBT<-reactive({
    
    DiscriminationsData()$crimes_delits_contraventions_LGBT %>% filter(statistiques==input$LGBT_crimes_delits_contraventions)
    
}) 
  



output$discriminationsUi<-renderUI({
  
  if(input$discriminationsID=="LGBT"){
    
    tagList(
      
      outside_container(
        
        
        BoxForChart(Button('LGBT_a'),highchartOutput("Nb_atteintes_LGBT")),
        
        BoxForChart(Button('LGBT_b'),highchartOutput('crimes_delits_contraventions_LGBT'),
                    
                    Selector=TRUE,selectInput("LGBT_crimes_delits_contraventions","",sort(unique(DiscriminationsData()$crimes_delits_contraventions_LGBT$statistiques)) ))),
      
      br()
      
    )
    
  } else if (input$discriminationsID=="outrages") {
    
    tagList(
      
      outside_container(BoxForChart(Button('outrages_a'),highchartOutput('Nb_infractions'),Selector=FALSE,FullPage=TRUE)),
      
      br()
      
    )
    
    
  } else if (input$discriminationsID=="racistes"){
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('caractere_raciste_a'),highchartOutput('Nb_atteintes_racistes')),
        
        BoxForChart(Button('caractere_raciste_b'),highchartOutput('repart_ar_type'))),
      
      br()
      
    )
    
    
  }
  
  
})




### LGBT  

output$Nb_atteintes_LGBT<-renderHighchart({
  
  highchartFunction(DiscriminationsData()$Nb_atteintes_LGBT,'column',c("#FF7F00","#1283f3"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"stacking",
                    
                    HEADER=unique(DiscriminationsData()$Nb_atteintes_LGBT$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Année")
  
  
  
})


output$crimes_delits_contraventions_LGBT<-renderHighchart({
  
  highchartFunction(Reactive_Crimes_Delits_Contraventions_LGBT(),'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(Reactive_Crimes_Delits_Contraventions_LGBT()$titre),  
                    
                    FormatLabelsData="sous_indicateurs")
  
})


###  Outrages sexistes  

output$Nb_infractions<-renderHighchart({
  
  highchartFunction(DiscriminationsData()$Nb_outrages_sexistes,'line',c("#FF7F00"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                    
                    HEADER=unique(DiscriminationsData()$Nb_outrages_sexistes$titre), 
                    
                    FormatLabelsData="unite_temps")
  
  
})


#### Atteintes à caractères racistes  

output$Nb_atteintes_racistes<-renderHighchart({
  
  highchartFunction(DiscriminationsData()$Nb_atteintes_racistes,'column',c("#FF7F00","#1283f3"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"stacking",
                    
                    HEADER=unique(DiscriminationsData()$Nb_atteintes_racistes$titre), 
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Année")
  
  
})



output$repart_ar_type<-renderHighchart({
  
  highchartFunction(DiscriminationsData()$repart_atteintes,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(DiscriminationsData()$repart_atteintes$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
})





  
# observeEvent(input$Discriminationsetcrimesdehaine,{
#     
#     output$discriminationsUi<-renderUI({
#       
#       if(input$discriminationsID=="LGBT"){
#         
#         tagList(
#           
#           outside_container(
#             
#             
#             BoxForChart(Button('LGBT_a'),highchartOutput("Nb_atteintes_LGBT")),
#                   
#             BoxForChart(Button('LGBT_b'),highchartOutput('crimes_delits_contraventions_LGBT'),
#                          
#                          Selector=TRUE,selectInput("LGBT_crimes_delits_contraventions","",sort(unique(DiscriminationsData()$crimes_delits_contraventions_LGBT$statistiques)) ))),
#           
#           br()
#           
#         )
#         
#       } else if (input$discriminationsID=="outrages") {
#         
#         tagList(
#           
#           outside_container(BoxForChart(Button('outrages_a'),highchartOutput('Nb_infractions'),Selector=FALSE,FullPage=TRUE)),
#           
#           br()
#           
#         )
#         
#         
#       } else if (input$discriminationsID=="racistes"){
#         
#         tagList(
# 
#           outside_container(
#             
#             BoxForChart(Button('caractere_raciste_a'),highchartOutput('Nb_atteintes_racistes')),
#             
#             BoxForChart(Button('caractere_raciste_b'),highchartOutput('repart_ar_type'))),
#           
#           br()
#           
#         )
#         
#         
#       }
#       
#       
#     })
#     
#     
#     
#     
# ### LGBT  
#     
# output$Nb_atteintes_LGBT<-renderHighchart({
# 
#   highchartFunction(DiscriminationsData()$Nb_atteintes_LGBT,'column',c("#FF7F00","#1283f3"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"stacking",
#                      
#                      HEADER=unique(DiscriminationsData()$Nb_atteintes_LGBT$titre),
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Année")
#   
#   
#       
# })
#     
#     
# output$crimes_delits_contraventions_LGBT<-renderHighchart({
# 
#   highchartFunction(Reactive_Crimes_Delits_Contraventions_LGBT(),'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(Reactive_Crimes_Delits_Contraventions_LGBT()$titre),  
#                      
#                      FormatLabelsData="sous_indicateurs")
# 
# })
#     
#     
# ###  Outrages sexistes  
#     
# output$Nb_infractions<-renderHighchart({
#       
#   highchartFunction(DiscriminationsData()$Nb_outrages_sexistes,'line',c("#FF7F00"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                      
#                      HEADER=unique(DiscriminationsData()$Nb_outrages_sexistes$titre), 
#                      
#                      FormatLabelsData="unite_temps")
#   
# 
# })
#     
#     
# #### Atteintes à caractères racistes  
#     
# output$Nb_atteintes_racistes<-renderHighchart({
# 
#   highchartFunction(DiscriminationsData()$Nb_atteintes_racistes,'column',c("#FF7F00","#1283f3"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"stacking",
#                      
#                      HEADER=unique(DiscriminationsData()$Nb_atteintes_racistes$titre), 
#                      
#                      FormatLabelsData="sous_indicateurs",InfosPlus="Année")
#   
#       
#   })
#     
#     
#     
# output$repart_ar_type<-renderHighchart({
#       
#   highchartFunction(DiscriminationsData()$repart_atteintes,'pie',ChoixCouleurs,
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(DiscriminationsData()$repart_atteintes$titre),
#                      
#                      FormatLabelsData="Indicateurs")
#   
#       
#   })
#     
#     
# },ignoreNULL=FALSE)
  
  
  
  
  
  
#### LGBT
  
observeEvent(input$LGBT_a, {showInfo(DiscriminationsData()$Nb_atteintes_LGBT)})

observeEvent(input$LGBT_b, {showInfo(Reactive_Crimes_Delits_Contraventions_LGBT())})
  
#### Outrages
  
observeEvent(input$outrages_a, {showInfo(DiscriminationsData()$Nb_outrages_sexistes)})
  
#### Atteintes à caractère racistes
  
observeEvent(input$caractere_raciste_a, {showInfo(DiscriminationsData()$Nb_atteintes_racistes)})
  
observeEvent(input$caractere_raciste_b, {showInfo(DiscriminationsData()$repart_atteintes)})
  

}










