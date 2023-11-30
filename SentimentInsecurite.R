
insecurite_ui <- PageLayoutFunction("Sentiment d'insécurité",uiOutput("sentiment_insecurite_UI"))


insecurite_server <- function(input, output, session) {
  
  Insecurite=eventReactive(input$Sentimentdinsecurite,{
    
    load("data/Nb_et_part_pers_en_insecurite.RData")
    load("data/insecurite_unite_urbaine.RData")
    load("data/insecurite_part_selon_age.RData")
    load("data/insecurite_part_selon_sexe.RData")
    
    return(list("Nb_et_part_pers_en_insecurite" = Nb_et_part_pers_en_insecurite,    
                "insecurite_unite_urbaine" = insecurite_unite_urbaine,
                "insecurite_part_selon_age" = insecurite_part_selon_age,
                "insecurite_part_selon_sexe" = insecurite_part_selon_sexe
    ))
    
},ignoreNULL=FALSE)
  
  
output$sentiment_insecurite_UI<-renderUI({
    
   tagList(
     
     outside_container(
       
       BoxForChart(Button('insecurite_a'),highchartOutput('Nb_pers_14plus'),
                  
                  Selector=TRUE,
                  
                  source="enq",
                  
                  selectInput("Nb_pers_14plus_select","",choices =unique(Insecurite()$Nb_et_part_pers_en_insecurite$Indicateurs),width="100%")
                  
       ),
       
       BoxForChart(Button('insecurite_b'),highchartOutput('insecurite_part_unite_urbaine'),
                  
                  Selector=TRUE,
                  
                  source="enq",
                  
                  selectInput("insecurite_part_unite_urbaine_select","",choices=unique(Insecurite()$insecurite_unite_urbaine$sous_indicateurs),width="100%")
                  
       )
       
       
       
     ),
     

     outside_container(
       
       BoxForChart(Button('insecurite_c'),highchartOutput('insecurite_part_age'),
                  
                  Selector=TRUE,
                  
                  source="enq",
                  
                  selectInput("insecurite_part_age_select","",choices =unique(Insecurite()$insecurite_part_selon_age$sous_indicateurs),width="100%")
                  
       ),
       
       BoxForChart(Button('insecurite_d'),highchartOutput('insecurite_part_sexe'),

                  Selector=TRUE,
                  
                  source="enq",

                  selectInput("insecurite_part_sexe_select","",choices =unique(Insecurite()$insecurite_part_selon_sexe$sous_indicateurs),width="100%")
                  
                  
                  
       )
       
     )
     
   )
      
      

  })
  
  
  
##### Parie des fonction reactives 
  

Reactive_Nb_pers_14plus<-reactive({

  Insecurite()$Nb_et_part_pers_en_insecurite %>% filter(Indicateurs==input$Nb_pers_14plus_select)

})



Reactive_insecurite_unite_urbaine<-reactive({

  Insecurite()$insecurite_unite_urbaine %>% filter(sous_indicateurs==input$insecurite_part_unite_urbaine_select)

})



Reactive_insecurite_part_age<-reactive({

  Insecurite()$insecurite_part_selon_age %>% filter(sous_indicateurs==input$insecurite_part_age_select)

})




Reactive_insecurite_part_sexe_select<-reactive({

  Insecurite()$insecurite_part_selon_sexe %>% filter(sous_indicateurs==input$insecurite_part_sexe_select)

})
  

  

output$Nb_pers_14plus<-renderHighchart({
  
  Nombre_pers_insecurite<-Reactive_Nb_pers_14plus() %>% filter(Reactive_Nb_pers_14plus()$statistiques=="Nombre (en millions)")
  
  part_prs_insecurite<-Reactive_Nb_pers_14plus() %>% filter(Reactive_Nb_pers_14plus()$statistiques=="Proportion de la population se sentant en insécurité")
  
  Multiaxe(
    Reactive_Nb_pers_14plus(),
    Nombre_pers_insecurite,
    part_prs_insecurite,
    Reactive_Nb_pers_14plus()$unite_temps,
    c("column","line"),
    c("#3497db","#FF7F00"),FirstDataLabelsFormat=list(enabled=TRUE,format = "{point.y:,.1f}",style=list(color="#000000",fontWeight="normal")))
  
})




output$insecurite_part_unite_urbaine<-renderHighchart({
  
  highchartFunction(Reactive_insecurite_unite_urbaine(),'column',c("#3497db"),
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"N",
                    
                    HEADER=unique(Reactive_insecurite_unite_urbaine()$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
  
})




output$insecurite_part_age<-renderHighchart({
  
  highchartFunction(Reactive_insecurite_part_age(),'bar',c("#3497db"),
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"P",
                    
                    HEADER=unique(Reactive_insecurite_part_age()$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
  
})



output$insecurite_part_sexe<-renderHighchart({
  
  highchartFunction(Reactive_insecurite_part_sexe_select(),'column',c("#3497db"),
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"P",
                    
                    HEADER=unique(Reactive_insecurite_part_sexe_select()$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
  
})  
  
  
  
# observeEvent(input$Sentimentdinsecurite,{
# 
#     output$Nb_pers_14plus<-renderHighchart({
# 
#       Nombre_pers_insecurite<-Reactive_Nb_pers_14plus() %>% filter(Reactive_Nb_pers_14plus()$statistiques=="Nombre (en millions)")
# 
#       part_prs_insecurite<-Reactive_Nb_pers_14plus() %>% filter(Reactive_Nb_pers_14plus()$statistiques=="Proportion de la population se sentant en insécurité")
# 
#       Multiaxe(
#         Reactive_Nb_pers_14plus(),
#         Nombre_pers_insecurite,
#         part_prs_insecurite,
#         Reactive_Nb_pers_14plus()$unite_temps,
#         c("column","line"),
#         c("#3497db","#FF7F00"),FirstDataLabelsFormat=list(enabled=TRUE,format = "{point.y:,.1f}",style=list(color="#000000",fontWeight="normal")))
# 
# })
# 
# 
# 
# 
# output$insecurite_part_unite_urbaine<-renderHighchart({
# 
#   highchartFunction(Reactive_insecurite_unite_urbaine(),'column',c("#3497db"),
# 
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"N",
# 
#                      HEADER=unique(Reactive_insecurite_unite_urbaine()$titre),
# 
#                      FormatLabelsData="Indicateurs")
# 
# 
# 
#   })
# 
# 
# 
# 
# output$insecurite_part_age<-renderHighchart({
# 
#   highchartFunction(Reactive_insecurite_part_age(),'bar',c("#3497db"),
# 
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"P",
# 
#                      HEADER=unique(Reactive_insecurite_part_age()$titre),
# 
#                      FormatLabelsData="Indicateurs")
# 
# 
# 
# })
# 
# 
# 
# output$insecurite_part_sexe<-renderHighchart({
# 
#   highchartFunction(Reactive_insecurite_part_sexe_select(),'column',c("#3497db"),
# 
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=statistiques),"P",
# 
#                      HEADER=unique(Reactive_insecurite_part_sexe_select()$titre),
# 
#                      FormatLabelsData="Indicateurs")
# 
# 
# 
#   })
# 
# 
# },ignoreNULL=FALSE)
  
  


observeEvent(input$insecurite_a, { showInfo( Reactive_Nb_pers_14plus()  )  })

observeEvent(input$insecurite_b, {showInfo( Reactive_insecurite_unite_urbaine()  )  })

observeEvent(input$insecurite_c, {showInfo( Reactive_insecurite_part_age() )})

observeEvent(input$insecurite_d, {showInfo( Reactive_insecurite_part_sexe_select() )})


  
}







