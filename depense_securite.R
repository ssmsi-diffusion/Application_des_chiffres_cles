

depense_securite_ui<-PageLayoutFunction("Dépenses en matière de sécurité",uiOutput("depense_securite_UI"))


depense_securite_server<-function(input,output,server){
  

  data_depense_securite<-eventReactive(input$Depensesenmatieredesecurite,{
    
    load("data/depenses_securite_en_matiere_de_securite.RData")

    return(
      
      list(
        
        "depenses_securite_en_matiere_de_securite"=depenses_securite_en_matiere_de_securite
      
      )
      
      
    )

},ignoreNULL=FALSE)  
  
  
  output$depense_securite_UI<-renderUI({
    
     tagList(
       
       outside_container(
         
         BoxForChart(Button('depense_securite_a'),highchartOutput("depense_en_matiere_de_securite"),Selector=FALSE,FullPage=TRUE)
       
       )

     )
      
   
    
  })
  
  
observeEvent(input$Depensesenmatieredesecurite,{
  

output$depense_en_matiere_de_securite<-renderHighchart({
  
  nb_depense_securite<-data_depense_securite()$depenses_securite_en_matiere_de_securite %>% filter(statistiques=="Nombre") %>% mutate(value=RoundValuesFunction(value))
  
  part_depense_securite<-data_depense_securite()$depenses_securite_en_matiere_de_securite %>% filter(statistiques=="Part")
  
  Multiaxe(
    data_depense_securite()$depenses_securite_en_matiere_de_securite,
    nb_depense_securite,
    part_depense_securite,
    data_depense_securite()$depenses_securite_en_matiere_de_securite$unite_temps,
    c("column","column"),
    c("#3497db","#FF7F00"))
  
})
  

  
},ignoreNULL=FALSE)
  


observeEvent(input$depense_securite_a, {showInfo(data_depense_securite()$depenses_securite_en_matiere_de_securite)})  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
}