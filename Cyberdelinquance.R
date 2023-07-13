

cyberdelinquance_ui<-PageLayoutFunction("Cyberdélinquance",
  
  tagList(
    
    uiOutput("cyberdelinquance_UI")
    
  )
  
)



cyberdelinquance_server<-function(input,output,session){
  
  Cyber_data<-eventReactive(input$Cyberdelinquance,{
    
    load("data/repartition_STAD.RData")
    
    load("data/repartition_STAD_type_attaques.RData")
    
    load("data/incident_STAD_par_secteur.RData")
    
    load("data/victimes_cyber_de_15_ans_ou_plus.RData")  
    
    load("data/infractions_cyber.RData")
    
    return(list(
      
     "repartition_STAD" = repartition_STAD,
     
     "repartition_STAD_type_attaques"=repartition_STAD_type_attaques,    
     
     "incident_STAD_par_secteur"=incident_STAD_par_secteur,
     
     "victimes_cyber_de_15_ans_ou_plus"=victimes_cyber_de_15_ans_ou_plus,
     
     "infractions_cyber"=infractions_cyber
                

    ))
    
  },ignoreNULL=FALSE)
  
 

  Reactive_incident_STAD_par_secteur<-reactive({
    
    Cyber_data()$incident_STAD_par_secteur %>% filter(sous_indicateurs==input$incident_STAD_par_select)
    
  })    
  
    

observeEvent(input$Cyberdelinquance,{

 output$cyberdelinquance_UI<-renderUI({
   
   tagList(
     
     outside_container(
       
       BoxForChart(Button('cyber_a'),highchartOutput('repart_stad')),
       
       BoxForChart(Button("cyber_b"),highchartOutput('repart_stad_par_type_attaques'))
       

       ),
     
     br(),
     outside_container(
       
       BoxForChart(Button("cyber_c"),highchartOutput('incident_STAD_par_secteur_chart'),Selector=TRUE,source="enq",
                    
                    selectInput("incident_STAD_par_select","",sort(unique(Cyber_data()$incident_STAD_par_secteur$sous_indicateurs)),width="70%")),
       
       
       BoxForChart(Button("cyber_d"),highchartOutput('victimes_de_15_ans_ou_plus_chart'),source="enq")
       
       
       
       ),
     
     br(),
     
     outside_container(
       
       BoxForChart(Button("cyber_e"),highchartOutput('infractions_cyber_chart'))
       
       )

   )
   
 })
  
  
 
output$repart_stad<-renderHighchart({
      
      highchartFunction(Cyber_data()$repartition_STAD,'bar',c("#77B5FE", "#FF7F00"),
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"P",
                         
                         HEADER=unique(Cyber_data()$repartition_STAD$titre),
                         
                         FormatLabelsData="sous_indicateurs",InfosPlus="Secteur d'activité") 
      
})
    

output$repart_stad_par_type_attaques<-renderHighchart({
  
  highchartFunction(Cyber_data()$repartition_STAD_type_attaques,'pie',ChoixCouleurs,
                     
                     hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                     
                     HEADER=unique(Cyber_data()$repartition_STAD_type_attaques$titre),
                     
                     FormatLabelsData="Indicateurs") 
  
})

 

output$incident_STAD_par_secteur_chart<-renderHighchart({
  
  highchartFunction(Reactive_incident_STAD_par_secteur(),'pie',ChoixCouleurs,
                     
                     hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                     
                     HEADER=unique(Reactive_incident_STAD_par_secteur()$titre),
                     
                     FormatLabelsData="Indicateurs") 
  
})




output$victimes_de_15_ans_ou_plus_chart<-renderHighchart({
  
  highchartFunction(Cyber_data()$victimes_cyber_de_15_ans_ou_plus,'bar',ChoixCouleursBar[2],
                     
                     hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
                     
                     HEADER=unique(Cyber_data()$victimes_cyber_de_15_ans_ou_plus$titre),
                     
                     FormatLabelsData="Indicateurs") 
  
})





output$infractions_cyber_chart<-renderHighchart({
  
  highchartFunction(Cyber_data()$infractions_cyber,'bar',ChoixCouleursBar[2],
                     
                     hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
                     
                     HEADER=unique(Cyber_data()$infractions_cyber$titre),
                     
                     FormatLabelsData="Indicateurs") 
  
})




    
},ignoreNULL=FALSE)
  
  

observeEvent(input$cyber_a, {showInfo(Cyber_data()$repartition_STAD)})

observeEvent(input$cyber_b, {showInfo(Cyber_data()$repartition_STAD_type_attaques)})

observeEvent(input$cyber_c, {showInfo( Reactive_incident_STAD_par_secteur() )})

observeEvent(input$cyber_d, {showInfo(Cyber_data()$victimes_cyber_de_15_ans_ou_plus)})

observeEvent(input$cyber_e, {showInfo(Cyber_data()$infractions_cyber)})
  
  
  
  
  
}