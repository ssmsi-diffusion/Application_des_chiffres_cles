
qpv_ui<-PageLayoutFunction("Délinquance dans les Quartiers Prioritaires de la politique de la Ville",
  
  tagList(
    outside_container(BoxForChart(Button('NbInfracQPV'),highchartOutput('Nb_infraction_qpv')),BoxForChart(Button('NbMecQPV'),highchartOutput('Nb_mec_qpv'))),
    br(),
    outside_container(BoxForChart(Button('PropVictMecQPV'),highchartOutput('prop_victimes_qpv'),Selector=TRUE,FullPage=TRUE,selectInput('PropVictMecQPVID',"",c("victimes","mis en cause")))),
    br()
    
    
  )
  
  
)



qpv_server<-function(input,output,session){
  
  qpv<-eventReactive(input$Quartiersprioritairesdelapolitiquedelaville,{
    
    load("data/Nb_infraction_qpv.RData")
    
    load("data/Nb_mec_qpv.RData")
    
    load("data/prop_victimeMec_qpv.RData")
    
    return(list("Nb_infraction_qpv" = Nb_infraction_qpv,
                
                "Nb_mec_qpv" = Nb_mec_qpv,
                
                "prop_victimeMec_qpv"= prop_victimeMec_qpv
                
    ))
    
},ignoreNULL=FALSE)
  
  
  PropVictimeMecQPV<-reactive({
    
    data<-qpv()$prop_victimeMec_qpv %>% filter(unite_de_compte==input$PropVictMecQPVID)
    
  })
  
  
observeEvent(input$Quartiersprioritairesdelapolitiquedelaville,{
    
output$Nb_infraction_qpv<-renderHighchart({
      
      highchartFunction(qpv()$Nb_infraction_qpv,'column',c("#FF7F00"),
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"N",
                         
                         HEADER=unique(qpv()$Nb_infraction_qpv$titre),
                         
                         FormatLabelsData="Indicateurs")
      
      
    })
    
    
    
output$Nb_mec_qpv<-renderHighchart({

      highchartFunction(qpv()$Nb_mec_qpv,'bar',c("#0daaf3"),
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"N",
                         
                         HEADER=unique(qpv()$Nb_mec_qpv$titre),
                         
                         FormatLabelsData="Indicateurs")
      

    })
    
    
    
output$prop_victimes_qpv<-renderHighchart({
      
      highchartFunction(PropVictimeMecQPV(),'bar',ChoixCouleursBar[1:length(unique(PropVictimeMecQPV()$sous_indicateurs))],
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
                         
                         HEADER=unique(PropVictimeMecQPV()$titre),
                         
                         FormatLabelsData="sous_indicateurs",InfosPlus="Indicateur")
      
      
      
      
      
    })
    
    
    
},ignoreNULL=FALSE)
  
  
  
observeEvent(input$NbInfracQPV, {showInfo(qpv()$Nb_infraction_qpv)})
  
observeEvent(input$NbMecQPV, {showInfo(qpv()$Nb_mec_qpv)})
  
observeEvent(input$PropVictMecQPV, {showInfo( PropVictimeMecQPV() )})
  
  
  
}