
teh_ui<-PageLayoutFunction("Traite des êtres humains",uiOutput("traite_etres_humains_UI"))


teh_server<-function(input,output,session){
  
  TehData = eventReactive(input$Traiteetexploitationdesetreshumains,{
    
    load("data/evol_nb_teh.RData")
    
    load("data/nb_victimes_teh.RData")
    
    load("data/nb_mec_teh.RData")
    
    load("data/repart_teh.RData")
    
    load("data/teh_victimes_accompagnees.RData")
    
    load("data/repart_vcitmes_accompagnnes_teh.RData")
    
    ##################################################

    
    load("data/part_femmes_victimes_admin.RData")
    
    load("data/part_femmes_victimes_asso.RData")
    
    load("data/profil_victimes_evol.RData")
    

    return(list("evol_nb_teh"=evol_nb_teh,
                
                "nb_victimes_teh"=nb_victimes_teh,
                
                "nb_mec_teh"=nb_mec_teh,
                
                "repartition_teh"=repart_teh,
                
                "teh_victimes_accompagnees"=teh_victimes_accompagnees,
                
                "repart_vcitmes_accompagnnes_teh"=repart_vcitmes_accompagnnes_teh,
                
                ##################################################

                "teh_evol_base_100"=profil_victimes_evol,
                
                "part_femmes_victimes_admin"=part_femmes_victimes_admin,
                
                "part_femmes_victimes_asso"=part_femmes_victimes_asso
                
                
                
    ))
    
    
},ignoreNULL=FALSE)
  
 
output$traite_etres_humains_UI<-renderUI({ 
    
    tagList(
      
      outside_container(
        
        BoxForChart(Button('teh_a'),highchartOutput('evol_nb_teh')),
        
        SingleValueChart(Button('teh_b'),TehData()$nb_victimes_teh)
        
        
      ),
      
      outside_container(
        
        SingleValueChart(Button('teh_c'),TehData()$teh_victimes_accompagnees,source="enq"),
        
        BoxForChart(Button('teh_d'),highchartOutput('repart_victimes_accompagnees'),source="enq")),
      
      outside_container(
        
        SingleValueChart(Button('teh_e'),TehData()$nb_mec_teh),
        
        BoxForChart(Button('teh_f'),highchartOutput('repart_teh'))),
      
      outside_container(
        
        BoxForChart(Button('teh_g'),highchartOutput('profil_victimes_part_femmes_admin')),
        
        BoxForChart(Button('teh_h'),highchartOutput('profil_victimes_part_femmes_asso'),source="enq"))
      
      
    ) # Fin TagList
    
  })
  
  
output$evol_nb_teh<-renderHighchart({
  
  highchartFunction(TehData()$evol_nb_teh,'line',ChoixCouleursBar[1],
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                    
                    HEADER=unique(TehData()$evol_nb_teh$titre),
                    
                    FormatLabelsData="unite_temps")
  
  
  
})



output$repart_victimes_accompagnees<-renderHighchart({
  
  highchartFunction(TehData()$repart_vcitmes_accompagnnes_teh,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(TehData()$repart_vcitmes_accompagnnes_teh$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
})


output$repart_teh<-renderHighchart({
  
  highchartFunction(TehData()$repartition_teh,'pie',ChoixCouleurs,
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                    
                    HEADER=unique(TehData()$nb_mec_teh$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
  
})




#####################################################################################################################

output$profil_victimes_part_femmes_admin<-renderHighchart({
  
  highchartFunction(TehData()$part_femmes_victimes_admin,'column',ChoixCouleursBar[1:length(unique(TehData()$part_femmes_victimes_admin$statistiques))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"P",
                    
                    HEADER=unique(TehData()$part_femmes_victimes_admin$titre),
                    
                    FormatLabelsData="sous_indicateurs")
  
  
  
})


output$profil_victimes_part_femmes_asso<-renderHighchart({
  
  highchartFunction(TehData()$part_femmes_victimes_asso,'column',ChoixCouleursBar[1:length(unique(TehData()$part_femmes_victimes_asso$statistiques))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"P",
                    
                    HEADER=unique(TehData()$part_femmes_victimes_asso$titre),
                    
                    FormatLabelsData="sous_indicateurs")
  
  
  
})




# observeEvent(input$Traiteetexploitationdesetreshumains,{
#     
# output$evol_nb_teh<-renderHighchart({
#       
#       highchartFunction(TehData()$evol_nb_teh,'line',ChoixCouleursBar[1],
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                          
#                          HEADER=unique(TehData()$evol_nb_teh$titre),
#                          
#                          FormatLabelsData="unite_temps")
#       
#       
#       
#   })
#     
#   
# 
# output$repart_victimes_accompagnees<-renderHighchart({
#       
#       highchartFunction(TehData()$repart_vcitmes_accompagnnes_teh,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                          
#                          HEADER=unique(TehData()$repart_vcitmes_accompagnnes_teh$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
# 
# })
#     
#   
# output$repart_teh<-renderHighchart({
#   
#       highchartFunction(TehData()$repartition_teh,'pie',ChoixCouleurs,
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                          
#                          HEADER=unique(TehData()$nb_mec_teh$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
#       
#       
#     })
#     
#     
#     
#     
#     #####################################################################################################################
#     
# output$profil_victimes_part_femmes_admin<-renderHighchart({
#       
#       highchartFunction(TehData()$part_femmes_victimes_admin,'column',ChoixCouleursBar[1:length(unique(TehData()$part_femmes_victimes_admin$statistiques))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"P",
#                          
#                          HEADER=unique(TehData()$part_femmes_victimes_admin$titre),
#                          
#                          FormatLabelsData="sous_indicateurs")
#       
#       
#       
#     })
#     
#     
#     output$profil_victimes_part_femmes_asso<-renderHighchart({
#       
#       highchartFunction(TehData()$part_femmes_victimes_asso,'column',ChoixCouleursBar[1:length(unique(TehData()$part_femmes_victimes_asso$statistiques))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),"P",
#                          
#                          HEADER=unique(TehData()$part_femmes_victimes_asso$titre),
#                          
#                          FormatLabelsData="sous_indicateurs")
#       
#       
#       
# })
#     
# 
#     
# },ignoreInit=FALSE,ignoreNULL=FALSE)
  


observeEvent(input$teh_a, {showInfo(TehData()$evol_nb_teh)})
  
observeEvent(input$teh_b, {showInfo(TehData()$nb_victimes_teh)})

observeEvent(input$teh_c, {showInfo(TehData()$teh_victimes_accompagnees)})

observeEvent(input$teh_d, {showInfo(TehData()$repart_vcitmes_accompagnnes_teh)})
  
observeEvent(input$teh_e, {showInfo(TehData()$nb_mec_teh)})
  
observeEvent(input$teh_f, {showInfo(TehData()$repartition_teh)})

observeEvent(input$teh_g, { showInfo(TehData()$part_femmes_victimes_admin)})
  
observeEvent(input$teh_h, {showInfo(TehData()$part_femmes_victimes_asso)})


  
}