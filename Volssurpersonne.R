
load("data/theme_list_vol_personne.RData")


Volssurpersonne_ui<-PageLayoutFunction("Vols sur personne",
  tagList(
    radioGroupButtons(inputId="VolPersonneID","",choices=theme_list_vol_personne,direction="horizontal",individual=TRUE,status="boutonGroup"),
    uiOutput("VolPersonnePageUI"),br(),br()
  )
)

Volssurpersonne_server<-function(input,output,session){
  
  VolsPersonnes = eventReactive(input$Volssurpersonne,{
    load("data/vol_personne_plus_14.RData")
    load("data/victime_vol_personne.RData")
    load("data/Nb_victime_transport.RData")
    load("data/part_faits_transport.RData")
    load("data/vol_personne_mec.RData")
    load("data/rep_vol_personne.RData")
    load("data/tx_plainte_vol_personne.RData")
    load("data/Nb_victimes_enquete.RData")
    load("data/Nombre.RData")
    load("data/proportion.RData")
    load("data/mec_vol_sans_violences_pers.RData")
    load("data/Nb_victimes_enreg.RData")
    load("data/rep_vol_sans_violence.RData")
    load("data/taux_plaint_vol_sans_violence.RData")
    
    return(list("vol_personne_plus_14" = vol_personne_plus_14,  
                "victime_vol_personne"=victime_vol_personne,
                "Nb_victime_transport"=Nb_victime_transport, 
                "part_faits_transport"=part_faits_transport,
                "vol_personne_mec"=vol_personne_mec,
                "rep_vol_personne"=rep_vol_personne,
                "tx_plainte_vol_personne"=tx_plainte_vol_personne,
                "Nb_victimes_enquete"=Nb_victimes_enquete,
                "Nombre"=Nombre,
                "proportion"=proportion,
                "mec_vol_sans_violences_pers"=mec_vol_sans_violences_pers,
                "Nb_victimes_enreg"=Nb_victimes_enreg,
                "rep_vol_sans_violence"=rep_vol_sans_violence,
                "taux_plaint_vol_sans_violence"=taux_plaint_vol_sans_violence
                
                
                
                
                
))
    
},ignoreInit=FALSE,ignoreNULL=FALSE) 
  

  output$VolPersonnePageUI<-renderUI({
    
    if (input$VolPersonneID=="Vols avec violences"){
      tagList(
        outside_container(
          BoxForChart(Button('vp_a'),highchartOutput("victime_vol_personne")),
          BoxForChart(Button('vp_b'),highchartOutput('personne_plus_14'),source="enq")), 
        br(),
        
        outside_container(
          
          SingleValueChart(Button('vp_c'),VolsPersonnes()$vol_personne_mec %>% filter(Indicateurs=="Vols avec violence (avec armes)")),
          
          SingleValueChart(Button('vp_k'),VolsPersonnes()$vol_personne_mec %>% filter(Indicateurs=="Vols avec violence (sans armes)"))
          
        ),
        
        br(),
        outside_container(
          
          BoxForChart(Button('vp_d'),highchartOutput('rep_vol_personne'),source="enq"),
          
          SingleValueChart(Button('vp_e'),VolsPersonnes()$tx_plainte_vol_personne,source="enq")
          
          
          
        )
      )
      
    } else if (input$VolPersonneID=="Vols sans violences") {
      tagList(
        outside_container(
          BoxForChart(Button('vp_h'),highchartOutput("Nb_victimes_enregistre")),
          BoxForChart(Button('vp_f'),highchartOutput("Nb_victimes_enquete_ind"),source="enq")
        ),
        
        br(),
        
        outside_container(
          
          SingleValueChart(Button('vp_g'),VolsPersonnes()$mec_vol_sans_violences_pers),
          
          BoxForChart(Button('vp_i'),highchartOutput('repart_vol_sans_violence'),source="enq")
          
        ),
        
        br(),
        outside_container(
          
          SingleValueChart(Button('vp_j'),VolsPersonnes()$taux_plaint_vol_sans_violence,source="enq")
          
          
        )
        
      )
      
    }
    
  })
 
 
  ## Partie I : Vols avec violences      
  
  output$victime_vol_personne<-renderHighchart({
    
    highchartFunction(VolsPersonnes()$victime_vol_personne,'line',c("#0444a5","#77B5FE"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"IntegerValues",
                      
                      HEADER=unique(VolsPersonnes()$victime_vol_personne$titre),
                      
                      FormatLabelsData="Indicateurs",InfosPlus="Année")
    
    
    
    
  })
  
  
  
  output$personne_plus_14<-renderHighchart({
    Multiaxe(
      VolsPersonnes()$vol_personne_plus_14,
      VolsPersonnes()$Nb_victime_transport,
      VolsPersonnes()$part_faits_transport,
      VolsPersonnes()$vol_personne_plus_14$unite_temps,
      c("column","line"),
      c("#3497db","#FF7F00"))
    
  })
  
  
  
  output$rep_vol_personne<-renderHighchart({
    
    highchartFunction(VolsPersonnes()$rep_vol_personne,'pie',list("#0444a5","#77B5FE"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                      
                      HEADER=unique(VolsPersonnes()$rep_vol_personne$titre),
                      
                      FormatLabelsData="Indicateurs")
    
    
    
    
  })
  
  
  
  ## Partie II : Vols sans violences
  
  
  output$Nb_victimes_enregistre<-renderHighchart({
    
    highchartFunction(VolsPersonnes()$Nb_victimes_enreg,'line',c("#0444a5"),
                      
                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                      
                      HEADER=unique(VolsPersonnes()$Nb_victimes_enreg$titre), 
                      
                      FormatLabelsData="unite_temps")
    
    
  })
  
  
  
  
  output$Nb_victimes_enquete_ind<-renderHighchart({
    Multiaxe(
      VolsPersonnes()$Nb_victimes_enquete,
      VolsPersonnes()$Nombre,
      VolsPersonnes()$proportion,
      VolsPersonnes()$Nb_victimes_enquete$unite_temps,
      c("column","line"),
      c("#3497db","#FF7F00"))
  })
  
  
  
  output$repart_vol_sans_violence<-renderHighchart({
    
    highchartFunction(VolsPersonnes()$rep_vol_sans_violence,'pie',list("#3497db","#FF7F00"),
                      
                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
                      
                      HEADER=unique(VolsPersonnes()$rep_vol_sans_violence$titre), 
                      
                      FormatLabelsData="Indicateurs")
    
    
    
  })  
   
  
  
# observeEvent(input$Volssurpersonne,{
#   
# ## Partie I : Vols avec violences      
#     
# output$victime_vol_personne<-renderHighchart({
#  
#   highchartFunction(VolsPersonnes()$victime_vol_personne,'line',c("#0444a5","#77B5FE"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value),group=Indicateurs),"IntegerValues",
#                      
#                      HEADER=unique(VolsPersonnes()$victime_vol_personne$titre),
#                      
#                      FormatLabelsData="Indicateurs",InfosPlus="Année")
#   
#   
#   
#       
#     })
#     
#     
#     
# output$personne_plus_14<-renderHighchart({
#       Multiaxe(
#         VolsPersonnes()$vol_personne_plus_14,
#         VolsPersonnes()$Nb_victime_transport,
#         VolsPersonnes()$part_faits_transport,
#         VolsPersonnes()$vol_personne_plus_14$unite_temps,
#         c("column","line"),
#         c("#3497db","#FF7F00"))
# 
# })
#     
#    
#     
# output$rep_vol_personne<-renderHighchart({
#  
#   highchartFunction(VolsPersonnes()$rep_vol_personne,'pie',list("#0444a5","#77B5FE"),
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(VolsPersonnes()$rep_vol_personne$titre),
#                      
#                      FormatLabelsData="Indicateurs")
#   
#   
#   
#       
#  })
#     
#    
# 
# ## Partie II : Vols sans violences
# 
# 
# output$Nb_victimes_enregistre<-renderHighchart({
#       
#   highchartFunction(VolsPersonnes()$Nb_victimes_enreg,'line',c("#0444a5"),
#                      
#                      hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
#                      
#                      HEADER=unique(VolsPersonnes()$Nb_victimes_enreg$titre), 
#                      
#                      FormatLabelsData="unite_temps")
#   
#       
#     })
# 
# 
# 
# 
# output$Nb_victimes_enquete_ind<-renderHighchart({
#   Multiaxe(
#     VolsPersonnes()$Nb_victimes_enquete,
#     VolsPersonnes()$Nombre,
#     VolsPersonnes()$proportion,
#     VolsPersonnes()$Nb_victimes_enquete$unite_temps,
#     c("column","line"),
#     c("#3497db","#FF7F00"))
# })
#     
#     
#     
# output$repart_vol_sans_violence<-renderHighchart({
# 
#   highchartFunction(VolsPersonnes()$rep_vol_sans_violence,'pie',list("#3497db","#FF7F00"),
#                      
#                      hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"PPIE",
#                      
#                      HEADER=unique(VolsPersonnes()$rep_vol_sans_violence$titre), 
#                      
#                      FormatLabelsData="Indicateurs")
#   
#   
#       
# })
#     
# 
# 
# },ignoreInit=FALSE,ignoreNULL=FALSE)
  

  
observeEvent(input$vp_a, {showInfo(VolsPersonnes()$victime_vol_personne)})
  
observeEvent(input$vp_b, {showInfo(VolsPersonnes()$vol_personne_plus_14)})
  
observeEvent(input$vp_c, {showInfo(VolsPersonnes()$vol_personne_mec %>% filter(Indicateurs=="Vols avec violence (avec armes)"))})

observeEvent(input$vp_k, {showInfo(VolsPersonnes()$vol_personne_mec %>% filter(Indicateurs=="Vols avec violence (sans armes)"))})
  
observeEvent(input$vp_d, {showInfo(VolsPersonnes()$rep_vol_personne)})
  
observeEvent(input$vp_e, {showInfo(VolsPersonnes()$tx_plainte_vol_personne)})
  
observeEvent(input$vp_f, {showInfo(VolsPersonnes()$Nb_victimes_enquete)})
  
observeEvent(input$vp_g, {showInfo(VolsPersonnes()$mec_vol_sans_violences_pers)})
  
observeEvent(input$vp_h, {showInfo(VolsPersonnes()$Nb_victimes_enreg)})
  
observeEvent(input$vp_i, {showInfo(VolsPersonnes()$rep_vol_sans_violence)})
  
observeEvent(input$vp_j, {showInfo(VolsPersonnes()$taux_plaint_vol_sans_violence)})

 
  
}