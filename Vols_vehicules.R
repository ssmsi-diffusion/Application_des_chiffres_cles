
load("data/List_nb_prop_menages_vols_vehicules.RData")

vols_vehicule_ui<-PageLayoutFunction("Vols liés aux véhicules",
                       
                       tagList(
                         
                         radioGroupButtons(inputId="VolsVehiculesID","",choices=c("Vols de véhicules","Vols dans ou sur les véhicules"),direction="horizontal",individual=TRUE,status="boutonGroup"),
                         
                         uiOutput("vol_vehicule_ui")))
                         
                         
 

vols_vehicule_server<-function(input,output,session){
  
  VolsVehicules = eventReactive(input$Volsliesauxvehicules,{
    ###### Vols de véhicules ##
    load("data/nb_prop_menages_vols_vehicules.RData")
    load("data/Nb_vols_vehicules.RData")
    load("data/mec_vols_vehicules.RData")
    load("data/tx_plainte_vols_vehicules.RData")
    load("data/repartition_vols_vehicules.RData")
    ###### Vols dans ou sur les véhicules ####
    load("data/Nb_vols_dans_ou_sur_vehicule.RData")
    load("data/Menages_Ordinaires_VDSV.RData")
    load("data/mec_vols_dans_ou_sur_vehicule.RData")
    load("data/tx_plainte_vols_dans_ou_sur_vehicule.RData")
    return(list(
      ###### Vols de véhicules ######
      "nb_prop_menages_vols_vehicules"=nb_prop_menages_vols_vehicules,
      "Nb_vols_vehicules"=Nb_vols_vehicules,
      "mec_vols_vehicules"=mec_vols_vehicules,
      "tx_plainte_vols_vehicules"=tx_plainte_vols_vehicules,
      "repartition_vols_vehicules"=repartition_vols_vehicules,
      ###### Vols dans ou sur les véhicules ####
      "Nb_vols_dans_ou_sur_vehicule"=Nb_vols_dans_ou_sur_vehicule,
      "Menages_Ordinaires_VDSV"=Menages_Ordinaires_VDSV,
      "mec_vols_dans_ou_sur_vehicule"=mec_vols_dans_ou_sur_vehicule,
      "tx_plainte_vols_dans_ou_sur_vehicule"=tx_plainte_vols_dans_ou_sur_vehicule
    ))
    
},ignoreInit=FALSE,ignoreNULL=FALSE)
  
  
  
  
  ractive_nb_prop_menages_vols_vehicules<-reactive({
    VolsVehicules()$nb_prop_menages_vols_vehicules %>% filter(Indicateurs==input$nb_et_prop_menages_vols_vehicules)

  })
  

  NB_vols_dans_ou_sur_vehiculeR<-reactive({
    
    VolsVehicules()$Nb_vols_dans_ou_sur_vehicule %>% filter(Indicateurs==input$NB_vols_dans_ou_sur_vehicule)

  })
  
 
  output$vol_vehicule_ui<-renderUI({
    
    if(input$VolsVehiculesID=="Vols de véhicules"){
      tagList( 
        outside_container(
          
          BoxForChart(Button('vv_a'),highchartOutput("Nb_vols_vehicules")),
          
          BoxForChart(Button('vv_e'),highchartOutput("Nb_et_prop_menages_vols_vehicules"),Selector=TRUE,source="enq",
                      selectInput("nb_et_prop_menages_vols_vehicules","",
                                  choices=List_nb_prop_menages_vols_vehicules,width ="70%"))
          
        ),  
        br(),
        outside_container(
          
          SingleValueChart(Button('vv_c'),VolsVehicules()$mec_vols_vehicules),
          
          BoxForChart(Button('vv_d'),highchartOutput('tx_plainte_vols_vehicules'),source="enq")),
        
        br(),
        outside_container(
          
          BoxForChart(Button('vv_b'),highchartOutput('rep_vols_vehicules'),source="enq")
          
        )
        
      )
      
      
    } else if(input$VolsVehiculesID=="Vols dans ou sur les véhicules"){
      tagList(
        outside_container(
          BoxForChart(Button('vv_f'),highchartOutput("Chart_NB_vols_dans_ou_sur_vehicule"),Selector=TRUE,
                      selectInput("NB_vols_dans_ou_sur_vehicule","",
                                  choices=unique(VolsVehicules()$Nb_vols_dans_ou_sur_vehicule$Indicateurs),width ="50%")),
          
          BoxForChart(Button('vv_g'),highchartOutput('Chart_Menages_Ordinaires_VDSV'),source="enq")
        ),
        
        br(),
        
        outside_container(
          
          SingleValueChart(Button('vv_h'),VolsVehicules()$mec_vols_dans_ou_sur_vehicule),
          
          SingleValueChart(Button('vv_i'),VolsVehicules()$tx_plainte_vols_dans_ou_sur_vehicule,source="enq")
          
        )
        
      )
      
    }
    
  })
  

observeEvent(input$Volsliesauxvehicules,{  
    
#############################################################################
### Vols de véhicules #######################################################
#############################################################################    
    
    
output$Nb_vols_vehicules<-renderHighchart({ 
 
 highchartFunction(VolsVehicules()$Nb_vols_vehicules,'line',c("#77B5FE"),
                     
                     hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                     
                     HEADER=unique(VolsVehicules()$Nb_vols_vehicules$titre),
                     
                     FormatLabelsData="unite_temps")
  
      
})   
    
    
output$rep_vols_vehicules<-renderHighchart({

      highchartFunction(VolsVehicules()$repartition_vols_vehicules,'column',c("#77B5FE","#FF7F00"),
                                       
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=Indicateurs),"P",
                     
                     HEADER=unique(VolsVehicules()$repartition_vols_vehicules$titre),
                     
                     FormatLabelsData="Indicateurs",InfosPlus="Véhicule")
  
  
    })
    
    
output$tx_plainte_vols_vehicules<-renderHighchart({
   
      highchartFunction(VolsVehicules()$tx_plainte_vols_vehicules,'column',c("#77B5FE"),
                         
                         hcaes(x=as.character(Indicateurs),y=as.numeric(value)),"P",
                         
                         HEADER=unique(VolsVehicules()$tx_plainte_vols_vehicules$titre), 
                         
                         FormatLabelsData="Indicateurs")
      
      
})
    
    
output$Nb_et_prop_menages_vols_vehicules<-renderHighchart({
      Nb_menages_vols_vehicules<-ractive_nb_prop_menages_vols_vehicules() %>% filter(ractive_nb_prop_menages_vols_vehicules()$statistiques=="Nombre")
      prop_menages_vols_vehicules<-ractive_nb_prop_menages_vols_vehicules() %>% filter(ractive_nb_prop_menages_vols_vehicules()$statistiques=="Proportion de ménages victimes")
      Multiaxe(
        ractive_nb_prop_menages_vols_vehicules(),
        Nb_menages_vols_vehicules,
        prop_menages_vols_vehicules,
        ractive_nb_prop_menages_vols_vehicules()$unite_temps,
        c("column","line"),
        c("#77B5FE","#FF7F00"))
}) 
    
    
    

##########################################################################################
###  Vols dans ou sur les véhicules ######################################################
##########################################################################################      

output$Chart_NB_vols_dans_ou_sur_vehicule<-renderHighchart({

  highchartFunction(NB_vols_dans_ou_sur_vehiculeR(),'line',c("#77B5FE"),
                     
                     hcaes(x=as.character(unite_temps),y=as.numeric(value)),"IntegerValues",
                     
                     HEADER=unique(NB_vols_dans_ou_sur_vehiculeR()$titre),
                     
                     FormatLabelsData="unite_temps")
  
  

    })
    
    
    
output$Chart_Menages_Ordinaires_VDSV<-renderHighchart({
      NB_Menages_Ordinaires_VDSV<-VolsVehicules()$Menages_Ordinaires_VDSV %>% filter(VolsVehicules()$Menages_Ordinaires_VDSV$sous_indicateurs=="Nombre de ménages victimes")
      Prop_Menages_Ordinaires_VDSV<-VolsVehicules()$Menages_Ordinaires_VDSV %>% filter(VolsVehicules()$Menages_Ordinaires_VDSV$sous_indicateurs=="Proportion de victimes parmi les ménages (%)")
      Multiaxe(
        VolsVehicules()$Menages_Ordinaires_VDSV,
        NB_Menages_Ordinaires_VDSV,
        Prop_Menages_Ordinaires_VDSV,
        VolsVehicules()$Menages_Ordinaires_VDSV$unite_temps,
        c("column","line"),
        c("#77B5FE","#FF7F00"))


}) 
    

},ignoreInit=FALSE,ignoreNULL=FALSE)
  
  
observeEvent(input$vv_a, {showInfo(VolsVehicules()$Nb_vols_vehicules)})
  
observeEvent(input$vv_b, {showInfo(VolsVehicules()$repartition_vols_vehicules)})
  
observeEvent(input$vv_c, {showInfo(VolsVehicules()$mec_vols_vehicules)})
  
observeEvent(input$vv_d, {showInfo(VolsVehicules()$tx_plainte_vols_vehicules)})
  
observeEvent(input$vv_e, {showInfo(ractive_nb_prop_menages_vols_vehicules())})

observeEvent(input$vv_f, {showInfo(NB_vols_dans_ou_sur_vehiculeR())})
  
observeEvent(input$vv_g, {showInfo(VolsVehicules()$Menages_Ordinaires_VDSV)})

observeEvent(input$vv_h, {showInfo(VolsVehicules()$mec_vols_dans_ou_sur_vehicule)})
  
observeEvent(input$vv_i, {showInfo(VolsVehicules()$tx_plainte_vols_dans_ou_sur_vehicule)})
  
  
}