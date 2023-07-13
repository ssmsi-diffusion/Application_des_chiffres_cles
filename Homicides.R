

homicides_ui<-PageLayoutFunction("Homicides",uiOutput("homicides_page_ui"))


homicides_server<-function(input, output, session) {
  
  data_homicides = eventReactive(input$Homicides,{
    
    load("data/Nb_victimes_homicides.RData")
    
    load("data/Total_MEC_homicides.RData")
    
    load("data/repart_mec_homicides.RData")
    
    load("data/tx_homicides_extra_100_000_hbts.RData")
    
    load("data/tx_homicides_intra_100_000_hbts.RData")
    
    load("data/Nb_moyen_homicides_annuel.RData")
    
    
    load("data/repartition_homicides.RData")
    
    
    
    return(list("Nb_victimes_homicides" = Nb_victimes_homicides,
                
                "Total_MEC_homicides" = Total_MEC_homicides,
                
                "repart_mec_homicides"=repart_mec_homicides,
                
                #################
                
                "tx_homicides_extra_100_000_hbts"=tx_homicides_extra_100_000_hbts,
                
                "tx_homicides_intra_100_000_hbts"=tx_homicides_intra_100_000_hbts,
                
                "Nb_moyen_homicides_annuel"=Nb_moyen_homicides_annuel,
                
                "repartition_homicides"=repartition_homicides
                
    ))
    
    
},ignoreNULL=FALSE)
  
 
output$homicides_page_ui<-renderUI({

    tagList(

      outside_container(

        BoxForChart(Button('homicides_a'),highchartOutput('victimes_homicide')),

        SingleValueChart(Button('homicides_b'),data_homicides()$Total_MEC_homicides)


      ),

      outside_container(BoxForChart(Button('homicides_c'),highchartOutput('repart_mec_homicides')),

                        BoxForChart(Button('homicides_d'),highchartOutput('chart_Nb_moyen_homicides_annuel'))),

      outside_container(BoxForChart(Button('homicides_e'),highchartOutput('tx_homicides_extra_100_000_hbts_chart')),

                        BoxForChart(Button('homicides_f'),highchartOutput('tx_homicides_intra_100_000_hbts_chart'))),

      outside_container(BoxForChart(Button('homicides_g'),highchartOutput('repartition_homicides_chart'))  )

    )


  })
  
  

output$victimes_homicide<-renderHighchart({
  
  highchartFunction(data_homicides()$Nb_victimes_homicides,'line',ChoixCouleursBar[1:length(unique(data_homicides()$Nb_victimes_homicides$sous_indicateurs))],
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
                    
                    HEADER=unique(data_homicides()$Nb_victimes_homicides$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Année")
  
  
  
})


output$Total_mec_homicides<-renderHighchart({
  
  highchartFunction(data_homicides()$Total_MEC_homicides,'pie',list("#77B5FE"),
                    
                    hcaes(x=as.character(unite_temps),y=as.numeric(value)),"SampleDonut",
                    
                    HEADER=unique(data_homicides()$Total_MEC_homicides$titre),
                    
                    FormatLabelsData="Indicateurs")
  
  
})





output$repart_mec_homicides<-renderHighchart({
  
  highchartFunction(data_homicides()$repart_mec_homicides,'column',ChoixCouleursBar[1:length(unique(data_homicides()$repart_mec_homicides$Indicateurs))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=Indicateurs),"stackingP",EnableLabels=TRUE,
                    
                    HEADER=unique(data_homicides()$repart_mec_homicides$titre),
                    
                    FormatLabelsData="Indicateurs",InfosPlus="Catégorie")
  
  
  
})



output$chart_Nb_moyen_homicides_annuel<-renderHighchart({
  
  highchartFunction(data_homicides()$Nb_moyen_homicides_annuel,'column',ChoixCouleursBar[1:length(unique(data_homicides()$Nb_moyen_homicides_annuel$sous_indicateurs))],
                    
                    hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
                    
                    HEADER=unique(data_homicides()$Nb_moyen_homicides_annuel$titre),
                    
                    FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie") 
  
  
  
})







output$tx_homicides_extra_100_000_hbts_chart<-renderHighchart({
  
  highchartFunction(data_homicides()$tx_homicides_extra_100_000_hbts,'line',ChoixCouleursBar[1:length(unique(data_homicides()$tx_homicides_extra_100_000_hbts$sous_sous_indicateurs))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"N",
                    
                    HEADER=unique(data_homicides()$tx_homicides_extra_100_000_hbts$titre),
                    
                    FormatLabelsData="sous_sous_indicateurs",InfosPlus="Catégorie")
  
  
})



output$tx_homicides_intra_100_000_hbts_chart<-renderHighchart({
  
  highchartFunction(data_homicides()$tx_homicides_intra_100_000_hbts,'line',ChoixCouleursBar[1:length(unique(data_homicides()$tx_homicides_intra_100_000_hbts$sous_sous_indicateurs))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"N",
                    
                    HEADER=unique(data_homicides()$tx_homicides_intra_100_000_hbts$titre),
                    
                    FormatLabelsData="sous_sous_indicateurs",InfosPlus="Catégorie")
  
  
})






output$repartition_homicides_chart<-renderHighchart({
  
  highchartFunction(data_homicides()$repartition_homicides,'column',ChoixCouleursBar[1:length(unique(data_homicides()$repartition_homicides$Indicateurs))],
                    
                    hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=Indicateurs),"stackingP",EnableLabels=TRUE,
                    
                    HEADER=unique(data_homicides()$repartition_homicides$titre),
                    
                    FormatLabelsData="Indicateurs",InfosPlus="Catégorie")
  
  
  
})




# observeEvent(input$Homicides,{
#   
#   
#     
#   output$victimes_homicide<-renderHighchart({
#       
#       highchartFunction(data_homicides()$Nb_victimes_homicides,'line',ChoixCouleursBar[1:length(unique(data_homicides()$Nb_victimes_homicides$sous_indicateurs))],
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value),group=sous_indicateurs),"IntegerValues",
#                          
#                          HEADER=unique(data_homicides()$Nb_victimes_homicides$titre),
#                          
#                          FormatLabelsData="sous_indicateurs",InfosPlus="Année")
#       
#       
#       
#     })
#     
#     
#     output$Total_mec_homicides<-renderHighchart({
#       
#       highchartFunction(data_homicides()$Total_MEC_homicides,'pie',list("#77B5FE"),
#                          
#                          hcaes(x=as.character(unite_temps),y=as.numeric(value)),"SampleDonut",
#                          
#                          HEADER=unique(data_homicides()$Total_MEC_homicides$titre),
#                          
#                          FormatLabelsData="Indicateurs")
#       
#       
#     })
#     
#     
#     
#     
#     
#     output$repart_mec_homicides<-renderHighchart({
#       
#       highchartFunction(data_homicides()$repart_mec_homicides,'column',ChoixCouleursBar[1:length(unique(data_homicides()$repart_mec_homicides$Indicateurs))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=Indicateurs),"stackingP",EnableLabels=TRUE,
#                          
#                          HEADER=unique(data_homicides()$repart_mec_homicides$titre),
#                          
#                          FormatLabelsData="Indicateurs",InfosPlus="Catégorie")
#       
#       
#       
#     })
#     
#     
#     
#     output$chart_Nb_moyen_homicides_annuel<-renderHighchart({
#       
#       highchartFunction(data_homicides()$Nb_moyen_homicides_annuel,'column',ChoixCouleursBar[1:length(unique(data_homicides()$Nb_moyen_homicides_annuel$sous_indicateurs))],
#                          
#                          hcaes(x=as.character(Indicateurs),y=as.numeric(value),group=sous_indicateurs),"stacking",EnableLabels=TRUE,
#                          
#                          HEADER=unique(data_homicides()$Nb_moyen_homicides_annuel$titre),
#                          
#                          FormatLabelsData="sous_indicateurs",InfosPlus="Catégorie") 
#       
#       
#       
#     })
#     
#     
#     
#     
#     
#     
#     
#     output$tx_homicides_extra_100_000_hbts_chart<-renderHighchart({
#       
#       highchartFunction(data_homicides()$tx_homicides_extra_100_000_hbts,'line',ChoixCouleursBar[1:length(unique(data_homicides()$tx_homicides_extra_100_000_hbts$sous_sous_indicateurs))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"N",
#                          
#                          HEADER=unique(data_homicides()$tx_homicides_extra_100_000_hbts$titre),
#                          
#                          FormatLabelsData="sous_sous_indicateurs",InfosPlus="Catégorie")
#       
#       
#     })
#     
#     
#     
#     output$tx_homicides_intra_100_000_hbts_chart<-renderHighchart({
#       
#       highchartFunction(data_homicides()$tx_homicides_intra_100_000_hbts,'line',ChoixCouleursBar[1:length(unique(data_homicides()$tx_homicides_intra_100_000_hbts$sous_sous_indicateurs))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=sous_sous_indicateurs),"N",
#                          
#                          HEADER=unique(data_homicides()$tx_homicides_intra_100_000_hbts$titre),
#                          
#                          FormatLabelsData="sous_sous_indicateurs",InfosPlus="Catégorie")
#       
#       
#     })
#     
#     
#     
#     
#     
#     
#     output$repartition_homicides_chart<-renderHighchart({
#       
#       highchartFunction(data_homicides()$repartition_homicides,'column',ChoixCouleursBar[1:length(unique(data_homicides()$repartition_homicides$Indicateurs))],
#                          
#                          hcaes(x=as.character(sous_indicateurs),y=as.numeric(value),group=Indicateurs),"stackingP",EnableLabels=TRUE,
#                          
#                          HEADER=unique(data_homicides()$repartition_homicides$titre),
#                          
#                          FormatLabelsData="Indicateurs",InfosPlus="Catégorie")
#       
#       
#       
#     })
#     
#     
#     
#     
#     
#     
#     
#     
#   },ignoreNULL=FALSE)
  
  
  
observeEvent(input$homicides_a, {showInfo(data_homicides()$Nb_victimes_homicides)})
  
observeEvent(input$homicides_b, {showInfo(data_homicides()$Total_MEC_homicides)})
  
observeEvent(input$homicides_c, {showInfo(data_homicides()$repart_mec_homicides)})
  
observeEvent(input$homicides_d, {showInfo( data_homicides()$Nb_moyen_homicides_annuel )})
  
observeEvent(input$homicides_e, {showInfo(data_homicides()$tx_homicides_extra_100_000_hbts)})
  
observeEvent(input$homicides_f, {showInfo(data_homicides()$tx_homicides_intra_100_000_hbts)})
  
observeEvent(input$homicides_g, {showInfo(data_homicides()$repartition_homicides)})
  
  
  
}


