
Conditionsdetravail_fs_ui<-PageLayoutFunction("Conditions de travail des policiers et des gendarmes",
  
  tagList(
    
    uiOutput("Conditions_de_travail_pg_UI"),
    
  )
  
)



Conditionsdetravail_fs_server<-function(input,output,session){
  
  ConditionsTravailData<-eventReactive(input$Conditionsdetravaildespoliciersetdesgendarmes,{
    

    load("data/conditions_de_travail_pn_gn.RData")
    
    return(
      
      list(
        
        "conditions_de_travail_pn_gn"=conditions_de_travail_pn_gn 
        
      )
      
    )
    
  },ignoreNULL=FALSE)
  

output$Conditions_de_travail_pg_UI<-renderUI({
    
    tagList(
      
      outside_container(
        
        div(class="col-md-4",
            
            div(class="col-md-12",style="border:1px solid #C0C0C0;height:550px;",
                
                radioButtons('indicateurs_id',"Indicateurs : ", choices=sort(unique(ConditionsTravailData()$conditions_de_travail_pn_gn$Indicateurs))),
                
                uiOutput('boutonsSousIndic')
                
                
            )
            
            
        ),
        
        
        div(class="col-md-8",
            
            BoxForChart(Button('condition_de_travail_a'),Selector=FALSE,FullPage=TRUE,
                     
                     highchartOutput('condition_de_travail_chart'),source="enq")
            
            
        )
        
        
      )
      
    )
    
    
  })
  
  
  
  Reactive_boutons_sous_indicateurs<-reactive({ 
    
    liste<-sort(unique((ConditionsTravailData()$conditions_de_travail_pn_gn %>% dplyr::filter(
      
      ConditionsTravailData()$conditions_de_travail_pn_gn$Indicateurs %in% input$indicateurs_id))$sous_indicateurs))
    
    
    
  })
  
  
  
  output$boutonsSousIndic<-renderUI({  
    
    req(Reactive_boutons_sous_indicateurs())
    
    radioButtons('sous_indicateurs_id',"Sous-indicateur(s) :",choices=sort(unique(Reactive_boutons_sous_indicateurs())))
    
  })
  
  
  
  
  reactiveData<-reactive({
    
    DataTableIndic<-ConditionsTravailData()$conditions_de_travail_pn_gn %>%
      
      filter(
        
        ConditionsTravailData()$conditions_de_travail_pn_gn$Indicateurs %in% input$indicateurs_id &
          
          ConditionsTravailData()$conditions_de_travail_pn_gn$sous_indicateurs %in% input$sous_indicateurs_id)
    
    
  })
  
  
  
  output$condition_de_travail_chart<-renderHighchart({
    
    data_graphique <- reactiveData()
    
    
    highchart() %>%
      
      hc_add_series(data=data_graphique,hcaes(x=as.character(sous_indicateurs),y=as.numeric(value)),type = "column",showInLegend = FALSE,
                    dataLabels=list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.0f}%")) %>%
      
      hc_xAxis(categories=as.character(data_graphique$sous_sous_indicateurs)) %>%  
      
      hc_title(text=ifelse(length(unique(data_graphique$unite_temps))>1,paste0(unique(data_graphique$titre)),paste0(unique(data_graphique$titre)," ","(",unique(data_graphique$unite_temps),")") ),
               margin = 20, align = "left",
               style = list(color = "#000000",fontWeight = "normal",fontSize='13px',useHTML = TRUE)) %>%
      
      hc_plotOptions(column = list(
        dataLabels = list(enabled=FALSE,style=list(color="#00561B")),marker=list(enabled=FALSE),
        animation=FALSE,
        allowOverlap=TRUE,
        enableMouseTracking = TRUE))%>%
      
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat=paste0('<br>','<span style="color:{point.color}">\u25CF</span>{point.sous_sous_indicateurs}: {point.y:,.0f}%'),
                 headerFormat=paste0(unique(data_graphique$titre),'<br><br><span style="font-size: 13px">{point.key}</span><br>')
                 
      ) %>%    
      
      hc_caption(
        text = str_c("Champ(s) : ",unique(data_graphique$champ),
                     "<br/> Source(s) :",unique(data_graphique$source),
                     "<br/> Lecture :",ifelse(is.na(unique(data_graphique$Lecture)),"Note de lecture",unique(data_graphique$Lecture)),
                     sep = " "),
        style = list(fontSize = "11px"),
        align = "left",verticalAlign = 'bottom'
        
      ) %>%   
      
      
      hc_legend(
        
        layout="horizontal", align="center", verticalAlign="top", itemStyle = list(fontSize = "11px",fontWeight = "normal")
        
      )
    
    
    
    
})    
  


observeEvent(input$condition_de_travail_a,{showInfo(reactiveData())})
  
  
  
}