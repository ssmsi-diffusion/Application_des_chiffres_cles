
load("data/ListeInternationale.RData")


comparaison_europe_ui<-PageLayoutFunction("Comparaisons internationales",

                                        tagList(

                                        outside_container(

                                        BoxForChart(Button('CI_a'),highchartOutput('CI_taux_100hbts'),
                                        Selector=TRUE,
                                        FullPage=TRUE,
                                        selectInput("CiID","",choices=ListeInternationale,width="60%")))))


comparaison_europe_server<-function(input,output,session){
  
  comparaisonInternationale = eventReactive(input$Comparaisonseuropeennes,{
    
    load("data/comparaison_internationale.RData")
    
    return(list(
      
      "comparaison_internationale"=comparaison_internationale
      
    ))
    
    
  },ignoreNULL=FALSE)
  
  
ReactiveCI<-reactive({
    
    comparaisonInternationale()$comparaison_internationale %>% filter(Indicateurs==input$CiID) %>%
      
      mutate(color_choice = ifelse(sous_indicateurs == "France", "#CD5C5C","#1283f3"))
    
    
})
  
  
output$CI_taux_100hbts<-renderHighchart({
  
  DataFinale <- ReactiveCI()[order(ReactiveCI()$value),] 
  
  highchart() %>%
    
    hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                 buttons=list(
                   contextButton=list(
                     y=23,
                     x=0,
                     text= "Télécharger",
                     menuItems=DowloadChart,
                     symbol=''))) %>%
    
    hc_add_series(data = DataFinale,hcaes(x=sous_indicateurs,y=as.numeric(value),color = color_choice), 
                  
                  type = 'column',
                  
                  name=unique(DataFinale$titre),showInLegend = FALSE,
                  
                  dataLabels = list(enabled = TRUE,format = "{point.y:,.1f}"))  %>%
    
    hc_xAxis(type="category",labels=list(enabled=TRUE),title=list(text=""),tickmarkPlacement='on',tickInterval=1) %>%
    
    #hc_xAxis(labels=list(enabled=TRUE),title=list(text="Pays"),tickmarkPlacement='on',tickInterval=1) %>%
    
    hc_title(text=ifelse(length(unique(DataFinale$unite_temps))>1,paste0(unique(DataFinale$titre)),paste0(unique(DataFinale$titre)," ","(",unique(DataFinale$unite_temps),")") ),
             margin = 20, align = "left",
             style = list(color = "#000000",fontWeight = "normal",fontSize='13px',useHTML = TRUE)) %>%
    
    hc_plotOptions(series = list(
      dataLabels = list(enabled=FALSE,style=list(color="#000000")),marker=list(enabled=FALSE),
      animation=FALSE,
      allowOverlap=TRUE,
      enableMouseTracking = TRUE))%>%
    
    hc_caption(
      text = str_c("Champ : ",unique(DataFinale$champ),
                   "<br/> Source :",unique(DataFinale$source),
                   "<br/> Unité de compte :",unique(DataFinale$unite_de_compte),
                   "<br/> Lecture :",ifelse(is.na(unique(DataFinale$Lecture)),"Note de lecture",unique(DataFinale$Lecture)),
                   sep = " "),
      style = list(fontWeight = "normal",fontSize = "11px"),
      align = "left",verticalAlign = 'bottom'
      
    ) 
  
})



  
# observeEvent(input$Comparaisonseuropeennes,{
# 
#     output$CI_taux_100hbts<-renderHighchart({
#       
#       DataFinale <- ReactiveCI()[order(ReactiveCI()$value),] 
#       
#       highchart() %>%
#         
#         hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
#                      buttons=list(
#                        contextButton=list(
#                          y=23,
#                          x=0,
#                          text= "Télécharger",
#                          menuItems=DowloadChart,
#                          symbol=''))) %>%
#         
#         hc_add_series(data = DataFinale,hcaes(x=sous_indicateurs,y=as.numeric(value),color = color_choice), 
#                       
#                       type = 'column',
#                       
#                       name=unique(DataFinale$titre),showInLegend = FALSE,
#                       
#                       dataLabels = list(enabled = TRUE,format = "{point.y:,.1f}"))  %>%
#         
#         hc_xAxis(type="category",labels=list(enabled=TRUE),title=list(text=""),tickmarkPlacement='on',tickInterval=1) %>%
#         
#         #hc_xAxis(labels=list(enabled=TRUE),title=list(text="Pays"),tickmarkPlacement='on',tickInterval=1) %>%
#         
#         hc_title(text=ifelse(length(unique(DataFinale$unite_temps))>1,paste0(unique(DataFinale$titre)),paste0(unique(DataFinale$titre)," ","(",unique(DataFinale$unite_temps),")") ),
#                  margin = 20, align = "left",
#                  style = list(color = "#000000",fontWeight = "normal",fontSize='13px',useHTML = TRUE)) %>%
#         
#         hc_plotOptions(series = list(
#           dataLabels = list(enabled=FALSE,style=list(color="#000000")),marker=list(enabled=FALSE),
#           animation=FALSE,
#           allowOverlap=TRUE,
#           enableMouseTracking = TRUE))%>%
#         
#         hc_caption(
#           text = str_c("Champ : ",unique(DataFinale$champ),
#                        "<br/> Source :",unique(DataFinale$source),
#                        "<br/> Unité de compte :",unique(DataFinale$unite_de_compte),
#                        "<br/> Lecture :",ifelse(is.na(unique(DataFinale$Lecture)),"Note de lecture",unique(DataFinale$Lecture)),
#                        sep = " "),
#           style = list(fontWeight = "normal",fontSize = "11px"),
#           align = "left",verticalAlign = 'bottom'
#           
#         ) 
#       
#     })
#     
#   },ignoreNULL=FALSE)
  
  

observeEvent(input$CI_a, {showInfo(ReactiveCI())}) 
  
}