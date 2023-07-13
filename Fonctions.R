# Arrondir les valeurs

RoundValuesFunction<-function(liste){
  
  Resultat<-lapply(liste,function(x){
    
    if(x<=1000){
      
      resultat<-round(x,-1) # Arrondir à la dizaine
      
    } else if (x>1000) {
      
      resultat<-round(x,-2) # Arrondir à la centaine
    }
    
    return(resultat)
    
  })
  
}





# Fonction de mise en forme des pages...

PageLayoutFunction<-function(Titre,Liste){

div(class="container-fluid custom-container-fluid",

div(class="list-items",

tags$li(class="accueil-nav",a(href = route_link("/"), paste0(h4("Accueil")) %>% lapply(htmltools::HTML)),style="padding-right:10px;"),

tags$li(class="icon-nav",HTML('<span style="background-color:#fff;color:#000;font-size:16px;font-weight:normal;">&#10148;</span>'),style="padding:10px;"),

tags$li(h4(Titre))


),Liste)}



# Bouton icône pour chaque figure

Button<-function(ID){ actionButton(ID,"",icon = icon("info")) }



SingleValueChart<-function(InfoButton,data,source="admin"){ 

  if(source=="admin"){
    
    Title="Données enregistrées par la police et la gendarmerie nationales ou autres données administratives"
    
    CSSstyle="color:#000091;background-color:#ececfe;display:flex;justify-content:space-between; align-items:center;padding:0px 10px;"
    

  } else if(source=="enq"){
    
    Title="Données issues d'enquêtes en population générale ou d'autres enquêtes"
    
    CSSstyle="color:#fc5d00;background-color:#ececfe;display:flex;justify-content:space-between; align-items:center;padding:0px 10px;"

  }
  
  
  if(data$statistiques=="Nombre"){
    
    HtmlText<-HTML( 
      
      '<span style="font-size:50px;color:#313178;font-weight:bold;">',format(round(data$value,-2),big.mark=" ",scientific=FALSE),'</span>','<br>',
      
      '<span style="font-size:18px;color:#313178;font-weight:bold;">',data$Lecture,'</span>', '<br>',
      
    )
    

  } else {
    
    HtmlText<-HTML( 
      
      '<span style="font-size:80px;color:#313178;font-weight:bold;">',paste0(data$value,"%"),'</span>','<br>',
      
      '<span style="font-size:18px;color:#313178;font-weight:bold;">',unique(data$Lecture),'</span>'
      
    )
    

  }
 
  cards_value<-div(class="col-md-6",
                   
    div(class="col-md-12 card-container",
                       
       tags$div(class="card",style="height:650px;position:relative;max-height:800px;",
                                
       div(class="card-header",style=CSSstyle,
                                    
        tags$h5(class="card-title",Title),
                                    
        tags$div(class='infobutton-admin',InfoButton)),
                                
        br(),
                                
        tags$p(class="card-text",ifelse(length(unique(data$unite_temps))>1,paste0(unique(data$titre)),paste0(unique(data$titre)," ","(",unique(data$unite_temps),")") ) ),
                                
        tags$div(class="card-body single-value-chart",HtmlText,

        style="text-align:center;position:absolute;top:40%;left:40%;transform:translate(-40%,-40%);"),
                                
        div(class="card-footer",style="position:absolute;bottom:100px;",
                                    
        tags$li("Champ(s) :", unique(data$champ),class="list-infos"),
                                    
        tags$li("Source(s) :",unique(data$source),class="list-infos")
                                    
)))) 
  
  
}






BoxForChart<-function(Link,Chart,Selector=NULL,SelectInputFunction=NULL,FullPage=NULL,source="admin"){
  
  if(source=="admin"){
    
    Title="Données enregistrées par la police et la gendarmerie nationales ou autres données administratives"
    
    CSSstyle="color:#000091;background-color:#ececfe;display:flex;justify-content:space-between;align-items:center;padding:0px 10px;"
    
    
  } else if(source=="enq"){
    
    Title="Données issues d'enquêtes en population générale ou d'autres enquêtes"
    
    CSSstyle="color:#fc5d00;background-color:#ececfe;display:flex;justify-content:space-between;align-items:center;padding:0px 10px;"
    
  }
  

  if(is.null(Selector)){
    
    div(class="col-md-6",
        
        div(class="col-md-12 card-container",
            
            tags$div(class="card card-admin",
                     
                     div(class="card-header card-header-admin",style=CSSstyle,
                         
                         tags$h5(class="card-title cards-title-admin",Title),
                         
                         tags$div(class='infobutton-admin',Link)),
                     
                     tags$div(class="card-body cards-body-admin",style="background-color:#fff;",Chart),
                     
                     
            )
            
            
        )
        
        
    )
    
  } else if (Selector==TRUE & is.null(FullPage)){
    
    Resultat<-div(class="col-md-6",
                  
                  div(class="col-md-12 card-container",
                      
                      tags$div(class="card card-admin",
                               
                               div(class="card-header card-header-admin",style=CSSstyle,
                                   
                                   tags$h5(class="card-title cards-title-admin",Title),
                                   
                                   tags$div(class='infobutton-admin',Link)),
                               
                               tags$div(class="card-body cards-body-admin",style="background-color:#fff;",div(class="col-md-12",SelectInputFunction),Chart))
                      
                      
                  ))
    
    
  } else if (Selector==TRUE & FullPage==TRUE){
    
    Resultat<-div(class="col-md-12",
                  
                  div(class="col-md-12 card-container",
                      
                      tags$div(class="card card-admin",
                               
                               div(class="card-header card-header-admin",style=CSSstyle,
                                   
                                   tags$h5(class="card-title cards-title-admin",Title),
                                   
                                   tags$div(class='infobutton-admin',Link)),
                               
                               tags$div(class="card-body cards-body-admin",style="background-color:#fff;",div(class="col-md-10 col-md-offset-3",SelectInputFunction),Chart))
                      
                      
                  ))
    
    
  } else if (Selector==FALSE & FullPage==TRUE){
    
    Resultat<-div(class="col-md-12",
                  
                  div(class="col-md-12 card-container",
                      
                      tags$div(class="card card-admin",
                               
                               div(class="card-header card-header-admin",style=CSSstyle,
                                   
                                   tags$h5(class="card-title cards-title-admin",Title),
                                   
                                   tags$div(class='infobutton-admin',Link)),
                               
                               tags$div(class="card-body cards-body-admin",style="background-color:#fff;",Chart))
                      
                      
                  ))
    
    
  }
  
}






outside_container<-function(FirstChart,secondChart=NULL){
  
  div(class="row",
      
  FirstChart,secondChart)

}





PaletteCouleurs<-c("#6a6af4","#000091","#9898f8","#aeaef9","#0078f3","#0063cb","#e1000f")


ChoixCouleursBar=c("#FF7F00","#1283f3","#77B5FE","#1560BD",'#710303',"#000080","#1b4f72","#7d3c98","#FF0000")
ChoixCouleurs=list("#FF7F00","#1283f3","#77B5FE","#1560BD",'#710303',"#000080","#1b4f72","#7d3c98","#FF0000")



Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}



DataTableCreation<-function(df,DownloadInfos){
  Table<-datatable(df,
   rownames = FALSE,
   extensions = c('Buttons'),
   options = list(
   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background': '#1a2980', 'color': '#fff'});","}"),
   columnDefs = list(list(targets='_all', orderable=FALSE)),
   pageLength = nrow(df),
   dom = 'Bt',
   deferRender = TRUE,
   scrollY = 400,
   scrollCollapse = TRUE,
   paging = FALSE,
   scroller = TRUE,
   buttons = list(
   list(extend = 'print',text='print',title=unique(DownloadInfos),filename=unique(DownloadInfos)),
   list(extend = 'csv',text='CSV',charset='utf-8',title=unique(DownloadInfos),filename=unique(DownloadInfos)),
   list(extend = 'excel',text='xlsx',title=unique(DownloadInfos),filename=unique(DownloadInfos)),
   list(extend = 'pdf',text='PDF',title=unique(DownloadInfos),filename=unique(DownloadInfos)))))
  return(Table)}



showInfo<-function(data){
  showModal(modalDialog(
    HTML('<strong> Titre :</strong>',ifelse(length(unique(data$unite_temps))>1,paste0(unique(data$titre)),paste0(unique(data$titre)," ","(",unique(data$unite_temps),")") ),'<br/>',
         '<br/>',
         '<strong> Définition : </strong> <br/>',ifelse(is.na(unique(data$definition)),"Note de lecture....",unique(data$definition)),'<br/>',
         '<br/>',
         '<strong> Unité de compte : </strong>',ifelse(is.na(unique(data$unite_de_compte)),"Unité de compte......",unique(data$unite_de_compte)),
         '<br/>',
         '<br/>',
         '<strong> Pour en savoir plus : </strong> <br/>',ifelse(is.na(unique(data$pour_en_savoir_plus)),"Pour en savour plus......",unique(data$pour_en_savoir_plus)),
         
    ), 
    easyClose = TRUE,
    footer = modalButton("Fermer"),
    size ="l"
  ))
  
}



mon_theme <- hc_theme(chart = list(backgroundColor = "#fff")) 

DowloadChart <- list(
  list(text = "PNG",onclick = JS("function () {this.exportChart({ type: 'image/png' });}")),
  list(text = "JPEG",onclick = JS("function () {this.exportChart({ type: 'image/jpeg' });}")),
  list(text="XLS",onclick=JS("function () {this.downloadXLS();}")),
  list(text = "PDF",onclick = JS("function () {this.exportChart({ type: 'application/pdf' });}")))




highchartFunction<-function(data,Type,ColorsList,liste,tag,HEADER="",FormatLabelsData,EnableLabels=TRUE,InfosPlus=NULL){ 
  
  if(is.null(InfosPlus)){
    
    AddPoints<-paste0(HEADER,'<br><span style="font-size: 13px"></span>')

  } else{
    
    AddPoints<-paste0(HEADER,'<br><br><span style="font-size: 13px">',glue('{InfosPlus}'),' : {point.key}</span><br>')
    
  }
  
  
  if(tag=="N"){
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.1f}")
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}')
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) 
    
  } else if(tag=="IntegerValues"){
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.0f}")
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}')
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) 
    
  } else if (tag=="stacking"){
    
    if(EnableLabels==TRUE){
      
      DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.0f}")
      
    }else{
      
      DataLabels <-list(enabled = FALSE)
    }
    
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}')
    
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) %>%
      
      hc_plotOptions(series = list(
        dataLabels = list(enabled=FALSE,style=list(color="#00561B")),marker=list(enabled=FALSE),
        animation=FALSE,
        stacking='normal',
        enableMouseTracking = TRUE))
    
    
  } else if (tag=="stackingP"){
    
    if(EnableLabels==TRUE){
      
      DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.0f}%")
      
    }else{
      
      DataLabels <-list(enabled = FALSE)
    }
    
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}%')
    
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) %>%
      
      hc_plotOptions(series = list(
        dataLabels = list(enabled=FALSE,style=list(color="#00561B")),marker=list(enabled=FALSE),
        animation=FALSE,
        stacking='normal',
        enableMouseTracking = TRUE))
    
    
  } else if (tag=="IntegerStackingP"){
    
    if(EnableLabels==TRUE){
      
      DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.1f}%")
      
    } else{
      
      DataLabels <-list(enabled = FALSE)
    }
    
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}%')
    
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) %>%
      
      hc_plotOptions(series = list(
        dataLabels = list(enabled=FALSE,style=list(color="#00561B")),marker=list(enabled=FALSE),
        animation=FALSE,
        stacking='normal',
        enableMouseTracking = TRUE))
    
    
  } else if (tag=="P"){
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = "{point.y:,.1f}%")
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}%')
    Chart<-data %>% hchart(
      Type,liste,
      color=ColorsList,
      dataLabels = DataLabels
    ) 
    
    
  }else if (tag=="NPIE"){
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}'))
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}')
    Chart<-data %>% hchart(
      Type,liste,
      colors=ColorsList,
      dataLabels = DataLabels
      
    ) 
    
  } else if (tag=="PPIE"){

    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}%'))
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}%')
    
    Chart<-data %>% hchart(
      Type,liste,
      colors=ColorsList,
      dataLabels = DataLabels
    ) 
    
  } else if (tag=="SampleDonut"){
    if(data$statistiques=="Nombre"){
      DataLabels <-list(enabled = FALSE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}'))
      FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}')
      Chart<-highchart() %>% hc_add_series(data = data,liste,colors=ColorsList,
                                           type=Type,name="",innerSize="70%",showInLegend = FALSE,
                                           dataLabels=DataLabels) %>%
        
        hc_title(text=paste0(unique(data$titre)," ","(",unique(data$unite_temps),")"),
                 margin = 20, align = "left",
                 style = list(color = "#000000", fontSize='13px',fontWeight = "normal",useHTML = TRUE)) %>% 
        
        hc_subtitle(
          text = str_c(  
            format(round(data$value,-2),big.mark=" ",scientific=FALSE),"<br/>",        
            data$unite_de_compte,
            sep = " "),
          style = list(fontWeight = "normal",fontSize='18px'), 
          floating=TRUE,verticalAlign = 'middle'
        ) 
      
    } else {
      DataLabels <-list(enabled = FALSE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}%'))
      FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.1f}%')
      Chart<-highchart() %>% hc_add_series(data = data,liste,colors=ColorsList,
                                           type=Type,name="",innerSize="70%",showInLegend = FALSE,
                                           dataLabels=DataLabels) %>%
        hc_title(text=paste0(unique(data$titre)," ","(",unique(data$unite_temps),")"),
                 margin = 20, align = "left",
                 style = list(color = "#000000", fontSize='13px',fontWeight = "normal",useHTML = TRUE)) %>% 
        
        hc_subtitle(
          text = str_c(
            "<h2>",paste0(data$value,"%"),"</h2>","<br/>",
            " ",
            sep = " "),
          style = list(fontWeight = "normal",fontSize='45px'), 
          floating=TRUE,verticalAlign = 'middle'
        ) 
      
      
    }
    
    
  }else if (tag=="NDonut"){ 
    
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}'))
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}')
    Chart<-highchart() %>% hc_add_series(data = data,liste,colors=ColorsList,
                                         type=Type,name="",innerSize="70%",showInLegend = FALSE,
                                         dataLabels=DataLabels)
    
  } else if (tag=="PDonut"){
    
    DataLabels <-list(enabled = TRUE,style=list(color="#000000",fontWeight='normal'),format = paste0('<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}%'))
    FormatLabels<-paste0('<br>','<span style="color:{point.color}">\u25CF</span>{',glue('point.{FormatLabelsData}'),'}: {point.y:,.0f}%')
    Chart<-highchart() %>% hc_add_series(data = data,liste,colors=ColorsList,
                                         type=Type,name="",innerSize="70%",showInLegend = FALSE,
                                         dataLabels=DataLabels)
    
  }
  
  Chart %>%
    hc_add_theme(mon_theme) %>%
    hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                 buttons=list(
                   contextButton=list(
                     y=23,
                     x=0,
                     text= "Télécharger",
                     menuItems=DowloadChart,
                     symbol=''))) %>%
    
    hc_xAxis(type="category",labels=list(enabled=TRUE),title=list(text=""),tickmarkPlacement='on',tickInterval=1) %>%
    hc_yAxis(labels=list(enabled=FALSE),title=list(text ='')) %>%
    hc_title(text=ifelse(length(unique(data$unite_temps))>1,paste0(unique(data$titre)),paste0(unique(data$titre)," ","(",unique(data$unite_temps),")") ),
             margin = 20, align = "left",
             style = list(color = "#000000",fontWeight = "normal",fontSize='13px',useHTML = TRUE)) %>%
    hc_plotOptions(series = list(
      dataLabels = list(enabled=FALSE,style=list(color="#00561B")),marker=list(enabled=FALSE),
      animation=FALSE,
      allowOverlap=TRUE,
      enableMouseTracking = TRUE))%>%
    
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat=FormatLabels,
               headerFormat=AddPoints
               
    ) %>%
    
    hc_caption(
      text = str_c("Champ(s) : ",unique(data$champ),
                   "<br/> Source(s) :",unique(data$source),
                   "<br/> Lecture :",ifelse(is.na(unique(data$Lecture)),"Note de lecture",unique(data$Lecture)),
                   sep = " "),
      style = list(fontSize = "11px"),
      align = "left",verticalAlign = 'bottom'
      
    ) %>%   

    hc_legend(

      layout="horizontal", align="center", verticalAlign="top", itemStyle = list(fontSize = "11px",fontWeight = "normal")

      )
  
}




Multiaxe<-function(FirstData,SecondData,ThirdData,Xaxis,Type,Color,
                   FirstLabelsFormat=list(enabled=FALSE,format = "{value::,.0f}",style=list(color="#000000",fontWeight="normal")),
                   FirstDataLabelsFormat=list(enabled=TRUE,format = "{point.y:,.0f}",style=list(color="#000000",fontWeight="normal")),
                   SecondDataLabelsFormat=list(enabled=TRUE,format = "{point.y:,.1f}%",style=list(color="#000000",fontWeight="normal"))){
  highchart() %>%
    hc_add_theme(mon_theme) %>%
    hc_exporting(enabled = TRUE,formAttributes = list(target = "_blank"),
                 buttons=list(
                   contextButton=list(
                     y=23,
                     x=0,
                     text= "Télécharger",
                     menuItems=DowloadChart,
                     symbol=''))) %>%
    hc_yAxis_multiples(
      list(labels=FirstLabelsFormat,opposite = FALSE,title = list(text=NULL) ),
      list(labels=list(enabled=FALSE,format = "{value::,.1f}%",style=list(color="#000000",fontWeight="normal")),opposite = TRUE, title = list(text=NULL) ))%>%
    hc_add_series_list(
      list(
        list(data=SecondData$value,name=unique(SecondData$sous_indicateurs),type=Type[1],color=Color[1],
             marker=list(enabled=FALSE),
             dataLabels=FirstDataLabelsFormat),
        list(data=ThirdData$value,name=unique(ThirdData$sous_indicateurs),type=Type[2],color=Color[2],
             marker=list(enabled=FALSE),
             dataLabels=SecondDataLabelsFormat,yAxis = 1)
      )

    ) %>%

    hc_xAxis(labels=list(enabled=TRUE),title=list(enabled=FALSE,text=""),categories=as.character(Xaxis),tickmarkPlacement='on',tickInterval=1) %>%
    hc_title(text=ifelse(length(unique(FirstData$unite_temps))>1,paste0(unique(FirstData$titre)),paste0(unique(FirstData$titre)," ","(",unique(FirstData$unite_temps),")") ),
             margin = 20, align = "left",
             style = list(color = "#000000",fontWeight = "normal",fontSize='13px',useHTML = TRUE)) %>%
    
    hc_plotOptions(column = list(
      animation=FALSE,
      animation=FALSE,
      enableMouseTracking = TRUE))%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: {point.y:,.1f}"),
               headerFormat = '<span style="font-size: 13px">{point.key}</span>'
               
    ) %>%
    
    hc_caption(
      text = str_c("Champ(s) : ",unique(FirstData$champ),
                   "<br/> Source(s) :",unique(FirstData$source),
                   "<br/> Lecture :",ifelse(is.na(unique(FirstData$Lecture)),"Note de lecture",unique(FirstData$Lecture)),
                   sep = " "),
      style = list(fontWeight = "normal",fontSize = "11px"),
      align = "left",verticalAlign = 'bottom'
      
    ) %>%
    hc_legend(layout = 'horizontal', align = 'center', verticalAlign = 'top', itemStyle = list(fontWeight="normal",fontSize = "11px",fontWeight = "normal"))
  
}










