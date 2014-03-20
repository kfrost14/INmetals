
options(RCHART_WIDTH = 700)
options(RCHART_HEIGHT = 600)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session)
  {
  
  
  datasetInput <- reactive({
    

    if(input$contab==1){
      
    
    if(input$County==("All Counties")){
       
            dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]  
            dataset = dataset[dataset$Year.GMT %in% seq(input$year[1], input$year[2]), ]}
    
    else{       dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]
                dataset = dataset[dataset$NAME==input$County,]
                dataset = dataset[dataset$Year.GMT %in% seq(input$year[1], input$year[2]), ]} 

    return(dataset) 
   
    }
  })
   
  
  parameterInput<-reactive({
  
  
    
    
    if(input$contab==1){
     
      
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      param = dataset[[11]]
      
      return(param)} else{return()}
    
    
    
  })
  
  
  countyInput<- reactive({
    
    if(input$contab==1){
      
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]
      county = dataset[[27]]
    return(county)} else{return()}
  })
  
  
  siteInput<-reactive({

    
    if (input$contab==1){
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      site = dataset[dataset$AQS.Parameter.Desc==input$DESCRIPTION,]
      site = site[site$DESCRIPTION==input$DESCRIPTION,]
      site = site[site$NAME==input$County,]
           
    }
      
      else{
        if(input$contab==2){      
                  
                  site = wswd[wswd$NAME==input$county,]
                                 
                                  
        }   
                                  else{
                                    if (input$contab==3){
                                      
                                      
                                      LatLon<-c("41.6035:-87.3326", "41.6121:-87.3263", "41.5995:-87.3443")
                                      Tip<-c("Gary Xact Monitor","US Steel Garyworks","Jefferson Elementary School")
                                      site <- data.frame(LatLon,Tip)
                                  
                                    }
                                      
                                      else{
                                        if(input$contab==4){ 
                                                                                  
                                          site = emissions[emissions$YEAR ==input$NEIyear, ]
                                          site = site[site$Pollutant_Code_Desc==input$pollutant,]
                                          site = site[site$name==input$CountyName,]
                                          
                                          #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
                                          }
                                        else{
                                          if(input$contab==5){
                                            site = emissions[emissions$YEAR =="2008", ]
                                            site = site[site$Pollutant_Code_Desc=="Lead",]
                                            site = site[site$name=="Marion",]
                                          }
                                        }
                                       
                                      }
                                  }
                                        
                                      }
                                    
                                  
       return(site)
     
  })
  

  
  wswdInput<-reactive({ 

    
    if(input$contab==2){
      wswd = wswd[wswd$NAME==input$county,]
      wswd = wswd[wswd$YEAR %in% seq(input$Year[1], input$Year[2]), ]
      wswd = wswd[wswd$MONTH %in% seq(input$Month[1], input$Month[2]), ]
        return(wswd)} else{return()}
                       })
  
  xactInput<-reactive({ 
    
 
   if(input$contab==3){
      
      test<-which(colnames(xact)==input$param)
      xact.vec<-xact[c(eval(test))]
      
      xact = data.frame(xact[,1],xact.vec,xact[,28:37])
      colnames(xact)<-c("date","param","YEAR","MONTH","DAY","HOUR","ws","wd","peak wind gust","std dev hz wd","vert wd","temp")
#       xact = xact[xact$date %in% seq(input$Date[1], input$Date[2]), ]
      
      return(xact)} else{return()}
    
        
  })

    emissionsInput<-reactive({ 
   
      
      if(input$contab==4){
   
        
        emissions = emissions[emissions$Pollutant_Code_Desc==input$pollutant,]
        emissions = emissions[emissions$name==input$CountyName,]
        emissions = emissions[emissions$YEAR==input$NEIyear, ]
       
        
    return(emissions)} else{return()}
                        
  
    })
  
  emissionsInput2<-reactive({ 
    
    
    if(input$contab==4){
      
      
      emissions = emissions[emissions$Pollutant_Code_Desc==input$pollutant,]
      emissions = emissions[emissions$YEAR==input$NEIyear, ]
      
      
      return(emissions)} else{return()}
  
    
  })
  
 
  pollInput<-reactive({ 

    
    
    if(input$contab==4){
      
      emissions = emissions[emissions$YEAR ==input$NEIyear, ]
      #emissions = emissions[emissions$COUNTY_NAME==input$CountyName,]
      emissionspollutant = emissions[[27]]
      
      #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
      return(emissionspollutant)}  else{return()}
  })  
  
  
  ecountyInput<-reactive({ 
   
 
    
    if(input$contab==4){
      
    
      emissionscounty = emissions[[83]]
      
      #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
      return(emissionscounty)} else {return()}
  })
  

  output$availparam<-renderUI({
    

      out<-parameterInput()
       params<-out
 
   
        selectInput("DESCRIPTION", "Choose a parameter:", 
                choices = sort(unique(params))
       ,
                          selected="Lead (TSP) LC")
      })   
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$DESCRIPTION, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file,row.names=F)
  
    })
      
      output$availcounty<-renderUI({
 

    out2<-countyInput()
    
    counties<-out2
    
    selectInput(inputId ="County",
                label = "Choose a county:",
                choices = c("All Counties",as.character(sort(unique(counties))))
                ,
                
               selected="All Counties")
   
})
  
  output$availemisscounty<-renderUI({
    
    s<-emissionsInput()
    #s2<-unique(s[83])
    s2<-s[83]
    out<-ecountyInput()
    
    counties<-out
    
    
    selectInput(inputId ="CountyName",
                label = "Choose a county:",
                choices = as.character(sort(unique(counties)))
    ,
    
   selected="Porter")
    
    
  })

  output$availcomp<-renderUI({

    
    out<-pollInput()
    params<-out
  
  
  selectInput("pollutant", "Choose the emissions category of metals:", 
              choices = sort(unique(params))
  ,
  selected="Lead")
  
})  
  
  
  output$inRadio<-renderUI({
    
    radioButtons("NEIyear",
                 "Select an NEI data year to view:",
                 choices=levels(as.factor(emissions$YEAR)),
                 selected="2008")
    
  })


  output$sideplot1<-renderPlot(function(){
    
  
    
    if(input$contab==1){
      
      
    
    site<-datasetInput()
    all<-map_data("county","indiana")
    
    p <- ggplot()
    p <- p + geom_polygon( data=all, aes(x=long, y=lat, group = group),color="gray") + coord_equal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    p <- p + geom_point(data=site, aes(x=LONG, y=LAT), color="coral1",size=4) + coord_equal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    #p <- p + geom_text( data=site, hjust=0.5, vjust=-0.5, aes(x=LONG, y=LAT, label=CITY), colour="gold2", size=3 )
    print(p)  
  }
})
   
    output$sideplot2<-renderPlot({
    
 
      
      if(input$contab==2){
        

        
    site2<-wswdInput()
    all<-map_data("county","indiana")
    p <- ggplot()
    p <- p + geom_polygon( data=all, aes(x=long, y=lat, group = group),colour="gray")+  coord_equal()+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    p <- p + geom_point(data=site2, aes(x=lon, y=lat), color="coral1",size=4) 
    print(p)
    }
    })
    
    output$sideplot3<-renderGvis({
    
      
      
      if(input$contab==3)  {
     
  
        
    site3<-siteInput()
    
    gvisMap(site3,"LatLon","Tip",
               options=list(displayMode = "Markers",  
                            useMapTypeControl=TRUE, showTip=TRUE,
                            enableScrollWheel=TRUE, width="85%"))
    }
      
    })

  
  output$main_plot <- renderPlot({
    
# 
#     withProgress(session, min=1, max=5, expr={
#       for(i in 1:5) {
#         setProgress(message = 'Calculation in progress',
#                     
#                     value=i)
#         print(i)
#         Sys.sleep(0.5)
#       }
#     })
    
    dataset<-datasetInput()
    plotlab<-input$DESCRIPTION
    plotlab2<-input$chart_type
    chart_title<-paste(plotlab,plotlab2,"by County and Site",sep=" ")
    
    
    
    if(input$chart_type=="Boxplot"){p<-ggplot(dataset,aes(factor(Year.GMT),Sample.Measurement))
                                    q<-p + geom_boxplot() + facet_wrap( ~ NAME+Site.Num,scales="free") + xlab("") + ylab("Concentration (ug/m3)") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))  #+ scale_y_log10()
                                    
        
  } else {  
     if(input$chart_type=="Trend")  {p<-ggplot(dataset,aes(x=as.Date(Date.GMT),y=Sample.Measurement))
                                    q<-p+ geom_point()+ stat_smooth(method="lm",se=TRUE) + facet_wrap( ~ NAME+Site.Num,scales="free") + scale_x_date()+ xlab("") + ylab("Concentration (ug/m3)") + labs(title=chart_title) + theme(strip.text.x=element_text(colour="black")) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="grey20",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
                                                                           
                                 
    }  else {
        if (input$chart_type=="Barplot"){p<-ggplot(dataset,aes(factor(Year.GMT))) 
                                       q<-p +geom_bar() + facet_wrap( ~ NAME+Site.Num,scales="free") + ylab("# of Samples Collected") + xlab("") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
                                                                        
               
     }   else {
         if (input$chart_type=="Histogram")
                                  
                                  {p<-ggplot(dataset,aes(x=Sample.Measurement))
                                  q<-p + geom_histogram() + facet_wrap( ~ NAME+Site.Num,scales="free") + xlab("Concentration") + ylab("Count") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
    
                    }}}}
  
      print(q)
    },height=675)
  
 
  output$main_plot2 <- renderPlot({
    
   wswd<-wswdInput()
   plotlab2<-input$county
   pr<- pollutionRose(wswd,ws="ws",wd="wd",pollutant="pm25_lc",main=paste("PM 2.5 Pollution Rose for",plotlab2,"County"))
   perRo<-percentileRose(wswd,ws="ws",wd="wd",pollutant="pm25_lc",percentile=c(75,95,99),main=paste0("PM 2.5 Percentile Rose Plot for ",plotlab2," County"))
   print(perRo,position=c(0,0,0.5,1),more=TRUE)
   print(pr,position=c(0.5,0,1,1))
   
    }, height=600)

  output$main_plot3 <- renderPlot({
    
    
#     updateProgressBar(session,inputId="pb1",value=100,visible=TRUE,color="standard",striped=TRUE,animate=FALSE)
    
    
    xact<-xactInput()
    plotlab2<-input$param
    
    rfc2<-rfc[rfc$Parameter==input$param,2]
    rfc3<-rfc[rfc$Parameter==input$param,3]
    
   
    pr2<-pollutionRose(xact,ws="ws",wd="wd",pollutant="param",main=paste0("Pollution Rose Plot for 2012-2013 ",plotlab2," Data"))
    perR<-percentileRose(xact,pollutant="param", percentile=c(75,95,99),main=paste0("Percentile Rose Plot for 2012-2013 ",plotlab2," Data"))
    tp<-timePlot(xact,pollutant="param",key=FALSE,ylab="Concentration (ng/m3)",main=paste0("2012-2013 Hourly ",plotlab2," Data"),ref.y=c(rfc2,rfc3),log=FALSE)
    tp2<-timePlot(xact,pollutant="param",key=FALSE,ylab="Concentration (ng/m3)",avg.time="day",main=paste0("2012-2013 Daily ",plotlab2," Data"),ref.y=c(rfc2,rfc3),log=FALSE)
    
    
    print(perR,position=c(0,0.6,0.5,1),more=TRUE)
    print(pr2,position=c(0.5,0.6,1,1),more=TRUE)
    print(tp, position=c(0,0.3,1,0.6),more=TRUE)
    print(tp2, position=c(0,0,1,0.3))
   # print(grid.text(x=0.10,y=0.3,label="--- Rfc"))
    print(grid.text(x=0.15,y=0.28,label=" --- SAT Screening Level")) 
    
    
  },   height=800)
  

  
  output$myChart2 <- renderMap({
    
    
     site<-emissionsInput()
     all<-emissionsInput2()
    
    all2<-data.frame(all$FacilityName,as.numeric(all$LAT),as.numeric(all$LONG),all$EmissionsTPY)
    colnames(all2)<-c("facility","x","y","emissions")
     yr<-as.factor(all[1,59])
    
     site2<-data.frame(site$FacilityName,as.numeric(site$LAT),as.numeric(site$LONG),site$EmissionsTPY)
    
     colnames(site2)<-c("facility","x","y","emissions")
     em<-aggregate(.~facility*x*y,data=site2,sum)
     em2<-as.numeric(em$emissions)
    
    em_all<-aggregate(.~facility*x*y,data=all2,sum)
    em_all2<-as.numeric(em_all$emissions)
    
    
    sum_rad_all<-sum(em_all2)
    labels_rad_all<-(em_all2/sum_rad_all)*100
     
    radius_all<-cut(labels_rad_all,breaks=5,labels=c(8,11,13,16,19))
    lab<-cut(em_all2,breaks=5,dig.lab=1)
 
    leg<-cut(em_all2,breaks=5,labels=brewer.pal(5,"Blues"))
    
    
    year_all<-paste("Year:",yr,"<br>")
    facility_all<-paste("Facility:",em_all$facility,"<br>")
    emissions3_all<-paste(input$pollutant,"Emissions:",em_all$emissions, "TPY")
    popup_all<-paste(year_all,facility_all,emissions3_all)
    
    em2_all<-cbind(em_all,leg,radius_all,popup_all)
    em_lat<-mean(em$x)
    em_lon<-mean(em$y)
        
        
    map3 <- Leaflet$new()
    map3$setView(c(em_lat,em_lon),zoom = 10)
    map3$tileLayer(provider="Stamen.Terrain",maxZoom=18)
    
                
    dat_list_all <- toJSONArray2(em2_all, json = F)
    
    
    map3$geoJson(toGeoJSON(dat_list_all, lat = 'x', lon = 'y'),
                 onEachFeature = '#! function(feature, layer){
    layer.bindPopup(feature.properties.popup_all)
 } !#',
                 pointToLayer =  "#! function(feature, latlng){
    return L.circleMarker(latlng, {
      radius: feature.properties.radius_all,
      fillColor: feature.properties.leg,
      color: '#000',
      weight: 1,
      fillOpacity: 0.8
    })
 } !#"         
    )
    
    leg1<-levels(lab)
    leg2<- cbind(lower = signif(as.numeric( sub("\\((.+),.*", "\\1", leg1) ),1),
          upper = signif(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", leg1) )),1)
    leg3<-paste(as.numeric(leg2[,1]),as.numeric(leg2[,2]),sep="-")
    leg4<-paste(leg3, " (TPY)")
    
    map3$legend(position="bottomright", colors=brewer.pal(5,"Blues"),labels=leg4)
    
    map3
   # feature.properties.radius_all,
    
  })
  
  
})


