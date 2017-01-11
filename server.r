rm(list=ls())
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
library(webshot)

server<-shinyServer(function(input, output){
  webshot::install_phantomjs()
  
  
  output$text1<-renderText({ "Generate maps of the United States by county for a collection of variables."})
  output$text2<-renderText({ "Select a variable in the sidebar, specify options, and view output in the Map tab."})
  output$text3<-renderText({ "By James Hennessy and Ben Berger"})
  
  output$text6<-renderText({ "James Hennessy is a BLAND and very AMATEUR dancer, who one day dreamed of being YUGE, but had to settle on DRY CLEANING PROFESSIONAL due to POLITICS"})
  output$text7<-renderText({"Ben Berger simply can't stack up 1000 PANCAKES, but most people would find this a difficult proposition."})
  
  
  st_fips <- read.csv("st_fips.csv")
  
  
  #Read in congressional district shape files
  getCongress<-reactive({
    cong<-readOGR(dsn="cb_2014_us_cd114_20m",layer="cb_2014_us_cd114_20m")
    cong<-cong[cong$STATEFP!=72,]
    cong<-cong[as.character(cong$STATEFP)!="02",]
    cong$NAME<-paste0(cong$STATEFP,cong$CD114FP)
    
    if(input$chooseStates != "United States"){
      number<-st_fips[st_fips$State== input$chooseStates,]$FIPS
      if(number<10){
        number<-as.character(number)
        number<-paste0("0",number)
        cong <- cong[cong$STATEFP == number,]
        
      }
      else{
        cong <- cong[cong$STATEFP == number,]
      }
    }
    cong<-cong
    
    
  })
  
  
  #Read in congressional district data
  getCongResults<-reactive({
    kos<-read.csv("bigData.csv")
    statesAbv <- read.csv("statesAbv.csv")
    
    names<-substr(kos$Code,1,2)
    dist<-substr(kos$Code,4,5)
    for(i in 1:length(names)){
      number<-statesAbv[which(names[i]==statesAbv$ABV),]$FIPS
      if(number<10){
        number<-as.character(number)
        number<-paste0("0",number)
        numberDist<-paste0(number,dist[i])
        kos$CDfull[i]<-numberDist
        kos$CDstate[i]<-number
        kos$CDdist[i]<-dist[i]
        
        
      }
      else{
        number<-as.character(number)
        
        numberDist<-paste0(number,dist[i])
        kos$CDfull[i]<-numberDist
        kos$CDstate[i]<-number
        kos$CDdist[i]<-dist[i]
        
      }
      if(!is.na(kos$Clinton16[i] )){
        if((kos$Clinton16[i])<(kos$Trump16[i])){
          kos$newWinner[i]<-"TRUE"
        }
        else{
          kos$newWinner[i]<-"FALSE"
        }
        
      }
      else{
        kos$newWinner[i]<-"NA"
      }
    }
    if(input$chooseStates != "United States"){
      number<-st_fips[st_fips$State== input$chooseStates,]$FIPS
      if(number<10){
        number<-as.character(number)
        number<-paste0("0",number)
        kos<-kos[which(kos$CDstate==number),]
        
      }
      else{
        kos<-kos[which(kos$CDstate==number),]
      }
    }
    kos<-kos
  })
  
  #read in county shapefiles
  getStates<-reactive({
    states <- readOGR(dsn="cb_2015_us_county_20m",layer="cb_2015_us_county_20m")
    states<-states[states$STATEFP!=72,]
    states<-states[as.character(states$STATEFP)!="02",]
    states<-states[states$NAME!="Kalawao",]
    states$FULLFP<-paste0(states$STATEFP,states$COUNTYFP)
    
    if(input$chooseStates != "United States"){
      number<-st_fips[st_fips$State== input$chooseStates,]$FIPS
      if(number<10){
        number<-as.character(number)
        number<-paste0("0",number)
        states <- states[states$STATEFP == number,]
        
      }
      else{
        states <- states[states$STATEFP == number,]
      }
    }
    states<-states
  })
  
  
  #Read in county-level data
  getData<-reactive({
    data <- read.csv("data.csv", header = T, sep = ",")
    states<-getStates()
    
    if(input$chooseStates != "United States"){
      
      num<-as.numeric(as.character(states$STATEFP))
      if(num<10){
        data<- data[data$StateFIPS==num,]
      }
      else{
        data <- data[data$StateFIPS==states$STATEFP,]
      }
      data<-data
    }
    ncounty <- length(states$COUNTYFP)
    
    data$winner <- "Hillary"
    data$winner[data$TrumpWin==1] <- "Trump"
    
    data<-data
    
  })
  
  #generate congressional district map
  output$CongMap<- renderLeaflet({
    finalCongMap()
  })
  
  finalCongMap<-reactive({
    cong<-getCongress()
    congResults<-getCongResults()
    for(i in 1:length(cong)){
      index<-match(cong$NAME[i],congResults$CDfull)
      cong$Incumbent[i]<-paste0(congResults$First_Name[index],congResults$Last_Name[index])
      cong$Party[i]<-congResults$Party[index]
      cong$Csix[i]<-congResults$Clinton16[index]
      cong$Tsix[i]<-congResults$Trump16[index]
      cong$Ot[i]<-congResults$Obama12[index]
      cong$Rt[i]<-congResults$Rom12[index]
      
    }
    data<-getData()
    states<-getStates()
    data <- data[order(order(as.numeric(as.character(states$GEOID)))),]
    #color <- rep('blue',length(congResults))
    #color[congResults$newWinner=="TRUE"]<- 'red'
    
    if (input$whatData == "RomTrump"){
      color <- colorBin(input$chooseColor, cong$Tsix , bins = 5)(cong$Tsix)
      
    }
    else if ( input$whatData == "2016Results"){
      
      color <- rep('green',length(congResults$Party))
      color[which(congResults$newWinner == "TRUE")]<- 'red'
      color[which(congResults$newWinner == "FALSE")]<- 'blue'
      
    }
    else if(input$whatData=="2016ResultsCongress"){
      color <- rep('blue',length(congResults$Party))
      color[which(congResults$Party == "Republican")]<- 'red'
    }
    else if(input$whatData=="PVI"){
      
      color <- rep('black',length(congResults$Party))
      color[which(congResults$PVI=="D+")]<-"blue"
      color[which(congResults$PVI=="R+")]<-"red"
      
    }
    else if(input$whatData=="PVInum"){
      color<-colorBin()
    }
    else{
      color <- colorBin(input$chooseColor, cong$Ot , bins = 8)(cong$Ot)
    }
    congMapper<-{
      leaflet(cong) %>%
        addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                     weight = .5, fill = T, fillColor = ~color)
    }
    if(input$labelYes){
      longLat<-read.csv("cd114_coordinates.csv")
      abbStates<-read.csv("states.csv")
      if(input$chooseStates != "United States"){
        intial<-abbStates$Abbreviation[which(abbStates$State==input$chooseStates)]
        
        long<-longLat$long[longLat$StateCode==toString(intial)]
        lat<-longLat$lat[longLat$StateCode==toString(intial)]
        names<-longLat$CD114_Name[longLat$StateCode==toString(intial)]
      }
      else{
        
        long<-longLat$long
        lat<-longLat$lat
        names<-longLat$CD114_Name
      }
      congMapper<-{ congMapper %>%
          addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(names),
                              labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
      }
    }
    if(input$legendYes){
      if(input$whatData=="2016Results"){
        pal<- colorFactor(c("blue","red"),
                          domain= c("Hillary","Trump"))
        value<-c("Hillary","Trump")
      }
      else{
        pal <- colorNumeric(
          
          palette   = input$chooseColor,
          domain = data$DrugDeathRate
        )
        value<-data$DrugDeathRate
      }
      value<-value
      pal<-pal
      congMapper<-congMapper%>%
        addLegend("bottomright", pal = pal, values = value,
                  title = input$whatData,
                  opacity = 1
        )
      
    }
    
    
    congMapper<-congMapper
    
  })
  
  
  
  
  
  ##County-level mapper
  output$countyMap<-renderLeaflet({
    finalMap<-finalMap()
  })
  
  finalMap<-reactive({
    data<-getData()
    
    states<-getStates()
    
    data <- data[order(order(as.numeric(as.character(states$GEOID)))),]
    if(input$chooseStates != "United States"){
      
      num<-as.numeric(as.character(states$STATEFP))
      if(num<10){
        data<- data[data$StateFIPS==num,]
      }
      else{
        data <- data[data$StateFIPS==states$STATEFP,]
      }
      data<-data
    }
    ncounty <- length(states$COUNTYFP)
    

#map user inputted variable choice to variable in data & color
    
    is_election = F
    is_trumpCapture = F
    
    #Election variables
    
      #Winner    
      if(input$whatData=="2016 Presidential election winner"){
        input_var <- data$TrumpWin
        color <- rep('blue',ncounty)
        color[input_var == 1]<- 'red'
        is_election = T
      }
      
      else if(input$whatData=="2012 Presidential election winner"){
        input_var <- data$gop_win_2012
        color <- rep('blue',ncounty)
        color[input_var == 1]<- 'red'
        is_election = T
      }
      
      else if(input$whatData=="2008 Presidential election winner"){
        input_var <- data$gop_win_2008
        color <- rep('blue',ncounty)
        color[input_var == 1]<- 'red'
        is_election = T
      }
        
      #Trump Capture
      else if(input$whatData=="Trump captures"){
        input_var <- data$TrumpFlip
        color <- rep('white', ncounty)
        color[input_var==T] <- 'black'
        is_trumpCapture = T
      }
    
    #Demographic variables
    
      #Ethnic population shares
      else if(input$whatData=="% of population White"){
        input_var <- data$WhiteShare 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
      
      else if(input$whatData=="% of population Black"){
        input_var <- data$BlackShare 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% of population Asian"){
        input_var <- data$AsianShare 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% of population Native American"){
        input_var <- data$AmerIndianShare 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% of population Multiracial"){
        input_var <- data$MultiRacialShare
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% of population Hispanic"){
        input_var <- data$HispShare
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% of population Male"){
        input_var <- data$MaleShare
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      #Median Age
      else if(input$whatData=="Median age"){
        input_var <- data$MedianAge 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
    #Population characteristics
    
      else if(input$whatData=="Population"){
        input_var <- log(data$Pop, 10)
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="Population density (persons per square mile)"){
        input_var <- log(data$PopDensity ,10)
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="% Increase in population (2011-2015)"){
        input_var <- data$PopPctChg 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
    #Education
    
      else if(input$whatData=="Bachelor's degree attainment"){
        input_var <- data$BA
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="Drug overdose rate (age-adjusted)"){
        input_var <- data$DrugDeathRate 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="Unemployment rate"){
        input_var <- data$UnemploymentRate 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="Labor force participation rate"){
        input_var <- data$LaborForce 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    
      else if(input$whatData=="Median household income"){
        input_var <- data$MedianHHIncome 
        color <- colorNumeric(input$chooseColor, input_var)(input_var)
      }
    

    
    map<-{
      leaflet(states) %>%
        addPolygons( stroke = input$borderYes, fillOpacity =.7, smoothFactor = 0, color = "black",
                     weight = .5, fill = T, fillColor = ~color) 
    }
    
    if(input$labelYes & input$chooseStates != "United States"){
      longLat<-read.csv("us_cty_area.csv")
      abbStates<-read.csv("states.csv")
      if(input$chooseStates != "United States"){
        intial<-abbStates$Abbreviation[which(abbStates$State==input$chooseStates)]
        
        long<-data$long[data$StateCode==toString(intial)]
        lat<-data$lat[data$StateCode==toString(intial)]
      }
      else{
        
        long<-data$long
        lat<-data$lat
      }
      map<-map%>%
        addLabelOnlyMarkers(~lat, ~long, label =  ~as.character(states$NAME),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, opacity = .8))
    }
    
    
#make legend    
    if(input$legendYes){
      
      #Election legend
      if(is_election == T){
        pal<- colorFactor(c("blue","red"),
                          domain= c("Dem","GOP"))
        value<-c("Dem","GOP")
      }
      
      
      #Trump capture legend
      else if(is_trumpCapture == T){
        pal<- colorFactor(c("black","white"),
                          domain= c("GOP hold or loss","GOP gain"))
        value<-c("GOP hold or loss","GOP gain")

      }
      
      
      
      else{
        pal <- colorNumeric(
          
          palette   = input$chooseColor,
          domain = input_var
        )
        value<-input_var
      }
      value<-value
      pal<-pal
      
      map<-map%>%
        addLegend("bottomright", pal = pal, values = value,
                  title = input$whatData,
                  opacity = 1
        )
      
    }
    
    else{
      map<-map
    }
    map<-map
  })
  
  
  
  
  
  
  #   output$genMap<-renderLeaflet({
  #     map<-getGenMap()
  #     states<-getStates()
  #     data<-getData()
  #     if(input$chooseStates != "United States"){
  #     }
  # 
  #     abbStates<-read.csv("states.csv")
  #     if(input$chooseStates != "United States"){
  #     intial<-abbStates$Abbreviation[which(abbStates$State==input$chooseStates)]
  # 
  #     long<-data$long[data$StateCode==toString(intial)]
  #     lat<-data$lat[data$StateCode==toString(intial)]
  #     }
  #     else{
  # 
  #       long<-data$long
  #       lat<-data$lat
  #     }
  #     map%>%
  #           addLabelOnlyMarkers(~lat, ~long, label =  ~as.character(states$NAME),
  #                                                        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
  #   })
  # 
  # 
  # generate map
  # getGenMap<-reactive({
  #   data<-getData()
  #   states<-getStates()
  #   if(input$chooseStates != "United States"){
  #   }
  #   data <- data[order(order(as.numeric(as.character(states$GEOID )))),]
  #   ncounty <- length(states$COUNTYFP)
  # 
  # 
  # color <- colorBin(input$chooseColor, data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
  # 
  # leaflet(states) %>%
  #   addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
  #                weight = .5, fill = T, fillColor = ~color
  #   )
  # })
  # 
  # output$mapRandomer<-renderLeaflet({
  #   color <- colorBin(input$chooseColor, data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
  # 
  #   leaflet(states) %>%
  #     addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
  #                  weight = .5, fill = T, fillColor = ~color
  #     )
  # })
  
  
  
  
  #   ##BOXPLOTS
  #   output$graphTwo<-renderPlot({
  #   data<-getData()
  #   bp <- ggplot(data = data, aes(x=data$DrugDeathRate, y=gop_margin_2016), order(as.numeric(data$DrugDeathRate))) + geom_boxplot(aes(fill=DrugDeathRate) )
  #
  #   bp <- bp + xlab( "Age-Adjusted drug deaths per 100,000 people") +
  #     ylab("Trump Victory Margin")
  #   bp <- bp + scale_fill_discrete(breaks=c("6.1-8","8.1-10","10.1-12","12.1-14","14.1-16","16.1-18","18.1-20",">20"))
  #   bp + ggtitle("Trump victory margin in North Carolina counties, by county drug overdose rate ")
  #   print(bp)
  #   })
  #
  #   ##BAR GRAPH
  #   output$graphThree <-renderPlot({
  #    # data$winner16 <- factor(data$winner16)
  #     #data$winner16 <- factor(data$winner16, levels = rev(levels(data$winner16)))
  #     data<-getData()
  #     bp2 <- ggplot(data, aes(DrugDeathRateCategory, fill = winner, order = as.numeric(DrugDeathRateCategory))) +
  #       geom_bar()
  #     bp2 <- bp2 + xlab( "Age-Adjusted drug deaths per 100,000 people") +
  #       ylab("Number of Counties")
  #     bp2 + ggtitle("2016 Election victor in State counties by county drug overdose rate")
  #   })
  #   ##REGRESSIONS
  #   getSummary<-renderText({
  #   summary(lm(TrumpPctVictory ~ RomneyPctVictory + DDR, data))
  #  # summary(glm(TrumpWin ~ RomneyWin + DDR,data,family="DDRomial"))
  #   cor(data$TrumpPctVictory, data$DDR)
  #   summary(lm(TrumpPctVictory ~ DDR, data[data$RomneyWin == F,])) ##effect of drug death on obama counties
  # })
  
  
  
  #download map
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste(input$chooseStates,input$whatFormat, sep='')
    },
    content = function(file) {
      #      src <- normalizePath('report.Rmd')
      if(input$tabs=="cong"){
        here<-finalCongMap()
        
        long<-((input$CongMap_bounds$north)+input$CongMap_bounds$south)/2
        latt<-((input$CongMap_bounds$west)+input$CongMap_bounds$east)/2
        zooms<-input$CongMap_zoom
      }
      else{
        here<-finalMap()
        
        long<-((input$countyMap_bounds$north)+input$countyMap_bounds$south)/2
        latt<-((input$countyMap_bounds$west)+input$countyMap_bounds$east)/2
        zooms<-input$countyMap_zoom
      }
      here<-here
      long<-((input$countyMap_bounds$north)+input$countyMap_bounds$south)/2
      latt<-((input$countyMap_bounds$west)+input$countyMap_bounds$east)/2
      
      heres<-here %>% setView(lng=latt, lat=long,zoom=zooms)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(heres, file="temp.html", selfcontained = F)
      
      webshot("temp.html", file = file,  vheight = 800, vwidth = 1500, 
              cliprect = "viewport")
    }
  )
  
  #download csv
  output$downloadData <- downloadHandler({
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$chooseStates, ".csv", sep = ".")
    }
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      sep <- ","
      
      # Write to a file specified by the 'file' argument
      write.table("data.csv", file, sep = sep,
                  row.names = FALSE)
    }
    
  })
}

)