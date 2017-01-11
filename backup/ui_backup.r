
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
ui <-shinyUI(pageWithSidebar(
  headerPanel(h1( a( "US election choropleth map generator", style = "font-weight: 200; line-height: 1.1; color: #000000;")),
  ),
  
  sidebarPanel(
    
    selectInput("chooseColor", "Choose Color Scheme:", c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples"
                                                         , "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens"
                                                         , "GnBu", "BuPu", "BuGn", "Blues")),
    checkboxInput("individualState","See Individual state"),
    
    selectInput("chooseStates","Choose state to view", selected = "North Carolina", c("Alabama","Alaska","Arkansas","Arizona","Connecticut"
                                                      ,"Colorado","California","Delaware","DC", "Florida","Georgia","Hawaii","Idaho","Illinois", "Indiana","Iowa"
                                                      ,"Kansas","Kentucky","Louisiana","Maine","Maryland", "Massachusetts", "Michigan", "Minnesota"
                                                      , "Mississippi","Missouri","Montana","Nebraska", "Nevada", "New Hampshire", "New Jersey"
                                                      , "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio","Oklahoma","Oregon","Pennsylvania"
                                                      , "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"
                                                      , "Washington", "West Virginia", "Wisconsin", "Wyoming")),

    selectInput("whatData","Choose the data for state",c("drugs","2016Results","2016ResultsCongress", "PVI","RomTrump","blackPop","PopDensity")),
    selectInput("whatFormat","Chooseformat to download map", c(".png",".pdf",".html")),
    checkboxInput("labelYes", "put Labels on the map"),
    checkboxInput("legendYes", "put a Legend on the map"),
    downloadButton("downloadMap","download Map"),
    downloadButton("downloadOutput","download csv")
    ),mainPanel(

tabsetPanel(id="tabs",

  tabPanel("Instructions", value="int", textOutput("text1") ,textOutput("text2"),textOutput("text3"),textOutput("text4")),
  # tabPanel("Graph", value="graph", plotOutput("graphTwo")),

  tabPanel("County-level Map", value="state", leafletOutput("drugMap", height = 750)),
  tabPanel("District-level Map", value="cong", leafletOutput("CongMap")),
  # tabPanel("Plot with Drug Rates", value="drugsM",plotOutput("graphThree")),
  tabPanel("About Authors", value="abaout",textOutput("text6"),h1(a(img(src = "ben.jpg", align = "left", width = "50%",length="50%"))),textOutput("text7"))
)

)


))
